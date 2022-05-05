
"""
Contains the main calcProofOfAbsence() function and supporting
functions for doing the calculations.
"""

# This file is part of Proof of Absence
# Copyright (C) 2016 Dean Anderson and Sam Gillingham
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

from __future__ import print_function, division
import numpy as np
import ctypes
import threading
import os
import sys
import copy
from numba import jit
from osgeo import gdal
from osgeo import gdalconst
from rios import calcstats
from rios.cuiprogress import GDALProgressBar

from proofofabsence import params

# find libpython and extract the locking functions
# so we can lock access to some arrays from threads
LOCK_INIT = ctypes.pythonapi.PyThread_allocate_lock
LOCK_INIT.argtypes = None
LOCK_INIT.restype = ctypes.c_void_p

LOCK_DESTROY = ctypes.pythonapi.PyThread_free_lock
LOCK_DESTROY.argtypes = [ctypes.c_void_p]
LOCK_DESTROY.restype = ctypes.c_int

LOCK_LOCK = ctypes.pythonapi.PyThread_acquire_lock
LOCK_LOCK.argtypes = [ctypes.c_void_p, ctypes.c_int]
LOCK_LOCK.restype = ctypes.c_int

LOCK_UNLOCK = ctypes.pythonapi.PyThread_release_lock
LOCK_UNLOCK.argtypes = [ctypes.c_void_p]
LOCK_UNLOCK.restype = None

# global locking for tricky sections
THREAD_LOCK = None

class POAException(Exception):
    "Base class for POA exceptions"

class InputParameterError(POAException):
    "There was something wrong with the input parameters"

class Results(object):
    "An instance of this class is returned from calcProofOfAbsence()"
    # sensitivity per year and per iteration
    sensitivityMatrix = None
    # Proof of Freedom per year and per iteration
    poFMatrix = None
    # params that were passed in 
    params = None
    # sensitivity per zone and per iteration
    zoneSeMatrix = None
    # number of zones processed
    numberZones = None
    # names of the zones for plotting
    Name_zone = None
    # the initial priors for each iteration; for plotting
    priorStore = None
    # proportion searched for each year in full extent
    proportionSearchedExtent = None
    # proportion searched for each zone
    proportionSearchedZone = None
    # output mean sensitivity raster - one layer per year
    meanSeuTifPathName = None
    # output relative risk raster (may have been modified from input due to buffering)
    relRiskTifName = None
    # output updated zone raster
    extZoneTifName = None
    # number of rows in cols in the above rasters
    rows = None
    cols = None
    # GDAL geotranform array used by the rasters
    match_geoTrans = None

def calcProofOfAbsence(poaparams, trapArray, RRArray, zoneArray, zoneCodes, match_geotrans, 
        wkt, outputDataPath, RR_zone, Pu_zone, Name_zone):
    """
    Main function. Parameters are:
        * an instance of params.POAParameters
        * a 1d trap array
        * a 2d array with the Relative Risk values.
        * a 2d array, != 0 where processing is to take place. Contains zone values
        * a 1d array containing the unique values for zones in the previous array
        * a GDAL style geotransform array to use when finding traps on the raster
        * a string with the WKT to use when writing out rasters
        * The path to use when outputting the result rasters
        * a 1d array containing RR_zone values for each zone (same order as zoneCodes)
        * a 1d array containing Pu_zone values for each zone (same order as zoneCodes)
        * a 1d array containing the names of the zones for presentation to the user (same order as zoneCodes).

    Returns a Results object. See above.
    
    """
    transform = np.array(match_geotrans) # ensure array to keep numba happy
    nZones = zoneCodes.size

    # some basic checking
    if poaparams.prior_min is None or poaparams.intro_min is None or poaparams.years is None:
        msg = 'Parameter object has not been completely filled in'
        raise InputParameterError(msg)

    if RRArray.shape != zoneArray.shape:
        msg = 'Input arrays must be the same shape'
        raise InputParameterError(msg)

    # do masking by minimum RR
    # keep original for addKBufferAroundTraps()
    origZoneArray = zoneArray.copy()

    # set zone to 0 where RR less than minRR - will be ignored
    zoneArray[RRArray < poaparams.minRR] = 0

    if poaparams.RRTrapDistance > 0.0:
        # need to mask around traps within that distance
        # find the value of K to fill in with
        maxRR = RRArray[zoneArray != 0].max() / 2.0
        if poaparams.minRR > maxRR:
            maxRR = poaparams.minRR

        # keep Numba happy
        years = np.array(poaparams.years)
        # converting a set to an array requires going via a list.
        RRBufferAnimals = np.array(list(poaparams.RRBufferAnimals))
        # do the buffering
        addRRBufferAroundTraps(trapArray, RRBufferAnimals, years, 
                RRArray, zoneArray, origZoneArray, poaparams.RRTrapDistance, 
                poaparams.minRR, maxRR, transform)


    # Do here as operations above may have changed it
    nTotalCells = (zoneArray != 0).sum()

    # get the sum of the k to use for weighted averages
    (sumRRWeights, rrHxZoneWeights, nCellsInZones) = getSumRRWeights(RRArray, 
                zoneArray, zoneCodes, RR_zone)

    if (transform[1] + transform[5]) > (transform[1] * 0.1):
        msg = 'input pixels must be square'
        raise InputParameterError(msg)

    # initialise result arrays
    sensitivityList = []
    nTotalYears = poaparams.years[-1] - poaparams.years[0] + 1
    SSeMat = np.zeros((nTotalYears, poaparams.nIter))
    PoFMat = np.zeros((nTotalYears, poaparams.nIter))
    priorStore = np.zeros(poaparams.nIter, dtype = float)
    ## FOR STORING ZONE-SPECIFIC RESULTS
    zoneSeResults = np.zeros((nTotalYears, poaparams.nIter, nZones))

    ## THIS IS FOR THE ENTIRE AREA PROPORTION SEARCHED
    proportionSearchedExtent = np.zeros(nTotalYears)
    ## PROPORTION SEARCHED FOR EACH ZONE
    proportionSearchedZone = np.zeros((nTotalYears, nZones))

    prior_range = poaparams.prior_max - poaparams.prior_min
    intro_range = poaparams.intro_max - poaparams.intro_min

    gridSurveyParams = poaparams.gridSurveyParams
    gridSurveyData = poaparams.gridSurveyData
    
    # work out the iterations for each thread
    if poaparams.nthreads > 1:
        nItersPerThread = int(np.ceil(poaparams.nIter / poaparams.nthreads))
        nIterList = []  # tuple of (start, end)
        for n in range(poaparams.nthreads):
            start = n * nItersPerThread
            end = start + nItersPerThread
            if end > poaparams.nIter:
                end = poaparams.nIter

            nIterList.append((start, end))

    # create the locks, we do this even if nthreads=1 to make it easier
    global THREAD_LOCK 
    THREAD_LOCK = LOCK_INIT()

    Pz = poaparams.Pz       ## ZONE DESIGN PREVALENCE

    print('Pz', Pz)

    # with all the missing years filled in
    yearSeq = range(poaparams.years[0], poaparams.years[-1] + 1)

    print('yearSeq', yearSeq, 'ntotalyears', nTotalYears, 
        'paramyears',poaparams.years)

    for yearCount in range(nTotalYears):
        # probability of not detecting = 1
        sensitivityRaster = np.zeros_like(zoneArray, dtype=np.float)
        currentYear = yearSeq[yearCount]
        # calc PStar for year
        PStar = Pu_zone + (float(yearCount) * poaparams.puRate) ## EITHER SCALAR OR ARRAY
        print('year = ', currentYear, 'PuYear = ', PStar, 'RR_zone', RR_zone)

        if currentYear in poaparams.years:
            # is a surveillance year

            # do the iterations - under this level is all numba
            if poaparams.nthreads == 1:
                # single thread. Do all the iterations for this year.
                doIterations(0, poaparams.nIter, sensitivityRaster, zoneSeResults, 
                    proportionSearchedExtent, proportionSearchedZone, SSeMat, 
                    PoFMat, RRArray, yearCount,
                    currentYear, gridSurveyParams, gridSurveyData, 
                    poaparams.parameterArray, trapArray, zoneArray, zoneCodes,
                    sumRRWeights, rrHxZoneWeights, Pu_zone,
                    transform, poaparams.nChewcardTraps, PStar, Pz, nTotalCells,
                    poaparams.prior_a, poaparams.prior_b, poaparams.prior_min, 
                    poaparams.prior_max, prior_range, poaparams.intro_a, poaparams.intro_b,
                    poaparams.intro_min, poaparams.intro_max, intro_range, priorStore)
            else:
                # multithreaded option - start a thread for each start, end in nIterList
                # for this year.
                threads = []
                for start, end in nIterList:
                    thread = threading.Thread(target=doIterations, args=(start, end, 
                            sensitivityRaster, zoneSeResults, proportionSearchedExtent,
                            proportionSearchedZone, SSeMat, PoFMat, RRArray, yearCount, currentYear,
                            gridSurveyParams, gridSurveyData,  
                            poaparams.parameterArray, trapArray, zoneArray, zoneCodes,
                            sumRRWeights, rrHxZoneWeights, Pu_zone, 
                            transform, poaparams.nChewcardTraps, PStar, Pz, nTotalCells,
                            poaparams.prior_a, poaparams.prior_b, poaparams.prior_min, 
                            poaparams.prior_max, prior_range, poaparams.intro_a, poaparams.intro_b,
                            poaparams.intro_min, poaparams.intro_max, intro_range, priorStore))
                    threads.append(thread)

                # start them
                for thread in threads:
                    thread.start()
                # wait for them to finish
                for thread in threads:
                    thread.join()

            # we have just been adding things to sensitivityRaster so divide 
            # it by the total number of iterations to get the average for this year 
            sensitivityRaster = sensitivityRaster / poaparams.nIter
            
            # reset to zero where we aren't using it (zone==0)
            # set to one every above
            sensitivityRaster[zoneArray == 0] = 0

            # same thing for proportionSearchedExtent - first find mean
            proportionSearchedExtent[yearCount] = (
                np.sum(proportionSearchedZone[yearCount]) / poaparams.nIter)
            # then change back to proportion
            proportionSearchedExtent[yearCount] = proportionSearchedExtent[yearCount] / nTotalCells
            
            ## Proportion searched in each zone - mean across iterations for each zone
            proportionSearchedZone[yearCount] = proportionSearchedZone[yearCount] / poaparams.nIter
            # then change back to proportion

            proportionSearchedZone[yearCount] = proportionSearchedZone[yearCount] / nCellsInZones
            
        else:
            # no surveillance this year
            if yearCount == 0:
                # first year - calc for all iterations
                PrPrior = doBetaPertArray(poaparams.prior_a, poaparams.prior_b, 
                            poaparams.prior_min, poaparams.prior_max, prior_range,
                            size=poaparams.nIter)
                PoFMat[yearCount] = PrPrior
                priorStore = PrPrior.copy()
            else:
                for currentIteration in range(poaparams.nIter):
                    PrIntro = doBetaPert(poaparams.intro_a, poaparams.intro_b,
                            poaparams.intro_min, poaparams.intro_max, intro_range)
                    # Equation 1 
                    # go through each iteration for last year and do it based on that 
                    PoFLastYear = PoFMat[yearCount - 1, currentIteration]
                    PrPrior = (1.0 - ((( 1.0 - PoFLastYear ) + PrIntro ) - 
                                ( ( 1.0 - PoFLastYear ) * PrIntro )))
                    PoFMat[yearCount, currentIteration] = PrPrior

            # leave sensitivityRaster blank

        ## KEEP YEAR ZERO PRIOR FOR PLOTTING IN postProcessing.py
        sensitivityList.append(sensitivityRaster)
        yearCount += 1

    # free the lock
    LOCK_DESTROY(THREAD_LOCK)

    # write the big rasters to directory as *.tif
    # (1) multilayered tif of annual mean SeU
#    print('sensitivityList size', sensitivityList.nbytes / (1024 * 1024 * 1024))
    meanSeuTifPathName = os.path.join(outputDataPath, 'meanSeuAllYears.tif')
    gdt_type = gdalconst.GDT_Float32
    writeTif(sensitivityList, meanSeuTifPathName, gdt_type, wkt, match_geotrans)


#    del sensitivityList
    # (2) modified Relative Risk Map
    print('RRArray size', RRArray.nbytes / (1024 * 1024 * 1024))
    relRiskTifName = os.path.join(outputDataPath, 'updatedRelRisk.tif')
    gdt_type = gdalconst.GDT_Float32
    writeTif(RRArray, relRiskTifName, gdt_type, wkt, match_geotrans)
#    del modifiedK

    # (3) updated Zone mask
    print('zoneArray size', zoneArray.nbytes / (1024 * 1024 * 1024))
    extZoneTifName = os.path.join(outputDataPath, 'updatedExtentZone.tif')
    gdt_type = gdal.GDT_Byte
    writeTif(zoneArray, extZoneTifName, gdt_type, wkt, match_geotrans)

    # create object with result information
    result = Results()
    result.sensitivityMatrix = SSeMat
    print('r.seMat size', SSeMat.nbytes / (1024 * 1024 * 1024))

    # swh: return values associated with 'meanSeuAllYears.tif' raster since it 
    #      doesn't get exported if not using GDAL libs
    result.sensitivityList = sensitivityList

    result.poFMatrix = PoFMat
    print('PofMat size', PoFMat.nbytes / (1024 * 1024 * 1024))

    # we want the parameters but not the gridSurveyData field
    # which can be very big. Take a copy and then clobber it on our
    # copy so the caller can re-use their parameters for something else
    result.params = copy.copy(poaparams)
    result.params.gridSurveyData = None

    result.priorStore = priorStore
    result.zoneSeMatrix = zoneSeResults
#    print('zoneSeMatrix', zoneSeResults)
#    print('sys Se matrix', SSeMat)
#    print('pofmat', PoFMat)
#    print('proportionSearchedExtent', proportionSearchedExtent)

    ## NUMBER OF ZONES
    result.numberZones = nZones
    ## NAMES OF ZONES FOR RESULTS
    result.Name_zone = Name_zone
    result.proportionSearchedExtent = proportionSearchedExtent

    result.proportionSearchedZone = proportionSearchedZone
    result.relRiskTifName = relRiskTifName
    result.extZoneTifName = extZoneTifName
    result.meanSeuTifPathName = meanSeuTifPathName
    (result.rows, result.cols) = zoneArray.shape
    result.match_geotrans = match_geotrans

    print('got to return result')

    return result


def writeTif(raster, tempTifName, gdt_type, wkt, match_geotrans):
    """
    write single or multi-band tifs to directory
    """
    # if single band
    if not isinstance(raster, list):
        (nrows, ncols) = np.shape(raster)
        ds = gdal.GetDriverByName('GTiff').Create(tempTifName, ncols,
        nrows, 1, gdt_type,
        options=['TILED=YES', 'COMPRESS=LZW', 'INTERLEAVE=BAND', 'BIGTIFF=IF_SAFER'])
        ds.SetGeoTransform(match_geotrans)
        ds.SetProjection(wkt)
        band = ds.GetRasterBand(1)
        band.WriteArray(raster)
    # if multi-layered array
    else:
        (nrows, ncols) = np.shape(raster[0])
        nlayers = len(raster)
        ds = gdal.GetDriverByName('GTiff').Create(tempTifName, ncols,
            nrows, nlayers, gdt_type,
            options=['TILED=YES', 'COMPRESS=LZW', 'INTERLEAVE=BAND', 'BIGTIFF=IF_SAFER'])
        ds.SetGeoTransform(match_geotrans)
        ds.SetProjection(wkt)
        # loop thru years (layers in tif)
        for n in range(nlayers):
            band = ds.GetRasterBand(n+1)
            band.WriteArray(raster[n])
            
    # calculate stats so we have the overviews
    progress = GDALProgressBar()
    # should we be ignoring zero in the stats??
    calcstats.calcStats(ds, progress, ignore=0)
    
    del ds  # Flush

@jit(nopython=True, nogil=True)
def doIterations(startItr, endItr, sensitivityRaster, zoneSeResults, 
        proportionSearchedExtent, proportionSearchedZone, 
        SSeMat, PoFMat, RRArray, yearCount,
        currentYear, gridSurveyParams, gridSurveyData, 
        parameterArray, trapArray, zoneArray, zoneCodes, sumRRWeights, rrHxZoneWeights, 
        Pu_zone, transform, nChewcardTraps,
        PStar, Pz, nTotalCells, prior_a, prior_b, prior_min, prior_max, 
        prior_range, intro_a, intro_b, intro_min, intro_max, intro_range, priorStore):
    """
    Does one iteration of the processing from startItr to endItr-1.
      * Also takes the sensitivityRaster and updates it with the sum of 
        all the individual 'thisSensitivities' calculated for each iteration
      * zoneSeResults are filled in for each year and iteration
      * proportionSearchedExtent and proportionSearchedZone which is updated for each year/iteration
      * SSeMat and PoFMat which is filled in for each year and iteration
      * RRArray has relative risk
      * The index of the year and the actual year we are processing. yearCount is index into zoneSeResults etc
      * gridSurveyParams, gridSurveyData are from the params
      * parameterArray is from the POAParameters class
      * trapArray, zoneArray, zoneCodes and transform are the parameters to calcProofOfAbsence()
      * sumRRWeights, rrHxZoneWeights come from getSumRRWeights()
      * Pu_zone and transform are passed into calcProofOfAbsence()
      * PStar is for the given year
      * priorStore (?)
      * Other parameters come from POAParameters
    """
    for currentIteration in range(startItr, endItr):
        # thisSensitivity starts with p(non-detect) = 1 everywhere:
        thisSensitivity = np.ones_like(sensitivityRaster)
        if gridSurveyParams is not None:
            # thisSensitivity output is a p(non-detect)
            applyGridSurveillance(gridSurveyParams,
                                gridSurveyData, thisSensitivity, 
                                currentYear)
        # process all traps
        # update thisSensitivity as p(non-detect), but after this function
        # and its nested functions, we are working p(detection)
        seWeighted, weightedAveSensitivity, nSearchedCells = (
                processAllTrapsForYear(currentYear, parameterArray, 
                    trapArray, thisSensitivity, transform, zoneArray, zoneCodes, 
                    RRArray, sumRRWeights, nChewcardTraps))


        # following lines need lock since the updated arrays are shared between all threads
        LOCK_LOCK(THREAD_LOCK, 1)
        # keep a total of the searchedCells for now - will divide later
        
        # ProportionSearchedZone is 2D. We are setting all zones
        for zoneIdx in range(zoneCodes.size):
            proportionSearchedZone[yearCount, zoneIdx] += nSearchedCells[zoneIdx]

        # update the sensitivity raster - we dived by nIter later
        # sensitivityRaster is used to make *.png of meanSeU across iterations
        # One is produced for each year.
        sensitivityRaster += thisSensitivity

        LOCK_UNLOCK(THREAD_LOCK)
        # Condition on presence or absence of surveillance in year
        # Sum() across all zones of number of searched cells.
        zoneSensitivity = np.zeros(zoneCodes.shape)
        if nSearchedCells.sum() == 0:
            systemSensitivity = 0.0
        else:
            # New - Equation 6
            wtAveAcrossZones = 0.0
            zoneSensitivity = (1.0 - np.power((1.0 - weightedAveSensitivity), PStar)) 
#            zoneSensitivity = (1.0 - np.power((1.0 - weightedAveSensitivity), Pu_zone)) 
            ## Calculate the system sensitivity
            ## Ave zone se weighted by rr and hx (history) = SSe with P*zone = 1
            # New - Equation 4  
            wtAveAcrossZones = np.sum(zoneSensitivity * rrHxZoneWeights)
            # New - Equation 3
            systemSensitivity = (1.0 - np.power((1.0 - wtAveAcrossZones), Pz))
        # lock not needed since currentIteration is unique to thread
        # zoneSeResults is 3D so we set all the zones
        for zoneIdx in range(zoneCodes.size):
            zoneSeResults[yearCount, currentIteration, zoneIdx] = zoneSensitivity[zoneIdx]

        if yearCount == 0:
            # Probability of Introduction in the first year
            PrPrior = doBetaPert(prior_a, prior_b, 
                    prior_min, prior_max, prior_range)
            priorStore[currentIteration] = PrPrior
        else:
            PrIntro = doBetaPert(intro_a, intro_b,
                    intro_min, intro_max, intro_range)
            # Equation 2
            PoFLastYear = PoFMat[yearCount - 1, currentIteration]
            PrPrior = (1.0 - ((( 1.0 - PoFLastYear ) + PrIntro ) - 
                ( ( 1.0 - PoFLastYear ) * PrIntro )))

        # Equation 1 
        # no lock needed since currentIteration unique to thread
        PrOfFreedom = PrPrior / ( 1.0 - systemSensitivity * ( 1.0 - PrPrior ) )
        SSeMat[yearCount, currentIteration] = systemSensitivity
        PoFMat[yearCount, currentIteration] = PrOfFreedom

@jit(nopython=True, nogil=True)
def applyGridSurveillance(gridSurveyParams, gridSurveyData, thisSensitivity, 
        currentYear):
    """
    Apply the grid surveillance to thisSensitivity. Takes the 
    survey data as parameters.
    """
    nApplied = 0
    rows, cols = thisSensitivity.shape
    for i in range(gridSurveyParams.shape[0]):
        if gridSurveyParams[i]['year'] == currentYear:
            # we need to lock the np.random.* functions as they are not thread safe
            dVal = np.random.beta(gridSurveyParams[i]['a'], 
                gridSurveyParams[i]['b'])
#            print('grid', dVal)
            if nApplied == 0:
                # first one - just set 1 minus to get prob(not detecting)
                for y in range(rows):
                    for x in range(cols):
                        if (gridSurveyData[y, x] & gridSurveyParams[i]['code']) != 0:
                            thisSensitivity[y, x] = 1.0 - dVal
                        else:
                            # not sure this line is needed since we initialise
                            # the whole array to 1...
                            thisSensitivity[y, x] = 1.0
            else:
                # more than one - combine them: prob(not detecting in all)
                for y in range(rows):
                    for x in range(cols):
                        if (gridSurveyData[y, x] & gridSurveyParams[i]['code']) != 0:
                            thisSensitivity[y, x] = thisSensitivity[y, x] * (1.0 - dVal)

            nApplied += 1


@jit(nopython=True, nogil=True)
def processAllTrapsForYear(currentYear, parameterArray, trapArray, thisSensitivity, 
            transform, zoneArray, zoneCodes, RRArray, sumRRWeights, nChewcardTraps):
    """
    Goes through an process all traps for given year. Parameters:
        * the current year
        * POFParameters
        * the traps
        * the initialised sensitivity raster for this iteration (may have had grid survey applied)
        * geo transform of the rasters
        * zone raster - use where != 0 (see zoneCodes)
        * zoneCodes to use in zoneArray
        * the RR raster and sumRRWeights

    Returns (arrays - value for each zone in zoneCodes):
        * seWeighted
        * weightedAveSensitivity
        * nSearchedCells
    """
    nTraps = trapArray.shape[0]
    nZones = zoneCodes.shape[0]

    for nCurrentTrap in range(nTraps):
        currentTrap = trapArray[nCurrentTrap]
        if currentTrap['year'] == currentYear:
            animalCode = currentTrap['animal']
            currentParam = parameterArray[animalCode]

            # dSigma = Spatial decay from point of the possums
            # utilisation of an area relative to that point
            dSigma = np.random.normal(currentParam['mean_sig'],
                                    currentParam['sd_sig'])

            # call it - we may have other types here in future...
            # Since Numba doesn't support passing of functions, 
            # we have to hard code what to do here (update: now does - must update code)
            # the updated thisSensitivity in 'circleFunction' is still a p(non-detect)
            circleFunction(dSigma, trapArray, nCurrentTrap, parameterArray, 
                        transform, thisSensitivity, zoneArray, nChewcardTraps)
                        
    # After the following function, working with p(detection)
    seWeighted, weightedAveSensitivity, nSearchedCells = (
        convertSensitivityIntoProbOfDetecting(thisSensitivity, 
                        RRArray, zoneArray, zoneCodes, sumRRWeights))
    
    return seWeighted, weightedAveSensitivity, nSearchedCells

@jit(nopython=True, nogil=True)
def convertSensitivityIntoProbOfDetecting(thisSensitivity, RRArray,
                        zoneArray, zoneCodes, sumRRWeights):
    """
    now turn every value in pThisSensitivity into prob of detecting
    and find for non zero cells seWeighted, weightedAveSensitivity
    and number of searched cells for each zone.
    """
    rows, cols = thisSensitivity.shape
    nSearchedCells = np.zeros_like(zoneCodes)
    seWeighted = np.zeros(zoneCodes.shape)
    for y in range(rows):
        for x in range(cols):
            zone = zoneArray[y, x]
            zoneIdx = findIn1DArray(zoneCodes, zone)
            if zoneIdx != -1:
                # old values are p(non-detect) from thisSensitivity
                dOldValue = thisSensitivity[y, x]
                # convert p(non-detect) into p(detect)
                dNewValue = 1.0 - dOldValue
                # thisSensitivity is now a p(detect) or SeU
                thisSensitivity[y, x] = dNewValue
                # surveillance is cell x,y was conducted: p(detect) > 0
                if dNewValue != 0:
                    # Keep a track of number of unique cells we have used 
                    nSearchedCells[zoneIdx] +=  1
                    # The summing of New - Equation 7 and 8 (written differently)
                    if RRArray is not None:
                        seWeighted[zoneIdx] += dNewValue * RRArray[y, x]
                    else:
                        seWeighted[zoneIdx] += dNewValue
    # calc weighted average of sensitivity for each zone
    # New - Equation 7
    weightedSensitivity = seWeighted / sumRRWeights
    
    return seWeighted, weightedSensitivity, nSearchedCells

@jit(nopython=True, nogil=True)
def doBetaPert(a, b, min, max, range):
    """
    Helper function. Does a Pert distribution which is basically a Beta with a multiplier 
    Also does range checking 
    """
    # we need to lock the np.random.* functions as they are not thread safe
    dVal = np.random.beta(a, b) * range + min
    if dVal < min:
        dVal = min
    elif dVal > max:
        dVal = max

    return dVal

def doBetaPertArray(a, b, min, max, range, size):
    """
    Same as doBetaPert, but for arrays. Not called from Numba.
    Numba got confused trying to do the same (scalar and array)
    in one function, so split out.
    """
    # we need to lock the np.random.* functions as they are not thread safe
    # probably not needed here since we know this function isn't called from a thread...
    dVal = np.random.beta(a, b, size) * range + min
    return np.clip(dVal, min, max)

@jit(nopython=True, nogil=True)
def circleFunction(dSigma, traps, nCurrentTrap, paramArray, 
        transform, thisSensitivity, zoneArray,  nChewcardTraps):
    """
    Calculates the pixels that are within 4*dSigma radius of the animal/trap
    and calls getSensitivityForCell() upon them.

    In future when Numba supports passing of functions as arguments
    we will change this so the appropriate function is passed in.
    """
    rows, cols = thisSensitivity.shape

    dFourSigma = 4.0 * dSigma
    # add another pixel to search just to be safe 
    dSearchDistance = dFourSigma + transform[1]

    # work out our search area 
    dSearchTLX = traps[nCurrentTrap]['easting'] - dSearchDistance
    dSearchTLY = traps[nCurrentTrap]['northing'] + dSearchDistance
    dSearchBRX = traps[nCurrentTrap]['easting'] + dSearchDistance
    dSearchBRY = traps[nCurrentTrap]['northing'] - dSearchDistance




    ## TODO: IS THERE AN ERROR IN THE ROW CALCULATION BELOW?



    # Convert to pixel coords 
    # Both could be outside the raster, but we need to search 
    # anyway in case the search area intersects with the raster 
    halfRes = transform[1] / 2
    nTLX = int(np.round((dSearchTLX - transform[0] - halfRes) / transform[1]))
    nTLY = int(np.round((dSearchTLY - transform[3] - halfRes) / transform[5]))
    nBRX = int(np.round((dSearchBRX - transform[0] - halfRes) / transform[1]))
    nBRY = int(np.round((dSearchBRY - transform[3] - halfRes) / transform[5]))







    # Correct back to coords on the grid 
    dSearchTLX = transform[0] + nTLX * transform[1] + halfRes
    dSearchTLY = transform[3] - nTLY * transform[1] - halfRes
    
    # All random draws outside of pixel looping 
    # by creating an empty array that calcFunction can put stuff into
    tmpData = np.zeros(10)

    # go through each pixel in the search area 
    testNorth = dSearchTLY
    for testY in range(nTLY, nBRY):
        testEast = dSearchTLX;
        for testX in range(nTLX, nBRX):
            # is this location within the raster? 
            # and not masked out? 
            if( testX >= 0 and testY >= 0 and testX < cols and testY < rows and
                        zoneArray[testY, testX] != 0 ):
                # work out distance and then the kernel value 
                dXDist = abs( traps[nCurrentTrap]['easting'] - testEast)
                dYDist = abs( traps[nCurrentTrap]['northing'] - testNorth)
                dDist = np.sqrt( dXDist * dXDist + dYDist * dYDist )
                # make sure our pixel is less than 4 sigma away from trap 
                if dDist <= dFourSigma:
                    # New Equation 12 
                    dKern = np.exp( -( dDist * dDist ) / ( 2.0 * ( dSigma * dSigma ) ) )

                    dSensitivityTrapCell = getSensitivityForCell(tmpData, dKern, traps, 
                            nCurrentTrap, paramArray,
                            nChewcardTraps)
                    #print(testX, testY, dSensitivityTrapCell)

                    # New Equation 9 
                    # product of 1-SeUij For each cell 
                    # multiply by previous value if exists 
                    # prob of not detecting; values 1 or less
                    dOldValue = thisSensitivity[testY, testX];
                    dNewValue = dOldValue * ( 1.0 - dSensitivityTrapCell )
                    # still a p(non-detect)
                    thisSensitivity[testY, testX] = dNewValue

            testEast += transform[1]
        testNorth -= transform[1]

@jit(nopython=True, nogil=True)
def getSensitivityForCell(tmpData, dKern, traps, nCurrentTrap, paramArray, 
                nChewcardTraps):
    """
    Calls the appropriate function. Numba currently doesn't support passing of 
    function as arguments so this appears to be the best way forward.
    """
    detectCode = traps[nCurrentTrap]['detect']
    dSensitivityTrapCell = 0.0
    if detectCode == params.DETECT_DISEASE_CHEWCARD:
        dSensitivityTrapCell = detectChewcardFunc(tmpData, dKern, traps, 
                nCurrentTrap, paramArray, nChewcardTraps)

    elif detectCode == params.DETECT_DISEASE_TRAP:
        dSensitivityTrapCell = detectTrapFunc(tmpData, dKern, traps, 
                nCurrentTrap, paramArray, nChewcardTraps)

    elif detectCode == params.DETECT_DISEASE_SENTINEL:
        dSensitivityTrapCell = detectSentinelFunc(tmpData, dKern, traps, 
                nCurrentTrap, paramArray, nChewcardTraps)

    elif detectCode == params.DETECT_ANIMAL:
        dSensitivityTrapCell = detectAnimalFunc(tmpData, dKern, traps, 
                nCurrentTrap, paramArray, nChewcardTraps)

    else:
        # we must work out what to do here
        raise ValueError('Unsupported animal detect code')
        
    return dSensitivityTrapCell

@jit(nopython=True, nogil=True)
def detectChewcardFunc(tmpData, dKern, traps, nCurrentTrap, paramArray, 
                nChewcardTraps):
    """
    Does the calculations of sensitivity for chewcards for a given kernel value
    """
    if tmpData[0] == 0.0:
        currentParams = paramArray[traps[nCurrentTrap]['animal']]

        # check inputs
        if ((currentParams['mean_test'] == 1 and currentParams['sd_test'] == 1) or
            (currentParams['mean_CC'] == 1 and currentParams['sd_CC'] == 1) or
            (currentParams['mean_capt'] == 1 and currentParams['sd_capt'] == 1)):
            msg = 'Must fill Test, Chewcard and Capture parameters'
            raise InputParameterError(msg)

        # Do random draws 
        # we need to lock the np.random.* functions as they are not thread safe
        tmpData[1] = np.random.beta(currentParams['a_test'], currentParams['b_test'])
        tmpData[2] = np.random.beta(currentParams['a_CC'], currentParams['b_CC'])
        tmpData[3] = np.random.beta(currentParams['a_capt'], currentParams['b_capt'])
        tmpData[0] = 1 # don't draw next time
        
    dTest = tmpData[1]
    dg0_CC = tmpData[2]
    dg0_capt = tmpData[3]

    # Note: chewcards not covered in paper
    ## New equation 12 
    dpChew = 1.0 - np.power( 1.0 - dg0_CC * dKern, traps[nCurrentTrap]['trapnights'] )
    dpCap = ( 1.0 - np.power( 1.0 - dg0_capt * dKern, 3.0 * nChewcardTraps ) ) * dpChew

    # Probability of detecting TB in cell i from device j
    ## New Equation 11 
    return dpCap * dTest

@jit(nopython=True, nogil=True)
def detectTrapFunc(tmpData, dKern, traps, nCurrentTrap, paramArray, 
                nChewcardTraps):
    """
    Does the calculations of sensitivity for possums/traps for a given kernel value
    """
    if tmpData[0] == 0.0:
        currentParams = paramArray[traps[nCurrentTrap]['animal']]

        # check inputs
        if ((currentParams['mean_test'] == 1 and currentParams['sd_test'] == 1) or
            (currentParams['mean_capt'] == 1 and currentParams['sd_capt'] == 1)):
            msg = 'Must fill Test and Capture parameters'
            raise InputParameterError(msg)

        # Do random draws 
        # we need to lock the np.random.* functions as they are not thread safe
        tmpData[1] = np.random.beta(currentParams['a_test'], currentParams['b_test'])
        tmpData[2] = np.random.beta(currentParams['a_capt'], currentParams['b_capt'])
        tmpData[0] = 1 # don't draw next time
        
    dTest = tmpData[1]
    dg0_capt = tmpData[2]
    ## New Equation 12
    dpCap = ( 1.0 - np.power( 1.0 - dg0_capt * dKern, traps[nCurrentTrap]['trapnights'] ) )
    ## New Equation 11
    # Probability of detecting TB in cell i from device j 
    return dpCap * dTest

@jit(nopython=True, nogil=True)
def detectSentinelFunc(tmpData, dKern, traps, nCurrentTrap, paramArray, 
                nChewcardTraps):
    """
    Does the calculations of sensitivity for sentinels for a given kernel value
    """
    if tmpData[0] == 0.0:
        currentParams = paramArray[traps[nCurrentTrap]['animal']]

        # check inputs
        if ((currentParams['mean_test'] == 1 and currentParams['sd_test'] == 1) or
            (currentParams['mean_infect'] == 1 and currentParams['sd_infect'] == 1)):
            msg = 'Must fill Test and Infect parameters'
            raise InputParameterError(msg)

        # Do random draws 
        # we need to lock the np.random.* functions as they are not thread safe
        tmpData[1] = np.random.beta(currentParams['a_test'], currentParams['b_test'])
        tmpData[2] = np.random.beta(currentParams['a_infect'], currentParams['b_infect'])
        tmpData[0] = 1 # don't draw next time
        
    dTest = tmpData[1]
    dLambda = tmpData[2]

    # Probability of detecting TB in cell i from device j 
    return (1.0 - np.power( 1.0 - dLambda * dKern, traps[nCurrentTrap]['age'])) * dTest

@jit(nopython=True, nogil=True)
def detectAnimalFunc(tmpData, dKern, traps, nCurrentTrap, paramArray, 
                nChewcardTraps):
    """
    Does the calculations of sensitivity for non diseased animals 
    for a given kernel value
    """
    if tmpData[0] == 0.0:
        currentParams = paramArray[traps[nCurrentTrap]['animal']]

        # check inputs
        if (currentParams['mean_capt'] == 1 and currentParams['sd_capt'] == 1):
            msg = 'Must fill Capture parameters'
            raise InputParameterError(msg)

        # Do random draws 
        # we need to lock the np.random.* functions as they are not thread safe
        tmpData[1] = np.random.beta(currentParams['a_capt'], currentParams['b_capt'])
        tmpData[0] = 1 # don't draw next time
        
    dg0_capt = tmpData[1]

    dpCap = ( 1.0 - np.power( 1.0 - dg0_capt * dKern, traps[nCurrentTrap]['trapnights'] ) )

    # Probability of detecting the target in cell i from device j 
    return dpCap

@jit(nopython=True, nogil=True)
def findIn1DArray(inArray, value):
    """
    Returns index if value in inArray, -1 otherwise
    Only for 1d Arrays.
    Workaround for older numba with no 'in' support
    """
    idx = -1
    size = inArray.shape[0]
    for i in range(size):
        if inArray[i] == value:
            idx = i
            break
    return idx

@jit(nopython=True, nogil=True)
def addRRBufferAroundTraps(traps, RRBufferAnimals, years, RRarray, zoneRaster, 
                origZoneRaster, RRTrapDistance, minRR, maxRR, transform):
    """
    Adds a buffer of the specified size around each trap of the specified
    RRBufferAnimals types.
    """
    rows, cols = RRarray.shape
    nTraps = traps.shape[0]
    for i in range(nTraps):
        if (findIn1DArray(RRBufferAnimals, traps[i]['animal']) != -1 and
                findIn1DArray(years, traps[i]['year']) != -1):
            # add another pixel to search just to be safe 
            dSearchDistance = RRTrapDistance + transform[1];
            # work out our search area 
            dSearchTLX = traps[i]['easting'] - dSearchDistance;
            dSearchTLY = traps[i]['northing'] + dSearchDistance;
            dSearchBRX = traps[i]['easting'] + dSearchDistance;
            dSearchBRY = traps[i]['northing'] - dSearchDistance;

            # Convert to pixel coords 
            # Both could be outside the raster, but we need to search 
            # anyway in case the search area intersects with the raster 
            halfRes = transform[1] / 2
            nTLX = int(np.round((dSearchTLX - transform[0] - halfRes) / transform[1]))
            nTLY = int(np.round((dSearchTLY - transform[3] - halfRes) / transform[5]))
            nBRX = int(np.round((dSearchBRX - transform[0] - halfRes) / transform[1]))
            nBRY = int(np.round((dSearchBRY - transform[3] - halfRes) / transform[5]))

            # Correct back to coords on the grid 
            dSearchTLX = transform[0] + nTLX * transform[1] + halfRes
            dSearchTLY = transform[3] - nTLY * transform[1] - halfRes
            
            # go through each pixel in the search area 
            testNorth = dSearchTLY
            for testY in range(nTLY, nBRY):
                testEast = dSearchTLX
                for testX in range(nTLX, nBRX):
                    # is this location within the raster? 
                    if( testX >= 0 and testY >= 0 and testX < cols and testY < rows):
                        # work out distance and then the kernel value 
                        dXDist = abs( traps[i]['easting'] - testEast)
                        dYDist = abs( traps[i]['northing'] - testNorth)
                        dDist = np.sqrt( dXDist * dXDist + dYDist * dYDist )

                        # make sure our pixel is less than dDistance away from trap 
                        # and included in the original mask 
                        if dDist <= RRTrapDistance and origZoneRaster[testY, testX] != 0:
                            zoneRaster[testY, testX] = origZoneRaster[testY, testX]
                            # do infill if value only smaller than nMinK 
                            if RRarray[testY, testX] < minRR:
                                RRarray[testY, testX] = maxRR

                    testEast += transform[1]
                testNorth -= transform[1]

@jit(nopython=True, nogil=True)
def getSumRRWeights(RRArray, zoneArray, zoneCodes, RR_zone):
    """
    Get the sum of RR weights within each zone
    We do this using numba since we have to take the mask into consideration
    when doing the calculations so it means looping.
    """
    ## SUM OF RR FOR EACH ZONE
    sumRRWeights = np.zeros(zoneCodes.shape)
    ## PRODUCT OF RR AND HISTORICAL RISKS FOR EACH ZONE
    RR_Hx_Zone = np.zeros(zoneCodes.shape)
    ## TO SUM UP ALL PRODUCTS OF RR AND HISTORICAL RISKS ACROSS ALL CELLS
    sumRR_Hx = 0
    ## TOTAL NUMBER OF CELLS IN ZONES 
    nCellsInZones = np.zeros(zoneCodes.shape)
    ysize, xsize = RRArray.shape
    for y in range(ysize):
        for x in range(xsize):
            zone = zoneArray[y, x]
            zoneIdx = findIn1DArray(zoneCodes, zone)
            if zoneIdx != -1:
                rr_yx = RRArray[y, x]
                # New - Equation 8 - Denominator within zone j weights to calc zone SeZ
                sumRRWeights[zoneIdx] += rr_yx
                # New - Equation 5 - Numerator for zone j to calc weights for SSe (cells zone j)
                RR_Hx_Zone[zoneIdx] += (rr_yx * RR_zone[zoneIdx])
                # New - Equation 5 - Denominator to calc weights for SSe (all cells in extent)
                sumRR_Hx += (rr_yx * RR_zone[zoneIdx])
                # to get proportion searched in each zone
                nCellsInZones[zoneIdx] += 1
    # New - equation 5
    rrHxZoneWeights = RR_Hx_Zone /  sumRR_Hx
    print('rrHxZoneWeights', rrHxZoneWeights, 'sumRRWeights', sumRRWeights, 
            'ncellsInZones', nCellsInZones, 'total ncells', np.sum(nCellsInZones))
    return(sumRRWeights, rrHxZoneWeights, nCellsInZones)
        

