#!/usr/bin/env python

########################################
########################################
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
########################################
########################################

import os
import pickle
import numpy as np

os.environ['PROJ_LIB'] = "C:\\ProgramData\\Anaconda3\\envs\\r_poa\\Library\\share\\proj"
# os.chdir("D:\\Users\\Si\\OneDrive - MWLR\\Projects\\Kaitake Possum Eradication\\deanpa-dean_poa")
os.chdir("D:\\Users\\Si\\OneDrive - MWLR\\Projects\\795208-0091 Ctr for Inv Spec Res-Erad Tool\\shiny_PoA")

from proofofabsence import preProcessing
from proofofabsence import params
from proofofabsence import calculation

######################
# Main function
def main():
    ############################################################
    ###################################### USER MODIFY HERE ONLY
    #################################
    # set paths to scripts and data
    inputDataPath = os.path.join(os.getenv('POFPROJDIR', default = '.'), 'testdata', 'input')
    if not os.path.isdir(inputDataPath):
        os.mkdir(inputDataPath)
        
    outputDataPath = os.path.join(os.getenv('POFPROJDIR', default = '.'), 'testdata', 'output')
    if not os.path.isdir(outputDataPath):
        os.mkdir(outputDataPath)

    # set INPUT extent and relative risk file names
    zoneShapeFName = os.path.join(inputDataPath, 'extent.shp') 
        #'extentOnly.shp')    #'extent_block1.shp')    
        #'extent_gradient.shp')
    relativeRiskFName = os.path.join(inputDataPath, 'relRiskRaster.tif') 
        #'habDistRR.tif')     
        #'RR_mahia.tif')  
#    relativeRiskFName = None

    # Surveillance data Name
    surveyFName = os.path.join(inputDataPath, 'devices.csv') # or None

    # set OUTPUT names for mask and relative risk map
    zoneOutFName = os.path.join(outputDataPath, 'zones.tif')
    relRiskRasterOutFName = os.path.join(outputDataPath, 'relRiskRaster.tif')
#    relRiskRasterOutFName = None

    ############ IF USE GRIDS
    useGrids = False 
    gridSurvey = None   # os.path.join(inputDataPath, 'gridScenario14.csv')

    ############ IF FIRST RUN CONDITION
    # if True, do preprocessing, else skip to calculations
    firstRun = True        # True or False

    # resolution for analysis
    Resolution = 100.0
    # EPSG - PROJECTION SYSTEM
    epsg = 2193    # NZ transverse mercator

    #############################################################################
    ##### ADD SURVEILLANCE TYPES                                                #
                                                                                #
    animals = params.AnimalTypes()                                              #
                                                                                #       
    TYPE_LEGHOLD = 12 # index number for leghold traps                          #
    animals.addAnimal(TYPE_LEGHOLD, "Leghold", params.DETECT_ANIMAL)            #
                                                                                #
    TYPE_SENTINEL = 13 # index number for Sentinel traps                        #
    animals.addAnimal(TYPE_SENTINEL, "Sentinel", params.DETECT_ANIMAL)          #
                                                                                #
    TYPE_POSSMASTER = 14 # index number for Sentinel traps                      #
    animals.addAnimal(TYPE_POSSMASTER, "PossMaster", params.DETECT_ANIMAL)      #
                                                                                #
    TYPE_CAMERA = 15 # index number for Sentinel traps                          #
    animals.addAnimal(TYPE_CAMERA, "Camera", params.DETECT_ANIMAL)              #
                                                                                #
    TYPE_CHEWDETECT = 16 # index number for chew card detection                 #
    animals.addAnimal(TYPE_CHEWDETECT, "CHEWDETECT", params.DETECT_ANIMAL)      #
                                                                                #
    TYPE_AT220 = 17 # index number for AT220                                    #
    animals.addAnimal(TYPE_AT220, "AT220", params.DETECT_ANIMAL)                #
                                                                                #
    #############################################################################

    # Instance of POAParameters class
    myParams = params.POAParameters(animals)

    ## USE MULTIPLE ZONES
    myParams.setMultipleZones(True)

    #############################################################################
    # ADD TRAP PARAMETERS                                                       #
    myParams.setCapture(12, 0.06, 0.03)   # note the 12 indicates LEGHOLD       #
    myParams.setSigma(12, 30.0, 10.0)                                           #
    myParams.addRRBufferAnimal(TYPE_LEGHOLD)                                    #
                                                                                #
    myParams.setCapture(13, 0.04, 0.03)   # note the 13 indicates SENTINEL      #
    myParams.setSigma(13, 30.0, 10.0)                                           #
    myParams.addRRBufferAnimal(TYPE_SENTINEL)                                   #
                                                                                #
    myParams.setCapture(14, 0.04, 0.03)   # note the 14 indicates POSSMASTER    #
    myParams.setSigma(14, 30.0, 10.0)                                           #
    myParams.addRRBufferAnimal(TYPE_POSSMASTER)                                 #
                                                                                #
    myParams.setCapture(15, 0.075, 0.03)  # note the 15 indicates CAMERA        #
    myParams.setSigma(15, 30.0, 10.0)                                           #
    myParams.addRRBufferAnimal(TYPE_CAMERA)                                     #
                                                                                #
    myParams.setCapture(16, 0.06, 0.03)  # note the 16 indicates CHEWDETECT     #
    myParams.setSigma(16, 30.0, 10.0)                                           #
    myParams.addRRBufferAnimal(TYPE_CHEWDETECT)                                 #
                                                                                #
    myParams.setCapture(17, 0.05, 0.03)  # note the 17 indicates AT220          #
    myParams.setSigma(17, 30.0, 10.0)                                           #
    myParams.addRRBufferAnimal(TYPE_AT220)                                      #
                                                                                #
    #############################################################################

    # number of cpu's from SLURM
    ncpus = int(os.getenv('SLURM_CPUS_PER_TASK', '1'))
    myParams.setNumThreads(ncpus)

    print('ncpus', ncpus)

    # number of iterations
    myParams.setNumIterations(10)
#    myParams.setNumChewcardTraps(3)
    myParams.setRRTrapDistance(100.0)

    myParams.setYears(int(1),int(1))

    ## THE startPu WILL NOT BE USED IF USE zoneData FILE - TURN OFF
    # starting Pu (GRID CELL PREVALENCE) and period rate of Pu increase
    startPu = 1.0

    ## SET THE RATE OF INCREASE OF PU
    PuIncreaseRate = 0.0
    myParams.setPu(startPu, PuIncreaseRate)

#    ## LOGISTIC GROWTH OF INDIVIDUALS AND GRID CELLS
#    rmax_pstar = 0.5
#    ki = 10.000001   # max individuals per flock/cell
#    ku = 1.0   # max number of flocks/cells
#    pind = 10.0   # starting individual design prevalence
#    p_disperse = 12.0    # when pind reaches this, start new cell/flock
#    pu = 1.0     # starting cell design prevalence
#    myParams.setPuLogistic(rmax_pstar, ki, ku, pind, p_disperse, pu) 

    #minimum RR value
    myParams.setMinRR(1.0)

    myParams.setPrior(0.6, 0.7, 0.799999)
    myParams.setIntro(0.00001, 0.00002, 0.00003)        # (min, mode, max)

#    myParams.setSigma(params.TYPE_POSSUM, 1060.0, 30.0)
#    myParams.setSigma(params.TYPE_POSSTRAP, 1060.0, 30.0)
#    myParams.setSigma(params.TYPE_CHEWCARD, 90.0, 0.000001)
#    myParams.setSigma(params.TYPE_FERRET, 287.0, 0.000001)
#    myParams.setSigma(params.TYPE_PIG, 910.0, 0.000001)
#    myParams.setSigma(params.TYPE_REDDEER, 2500.0, 1.0)

#    myParams.setChewcard(params.TYPE_CHEWCARD, 0.2, 0.000001)
#    myParams.setCapture(params.TYPE_POSSUM, 0.12, 0.05)
#    myParams.setCapture(params.TYPE_POSSTRAP, 0.03, 0.015)
#    myParams.setCapture(params.TYPE_CHEWCARD, 0.13, 0.000001)

#    myParams.setTest(params.TYPE_POSSUM, 0.99999, 0.000001)
#    myParams.setTest(params.TYPE_POSSTRAP, 0.99999, 0.000001)
#    myParams.setTest(params.TYPE_CHEWCARD, 0.95, 0.00001)
#    myParams.setTest(params.TYPE_FERRET, 0.95, 0.00001)
#    myParams.setTest(params.TYPE_PIG, 0.95, 0.000001)
#    myParams.setTest(params.TYPE_REDDEER, 0.95, 0.00001)

#    myParams.setInfect(params.TYPE_POSSUM, 0.99999, 0.000001)
#    myParams.setInfect(params.TYPE_POSSTRAP, 0.99999, 0.000001)
#    myParams.setInfect(params.TYPE_CHEWCARD, 1.0, 0.1)
#    myParams.setInfect(params.TYPE_FERRET, 0.187, 0.01)
#    myParams.setInfect(params.TYPE_PIG, 0.472, 0.000001)
#    myParams.setInfect(params.TYPE_REDDEER, 0.009, 0.01)

    #####################################   END USER MODIFICATION
    #############################################################
    #############################################################

    print('firstRun = ', firstRun)

    if firstRun:
        # initiate instances of Classes
        rawdata = preProcessing.RawData(zoneShapeFName, relativeRiskFName, 
                zoneOutFName, relRiskRasterOutFName, Resolution, epsg, 
                surveyFName, myParams, gridSurvey)

        print('finish preProcessing')

        ## condition on presence of grid data
        if useGrids:
            myParams.setGridSurvey(rawdata.gridSurveyYears, rawdata.gridSurveyData,
                rawdata.gridSurveyMeans, rawdata.gridSurveySD, rawdata.gridSurveyCodes)

        # make object for pickling spatial data
        pickledat = preProcessing.PickleDat(rawdata)
        # pickle to output directory
        pickleName = os.path.join(outputDataPath, 'spatialData.pkl')
        fileobj = open(pickleName, 'wb')
        pickle.dump(pickledat, fileobj, protocol=4)
        fileobj.close()

    # If preProcessing has already been run (not first run)
    else:
        # unpickle results from preProcessing.py
        PKLFName = os.path.join(outputDataPath, 'spatialData.pkl')
        # unpickle preprocessing
        fileobj = open(PKLFName, 'rb')
        pickledat = pickle.load(fileobj)
        fileobj.close()

        myParams.setGridSurvey(pickledat.gridSurveyYears, pickledat.gridSurveyData,
            pickledat.gridSurveyMeans, pickledat.gridSurveySD, pickledat.gridSurveyCodes)

    result = calculation.calcProofOfAbsence(myParams, pickledat.survey, 
                pickledat.relativeRiskRaster, pickledat.zoneArray, pickledat.zoneCodes, 
                pickledat.match_geotrans, pickledat.wkt, outputDataPath,
                pickledat.RR_zone, pickledat.Pu_zone, pickledat.Name_zone)

    ################################################
    ####################
    # temp for debugging    
#    result.modifiedK = None
#    result.sensitivitiesPerYear = None
#    result.updatedMask = None
    ###################
    ################################################

    ## PRINT INTERMEDIATE RESULTS
#    print(result.intermediateResults)
    pickleName = os.path.join(outputDataPath, 'resultData.pkl')
    fileobj = open(pickleName, 'wb')
    pickle.dump(result, fileobj)
#    pickle.dump(result, fileobj, protocol=4)
    fileobj.close()
 
if __name__ == '__main__':
    main()


