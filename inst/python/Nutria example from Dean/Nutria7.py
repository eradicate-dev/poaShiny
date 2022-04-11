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

from proofofabsence import preProcessing
from proofofabsence import params
from proofofabsence import calculation

######################
# Main function
def main():
    ############################################################
    ###################################### USER MODIFY HERE ONLY
    #################################

    ## NOTES:  No grid surveillance


    # set paths to scripts and data                                             
    ##   <<< COMPULSORY >>>
    inputDataPath = os.path.join(os.getenv('POFPROJDIR', default = '.'), 'poa', 
            'Nutria', 'Data')
        
    ##   <<< COMPULSORY >>>
    outputDataPath = os.path.join(os.getenv('POFPROJDIR', default = '.'), 'poa', 
            'Nutria', 'Results', 'mod7')

    ##   <<< COMPULSORY >>>
    # set INPUT extent and relative risk file names
    zoneShapeFName = os.path.join(inputDataPath, 'mngtZone_LowPu.shp')  # 'mngtZonesPu_1.shp')     #'mngtZone_HalfCellGrow.shp')    # 'mngtZonesPu_3.shp')
#    zoneShapeFName = os.path.join(inputDataPath, 'noZoneTestNutria.shp')  # 'mngtZonesPu_1.shp')     #'mngtZone_HalfCellGrow.shp')    # 'mngtZonesPu_3.shp')



    ##   <<< OPTIONAL WITH DEFAULT = None >>>
    # relativeRiskFName = os.path.join(inputDataPath, 'RR_High500.img')   # 'RelRiskNutria.img')  
    relativeRiskFName = None

   
    ##   <<< OPTIONAL >>>
    # Surveillance data Name
    surveyFName = os.path.join(inputDataPath, 'AllNutriaSurvey_2024.csv') # or None 'testDat.csv')

    ## <<< HARD WIRED FOR NOW >>>
    # set OUTPUT names for mask and relative risk map
    zoneOutFName = os.path.join(outputDataPath, 'zones.tif')
    relRiskRasterOutFName = os.path.join(outputDataPath, 'relRiskRaster.tif')

    ##   <<< COMPULSORY WITH DEFAULT = False >>>
    ############ IF USE GRIDS
    useGrids = True
    ##   <<< OPTIONAL >>>
    gridSurvey = os.path.join(inputDataPath, 'gridPublicSur7.csv')  # None

    ##   <<< OPTIONAL WITH DEFAULT = True >>>
    ############ IF FIRST RUN CONDITION
    # if True, do preprocessing, else skip to calculations   
    firstRun = True        # True or False

    ##   <<< COMPULSORY >>>
    # resolution for analysis
    Resolution = 100.0

    ##   <<< COMPULSORY >>>
    # EPSG - PROJECTION SYSTEM
    epsg = 26918    # "NAD_1983_UTM_Zone_18N"
   
    ##   <<< OPTIONAL >>>
    #############################################################################
    ##### ADD SURVEILLANCE TYPES                                                #
    animals = params.AnimalTypes()                                              #
    TYPE_LANDPLATFORM = 12 # index number for land platform                     #
    animals.addAnimal(TYPE_LANDPLATFORM, "LandPlatform", params.DETECT_ANIMAL)  #
    TYPE_WATERPLATFORM = 13 # index number for WATER PLATFORM                   #
    animals.addAnimal(TYPE_WATERPLATFORM, "WaterPlatform", params.DETECT_ANIMAL)#
    TYPE_CAMERA = 14 # index number for Camera traps                            #
    animals.addAnimal(TYPE_CAMERA, "Camera", params.DETECT_ANIMAL)              #
    TYPE_DOGTRACK = 15 # index number for DOG ONLY TRACKS                       #
    animals.addAnimal(TYPE_DOGTRACK, "DogTrack", params.DETECT_ANIMAL)          #
    TYPE_GROUND = 16 # index number for SPECIALITY GROUND SURVEY                #
    animals.addAnimal(TYPE_GROUND, "Ground", params.DETECT_ANIMAL)              #
    TYPE_GROUNDDOG = 17 # index number for HUMAN TRACKS WITH DOG GROUND SURVEY  #
    animals.addAnimal(TYPE_GROUNDDOG, "GroundDog", params.DETECT_ANIMAL)        #
    TYPE_HANDLER = 18 # index number for DOG HANDLER TRACKS                     #
    animals.addAnimal(TYPE_HANDLER, "Handler", params.DETECT_ANIMAL)            #
    TYPE_SHOREDOG = 19 # index number for SPECIALITY ON SHORE WITH DOG          #
    animals.addAnimal(TYPE_SHOREDOG, "ShoreDog", params.DETECT_ANIMAL)          #
    TYPE_SHORELINE = 20 # index number for SPECIALITY Shoreline                 #
    animals.addAnimal(TYPE_SHORELINE, "Shoreline", params.DETECT_ANIMAL)        #
    #############################################################################

    # Instance of POAParameters class
    myParams = params.POAParameters(animals)
   
    ##   <<< OPTIONAL >>>
    #############################################################################
    # ADD LANDPLATFORM PARAMETERS                                               #
    myParams.setCapture(12, 0.04, 0.01)   # note the 12 indicates LANDPLATFORM #
    myParams.setSigma(12, 100.0, 40.0)                                          #
    myParams.addRRBufferAnimal(TYPE_LANDPLATFORM)                               #
    # ADD WATERPLATFORM PARAMETERS                                              #
    myParams.setCapture(13, 0.02, 0.01)   # .008                               #
    myParams.setSigma(13, 100.0, 40.0)                                          #
    myParams.addRRBufferAnimal(TYPE_WATERPLATFORM)                              #
    # ADD CAMERA PARAMETERS                                                     #
    myParams.setCapture(14, 0.06, 0.02)                                        #
    myParams.setSigma(14, 100.0, 40.0)                                          #
    myParams.addRRBufferAnimal(TYPE_CAMERA)                                     #
    # DOGTRACK                                                                  #
    myParams.setCapture(15, 0.14, 0.06)                                       #
    myParams.setSigma(15, 100.0, 40.0)                                          #
    myParams.addRRBufferAnimal(TYPE_DOGTRACK)                                   #
    # GROUND                                                                    #
    myParams.setCapture(16, 0.02, 0.01)                                      #
    myParams.setSigma(16, 100.0, 40.0)                                          #
    myParams.addRRBufferAnimal(TYPE_GROUND)                                     #
    # GROUNDDOG                                                                 #
    myParams.setCapture(17, 0.02, 0.01)                                      #
    myParams.setSigma(17, 100.0, 40.0)                                          #
    myParams.addRRBufferAnimal(TYPE_GROUNDDOG)                                  #
    # HANDLER                                                                   #
    myParams.setCapture(18, 0.02, 0.01)                                      #
    myParams.setSigma(18, 100.0, 40.0)                                          #
    myParams.addRRBufferAnimal(TYPE_HANDLER)                                    #
    # SHOREDOG                                                                  #
    myParams.setCapture(19, 0.02, 0.01)                                      #
    myParams.setSigma(19, 100.0, 40.0)                                          #
    myParams.addRRBufferAnimal(TYPE_SHOREDOG)                                   #
    # SHORELINE                                                                 #
    myParams.setCapture(20, 0.02, 0.01)                                      #
    myParams.setSigma(20, 100.0, 40.0)                                          #
    myParams.addRRBufferAnimal(TYPE_SHORELINE)                                  #
    #############################################################################
   


    
    ##   <<< OPTIONAL - MULTI THREADING IS SET UP FOR SLURM ON NESI >>>
    # number of cpu's from SLURM
    ncpus = int(os.getenv('SLURM_CPUS_PER_TASK', '1'))
    myParams.setNumThreads(ncpus)


    ##   <<< COMPULSORY >>>
    # number of iterations
    myParams.setNumIterations(2)
   
    ##   <<< OPTIONAL IF USING POINT SURVEILLANCE - SEE LINE 62 >>>
    myParams.setRRTrapDistance(100.0)

    ##   <<< COMPULSORY - PERHAPS CHANGE TO SESSIONS NOT YEARS >>>
    startYear = 2015
    endYear = 2024
    myParams.setYears(startYear, endYear)

    ##   <<< OPTIONAL - DEFAULT = False >>>
    ## USE MULTIPLE ZONES
#    myParams.setMultipleZones(False)
    myParams.setMultipleZones(True)

    ##   <<< OPTIONAL WITH DEFAULT = 1 >>>
    ## THE startPu WILL NOT BE USED IF USE zoneData FILE - TURN OFF
    # starting Pu (GRID CELL PREVALENCE) and period rate of Pu increase
    startPu = 1.0

   
    ##   <<< OPTIONAL WITH DEFAULT = 0 >>>
    ## SET THE RATE OF INCREASE OF PU
    PuIncreaseRate = 1.0
    myParams.setPu(startPu, PuIncreaseRate)

   
    ##   <<< OPTIONAL; SET IF USE RELATIVE RISK (LINE 55) >>>
    #minimum RR value
    myParams.setMinRR(0.5)

    ##   <<< COMPULSORY >>>
    myParams.setPrior(0.001, 0.01, 0.1)
#    myParams.setPrior(0.005, 0.15, 0.3)

    ##   <<< COMPULSORY >>>
    myParams.setIntro(0.00005, 0.0001, 0.0002)

    #####################################   END USER MODIFICATION
    #############################################################
    #############################################################

    print('firstRun = ', firstRun)
    print('Number of Iterations', myParams.nIter)

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

            print('Grid raster FName', gridSurvey)

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
            pickledat.gridSurveyMeans, pickledat.gridSurveySD, 
            pickledat.gridSurveyCodes)

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


