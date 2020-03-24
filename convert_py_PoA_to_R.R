# Description: ------------------------------------------------------------
#
# convert py PoA scripts to R functions
#  - note1
#  - note2
#  - note3
# Built under R version 3.6.1 (2019-07-05)
# Simon Howard; howards@landcareresearch.co.nz | si.w.howard@gmail.com
#-------------------------------------------------------------------------#

# packages ----------------------------------------------------------------
library(reticulate)
use_condaenv("r_poa", required = TRUE)     # select conda envt

# file paths --------------------------------------------------------------


# load raw data -----------------------------------------------------------


# tidy data ---------------------------------------------------------------

# import proofofabsence package
# params <- import_from_path(module = "params", path = "proofofabsence", convert = FALSE)


params <- py_run_file(file = "proofofabsence/params.py", convert = FALSE)
preProcessing <- py_run_file("proofofabsence/preProcessing.py", convert = FALSE)
calculation <- py_run_file("proofofabsence/calculation.py", convert = FALSE)

#-------------------------------------------------------------------------#
# set animal parameters
animals = params$AnimalTypes()
animal.params <-
  list(TYPENUM = as.integer(6),    # start IDs at arbritary number
       TYPECHAR = "Camera",
       g0mean = 0.1,
       g0sd = 0.02,
       sigmean = 90,
       sigsd = 10)
for(i in seq_along(animal.params$TYPENUM)){
  print(i)
  animals$addAnimal(animal = animal.params$TYPENUM[i],
                    name = animal.params$TYPECHAR[i],
                    detect = params$DETECT_ANIMAL)
}
myParams = params$POAParameters(animals)

## USE MULTIPLE ZONES
myParams$setMultipleZones(TRUE)

for(i in seq_along(animal.params$TYPENUM)){
  TYPE <- as.integer(animal.params$TYPENUM[i])
  myParams$setCapture(TYPE, animal.params$g0mean[i], animal.params$g0sd[i])
  myParams$setSigma(TYPE, animal.params$sigmean[i], animal.params$sigsd[i])
  myParams$addRRBufferAnimal(animalCode = TYPE)
}


# number of iterations
myParams$setNumIterations(as.integer(10))
#    myParams.setNumChewcardTraps(3)
myParams$setRRTrapDistance(10)

myParams$setYears(as.integer(1), as.integer(1))

## THE startPu WILL NOT BE USED IF USE zoneData FILE - TURN OFF
# starting Pu (GRID CELL PREVALENCE) and period rate of Pu increase
startPu = as.double(as.integer(1))

## SET THE RATE OF INCREASE OF PU
PuIncreaseRate = as.double(0.0)
myParams$setPu(startPu, PuIncreaseRate)

# minimum RR value
myParams$setMinRR(as.double(1))

myParams$setPrior(0.45, 0.5, 0.55)
myParams$setIntro(0.00001, 0.00002, 0.00003)        # (min, mode, max)

rawdata <- # preProcessing$
  preProcessing$RawData(zonesShapeFName = "testdata/input/extent.shp", # "app\\www\\poa\\Kaitake\\Data\\extent_Kait.shp",
          relativeRiskFName = "testdata/input/relRiskRaster.tif", # "app\\www\\poa\\Kaitake\\Data\\relRiskRaster.tif",
          zonesOutFName = "testdata/output/zones.tif", #defaults$zonesOutFName$value, # "app\\www\\poa\\Kaitake\\Results\\Model_0\\zones.tif",
          relRiskRasterOutFName = "testdata/output/relRiskRaster.tif", # "app\\www\\poa\\Kaitake\\Results\\Model_0\\relRiskRaster.tif",
          resolution = as.double(50),
          epsg = as.integer(2193),
          surveyFName = "testdata/input/devices.csv",
          params = myParams, 
          gridSurveyFname = NULL)

# load builtin py functions (required for open() below)
py <- reticulate::import_builtins()
# make object for pickling spatial data
pickledat = preProcessing$PickleDat(rawdata)

# pickle to output directory
pickleName = "testdata/output/spatialData.pkl"
fileobj = py$open(pickleName, 'wb')
preProcessing$pickle$dump(pickledat, fileobj, protocol=4)
fileobj$close()


result = # calculation$
  calculation$calcProofOfAbsence(myParams, pickledat$survey,
                     pickledat$relativeRiskRaster, pickledat$zoneArray, pickledat$zoneCodes,
                     pickledat$match_geotrans, pickledat$wkt, "testdata/output",
                     pickledat$RR_zone, pickledat$Pu_zone, pickledat$Name_zone)
