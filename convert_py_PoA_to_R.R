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

print('Grid Survey Data', gridSurveyData)

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



# getSensitivityForCell ---------------------------------------------------
def getSensitivityForCell(tmpData, dKern, traps, nCurrentTrap, paramArray,
                          nChewcardTraps):
#- inputs ----------------------------------------------------------------#
nCurrentTrap <- 6
py_to_r(pickledat$survey)

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




#-------------------------------------------------------------------------#
# inputs
calculation$getSensitivityForCell(tmpData, dKern, traps, 
                                  nCurrentTrap, paramArray,
                                  nChewcardTraps)
py_eval("np.zeros(10)", convert = FALSE)


