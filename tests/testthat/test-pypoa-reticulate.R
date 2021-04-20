context("Match Kaitake example")
library(proofofabsence)


test_that("python functions match reticulated equivalents", {

  # load packages and modules
  proofofabsence::poa_paks_full()

  # set inputs to run Kaitake example
  inputDataPath <- system.file("example_data/Kaitake_possums", package = "proofofabsence")
  outputDataPath <- "results"
  zoneShapeFName <- "extent.shp"
  relativeRiskFName <- "relRiskRaster.tif"
  surveyFName <- "devices.csv"
  useGrids <- FALSE
  firstRun <- TRUE
  Resolution <- 100.0
  epsg <- 2193
  myParams <- proofofabsence::makeParams(setMultipleZones = FALSE, setNumIterations = 1000,
                          startYear = 1, endYear = 1,
                          startPu = 1.0, PuIncreaseRate = 0.0,
                          setRRTrapDistance = 100.0, setMinRR = 1.0,
                          setPrior = c(0.6,0.8,0.85), setIntro = c(0.00001, 0.00002, 0.00003))

  #----------------------------------------------------------#
  #------------------------------------# USER MODIFY HERE ONLY
  #-------------------------------#
  # set paths to scripts and data
  if(!dir.exists(inputDataPath)) dir.create(inputDataPath, recursive = T)
  if(!dir.exists(outputDataPath)) dir.create(outputDataPath, recursive = T)

  # set INPUT extent and relative risk file names
  zoneShapeFName = os$path$join(inputDataPath, zoneShapeFName)
  relativeRiskFName = os$path$join(inputDataPath, relativeRiskFName)

  # Surveillance data Name
  surveyFName = os$path$join(inputDataPath, surveyFName) # or None

  # set OUTPUT names for mask and relative risk map
  zoneOutFName = os$path$join(outputDataPath, 'zones.tif')
  relRiskRasterOutFName = os$path$join(outputDataPath, 'relRiskRaster.tif')


  ############ IF USE GRIDS
  useGrids = r_to_py(useGrids)
  gridSurvey = r_to_py(NULL)   # os.path.join(inputDataPath, 'gridScenario14.csv')

  ############ IF FIRST RUN CONDITION
  # if True, do preprocessing, else skip to calculations
  firstRun = r_to_py(firstRun)        # True or False

  # resolution for analysis
  Resolution = np$double(np$double(Resolution))
  # EPSG - PROJECTION SYSTEM
  epsg = np$int(epsg)    # NZ transverse mercator

  myParams <- proofofabsence::addAnimalParams(POAParameters = myParams,
                              deviceName = c("Leghold", "Sentinel", "PossMaster", "Camera", "CHEWDETECT", "AT220"),
                              g0 = c(0.06,0.04,0.04,0.075,0.09,0.05), g0sd = c(0.03,0.03,0.03,0.03,0.03,0.03),
                              sig = rep(150,6), sigsd = rep(20,6))

  #-----------------------------------#   END USER MODIFICATION
  #-----------------------------------------------------------#
  #-----------------------------------------------------------#

  # initiate instances of Classes
  rawdata = poa$preProcessing$RawData(zoneShapeFName, relativeRiskFName,
                                      zoneOutFName, relRiskRasterOutFName, Resolution, epsg,
                                      surveyFName, myParams, gridSurvey)
  pickledat1 = poa$preProcessing$PickleDat(rawdata)



  # unpack pre-run test data ------------------------------------------------

  validDataPath <- system.file("validation_data", package = "proofofabsence")
  PKLFName = os$path$join(validDataPath, 'spatialData.pkl')
  # unpickle preprocessing
  fileobj = bi$open(PKLFName, 'rb')
  pickledat0 = pickle$load(fileobj)
  fileobj$close()
  rm(fileobj)


  # compare pre-run inputs --------------------------------------------------

  # function to test equality in numpy objects
  test_np_equal <- function(x,y) np$all(np$equal(x,y)) == bi$True

  testthat::expect_true(test_np_equal(pickledat0$Pu_zone, pickledat1$Pu_zone))
  testthat::expect_true(test_np_equal(pickledat0$RR_zone, pickledat1$RR_zone))
  testthat::expect_true(test_np_equal(pickledat0$relativeRiskRaster, pickledat1$relativeRiskRaster))
  testthat::expect_true(test_np_equal(pickledat0$zoneArray, pickledat1$zoneArray))


  # calculate poa on inputs -------------------------------------------------

  result1 <- poa$calculation$calcProofOfAbsence(myParams, pickledat1$survey,
                                     pickledat1$relativeRiskRaster, pickledat1$zoneArray, pickledat1$zoneCodes,
                                     pickledat1$match_geotrans, pickledat1$wkt, outputDataPath,
                                     pickledat1$RR_zone, pickledat1$Pu_zone, pickledat1$Name_zone)


  # unpack pre-run result data ----------------------------------------------

  validDataPath <- system.file("validation_data", package = "proofofabsence")
  PKLFName = os$path$join(validDataPath, 'resultData.pkl')
  # unpickle preprocessing
  fileobj = bi$open(PKLFName, 'rb')
  result0 = pickle$load(fileobj)
  fileobj$close()
  rm(fileobj)

  # compare pre-run results--------------------------------------------------

  probs <- c(0.05,0.1,0.5,0.9,0.95)

  # compare priors
  expect_equal(quantile(as.double(result0$priorStore), probs),
               quantile(as.double(result1$priorStore), probs), tolerance = 1e-2)
  # compare sensitivity
  expect_equal(quantile(as.double(result0$sensitivityMatrix), probs),
               quantile(as.double(result1$sensitivityMatrix), probs), tolerance = 1e-3)
  # compare posteriors
  expect_equal(quantile(as.double(result0$poFMatrix), probs),
               quantile(as.double(result1$poFMatrix), probs), tolerance = 1e-4)
  # compare proportion searched
  expect_equal(as.double(result0$proportionSearchedExtent),
               as.double(result1$proportionSearchedExtent), tolerance = 1e-3)

  # tidy up result files
  unlink("results", recursive = TRUE)

})
#> Test passed 😀
