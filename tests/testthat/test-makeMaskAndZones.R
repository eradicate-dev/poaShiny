context("Compare makeMaskAndZones methods")
library(proofofabsence)


testthat::test_that("makeMaskAndZones is identical between python and R methods", {

  # load packages and modules
  proofofabsence::poa_paks_full()

  params = proofofabsence::makeParams(setMultipleZones = TRUE, setNumIterations = 1000,
                                      startYear = 1, endYear = 1,
                                      startPu = 1.0, PuIncreaseRate = 0.0,
                                      setRRTrapDistance = 100.0, setMinRR = 1.0,
                                      setPrior = c(0.6,0.8,0.85), setIntro = c(0.00001, 0.00002, 0.00003))

  # Create empty RawData class with paths to Nutria shapefile
  self <- PyClass(classname = "RawData",
                  defs = list(zonesShapeFName = np$str(system.file("example_data/Nutria_data_dean/extent.shp", package = "proofofabsence")),
                              relativeRiskFName = np$str(system.file("example_data/Nutria_data_dean/RR_High500.img", package = "proofofabsence")),
                              zonesOutFName = np$str("results/zones.tif"),
                              relRiskRasterOutFName = np$str("results/rr.tif"),
                              resol = np$double(100.0),
                              epsg = bi$int(26918))
  )

  # create directory for temporary results
  dir.create("results")

  # get spatial reference
  sr = osr$SpatialReference()
  sr$ImportFromEPSG(self$epsg)
  self$wkt = sr$ExportToWkt()

  # Get layer dimensions of extent shapefile
  # self.xmin, self.xmax, self.ymin, self.ymax = self.getShapefileDimensions(definition=False)
  dimsOut <- poa$preProcessing$RawData$getShapefileDimensions(self)
  self$xmin <- dimsOut[0]; self$xmax <- dimsOut[1]
  self$ymin <- dimsOut[2]; self$ymax <- dimsOut[3]

  # get number of columns and rows and set transformation
  # self.cols, self.rows, self.match_geotrans = self.getGeoTrans()
  GeoTrans <- poa$preProcessing$RawData$getGeoTrans(self)
  self$cols <- GeoTrans[0]; self$rows <-  GeoTrans[1]; self$match_geotrans <- GeoTrans[2]

  ##########################################
  # RUN FUNCTIONS
  ##########################################
  # (self.zoneArray, self.zoneCodes, self.Pu_zone, self.RR_zone, self.Name_zone) = (
  #             poa$preProcessing$RawData$makeMaskAndZones(self, params$multipleZones, params))

  zone0 <- poa$preProcessing$RawData$makeMaskAndZones(self, multipleZones = params$multipleZones, params = params)
  zone1 <- proofofabsence::makeMaskAndZones_reticulated(self, params$multipleZones, params)
  zone2 <- proofofabsence::makeMaskAndZones(self, TRUE, params)


  # function to test equality in numpy objects
  test_np_equal <- function(x,y) np$all(np$equal(x,y)) == bi$True

  testthat::expect_true(test_np_equal(zone0[0], zone1[[1]]))
  testthat::expect_true(test_np_equal(zone0[1], zone1[[2]]))
  testthat::expect_true(test_np_equal(zone0[2], zone1[[3]]))
  testthat::expect_true(test_np_equal(zone0[3], zone1[[4]]))
  # testthat::expect_true(test_np_equal(zone0[4], zone1[[5]]))

  testthat::expect_true(test_np_equal(zone0[0], zone2[[1]]))
  testthat::expect_true(test_np_equal(zone0[1], zone2[[2]]))
  testthat::expect_true(test_np_equal(zone0[2], zone2[[3]]))
  testthat::expect_true(test_np_equal(zone0[3], zone2[[4]]))
  # testthat::expect_true(test_np_equal(zone0[4], zone2[[5]]))

  # tidy up result files
  unlink("results", recursive = TRUE)

})
#> Test passed 😀
