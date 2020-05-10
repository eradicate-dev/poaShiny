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
library(raster)

use_condaenv("r_poa", required = TRUE)     # select conda envt

# file paths --------------------------------------------------------------


# load raw data -----------------------------------------------------------


# functions ---------------------------------------------------------------

unpickleWrapper <- function(picklePath){
  # unpickle results
  pickle <- reticulate::import("pickle", convert = FALSE, delay_load = TRUE)  
  # unpickle preprocessing
  fileobj = py$open(picklePath, 'rb')
  pickledat = pickle$load(fileobj)
  fileobj$close()
  return(pickledat)
}

# tidy data ---------------------------------------------------------------

# import proofofabsence package
# params <- import_from_path(module = "params", path = "proofofabsence", convert = FALSE)


# load py modules ---------------------------------------------------------

# load builtin py functions (required for open() below)
py <- reticulate::import_builtins(convert = FALSE)

# load PoA modules
params <- py_run_file(file = "proofofabsence/params.py", convert = FALSE)
preProcessing <- py_run_file("proofofabsence/preProcessing.py", convert = FALSE)
calculation <- py_run_file("proofofabsence/calculation.py", convert = FALSE)


# create parameter objects ------------------------------------------------

#-------------------------------------------------------------------------#
# set animal parameters
animals = params$AnimalTypes()
animal.params <-
  list(TYPENUM = as.integer(12:17),    # start IDs at arbritary number
       TYPECHAR = c("Leghold", "Sentinel", "PossMaster", "Camera", "CHEWDETECT", "AT220"),
       g0mean = c(0.06, 0.04, 0.04, 0.075, 0.06, 0.05),
       g0sd = rep(0.03, 6),
       sigmean = rep(30, 6),
       sigsd = rep(10, 6))
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
myParams$setRRTrapDistance(100)

myParams$setYears(r_to_py(as.integer(1)), r_to_py(as.integer(1)))

## THE startPu WILL NOT BE USED IF USE zoneData FILE - TURN OFF
# starting Pu (GRID CELL PREVALENCE) and period rate of Pu increase
startPu = as.double(as.integer(1))

## SET THE RATE OF INCREASE OF PU
PuIncreaseRate = as.double(0.0)
myParams$setPu(startPu, PuIncreaseRate)

# minimum RR value
myParams$setMinRR(as.double(1.0))

myParams$setPrior(0.7, 0.8, 0.899999)
myParams$setIntro(0.00001, 0.00002, 0.00003)        # (min, mode, max)


# create rawdata using preProcessing.RawData() ----------------------------

rawdata <- # preProcessing$
  preProcessing$RawData(zonesShapeFName = "test_data/Input/extent.shp", # "app\\www\\poa\\Kaitake\\Data\\extent_Kait.shp",
                        relativeRiskFName = "test_data/Input/relRiskRaster.tif", # "app\\www\\poa\\Kaitake\\Data\\relRiskRaster.tif",
                        zonesOutFName = "test_data/Results/zones.tif", #defaults$zonesOutFName$value, # "app\\www\\poa\\Kaitake\\Results\\Model_0\\zones.tif",
                        relRiskRasterOutFName = "test_data/Results/relRiskRaster.tif", # "app\\www\\poa\\Kaitake\\Results\\Model_0\\relRiskRaster.tif",
                        resolution = as.double(100),
                        epsg = as.integer(2193),
                        surveyFName = "test_data/Input/devices.csv",
                        params = myParams, 
                        gridSurveyFname = NULL)


# pickle rawdata using preprocessing.PickleDat() --------------------------

# make object for pickling spatial data
pickledat = preProcessing$PickleDat(rawdata)

# pickle to output directory
pickleName = "test_data/Results/spatialData.pkl"
fileobj = py$open(pickleName, 'wb')
preProcessing$pickle$dump(pickledat, fileobj, protocol=4)
fileobj$close()


# compare scenarios run using calculation.calcProofOfAbsence() ------------

# run /test_data scenario files
myParams$nIter <- r_to_py(as.integer(1000))                                       # set to 1000 iterations
res_calc <- calculation$calcProofOfAbsence(myParams, pickledat$survey,
                                           pickledat$relativeRiskRaster, pickledat$zoneArray, pickledat$zoneCodes,
                                           pickledat$match_geotrans, pickledat$wkt, "test_data/Results",
                                           pickledat$RR_zone, pickledat$Pu_zone, pickledat$Name_zone)

# run main() on same scenario files as above
py_run_file(file = "CopyOfKaitake_sc0_Farm.py")
res_main <- unpickleWrapper("test_data/Results/resultData.pkl")

# compare proportions searched
res_calc$proportionSearchedExtent
res_main$proportionSearchedExtent

# compare posterior quantiles
quantile(py_to_r(res_calc$poFMatrix))
quantile(py_to_r(res_main$poFMatrix))

# compare posterior densities
res_poFMatrix <- rbind(data.frame(method = "calculation.calcProofOfAbsence", 
                                  x = as.vector(py_to_r(res_calc$poFMatrix))),
                       data.frame(method = "CopyOfKaitake_sc0_Farm.py", 
                                  x = as.vector(py_to_r(res_main$poFMatrix))))
lattice::densityplot(x = ~x, groups = method, data = res_poFMatrix, bw = 0.01)


# trial R version of relative risk processing -----------------------------

# rawdata object stores makeRelativeRiskTif() function for processing relative risk map
rawdata_makeRelativeRiskTif <- rawdata$makeRelativeRiskTif("test_data/Input/relRiskRaster.tif", "test_data/Results/relRiskRaster.tif")

# relativeRiskRaster = rawdata.RelRiskExtent in PickleDat() 
# gets pickled as spatialData.pkl
all(py_to_r(rawdata$RelRiskExtent) == py_to_r(pickledat$relativeRiskRaster))
pickledat.relativeRiskRaster <- raster(py_to_r(pickledat$relativeRiskRaster), crs = sp::CRS("+init=epsg:2193"))

Input.relativeRiskRaster <- raster(x = "test_data/Input/relRiskRaster.tif")
# plot(reclassify(relativeRiskRaster, rcl = matrix(c(0,1,999), nrow = 1), right = TRUE))

# compare input and pre-processed rasters
plot(Input.relativeRiskRaster)          # plot rasters
plot(pickledat.relativeRiskRaster)

py_to_r(pickledat$relativeRiskRaster)[10:20,10:20]    # compare matrices
as.matrix(Input.relativeRiskRaster)[10:20,10:20]

# replicate preProcessing$makeRelativeRiskTif() ---------------------------

# load required modules from start of preProcessing.py
# - trying to replace these with R equivalents
gdal <- reticulate::import("gdal", convert = FALSE)
ogr <- reticulate::import("ogr", convert = FALSE)
osr <- reticulate::import("osr", convert = FALSE)
gdalconst <- reticulate::import("gdalconst", convert = FALSE)

# def makeRelativeRiskTif(self, relativeRiskFName, relRiskRasterOutFName):
relativeRiskFName <- "test_data/Input/relRiskRaster.tif"
relRiskRasterOutFName <- "test_data/Results/relRiskRaster.tif"

RR <- raster(relativeRiskFName)

#     """
#     read in rel risk ascii, and write relative risk Tiff to directory
#     if RR not given, then it is derived from the zones data
#     """
#     print('rr name', relativeRiskFName)
# 
self.cols <- as.integer(ncol(RR))
self.rows <- as.integer(nrow(RR))
dst = gdal$GetDriverByName('GTiff')$Create(relRiskRasterOutFName, r_to_py(self.cols), r_to_py(self.rows),
                                           r_to_py(as.integer(1)), gdalconst$GDT_Float32)
self.match_geotrans <- pickledat$match_geotrans
dst$SetGeoTransform(self.match_geotrans)
# spatial reference from making extent mask
self.wkt <- pickledat$wkt
dst$SetProjection(self.wkt)
 
if(!is.na(relativeRiskFName)){
  # Source - Kmap
  RR_src = gdal$Open(relativeRiskFName, gdalconst$GA_ReadOnly)
  if(!py_to_r(self.wkt) == py_to_r(RR_src$GetProjection())){
      stop("Projection of relative risk raster not the same as EPSG given")
  }
  # write kmap array to tif in directory
  # temp kmap tif name and directory
  # Reproject the kmap to the dimensions of the extent
  USE_GDAL_FOR_BILINEAR <- FALSE
  if(USE_GDAL_FOR_BILINEAR){
    gdal$ReprojectImage(RR_src, dst, self.wkt, self.wkt, gdalconst$GRA_Bilinear)
    RelRiskExtent = dst$GetRasterBand(r_to_py(as.integer(1)))$ReadAsArray()
  } else {
    inband = RR_src$GetRasterBand(r_to_py(as.integer(1)))
    inband$SetNoDataValue(r_to_py(as.integer(0)))
    in_im = inband$ReadAsArray()
  }
  
  # check in_im matches relativeRiskRaster
  all(py_to_r(in_im) == as.matrix(Input.relativeRiskRaster))
  
  # subset to extent of shape file
  geoTrans = RR_src$GetGeoTransform()
  invGeoTrans = gdal$InvGeoTransform(geoTrans)
  
  # Get layer dimensions of extent shapefile
  xy.minmax <- sf::st_bbox(sf::st_read("test_data/Input/extent.shp"))
  self.xmin <- xy.minmax["xmin"]
  self.ymin <- xy.minmax["ymin"]
  self.xmax <- xy.minmax["xmax"]
  self.ymax <- xy.minmax["ymax"]
  
  tlx = gdal$ApplyGeoTransform(invGeoTrans, r_to_py(self.xmin), r_to_py(self.ymax))[0]
  tly = gdal$ApplyGeoTransform(invGeoTrans, r_to_py(self.xmin), r_to_py(self.ymax))[1]
  self.resol = as.integer(100)
  brxy = gdal$ApplyGeoTransform(invGeoTrans,
                                    r_to_py(self.xmin + (self.resol * self.cols)),
                                    r_to_py(self.ymax - (self.resol * self.rows)))
  brx = brxy[0]
  bry = brxy[1]
    
  np <- reticulate::import("numpy", convert = FALSE)
  tlx = py$int(np$round(tlx))
  tly = py$int(np$round(tly))
  brx = py$int(np$round(brx))
  bry = py$int(np$round(bry))
  in_im = np_array(py_to_r(in_im)[py_to_r(tly):py_to_r(bry),py_to_r(tlx):py_to_r(brx)], dtype = "float32")
  
  # check in_im matches relativeRiskRaster
  all(py_to_r(in_im) == as.matrix(Input.relativeRiskRaster))
  
  nodata = inband$GetNoDataValue()
#             if nodata is None:
#                 raise ValueError("No Data value must be set on RR raster")
  RelRiskExtent = np$empty(c(r_to_py(self.rows), r_to_py(self.cols)), dtype=np$float32)
  
  all(py_to_r(RelRiskExtent) == py_to_r(pickledat$relativeRiskRaster))
  
  preProcessing$bilinear(in_im, RelRiskExtent, nodata)
  
  all(py_to_r(RelRiskExtent) == py_to_r(pickledat$relativeRiskRaster))
  
  all(py_to_r(unpickleWrapper("test_data/Results/spatialData.pkl")$relativeRiskRaster) ==
        py_to_r(rawdata$RelRiskExtent))
    
  # preProcessing.bilinear internals
  # out_im <- RelRiskExtent
  # 
  # xratio <- py_to_r(in_im$shape[1]) / py_to_r(out_im$shape[1])
  # yratio <- py_to_r(in_im$shape[0]) / py_to_r(out_im$shape[0])
  # 
  # out_im <- py_to_r(out_im)   # convert to matrix (dtype = float32)
  # for(y in seq_len(dim(out_im)[1])){
  #   for(x in seq_len(dim(out_im)[2])){
  #     out_im[y, x] = py_to_r(preProcessing$bilinear_interpolate(in_im, r_to_py(x * xratio), r_to_py(y * yratio), nodata))
  #   }
  # }
  # out_im <- np_array(out_im, dtype = "float32")
  # all(py_to_r(out_im) == as.matrix(RelRiskExtent))
  
  # check in_im matches relativeRiskRaster
  all(py_to_r(in_im) == as.matrix(Input.relativeRiskRaster))
  all(py_to_r(RelRiskExtent) == py_to_r(pickledat$relativeRiskRaster))
  
  py_to_r(RelRiskExtent) - py_to_r(pickledat$relativeRiskRaster)
  
  RelRiskExtent[11]
  pickledat$relativeRiskRaster[11]
  
  
  outband = dst$GetRasterBand(r_to_py(as.integer(1)))
  outband$WriteArray(RelRiskExtent)
 
  rm(dst)  # Flush
  rm(RR_src)
} else {
  # Source - zonecodes - RR=1 where zone>0
  self.zonesOutFName <- "test_data/Results/zones.tif"
  zoneArray = gdal$Open(self.zonesOutFName)$ReadAsArray()
  RelRiskExtent = np$where(r_to_py(py_to_r(zoneArray) > 0), r_to_py(as.integer(1)), r_to_py(as.integer(1)))$astype(np$float32)
  outband = dst$GetRasterBand(r_to_py(as.integer(1)))
  outband$WriteArray(RelRiskExtent)
#         
#     print('finish makeRRTif')
#     return RelRiskExtent




res_calc <- calculation$calcProofOfAbsence(myParams, pickledat$survey,
                                           pickledat$relativeRiskRaster, pickledat$zoneArray, pickledat$zoneCodes,
                                           pickledat$match_geotrans, pickledat$wkt, "test_data/Results",
                                           pickledat$RR_zone, pickledat$Pu_zone, pickledat$Name_zone)


