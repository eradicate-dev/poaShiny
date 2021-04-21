# TODO --------------------------------------------------------------------

# make reticulated readGridSurveyData when I get access to example data


#' Import required packages and modules required to run proof of absence scripts
#'
#' @param condaenv Name of configured conda environment
#'
#' @return
#' @export
#'
#' @examples
#' proofofabsence::poa_paks_full()
poa_paks_min <- function(condaenv = "proofofabsence"){

  # configure reticulate ----------------------------------------------------

  reticulate::use_condaenv(condaenv)
  reticulate::py_config()
  library(reticulate)
  library(proofofabsence)

  #-------------------------------------------------------------------------#
  # lines to set GDAL path to conda environment
  #  - only needed if using outside an activated conda environment
  # Sys.setenv(GDAL_DATA = "C:/ProgramData/Anaconda3/envs/proofofabsence/Library/share/gdal")
  #-------------------------------------------------------------------------#

  # IMPORT MODULES
  os        <<- reticulate::import("os", convert = FALSE)
  np        <<- reticulate::import("numpy", convert = FALSE)
  pickle    <<- reticulate::import("pickle", convert = FALSE)
  numba     <<- reticulate::import("numba", convert = FALSE)

  bi <<- import_builtins(convert = FALSE)
  poa <<- import_from_path(path = system.file("python", package = "proofofabsence"),
                           module = "proofofabsence_min", convert = FALSE)
}


# class RawData():
#' Title
#'
#' @param zonesShapeFName Path to zones shapefile
#' @param relativeRiskFName Path to relative risk raster file
#' @param zonesOutFName Save path for processed zones raster file
#' @param relRiskRasterOutFName Save path for processed relative risk raster file
#' @param resolution Set resolution for processed rasters
#' @param epsg EPSG for target shapefiles and rasters
#' @param surveyFName Paths to survey devices .csv
#' @param params POAParameters object
#' @param gridSurveyFname gridSurveyFname
#'
#' @return
#' @export
#'
#' @examples
#' proofofabsence::RawData_R()
RawData_R <- function(
        # inputs
        zonesShapeFName = "inst/example_data/Kaitake_possums/extent.shp",
        relativeRiskFName = "inst/example_data/Kaitake_possums/relRiskRaster.tif",
        zonesOutFName = "results/zones.tif",
        relRiskRasterOutFName = "results/rr.tif",
        resolution = 100.0,
        epsg = 2193,
        surveyFName = bi$str("inst/example_data/Kaitake_possums/devices.csv"),
        params = proofofabsence::preProcessing_reticulated()[["myParams"]],
        gridSurveyFname=bi$None){

        # replace RawData class self object with an R list
        self <- list()

        self$zonesShapeFName = zonesShapeFName
        self$relativeRiskFName = relativeRiskFName
        self$zonesOutFName = zonesOutFName
        self$relRiskRasterOutFName = relRiskRasterOutFName
        self$resol = resolution
        self$epsg = epsg

        # get spatial reference
        # sr = osr$SpatialReference()
        # sr$ImportFromEPSG(bi$int(self$epsg))
        # self$wkt = sr$ExportToWkt()
        self$wkt <- rgdal::showWKT(p4s = "+init=epsg:2193", morphToESRI = TRUE)
        
        
        # Get layer dimensions of extent shapefile
        # self[c("xmin","xmax","ymin", "ymax")] <- getShapefileDimensions_reticulated(self = self, definition=np$False_)
        self[c("xmin","xmax","ymin", "ymax")] <- getShapefileDimensions_R(zonesShapeFName)
        # get number of columns and rows and set transformation
        self[c("cols","rows","match_geotrans")] <- getGeoTrans_R(self)

        #----------------------------------------#
        # RUN FUNCTIONS
        #----------------------------------------#
        # (self.zoneArray, self.zoneCodes, self.Pu_zone, self.RR_zone, self.Name_zone) = (
        #             self.makeMaskAndZones(params.multipleZones, params))
        self[c("zoneArray", "zoneCodes", "Pu_zone", "RR_zone", "Name_zone")] <-
          # suppressWarnings(makeMaskAndZones(self, multipleZones = py_to_r(params$multipleZones), params))
          suppressWarnings(makeMaskAndZones(self, multipleZones = py_to_r(params$multipleZones), params))

        print(paste('Name_zone', self$Name_zone))

        # self.RelRiskExtent = self.makeRelativeRiskTif(relativeRiskFName,
        #                             relRiskRasterOutFName)
        
        self$RelRiskExtent <- makeRelativeRiskTif(self = self, relativeRiskFName = relativeRiskFName,relRiskRasterOutFName)

        print(paste('surveyFName', surveyFName))

        # condition to use point survey data or not
        # if surveyFName is not None:
        if(!is.null(surveyFName)){
            print(paste('params.animals', params$animals))
            self$survey = poa$preProcessing$RawData$readSurveyData(self, surveyFName, params$animals)
        } else {
            # not present, but need an empty array for processing
            # self.survey = np.empty((0,), dtype=TRAP_PARAM_DTYPE)
            self$survey = np$empty(tuple(0L), dtype=TRAP_PARAM_DTYPE)
        }

        # if gridSurveyFname is not None:
        if(!is.null(gridSurveyFname)){
            # (self.gridSurveyYears, self.gridSurveyMeans, self.gridSurveySD,
            #         self.gridSurveyCodes, self.gridSurveyData) = self.readGridSurveyData(
            #         gridSurveyFname, params)
        } else {
            # not present
            self$gridSurveyYears = bi$None
            self$gridSurveyData = bi$None
            self$gridSurveyMeans = bi$None
            self$gridSurveySD = bi$None
            self$gridSurveyCodes = bi$None
        }

        # END RUN FUNCTIONS
        #----------------------------------------#
        return(self)
}

#' preProcessing_reticulated
#'
#' preProcessing_reticulated
#' @param parameter 1
#' @param parameter 2
#' @keywords keywords
#' @export
#' @examples
#'  proofofabsence::preProcessing_reticulated()

preProcessing_reticulated <- function(
           inputDataPath = "inst/example_data/Kaitake_possums", # system.file("example_data/Kaitake_possums", package = "proofofabsence"),
           outputDataPath = "results",
           zoneShapeFName = "extent.shp",
           relativeRiskFName = "relRiskRaster.tif",
           surveyFName = "devices.csv",
           useGrids = FALSE,
           firstRun = TRUE,
           Resolution = 100.0,
           epsg = 2193,
           setMultipleZones = FALSE,
           setNumIterations = 2,
           setRRTrapDistance = 100.0,
           startYear = 1,
           endYear = 1,
           startPu = 1.0,
           PuIncreaseRate = 0.0,
           setMinRR = 1.0,
           setPrior = c(0.10, 0.2, 0.70),
           setIntro = c(0.001, 0.05, 0.30)){

  # source("poa_objs.R")

  #----------------------------------------------------------#
  #------------------------------------# USER MODIFY HERE ONLY
  #-------------------------------#
  # set paths to scripts and data
  if(!dir.exists(inputDataPath)) dir.create(inputDataPath, recursive = T)
  if(!dir.exists(outputDataPath)) dir.create(outputDataPath, recursive = T)

  # set INPUT extent and relative risk file names
  zoneShapeFName = os$path$join(inputDataPath, zoneShapeFName)

  #'extentOnly.shp')    #'extent_block1.shp')
  #'extent_gradient.shp')
  relativeRiskFName = os$path$join(inputDataPath, relativeRiskFName)
  #'habDistRR.tif')
  #'RR_mahia.tif')
  #    relativeRiskFName = None

  # Surveillance data Name
  surveyFName = os$path$join(inputDataPath, surveyFName) # or None

  # set OUTPUT names for mask and relative risk map
  zoneOutFName = os$path$join(outputDataPath, 'zones.tif')
  relRiskRasterOutFName = os$path$join(outputDataPath, 'relRiskRaster.tif')
  #    relRiskRasterOutFName = None

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

  #---------------------------------------------------------------------------#
  #---# ADD SURVEILLANCE TYPES                                                #
  TYPE_LEGHOLD = np$int(12) # index number for leghold traps                  #
  animals = poa$params$AnimalTypes()                                              #
  animals$addAnimal(TYPE_LEGHOLD, "Leghold", poa$params$DETECT_ANIMAL)            #
  #---------------------------------------------------------------------------#

  # Instance of POAParameters class
  myParams = poa$params$POAParameters(animals)

  ## USE MULTIPLE ZONES
  myParams$setMultipleZones(r_to_py(setMultipleZones))

  #---------------------------------------------------------------------------#
  # ADD LEGHOLD TRAP PARAMETERS                                               #
  myParams$setCapture(np$int(12), np$double(0.06), np$double(0.03))           # note the 12 indicates LEGHOLD
  myParams$setSigma(np$int(12), np$double(150.0), np$double(20.0))            #
  myParams$addRRBufferAnimal(TYPE_LEGHOLD)                                    #
  #---------------------------------------------------------------------------#

  # number of cpu's from SLURM
  ncpus = np$int(os$getenv('SLURM_CPUS_PER_TASK', '1'))
  myParams$setNumThreads(ncpus)

  # print('ncpus', ncpus)

  # number of iterations
  myParams$setNumIterations(np$int(setNumIterations))
  #    myParams$setNumChewcardTraps(3)
  myParams$setRRTrapDistance(np$double(setRRTrapDistance))

  ##   <<< COMPULSORY - PERHAPS CHANGE TO SESSIONS NOT YEARS >>>
  # startYear = np$int(1)
  # endYear = np$int(1)
  myParams$setYears(np$int(startYear), np$int(endYear))

  ## THE startPu WILL NOT BE USED IF USE zoneData FILE - TURN OFF
  # starting Pu (GRID CELL PREVALENCE) and period rate of Pu increase
  # startPu = np$double(1.0)

  ## SET THE RATE OF INCREASE OF PU
  # PuIncreaseRate = np$double(0.0)
  # myParams$setPu(startPu, PuIncreaseRate)
  myParams$setPu(np$double(startPu), np$double(PuIncreaseRate))

  #    ## LOGISTIC GROWTH OF INDIVIDUALS AND GRID CELLS
  #    rmax_pstar = 0.5
  #    ki = 10.000001   # max individuals per flock/cell
  #    ku = 1.0   # max number of flocks/cells
  #    pind = 10.0   # starting individual design prevalence
  #    p_disperse = 12.0    # when pind reaches this, start new cell/flock
  #    pu = 1.0     # starting cell design prevalence
  #    myParams.setPuLogistic(rmax_pstar, ki, ku, pind, p_disperse, pu)

  #minimum RR value
  myParams$setMinRR(np$double(setMinRR))

  myParams$setPrior(np$double(setPrior[1]), np$double(setPrior[2]), np$double(setPrior[3]))
  myParams$setIntro(np$double(setIntro[1]), np$double(setIntro[2]), np$double(setIntro[3]))

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

  #-----------------------------------#   END USER MODIFICATION
  #-----------------------------------------------------------#
  #-----------------------------------------------------------#

  # print('firstRun = ', firstRun)

  if(py_to_r(firstRun)){
    # initiate instances of Classes
    rawdata = poa$preProcessing$RawData(zoneShapeFName, relativeRiskFName,
                                    zoneOutFName, relRiskRasterOutFName, Resolution, epsg,
                                    surveyFName, myParams, gridSurvey)

    print('finish preProcessing')

    ## condition on presence of grid data
    if(py_to_r(useGrids)){
      myParams$setGridSurvey(rawdata.gridSurveyYears, rawdata.gridSurveyData,
                             rawdata.gridSurveyMeans, rawdata.gridSurveySD, rawdata.gridSurveyCodes)
    }

    # make object for pickling spatial data
    pickledat = poa$preProcessing$PickleDat(rawdata)
    # pickle to output directory
    pickleName = os$path$join(outputDataPath, 'spatialData.pkl')
    fileobj = bi$open(pickleName, 'wb')
    pickle$dump(pickledat, fileobj, protocol=4)
    fileobj$close()

    # If preProcessing has already been run (not first run)
  } else {
    # unpickle results from preProcessing.py
    PKLFName = os$path$join(outputDataPath, 'spatialData.pkl')
    # unpickle preprocessing
    fileobj = bi$open(PKLFName, 'rb')
    pickledat = pickle$load(fileobj)
    fileobj$close()

    # myParams$setGridSurvey(pickledat$gridSurveyYears, pickledat$gridSurveyData,
    #                        pickledat$gridSurveyMeans, pickledat$gridSurveySD, pickledat$gridSurveyCodes)
  }

  # result = poa$calculation$calcProofOfAbsence(myParams, pickledat$survey,
  #                                         pickledat$relativeRiskRaster, pickledat$zoneArray, pickledat$zoneCodes,
  #                                         pickledat$match_geotrans, pickledat$wkt, outputDataPath,
  #                                         pickledat$RR_zone, pickledat$Pu_zone, pickledat$Name_zone)

  #----------------------------------------------#
  #-----------------#
  # temp for debugging
  #    result.modifiedK = None
  #    result.sensitivitiesPerYear = None
  #    result.updatedMask = None
  #-----------------#
  #----------------------------------------------#

  ## PRINT INTERMEDIATE RESULTS
  #    print(result.intermediateResults)
  # pickleName = os$path$join(outputDataPath, 'resultData.pkl')
  # fileobj = builtins$open(pickleName, 'wb')
  # pickle$dump(result, fileobj)
  # #    pickle.dump(result, fileobj, protocol=4)
  # fileobj$close()

  # if __name__ == '__main__':
  #     main()
  return(list(pickledat = pickledat, myParams = myParams, rawdata = rawdata))
}

#' title
#'
#' description
#' @param parameter 1
#' @param parameter 2
#' @keywords keywords
#' @export
#' @examples
#'  example code


calcProofOfAbsence_reticulated <- function(myParams, pickledat, outputDataPath = "example_data/result0"){

  require(reticulate)
  py_available()
  use_condaenv("proofofabsence")
  py_config()

  py <- import_main(convert = F)
  builtins <- import_builtins(convert = FALSE)

  os <- import("os", convert = F, as = "os")
  pickle <- import("pickle", convert = F)
  np <- import("numpy", convert = F)

  poa <- proofofabsence::poa_py()
  calculation <- poa$calculation

  calculation$calcProofOfAbsence(myParams, pickledat$survey,
                                 pickledat$relativeRiskRaster, pickledat$zoneArray, pickledat$zoneCodes,
                                 pickledat$match_geotrans, pickledat$wkt, outputDataPath,
                                 pickledat$RR_zone, pickledat$Pu_zone, pickledat$Name_zone)
}

#' makeMaskAndZones
#'
#' makeMaskAndZones
#' @param parameter 1
#' @param parameter 2
#' @keywords keywords
#' @export
makeMaskAndZones <- function(self, multipleZones, params){

  #-------------------------------------------------------------------------#
  # Original attributes list
  #
  # ZONE_CODE_ATTRIBUTE = "zoneID"
  # # Unique attribute to use when rasterising the file
  # PU_CODE_ATTRIBUTE = "Pu_zone"
  # RRZONE_CODE_ATTRIBUTE = "RR_zone"
  # ## CODE FOR NAME OF ZONES
  # NAMEZONE_CODE_ATTRIBUTE = "zoneName"
  #
  # EXPECTED_SHP_ATTRIBUTES = bi$list(list(tuple(ZONE_CODE_ATTRIBUTE, bi$list(list(ogr$OFTInteger, ogr$OFTInteger64))),
  #                                        tuple(RRZONE_CODE_ATTRIBUTE, bi$list(list(ogr$OFTReal))),
  #                                        tuple(PU_CODE_ATTRIBUTE, bi$list(list(ogr$OFTInteger, ogr$OFTInteger64))),
  #                                        tuple(NAMEZONE_CODE_ATTRIBUTE, bi$list(list(ogr$OFTString)))))

  #-------------------------------------------------------------------------#
  # R-friendly attributes list
  ZONE_CODE_ATTRIBUTE = "zoneID"
  # "Unique attribute to use when rasterising the file"
  PU_CODE_ATTRIBUTE = "Pu_zone"
  RRZONE_CODE_ATTRIBUTE = "RR_zone"
  ## CODE FOR NAME OF ZONES
  NAMEZONE_CODE_ATTRIBUTE = "zoneName"

  EXPECTED_SHP_ATTRIBUTES = list(c("integer","character"), c("character","numeric", "integer"),
                                 c("character","numeric", "integer"), "character")
  names(EXPECTED_SHP_ATTRIBUTES) <- c(ZONE_CODE_ATTRIBUTE, PU_CODE_ATTRIBUTE, RRZONE_CODE_ATTRIBUTE, NAMEZONE_CODE_ATTRIBUTE)

  # Use zone shapefile to make zone raster

  # create extent raster tiff
  zones_layer = sf::st_read(self$zonesShapeFName, crs = self$epsg,
                            int64_as_string = TRUE, stringsAsFactors = FALSE, quiet = TRUE)

  # OK now check all the expected attributes are in the shape file
  # if we are doing the multiple zones thing
  if(multipleZones){

    # check required fields present
    reqFields <- names(EXPECTED_SHP_ATTRIBUTES)
    missingShpFields <- setdiff(reqFields,  names(zones_layer))
    if(length(missingShpFields) > 0){
      stop(sprintf('Required Field \'%s\' not found in shape file\n', missingShpFields))
    }
    # check required fields
    reqClasses <- EXPECTED_SHP_ATTRIBUTES
    matchClasses <- sapply(zones_layer, class)[names(reqClasses)]
    for(i in names(reqClasses)){
      if(!any(reqClasses[[i]] %in% matchClasses[[i]])){
        stop(
          sprintf('%s is of type %s. Expected: %s', i, matchClasses[[i]], paste(reqClasses[[i]], collapse = " or "))
        )

      }
    }
  }

  # int64 values get imported as floating point using st_read
  # - st_read() has option to import these as strings
  # - following checks for numeric strings and converts to integer of fails w/error
  for(j in c(ZONE_CODE_ATTRIBUTE, PU_CODE_ATTRIBUTE,RRZONE_CODE_ATTRIBUTE)){
    fieldvals <- zones_layer[[j]]
    if(is.character(fieldvals) & all(grepl("^\\d*$", fieldvals))){
      zones_layer[[j]] <- as.integer(zones_layer[[j]])
    } else {
      sprintf('zoneID is a non-integer. Expected: integer')
    }
  }


  # Rasterize Extent and write to directory

  # make destination raster using stored xy min-max and rows and columns
  zones_ds <- raster::raster(xmn = self$xmin, xmx = self$xmax,
                      ymn = self$ymin, ymx = self$ymax,
                      resolution = self$resol,
                      crs = sp::CRS(paste0("+init=epsg:", self$epsg)))
  # crop the extent by row and column numbers
  zones_ds <- raster::crop(zones_ds, raster::extent(zones_ds, 1, self$rows, 1, self$cols))

  # rasterise zone shapefile to target raster
  zones_ds <- fasterize::fasterize(sf = zones_layer, raster = zones_ds, field = ZONE_CODE_ATTRIBUTE, background = 0)

  # create zone arrays
  zoneCodes <- np_array(zones_layer$zoneID, dtype = "int")
  Pu_zone <- np_array(zones_layer$Pu_zone, dtype = "float")
  RR_zone <- np_array(zones_layer$RR_zone, dtype = "float")
  Name_zone <- np_array(zones_layer$zoneName, dtype = "str")

  if(!multipleZones){
    # just burn 1 inside the polygon(s)
    zones_ds <- raster::clamp(zones_ds, upper = 1)
    zoneCodes = np$array(bi$list(list(bi$int(1))))
    Pu_zone = np$array(bi$list(list(params$pu)))
    RR_zone = np$array(bi$list(list(bi$int(1))))
    Name_zone = np$array(bi$list(list('oneZone')))
  }

  # read in the data so we can return it
  zoneArray <- np_array(raster::as.matrix(zones_ds))

  # write to file
  print(self$zonesOutFName)
  raster::writeRaster(zones_ds, self$zonesOutFName, overwrite = TRUE)

  rm(zones_ds)
  rm(zones_layer)

  return(list(zoneArray = zoneArray,
              zoneCodes = zoneCodes,
              Pu_zone = Pu_zone,
              RR_zone = RR_zone,
              Name_zone = RR_zone))
}


#' makeRelativeRiskTif
#'
#' makeRelativeRiskTif
#' @param parameter 1
#' @param parameter 2
#' @keywords keywords
#' @export

makeRelativeRiskTif <- function(self, relativeRiskFName, relRiskRasterOutFName){

  # read in rel risk ascii, and write relative risk Tiff to directory
  # if RR not given, then it is derived from the zones data

  # make extent object using x/y min/max in self
  ext <- raster::extent(self$xmin, self$xmax, self$ymin, self$ymax)

  if(!is.null(relativeRiskFName)){

    # import relative risk raster
    RR_src = raster::raster(relativeRiskFName)
    # crop raster to extent
    RR_src <- raster::crop(RR_src, ext)

    # convert raster values to matrix
    in_im <- raster::as.matrix(RR_src)
    # set missing values to zero
    in_im[is.na(in_im)] <- 0
    # convert to numpy array
    in_im = np_array(in_im, "float32")

    # make empty numpy array with rows:columns from self
    RelRiskExtent = np$empty(bi$tuple(list(self$rows, self$cols)), dtype=np$float32)

    # use bilinear() from preProcessing.py (updates RelRiskExtent)
    poa$preProcessing$bilinear(in_im, RelRiskExtent, bi$int(0))

    # convert numpy array back to R
    RRmat <- reticulate::py_to_r(RelRiskExtent)
    # create re-projected raster
    rast_bilinear <-
      raster::raster(RRmat,
                     xmn = self$xmin, xmx = self$xmin + ncol(RRmat) * self$resol,
                     ymn = self$ymax - nrow(RRmat) * self$resol, ymx = self$ymax)
    crs_out <- sp::CRS(sprintf("+init=epsg:%s", self$epsg))
    raster::crs(rast_bilinear) <- crs_out

    # write out to relRiskRasterOutFName path
    print(paste0("writing processed relative risk raster to ", relRiskRasterOutFName))
    try(raster::writeRaster(rast_bilinear, relRiskRasterOutFName, overwrite = TRUE))

  } else {

    rast.zones <- raster::raster(zonesOutFName)
    rast.zero <- raster::clamp(rast.zones, lower = -Inf, upper = 1, useValues = TRUE)

    RelRiskExtent <- np_array(raster::as.matrix(!is.na(rast.zero)), dtype=np$float32)

    # write out to relRiskRasterOutFName path
    raster::values(rast.zero) <- reticulate::py_to_r(RelRiskExtent)
    print(paste0("writing processed relative risk raster to ", relRiskRasterOutFName))
    raster::writeRaster(rast.zero, relRiskRasterOutFName, overwrite = T)
  }

  print('finish makeRRTif')
  return(RelRiskExtent)
}

#' getShapefileDimensions_reticulated
#'
#' getShapefileDimensions_reticulated
#' @param parameter 1
#' @param parameter 2
#' @keywords keywords
#' @export

getShapefileDimensions_R <- function(zonesShapeFName){

  # get x and y min and max from shapefile
  layer = sf::st_read(zonesShapeFName, quiet = FALSE)
  # get dimensions
  # (xmin, xmax, ymin, ymax) = layer.GetExtent()
  xyminmax = as.numeric(sf::st_bbox(layer))
  # del layer
  rm(layer)

  # return xmin, xmax, ymin, ymax
  return(list(xyminmax[1], xyminmax[3],
              xyminmax[2], xyminmax[4]))
}

getGeoTrans_R <- function(self){
  #### get dimensions that incorporate both extent shape and farm boundaries
  # cols = int((self.xmax - self.xmin) / self.resol)
  cols = np$int(np$divide(np$subtract(self$xmax, self$xmin), self$resol))
  # rows = int((self.ymax - self.ymin) / self.resol)
  rows = np$int(np$divide(np$subtract(self$ymax, self$ymin), self$resol))
  # match_geotrans = [self.xmin, self.resol, 0, self.ymax, 0,
  #                        -self.resol]
  match_geotrans = list(self$xmin, self$resol, 0, self$ymax, 0,
                        np$multiply(self$resol, -1))
  # print('cols', cols, 'rows', rows)
  paste('cols', cols, 'rows', rows)
  # return cols, rows, match_geotrans
  return(list(cols = as.integer(py_to_r(cols)), 
              rows = as.integer(py_to_r(rows)), 
              match_geotrans = bi$list(match_geotrans)))
}

