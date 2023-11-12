# TODO --------------------------------------------------------------------

# make reticulated readGridSurveyData when I get access to example data


#' Import required packages and modules required to run proof of absence scripts
#'
#' @param modules Select the the full set of proofofabsence modules (default =
#'   "full") to load from python scripts or a minimal set primarily for handling
#'   calculations ("minimal")
#' @param delay_load Pass delay_load to reticulate import functions
#' 
#' @return
#' @export
poa_paks <- function(modules = "minimal", delay_load = TRUE){

  # check modules argument
  if(!modules %in% c("full", "minimal")) stop("modules argument must be 'full' or 'minimal'")

  #-------------------------------------------------------------------------#
  # lines to set GDAL path to conda environment
  #  - only needed if using outside an activated conda environment
  # Sys.setenv(GDAL_DATA = "C:/ProgramData/Anaconda3/envs/proofofabsence/Library/share/gdal")
  #-------------------------------------------------------------------------#

  # IMPORT MODULES
  
  # check for required modules
  reqmodules <- 
    switch(modules,
           minimal = c("numpy", "pickle", "numba"),
           full = c("numpy", "pickle", "numba", "osgeo"))

  for(i in reqmodules){
    if(!reticulate::py_module_available(i)){
      stop(i, " module missing from python environment.")
    }
  }
  
  # load required modules and assign to .GlobalEnv
  if(modules == "full"){
    os <<- reticulate::import(module = "os", convert = FALSE, delay_load = delay_load)
    np <<- reticulate::import(module = "numpy", convert = FALSE, delay_load = delay_load)
    gdal <<- reticulate::import(module = "osgeo.gdal", convert = FALSE, delay_load = delay_load)
    ogr <<- reticulate::import(module = "osgeo.ogr", convert = FALSE, delay_load = delay_load)
    osr <<- reticulate::import(module = "osgeo.osr", convert = FALSE, delay_load = delay_load)
    gdalconst <<- reticulate::import(module = "osgeo.gdalconst", convert = FALSE, delay_load = delay_load)
  }
  
  if(modules == "minimal"){
    np <<- reticulate::import(module = "numpy", convert = FALSE, delay_load = delay_load)
  }
  
  # always load builtin modules
  bi <<- reticulate::import_builtins(convert = FALSE)
  
  ## Import POA modules from package folder
  
  # directory containing proofofabsence python scripts
  pydir <- switch(modules, full = "proofofabsence", 
                    minimal = "proofofabsence_min")
  # find file paths in package directory
  pypath <- file.path(system.file("python", package = "proofofabsence"), pydir)
  
  # load proofofabsence module
  # - run once without assigning to build __pycache__
  reticulate::import_from_path(
    path = system.file("python", package = "proofofabsence"),
    module = pydir, 
    convert = FALSE, delay_load = FALSE)
  # - then assign to globalEnv
  poa <<- 
    reticulate::import_from_path(
      path = system.file("python", package = "proofofabsence"),
      module = pydir, 
      convert = FALSE, delay_load = delay_load)
  
  # list and module files in package directory python folder
  module_files <- list.files(pypath, pattern = ".py$")
  module_names <- sub("\\.\\w*$", "", module_files[!grepl("^__",module_files)])
  
  # check sub-modules loaded
  for(i in module_names){
    try( print(reticulate::py_str( poa[[i]] ) ), silent = TRUE)
    if(!reticulate::py_has_attr(x = poa, i)){
      stop("Sub module '", i, "' not loaded from ", pypath)
    }
  }
  
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
#' 
#' # load python packages from anaconda install
#' reticulate::use_condaenv("proofofabsence")
#' poa_paks(modules = "minimal")
#' 
#' # create minimal parameters object
#' myParams <- makeParams()
#' 
#' # show parameterArray where detection device  values are stored
#' myParams$parameterArray
#' 
#' # append animal/device parameters
#' myParams <- addAnimalParams(myParams, deviceName = "legholdtrap",
#'                             g0 = 0.05, g0sd = 0.01, sig = 90, sigsd = 10)
#' myParams$parameterArray
#' 
#' # paths to shape and survey files
#' zonesShapeFName <- system.file("example_data/Mahia_Audrey/extent_block1ABCD.shp", package = "proofofabsence")
#' relativeRiskFName <- system.file("example_data/Mahia_Audrey/habDistRR_block1ABCD.tif", package = "proofofabsence")
#' surveyFName <- system.file("example_data/Mahia_Audrey/Surveillance_location.csv", package = "proofofabsence")
#' zonesOutFName <- tempfile(fileext = ".tif")
#' relRiskRasterOutFName <- tempfile(fileext = ".tif")
#' 
#' #' # pass paths to RawData function
#' rawdata <- RawData_R(zonesShapeFName = zonesShapeFName,
#'   relativeRiskFName = relativeRiskFName,
#'   zonesOutFName = zonesOutFName,
#'   relRiskRasterOutFName = relRiskRasterOutFName,
#'   resolution = 100.0,
#'   epsg = 2193,
#'   surveyFName = bi$str(surveyFName),
#'   params = myParams,
#'   gridSurveyFname=NULL)
RawData_R <- function(
        zonesShapeFName,
        relativeRiskFName,
        zonesOutFName,
        relRiskRasterOutFName,
        resolution,
        epsg,
        surveyFName,
        params,
        gridSurveyFname){

  # Set TRAP_PARAM_DTYPE - in py scripts this is set at the start of the preProcessing.py
  TRAP_PARAM_DTYPE = bi$list(list(tuple('year', 'u4'), tuple('animal', 'u4'), tuple('detect', 'u4'), 
                                  tuple('easting', 'f8'), tuple('northing', 'f8'), tuple('age', 'f8'), tuple('sex', 'u1'), 
                                  tuple('trapnights', 'f8')))
  
        # replace RawData class self object with an R list
        self <- list()

        self$zonesShapeFName = zonesShapeFName
        self$relativeRiskFName = relativeRiskFName
        self$zonesOutFName = zonesOutFName
        self$relRiskRasterOutFName = relRiskRasterOutFName
        self$resol = resolution
        self$epsg = epsg

        # get spatial reference
        if("osr" %in% ls(envir = .GlobalEnv) && "python.builtin.module" %in% class(osr)){
          sr <- osr$SpatialReference()
          sr$ImportFromEPSG(bi$int(self$epsg))  
          self$wkt_alt = sr$ExportToWkt()
        }
        
        self$wkt <- sf::st_crs(epsg)[["wkt"]]
        
        
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
          suppressWarnings(
            makeMaskAndZones(self = self, params = params,
                             multipleZones = reticulate::py_to_r(params$multipleZones)))

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
            self$survey = np$empty(reticulate::tuple(0L), dtype=TRAP_PARAM_DTYPE)
        }

        # if gridSurveyFname is not None:
        if(!is.null(gridSurveyFname)){
          .tmp <- readGridSurveyData(self, gridSurveyFname, params)
          self$gridSurveyYears <- .tmp[[0]]
          self$gridSurveyMeans <- .tmp[[1]]
          self$gridSurveySD <-    .tmp[[2]]
          self$gridSurveyCodes <- .tmp[[3]]
          self$gridSurveyData <-  .tmp[[4]]
          rm(.tmp)
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
#'
#' @export
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

  #extentOnly.shp')    #'extent_block1.shp')
  #extent_gradient.shp')
  relativeRiskFName = os$path$join(inputDataPath, relativeRiskFName)
  #habDistRR.tif')
  #RR_mahia.tif')
  #    relativeRiskFName = None

  # Surveillance data Name
  surveyFName = os$path$join(inputDataPath, surveyFName) # or None

  # set OUTPUT names for mask and relative risk map
  zoneOutFName = os$path$join(outputDataPath, 'zones.tif')
  relRiskRasterOutFName = os$path$join(outputDataPath, 'relRiskRaster.tif')
  #    relRiskRasterOutFName = None

  ############ IF USE GRIDS
  useGrids = reticulate::r_to_py(useGrids)
  gridSurvey = reticulate::r_to_py(NULL)   # os.path.join(inputDataPath, 'gridScenario14.csv')

  ############ IF FIRST RUN CONDITION
  # if True, do preprocessing, else skip to calculations
  firstRun = r_to_py(firstRun)        # True or False

  # resolution for analysis
  Resolution = np$double(np$double(Resolution))
  # EPSG - PROJECTION SYSTEM
  epsg = bi$int(epsg)    # NZ transverse mercator

  #---------------------------------------------------------------------------#
  #---# ADD SURVEILLANCE TYPES                                                #
  TYPE_LEGHOLD = bi$int(12) # index number for leghold traps                  #
  animals = poa$params$AnimalTypes()                                              #
  animals$addAnimal(TYPE_LEGHOLD, "Leghold", poa$params$DETECT_ANIMAL)            #
  #---------------------------------------------------------------------------#

  # Instance of POAParameters class
  myParams = poa$params$POAParameters(animals)

  ## USE MULTIPLE ZONES
  myParams$setMultipleZones(r_to_py(setMultipleZones))

  #---------------------------------------------------------------------------#
  # ADD LEGHOLD TRAP PARAMETERS                                               #
  myParams$setCapture(bi$int(12), np$double(0.06), np$double(0.03))           # note the 12 indicates LEGHOLD
  myParams$setSigma(bi$int(12), np$double(150.0), np$double(20.0))            #
  myParams$addRRBufferAnimal(TYPE_LEGHOLD)                                    #
  #---------------------------------------------------------------------------#

  # number of cpu's from SLURM
  ncpus = bi$int(os$getenv('SLURM_CPUS_PER_TASK', '1'))
  myParams$setNumThreads(ncpus)

  # print('ncpus', ncpus)

  # number of iterations
  myParams$setNumIterations(bi$int(setNumIterations))
  #    myParams$setNumChewcardTraps(3)
  myParams$setRRTrapDistance(np$double(setRRTrapDistance))

  ##   <<< COMPULSORY - PERHAPS CHANGE TO SESSIONS NOT YEARS >>>
  # startYear = bi$int(1)
  # endYear = bi$int(1)
  myParams$setYears(bi$int(startYear), bi$int(endYear))

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

  if(reticulate::py_to_r(firstRun)){
    # initiate instances of Classes
    rawdata = poa$preProcessing$RawData(zoneShapeFName, relativeRiskFName,
                                    zoneOutFName, relRiskRasterOutFName, Resolution, epsg,
                                    surveyFName, myParams, gridSurvey)

    print('finish preProcessing')

    ## condition on presence of grid data
    if(reticulate::py_to_r(useGrids)){
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
#'
#' @param myParams POAParameters python class created using
#'   \code{\link{makeParams}}
#' @param rawdata Python dictionary containing survey data and spatial
#'   information. Created using \code{\link{RawData_R}}
#' @param outputDataPath Output directory to save output data.
#'
#' @export
#' @examples
#' reticulate::use_condaenv("proofofabsence")
#' poa_paks(modules = "minimal")
#' 
#' myParams <-
#'   makeParams(setMultipleZones = TRUE,
#'              setNumIterations = 10,
#'              setRRTrapDistance = 100,
#'              startYear = 1, endYear = 2,
#'              startPu = 1.0, PuIncreaseRate = 0.0,
#'              setMinRR = 1.0,
#'              setPrior = c(0.10, 0.2, 0.70),
#'              setIntro = c(0.10, 0.2, 0.70))
#' 
#' myParams <-
#'   addAnimalParams(
#'     myParams,
#'     deviceName = c("AT220", "Camera", "CHEWDETECT",
#'                    "Leghold", "PossMaster", "Sentinel"),
#'     g0 = rep(0.2, 6),
#'     g0sd = rep(0.05, 6),
#'     sig = rep(90, 6),
#'     sigsd = rep(10, 6))
#' # view added parameters
#' myParams$parameterArray
#' 
#' # path to example input shape files and relative risk raster
#' inputshp <- system.file("example_data/Kaitake_possums/extent.shp",
#'                         package = "proofofabsence")
#' inputrast <- system.file("example_data/Kaitake_possums/relRiskRaster.tif",
#'                          package = "proofofabsence")
#' inputsurv <- system.file("example_data/Kaitake_possums/devices.csv",
#'                          package = "proofofabsence")
#' # output raster to temporary file
#' outputrast <- tempfile(fileext = ".tif")
#' # output results to temporary folder
#' outputdir <- tempdir()
#' 
#' rawdata <-
#'   RawData_R(zonesShapeFName = inputshp,
#'             relativeRiskFName = inputrast,
#'             zonesOutFName = tempfile(fileext = ".tif"),
#'             relRiskRasterOutFName = outputrast,
#'             resolution = as.double(100),
#'             epsg = as.integer(2193),
#'             surveyFName = inputsurv,
#'             params = myParams,
#'             gridSurveyFname = NULL)
#' 
#' res <- calcProofOfAbsence_reticulated(myParams = myParams, rawdata = rawdata, outputDataPath = outputdir)
calcProofOfAbsence_reticulated <- function(myParams, rawdata, outputDataPath){

  # create save directory if missing
  dir.create(outputDataPath, recursive = TRUE)
  
  # run calcs
  res <- poa$calculation$calcProofOfAbsence(myParams, rawdata$survey,
                                     rawdata$RelRiskExtent, rawdata$zoneArray, rawdata$zoneCodes,
                                     rawdata$match_geotrans, rawdata$wkt, outputDataPath,
                                     rawdata$RR_zone, rawdata$Pu_zone, rawdata$Name_zone)
  class(res) <- c(class(res), "POAresults")
  
  return(res)
  
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

  # field names to match to loaded shape file (case-insensitive, but can be regex)
  shp_fields <- c(zoneID = "zone.?ID",
                  Pu_zone = "Pu.?zone",
                  RR_zone = "RR.?zone",
                  zoneName = "zone.?Name")
  
  # Use zone shapefile to make zone raster

  # create extent raster tiff
  zones_layer = sf::st_read(self$zonesShapeFName, crs = self$epsg,
                            int64_as_string = TRUE, stringsAsFactors = FALSE, quiet = TRUE)

  # check each shape file field
  for(i in seq_along(shp_fields)){
    # find matching shape file field 
    selectCol <- grepl(pattern = shp_fields[i], 
                       x = names(zones_layer), ignore.case = TRUE)
    if(!any(selectCol)){
      # warn if none found
      stop("Shapefile field matching ", names(shp_fields)[i], " not found.")
    } else {
      # replace with strict value if matching pattern in shp_fields
      # (i.e. ZoneID replaced with zoneID)
      names(zones_layer)[selectCol] <- names(shp_fields)[i]
    }
  }
  
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
  zoneCodes <- reticulate::np_array(zones_layer$zoneID, dtype = "int")
  Pu_zone <- reticulate::np_array(zones_layer$Pu_zone, dtype = "float")
  RR_zone <- reticulate::np_array(zones_layer$RR_zone, dtype = "float")
  Name_zone <- reticulate::np_array(zones_layer$zoneName, dtype = "str")

  if(!multipleZones){
    # just burn 1 inside the polygon(s)
    zones_ds <- raster::clamp(zones_ds, upper = 1)
    zoneCodes = np$array(bi$list(list(bi$int(1))))
    Pu_zone = np$array(bi$list(list(params$pu)))
    RR_zone = np$array(bi$list(list(bi$int(1))))
    Name_zone = np$array(bi$list(list('oneZone')))
  }

  # read in the data so we can return it
  zoneArray <- reticulate::np_array(raster::as.matrix(zones_ds))

  # write to file
  print(self$zonesOutFName)
  raster::writeRaster(zones_ds, self$zonesOutFName, overwrite = TRUE)

  rm(zones_ds)
  rm(zones_layer)

  return(list(zoneArray = zoneArray,
              zoneCodes = zoneCodes,
              Pu_zone = Pu_zone,
              RR_zone = RR_zone,
              Name_zone = Name_zone))
}

#' readGridSurveyData
#'
#' @param self 
#' @param gridSurveyFname 
#' @param params 
#'
#' @return
#' @export
#'
#' @examples
readGridSurveyData <- function(self, gridSurveyFname = NULL, params = poa$params){
  
  #-------------------------------------------------------------------------#
  # check self for missing entries (py crashes if missing)
  req <- c("zoneArray")
  noreq <- sapply(self[req], is.null)
  if(any(noreq)) stop("missing self entries:", paste(req[noreq], collapse = " "))
  
  # load grid survey csv and check specified rasters exist in same folder
  chkcsv <- read.csv(gridSurveyFname)
  chkgridpaths <- file.path(dirname(gridSurveyFname), chkcsv$gridName)
  missinggrids <- chkgridpaths[!file.exists(chkgridpaths)]
  if(length(missinggrids) > 0){
    stop("the following raster files specified in grid survey file are missing: ",
         paste(unique(basename(missinggrids)), collapse = ","), 
         ". Add the files to the same folder as the grid survey file, ", 
         basename(gridSurveyFname), ".")
  }
  #-------------------------------------------------------------------------#
  
  
  # Read all the grid survey data and make sure it is converted to
  # files of the correct spatial reference and extent.
  
  rawGridSurvey <- read.csv(as.character(gridSurveyFname), 
                            colClasses = c("character", "integer", "numeric", "numeric"))
  
  gridSurveyYears = rawGridSurvey[['year']]
  
  nGrids <- length(gridSurveyYears)
  #        print('gridyears', self.gridSurveyYears, 'type', type(self.gridSurveyYears),
  #            'is scalar', np.isscalar(self.gridSurveyYears), 'gridsize', nGrids)
  
  gridSurveyMeans = rawGridSurvey[['mean']]
  gridSurveySD = rawGridSurvey[['sd']]
  # get an array of powers of 2 so we can encode
  # each year in a raster as a bit
  gridSurveyCodes <- 2 ^ (seq_len(nGrids) - 1)
  dirName <- dirname(gridSurveyFname)
  
  # work out the data type to use - find the minimum data type we can use
  # to save memory
  maxCode <- gridSurveyCodes[length(gridSurveyCodes)]  # 2**number of grids-1
  npDType = bi$None
  for(dt in c(np$uint8, np$uint16, np$uint32, np$uint64)) {  # loop potential data types
    info = np$iinfo(dt)  # get min and max of datetype
    infomax <- as.numeric(as.character(info$max))
    if (maxCode <= infomax){  # if the required integer < max of data type, keep
      npDType = dt
      break
    }
  }
  
  if (reticulate::py_to_r(npDType == bi$None)) {  # if have more than 61 grids, it will be error
    stop('Too many grid survey years to store in an integer')
  }
  
  message('chosen dtype', as.character(npDType))
  
  # have to create a mask to update the grid survey rasters
  # this is not ideal because we do this in calculation.py
  
  tmpExtMask <- self$zoneArray$copy()
  # tmpExtMask[self.RelRiskExtent < params.minRR] = 0
  tmpExtMask <- np$where(np$less(self$RelRiskExtent, params$minRR), 0, tmpExtMask)
  
  # so we know when we got to the first one
  gridSurveyData <- bi$None
  
  # reproject each dataset into a temproary file and
  # then read it in.
  # for(i in reticulate::py_to_r(np$arange(rawGridSurvey$size))){
  for(i in seq_len(nrow(rawGridSurvey))){
    # i <- 1
    # browser()
    
    # get fname and code separately in for loop instead of zip() in py
    fname <- rawGridSurvey[['gridName']][i]
    code <- gridSurveyCodes[i]
    
    # input
    fname <- file.path(dirName, fname)
    src_ds <- terra::rast(fname)
    
    # add lines to replace NaN values when importing raster using terra:rast()
    # - python version uses gdal.Open() which sets 'no data' values to zero
    terra::values(src_ds)[is.na(terra::values(src_ds))] <- 0

    #-------------------------------------------------------------------------#
    # quick check
    # a <- as.matrix(src_ds_orig$GetRasterBand(bi$int(1))$ReadAsArray())
    # b <- terra::as.array(src_ds)[,,1]
    # all(a == b)
    #-------------------------------------------------------------------------#
    
    # temp file - maybe should be able to set directory?
    # original py lines:
    # handle, reprojFName = tempfile.mkstemp(suffix='.tif')
    # os.close(handle)
    # R equivalent? - TODO this affects deleting the file at the end of the
    # script. Might only be a problem in the reticulated version ...
    reprojFName <- tempfile(fileext = ".tif")
    
    reproj_ds <- terra::rast(nrows = self$rows, ncols = self$cols)
    terra::values(reproj_ds) <- 1
    
    # get extent values from geotrans
    match_geotrans <- reticulate::py_to_r(self$match_geotrans)
    xmin <- match_geotrans[[1]]
    xmax <- xmin + self$cols * self$resol
    ymax <- match_geotrans[[4]]
    ymin <- ymax - self$rows * self$resol
    terra::ext(reproj_ds) <- c(xmin, xmax, ymin, ymax)
    
    # spatial reference from making extent mask
    terra::crs(reproj_ds) <- paste("epsg", self$epsg, sep = ":")
    # Reproject the grid survey to the dimensions of the extent
    reproj_ds <- terra::project(src_ds, reproj_ds, method = "near")
    
    data <- terra::as.array(reproj_ds)[,,1]
    
    # remove non-risk cells
    data[] <- ifelse(as.matrix(tmpExtMask) == 0, yes = 0, no = data)
    
    # is this the first one?
    # create empty 2d array of the right type for storing the codes
    if("python.builtin.NoneType" %in% class(gridSurveyData)){
      
      gridSurveyData <- np$zeros_like(data, dtype=npDType)
      gridSurveyData <- reticulate::py_to_r(gridSurveyData)
      gridSurveyData[data != 0] <- code
      gridSurveyData <- np$array(gridSurveyData, dtype = npDType)
      
    } else {
      
      # subsequent - bitwise or the code in
      # gridSurveyData[data != 0] |= code
      
      #------------------------------------------------------------------------#
      # converting to R matrix an and back to np.array
      # workaround for |=
      gridSurveyData <- reticulate::py_to_r(gridSurveyData)
      
      gridSurveyData[data != 0] <- 
        bitwOr(a = gridSurveyData[data != 0], code)
      
      gridSurveyData <- np$array(gridSurveyData, dtype = npDType)
      #------------------------------------------------------------------------#
    }
    
    # del reproj_ds
    # del src_ds
    # os.remove(reprojFName)
    
  }
  
  # convert R vectors back to numpy arrays
  gridSurveyYears <- np$atleast_1d(np$array(gridSurveyYears, dtype=np$int32))
  gridSurveyMeans <- np$atleast_1d(np$array(gridSurveyMeans, dtype=np$float64))
  gridSurveySD <- np$atleast_1d(np$array(gridSurveySD, dtype=np$float64))
  gridSurveyCodes <- np$atleast_1d(np$array(gridSurveyCodes, dtype=np$float64))
  
  return(reticulate::tuple(gridSurveyYears, gridSurveyMeans, gridSurveySD, gridSurveyCodes,
                           gridSurveyData))
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
  ext <- terra::ext(self$xmin, self$xmax, self$ymin, self$ymax)

  if(!is.null(relativeRiskFName)){

    # import relative risk raster
    RR_src <- terra::rast(relativeRiskFName)
    
    # check only single layer given for relative risk
    if(terra::nlyr(RR_src) != 1) stop("raster file '", relativeRiskFName, "' can only have a single layer")

    # crop raster to extent
    RR_src <- terra::crop(RR_src, ext)

    # convert raster values to matrix
    in_im <- terra::as.array(RR_src)[,,1]
    # set missing values to zero
    in_im[is.na(in_im)] <- 0
    # convert to numpy array
    in_im = reticulate::np_array(in_im, "float32")

    # make empty numpy array with rows:columns from self
    RelRiskExtent = np$empty(bi$tuple(list(self$rows, self$cols)), dtype=np$float32)

    # use bilinear() from preProcessing.py (updates RelRiskExtent)
    poa$preProcessing$bilinear(in_im, RelRiskExtent, bi$int(0))

    # convert numpy array back to R
    RRmat <- reticulate::py_to_r(RelRiskExtent)
    # create re-projected raster
    rast_bilinear <- 
      terra::rast(nrows = nrow(RRmat), ncols = ncol(RRmat),
                  xmin = self$xmin, xmax = self$xmin + ncol(RRmat) * self$resol,
                  ymin = self$ymax - nrow(RRmat) * self$resol, ymax = self$ymax,
                  vals = RRmat, 
                  crs = sf::st_crs(self$epsg)[["wkt"]])
    
    # write out to relRiskRasterOutFName path
    print(paste0("writing processed relative risk raster to ", relRiskRasterOutFName))
    try(terra::writeRaster(x = rast_bilinear, filename = relRiskRasterOutFName, overwrite = TRUE))

  } else {

    rast.zones <- terra::rast(self$zonesOutFName)
    rast.zero <- terra::clamp(rast.zones, lower = -Inf, upper = 1, values = TRUE)
    
    RelRiskExtent <- reticulate::np_array(raster::as.matrix(!is.na(rast.zero), wide = TRUE), dtype=np$float32)
    
    # write out to relRiskRasterOutFName path
    terra::values(rast.zero) <- reticulate::py_to_r(RelRiskExtent)
    print(paste0("writing processed relative risk raster to ", relRiskRasterOutFName))
    terra::writeRaster(x = rast.zero, filename = relRiskRasterOutFName, overwrite = TRUE)
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
  cols = bi$int(np$divide(np$subtract(self$xmax, self$xmin), self$resol))
  # rows = int((self.ymax - self.ymin) / self.resol)
  rows = bi$int(np$divide(np$subtract(self$ymax, self$ymin), self$resol))
  # match_geotrans = [self.xmin, self.resol, 0, self.ymax, 0,
  #                        -self.resol]
  match_geotrans = list(self$xmin, self$resol, 0, self$ymax, 0,
                        np$multiply(self$resol, -1))
  # print('cols', cols, 'rows', rows)
  paste('cols', cols, 'rows', rows)
  # return cols, rows, match_geotrans
  return(list(cols = as.integer(reticulate::py_to_r(cols)), 
              rows = as.integer(reticulate::py_to_r(rows)), 
              match_geotrans = bi$list(match_geotrans)))
}






