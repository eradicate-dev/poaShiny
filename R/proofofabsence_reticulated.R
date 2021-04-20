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
poa_paks_full <- function(condaenv = "proofofabsence"){

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
  # tempfile  <<- reticulate::import("tempfile", convert = FALSE)
  gdal      <<- reticulate::import("gdal", convert = FALSE)
  ogr       <<- reticulate::import("ogr", convert = FALSE)
  osr       <<- reticulate::import("osr", convert = FALSE)
  gdalconst <<- reticulate::import("gdalconst", convert = FALSE)
  numba     <<- reticulate::import("numba", convert = FALSE)

  bi <<- import_builtins(convert = FALSE)
  # py <<- import_main(convert = FALSE)
  poa <<- import_from_path(path = system.file("python", package = "proofofabsence"),
                           module = "proofofabsence", convert = FALSE)
}


#' addAnimalParams
#'
#' Add animal parameters to POAParameters object
#'
#' @param POAParameters POAParameters object
#' @param deviceName Character vector of device names matching names in survey file
#' @param g0 numeric vector of gnaught values
#' @param g0sd numeric vector of gnaught standard deviation values
#' @param sig numeric vector of sigma values
#' @param sigsd numeric vector of sigma standard deviation values
#'
#' @return
#' @export
#'
#' @examples
#' addAnimalParams()
addAnimalParams <- function(POAParameters = makeParams(),
                            deviceName = LETTERS[1:6],
                            g0 = rep(0.05, 6),
                            g0sd = rep(0.01, 6),
                            sig = rep(90,6)[1],
                            sigsd = rep(10,6)[1]){

  # d <- read.csv("inst/example_data/Kaitake_possums/devices - original.csv")
  # deviceName <- unique(as.character(d$Species))

  ndevices <- length(deviceName)
  if(ndevices > 1 & length(sig) == 1 & length(sigsd) == 1){
    message('addAnimalParams(); single sigma value given, recycling sigma for all devices')
    sig <- rep(sig, ndevices)
    sigsd <- rep(sigsd, ndevices)
  }

  # default AnimalTypes class
  .animals <- poa$params$AnimalTypes()
  # get length of POAParameters$animals. TYPE integer adds to this.
  dictlen <- py_to_r(bi$len(.animals$functionDict))

  # add animals
  for(i in seq_along(deviceName)){
    TYPE_INT <- np$int(i + dictlen - 1)    # set type integer
    .animals$addAnimal(TYPE_INT, deviceName[i], poa$params$DETECT_ANIMAL)
  }

  # temporary POAParameters object
  .POAParameters <- poa$params$POAParameters(.animals)

  # add animal parameters to temporary object
  for(i in seq_along(deviceName)){
    TYPE_INT <- np$int(i + dictlen - 1)    # set type integer
    .POAParameters$setCapture(TYPE_INT, np$double(g0[i]), np$double(g0sd[i]))   # note the 12 indicates LEGHOLD
    .POAParameters$setSigma(TYPE_INT, np$double(sig[i]), np$double(sigsd[i]))   #
    .POAParameters$addRRBufferAnimal(TYPE_INT)
  }

  # update original parameter array and buffer animal values
  POAParameters$animals <- .animals
  POAParameters$parameterArray <- .POAParameters$parameterArray
  POAParameters$RRBufferAnimals <- .POAParameters$RRBufferAnimals

  return(POAParameters)

}

#' Create proofofabsence.params.POAParameters object
#'
#' @param setMultipleZones Handle multiple polygons in shapefile as separate zones
#' @param setNumIterations Set number of iterations to run
#' @param setRRTrapDistance Set distance to buffer relative risk layer around devices
#' @param setMinRR Set minimum value relative risk around buffered devices
#' @param startYear Start year/session
#' @param endYear End year/session
#' @param startPu Set starting Pu (default = 1)
#' @param PuIncreaseRate Rate of increase in Pu (default = 0)
#' @param setPrior Vector of prior probabilities (min, mode and max)
#' @param setIntro Vector of re-introduction probabilities (min, mode and max)
#'
#' @return
#' @export
#'
#' @examples
#' proofofabsence.params.POAParameters()
makeParams <- function(
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

    #---------------------------------------------------------------------------#
    #---# ADD SURVEILLANCE TYPES                                                #
    # TYPE_LEGHOLD = np$int(12) # index number for leghold traps                  #
    animals = poa$params$AnimalTypes()                                              #
    # animals$addAnimal(TYPE_LEGHOLD, "Leghold", poa$params$DETECT_ANIMAL)            #
    #---------------------------------------------------------------------------#

    # Instance of POAParameters class
    myParams = poa$params$POAParameters(animals)

    ## USE MULTIPLE ZONES
    myParams$setMultipleZones(r_to_py(setMultipleZones))

    #---------------------------------------------------------------------------#
    # ADD LEGHOLD TRAP PARAMETERS                                               #
    # myParams$setCapture(np$int(12), np$double(0.06), np$double(0.03))           # note the 12 indicates LEGHOLD
    # myParams$setSigma(np$int(12), np$double(150.0), np$double(20.0))            #
    # myParams$addRRBufferAnimal(TYPE_LEGHOLD)                                    #
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
    myParams$setYears(np$int(startYear), np$int(endYear))

    ## THE startPu WILL NOT BE USED IF USE zoneData FILE - TURN OFF
    # starting Pu (GRID CELL PREVALENCE) and period rate of Pu increase

    ## SET THE RATE OF INCREASE OF PU
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

    #-----------------------------------#   END USER MODIFICATION
    #-----------------------------------------------------------#
    #-----------------------------------------------------------#

    return(myParams)
  }

#' RawData_reticulated
#'
#' RawData_reticulated
#' @param parameter 1
#' @param parameter 2
#' @export

# class RawData():
RawData_reticulated <- function(
        # inputs
        zonesShapeFName = "inst/example_data/Kaitake_possums/extent.shp",
        relativeRiskFName = "inst/example_data/Kaitake_possums/relRiskRaster.tif",
        zonesOutFName = "results/zones.tif",
        relRiskRasterOutFName = "results/rr.tif",
        resolution = 100.0,
        epsg = 2193,
        surveyFName = bi$str("inst/example_data/Kaitake_possums/devices.csv"),
        params = makeParams(),
        gridSurveyFname=NULL){

  proofofabsence::poa_paks_full()

        # replace RawData class self object with an R list
        self <- list()

        self$zonesShapeFName = zonesShapeFName
        self$relativeRiskFName = relativeRiskFName
        self$zonesOutFName = zonesOutFName
        self$relRiskRasterOutFName = relRiskRasterOutFName
        self$resol = resolution
        self$epsg = epsg

        # get spatial reference
        sr = osr$SpatialReference()
        sr$ImportFromEPSG(bi$int(self$epsg))
        self$wkt = sr$ExportToWkt()

        # Get layer dimensions of extent shapefile
        self[c("xmin","xmax","ymin", "ymax")] <- getShapefileDimensions_reticulated(self, definition=np$False_)
        # get number of columns and rows and set transformation
        self[c("cols","rows","match_geotrans")] <- getGeoTrans_reticulated(self)

        #----------------------------------------#
        # RUN FUNCTIONS
        #----------------------------------------#
        # (self.zoneArray, self.zoneCodes, self.Pu_zone, self.RR_zone, self.Name_zone) = (
        #             self.makeMaskAndZones(params.multipleZones, params))
        self[c("zoneArray", "zoneCodes", "Pu_zone", "RR_zone", "Name_zone")] <-
          makeMaskAndZones_reticulated(self, params$multipleZones, params)

        print(paste('Name_zone', self$Name_zone))

        # self.RelRiskExtent = self.makeRelativeRiskTif(relativeRiskFName,
        #                             relRiskRasterOutFName)
        self$RelRiskExtent <- makeRelativeRiskTif_reticulated(self, relativeRiskFName,relRiskRasterOutFName)

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
           myParams = makeParams(setMultipleZones = FALSE, setNumIterations = 1000,
                                 startYear = 1, endYear = 1,
                                 startPu = 1.0, PuIncreaseRate = 0.0,
                                 setRRTrapDistance = 100.0, setMinRR = 1.0,
                                 setPrior = c(0.6,0.8,0.85), setIntro = c(0.00001, 0.00002, 0.00003))){

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

  myParams <- addAnimalParams(POAParameters = myParams,
                              deviceName = c("Leghold", "Sentinel", "PossMaster", "Camera", "CHEWDETECT", "AT220"),
                              g0 = c(0.06,0.04,0.04,0.075,0.09,0.05), g0sd = c(0.03,0.03,0.03,0.03,0.03,0.03),
                              sig = rep(150,6), sigsd = rep(20,6))

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

#' makeMaskAndZones_reticulated
#'
#' makeMaskAndZones_reticulated
#' @param parameter 1
#' @param parameter 2
#' @keywords keywords
#' @export
makeMaskAndZones_reticulated <- function(self, multipleZones, params){
  # """
  # Use zone shapefile to make zone raster
  # """
  # create extent raster tiff
  # dataset = ogr.Open(self.zonesShapeFName)

  #-------------------------------------------------------------------------#
  # Original attributes list
  #
  ZONE_CODE_ATTRIBUTE = "zoneID"
  # Unique attribute to use when rasterising the file
  PU_CODE_ATTRIBUTE = "Pu_zone"
  RRZONE_CODE_ATTRIBUTE = "RR_zone"
  ## CODE FOR NAME OF ZONES
  NAMEZONE_CODE_ATTRIBUTE = "zoneName"

  EXPECTED_SHP_ATTRIBUTES = bi$list(list(tuple(ZONE_CODE_ATTRIBUTE, bi$list(list(ogr$OFTInteger, ogr$OFTInteger64))),
                                        tuple(RRZONE_CODE_ATTRIBUTE, bi$list(list(ogr$OFTReal))),
                                        tuple(PU_CODE_ATTRIBUTE, bi$list(list(ogr$OFTInteger, ogr$OFTInteger64))),
                                        tuple(NAMEZONE_CODE_ATTRIBUTE, bi$list(list(ogr$OFTString)))))


  dataset = ogr$Open(self$zonesShapeFName)
  # zones_layer = dataset.GetLayer()
  zones_layer = dataset$GetLayer()

  # OK now check all the expected attributes are in the shape file
  # if we are doing the multiple zones thing
  if(py_to_r(multipleZones)){
    # featDefn = zones_layer.GetLayerDefn()
    featDefn = zones_layer$GetLayerDefn()
    # for name, types in EXPECTED_SHP_ATTRIBUTES:
    for(i in iterate(EXPECTED_SHP_ATTRIBUTES)){
      name <- i[0]; types <- i[1]
      # idx = featDefn.GetFieldIndex(name)
      idx = featDefn$GetFieldIndex(name)
      # if idx == -1:
      #   msg = 'Required Field %s not found in shape file' % name
      #   raise ValueError(msg)
      if(idx == -1){
        msg = sprintf('Required Field %s not found in shape file', name)
        stop(msg)
      }
      # fieldDefn = featDefn.GetFieldDefn(idx)
      fieldDefn = featDefn$GetFieldDefn(idx)
      # ogrType = fieldDefn.GetType()
      ogrType = fieldDefn$GetType()

      # if ogrType not in types:
      if(!py_to_r(ogrType) %in% py_to_r(types)){
        # foundName = fieldDefn.GetTypeName()
        foundName = fieldDefn$GetTypeName()
        # expectedNames = [ogr$GetFieldTypeName(x) for x in types]
        expectedNames <- list()
        for(x in iterate(types)) expectedNames[[as.character(x)]] <- as.character(ogr$GetFieldTypeName(x))
        # msg = '%s is of type %s. Expected: %s' % (name, foundName,
        #                                           ','.join(expectedNames))
        msg = sprintf('%s is of type %s. Expected: %s', name, foundName,
                      paste(expectedNames, collapse = ", "))
        # raise ValueError(msg)
        stop(msg)
      }
    }
  }

  zones_ds = gdal$GetDriverByName('GTiff')$Create(self$zonesOutFName, self$cols,
                                                  self$rows, np$int(1), gdal$GDT_Byte)
  zones_ds$SetGeoTransform(self$match_geotrans)
  zones_ds$SetProjection(self$wkt)
  band = zones_ds$GetRasterBand(np$int(1))
  NoData_value = np$int(0)    #-9999
  band$SetNoDataValue(NoData_value)
  # Rasterize Extent and write to directory

  # if multipleZones:
  if(py_to_r(multipleZones)){
    # burn the value of the ZONE_CODE_ATTRIBUTE

    # gdal.RasterizeLayer(zones_ds, [1], zones_layer,
    #         options=['ATTRIBUTE=%s' % ZONE_CODE_ATTRIBUTE])
    gdal$RasterizeLayer(zones_ds, bi$list(list(np$int(1))), zones_layer,
                        options=bi$list(list(sprintf('ATTRIBUTE=%s', ZONE_CODE_ATTRIBUTE))))
    # get the unique zones
    # and create a mapping between the zone and RR_zone, Pu_zone
    # zones = set()
    # zoneToRRDict = {}
    # zoneToPuDict = {}
    zones = bi$set()
    zoneToRRDict = bi$dict()
    zoneToPuDict = bi$dict()
    ## GET NAME OF ZONE - FOR USE IN RESULTS TABLE
    # zoneToNameDict = {}
    zoneToNameDict = bi$dict()
    # zones_layer.ResetReading()
    zones_layer$ResetReading()
    # for feature in zones_layer:
    for(feature in iterate(zones_layer)){
      # zone = feature.GetFieldAsInteger(ZONE_CODE_ATTRIBUTE)
      zone = feature$GetFieldAsInteger(ZONE_CODE_ATTRIBUTE)
      # if zone == 0:
      #     raise ValueError("Can't use zero as a zone code")
      if(zone == 0) stop("Can't use zero as a zone code")
      # zones.add(zone)
      zones$add(zone)
      # pu = feature.GetFieldAsDouble(PU_CODE_ATTRIBUTE)
      pu = feature$GetFieldAsDouble(PU_CODE_ATTRIBUTE)
      # zoneToPuDict[zone] = pu
      zoneToPuDict$update(py_dict(keys = py_to_r(zone), values = py_to_r(pu), convert = FALSE))
      # rr = feature.GetFieldAsDouble(RRZONE_CODE_ATTRIBUTE)
      rr = feature$GetFieldAsDouble(RRZONE_CODE_ATTRIBUTE)
      # zoneToRRDict[zone] = rr
      zoneToRRDict$update(py_dict(keys = py_to_r(zone), values = py_to_r(rr), convert = FALSE))
      ## GET NAMES OF ZONES FOR RESULTS TABLE:
      # nn = feature.GetFieldAsString(NAMEZONE_CODE_ATTRIBUTE)
      nn = feature$GetFieldAsString(NAMEZONE_CODE_ATTRIBUTE)
      # zoneToNameDict[zone] = nn
      zoneToNameDict$update(py_dict(keys = py_to_r(zone), values = py_to_r(nn), convert = FALSE))
    }

    # ok turn zones into an array of unique values
    # zoneCodes = np.array(list(zones))
    zoneCodes = np$array(bi$list(zones))
    # zoneCodes.sort()
    zoneCodes$sort()
    # rrlist = []
    rrlist = bi$list()
    # pulist = []
    pulist = bi$list()
    # namelist = []
    namelist = bi$list()
    # now ensure RR and Pu are in the same order as zoneCodes
    # for zone in zoneCodes:
    for(zone in iterate(zoneCodes)){
      # rrlist.append(zoneToRRDict[zone])
      rrlist$append(zoneToRRDict[zone])
      # pulist.append(zoneToPuDict[zone])
      pulist$append(zoneToPuDict[zone])
    }
    Pu_zone = np$array(pulist)
    RR_zone = np$array(rrlist)
    Name_zone = np$array(namelist)

    # else:
  } else {
    # just burn 1 inside the polygon(s)
    # gdal.RasterizeLayer(zones_ds, [1], zones_layer, burn_values=[1])
    gdal$RasterizeLayer(zones_ds, bi$list(list(np$int(1))), zones_layer, burn_values=bi$list(list(np$int(1))))
    # zoneCodes = np.array([1])
    zoneCodes = np$array(bi$list(list(np$int(1))))
    # Pu_zone = np.array([params.pu])
    Pu_zone = np$array(bi$list(list(params$pu)))
    # RR_zone = np.array([1])
    RR_zone = np$array(bi$list(list(np$int(1))))
    # Name_zone = np.array(['oneZone'])
    Name_zone = np$array(bi$list(list(np$str('oneZone'))))
  }

  zones_ds$FlushCache()

  # read in the data so we can return it
  # zoneArray = zones_ds$GetRasterBand(1).ReadAsArray()
  zoneArray = zones_ds$GetRasterBand(np$int(1))$ReadAsArray()

  # del dataset
  rm(dataset)
  # del zones_ds
  rm(zones_ds)
  # return zoneArray, zoneCodes, Pu_zone, RR_zone, Name_zone
  return(list(zoneArray = zoneArray,
              zoneCodes = zoneCodes,
              Pu_zone = Pu_zone,
              RR_zone = RR_zone,
              Name_zone = RR_zone))
}


#' title
#'
#' description
#' @param parameter 1
#' @param parameter 2
#' @keywords keywords
#' @export

# def makeRelativeRiskTif(self, relativeRiskFName, relRiskRasterOutFName):
makeRelativeRiskTif_reticulated <- function(self, relativeRiskFName, relRiskRasterOutFName){
  # """
  # read in rel risk ascii, and write relative risk Tiff to directory
  # if RR not given, then it is derived from the zones data

  USE_GDAL_FOR_BILINEAR = FALSE

  # Change to True to use GDAL for bilinear interpolation.
  # Problems were found with GDAL 2.x so default is to use our own implementation.

  # print('rr name', relativeRiskFName)
  print(paste('rr name', relativeRiskFName))

  # dst = gdal.GetDriverByName('GTiff').Create(relRiskRasterOutFName, self$cols, self$rows,
  #                         1, gdalconst.GDT_Float32)
  dst = gdal$GetDriverByName('GTiff')$Create(relRiskRasterOutFName, self$cols, self$rows,
                                             np$int(1), gdalconst$GDT_Float32)
  # dst.SetGeoTransform(self$match_geotrans)
  dst$SetGeoTransform(self$match_geotrans)
  # spatial reference from making extent mask
  # dst.SetProjection(self$wkt)
  dst$SetProjection(self$wkt)

  # if relativeRiskFName is not None:
  if(!is.null(relativeRiskFName)){
    # Source - Kmap
    # RR_src = gdal.Open(relativeRiskFName, gdalconst.GA_ReadOnly)
    RR_src = gdal$Open(relativeRiskFName, gdalconst$GA_ReadOnly)

    # if not equalProjection(self.wkt, RR_src.GetProjection()):
    if(!py_to_r(poa$preProcessing$equalProjection(self$wkt, RR_src$GetProjection()))){
      # raise ValueError("Projection of relative risk raster not the same as EPSG given")
      stop("Projection of relative risk raster not the same as EPSG given")}

    # write kmap array to tif in directory
    # temp kmap tif name and directory
    # Reproject the kmap to the dimensions of the extent
    # if USE_GDAL_FOR_BILINEAR:
    if(USE_GDAL_FOR_BILINEAR){
      # gdal.ReprojectImage(RR_src, dst, self$wkt, self$wkt, gdalconst.GRA_Bilinear)
      gdal$ReprojectImage(RR_src, dst, self$wkt, self$wkt, gdalconst$GRA_Bilinear)
      # RelRiskExtent = dst.GetRasterBand(1).ReadAsArray()
      RelRiskExtent = dst$GetRasterBand(np$int(1))$ReadAsArray()
      # else:
    } else {
      # inband = RR_src.GetRasterBand(1)
      inband = RR_src$GetRasterBand(np$int(1))
      # inband.SetNoDataValue(0)
      inband$SetNoDataValue(bi$int(0))
      # in_im = inband.ReadAsArray()
      in_im = inband$ReadAsArray()

      # subset to extent of shape file



      # geoTrans = RR_src.GetGeoTransform()
      geoTrans = RR_src$GetGeoTransform()
      # invGeoTrans = gdal.InvGeoTransform(geoTrans)
      invGeoTrans = gdal$InvGeoTransform(geoTrans)
      # tlx, tly = gdal.ApplyGeoTransform(invGeoTrans,
      #                 self$xmin, self$ymax)
      tlxy = gdal$ApplyGeoTransform(invGeoTrans,
                                    self$xmin, self$ymax)
      tlxy <- as.integer(py_to_r(np$round(tlxy)))

      # brx, bry = gdal.ApplyGeoTransform(invGeoTrans,
      #                 self$xmin + (self$resol * self$cols),
      #                 self$ymax - (self$resol * self$rows))
      brxy = gdal$ApplyGeoTransform(invGeoTrans,
                                    np$add(self$xmin, np$multiply(self$resol, self$cols)),
                                    np$subtract(self$ymax, np$multiply(self$resol,self$rows)))
      brxy <- as.integer(py_to_r(np$round(brxy)))
      # tlx = int(np.round(tlx))
      # tly = int(np.round(tly))
      tlx = tlxy[1]; tly = tlxy[2]
      # brx = int(np.round(brx))
      # bry = int(np.round(bry))
      brx = brxy[1]; bry = brxy[2]
      # in_im = in_im[tly:bry, tlx:brx]
      #------------------------------------------------------------------------#
      # NOTE: indexing in numpy arrays drops the last value in a sequence in
      #       addition to being zero indexed
      # e.g. using a three value sequence 0,1,2 to select rows will return the
      #      first two rows only, not three.
      # Equivalent behaviour isn't available using reticulate so using the
      # take() method for ndarray and dropping the last index value.
      in_im <- in_im$take(indices = tly:(bry-1), axis = 0L, mode = 'clip')
      in_im <- in_im$take(indices = tlx:(brx-1), axis = 1L, mode = 'clip')
      #------------------------------------------------------------------------#
      # nodata = inband.GetNoDataValue()
      nodata = inband$GetNoDataValue()
      # if nodata is None:
      # raise ValueError("No Data value must be set on RR raster")
      if(nodata == bi$None) stop("No Data value must be set on RR raster")
      # RelRiskExtent = np.empty((self$rows, self$cols), dtype=np.float32)
      RelRiskExtent = np$empty(tuple(self$rows, self$cols), dtype=np$float32)
      # bilinear(in_im, RelRiskExtent, nodata)
      poa$preProcessing$bilinear(in_im, RelRiskExtent, nodata)
      # outband = dst.GetRasterBand(1)
      outband = dst$GetRasterBand(np$int(1))
      # outband.WriteArray(RelRiskExtent)
      outband$WriteArray(RelRiskExtent)
    }

    # del dst  # Flush
    # del RR_src
    rm(dst); rm(RR_src)
    # else:
  } else {
    # Source - zonecodes - RR=1 where zone>0
    zoneArray = gdal$Open(self$zonesOutFName)$ReadAsArray()
    RelRiskExtent = np$where(np$greater(zoneArray, 0), 1, 0)$astype(np$float32)
    outband = dst$GetRasterBand(bi$int(1))
    outband$WriteArray(RelRiskExtent)
  }

  print('finish makeRRTif')
  # return RelRiskExtent
  return(RelRiskExtent)
}

#' getShapefileDimensions_reticulated
#'
#' getShapefileDimensions_reticulated
#' @param parameter 1
#' @param parameter 2
#' @keywords keywords
#' @export

getShapefileDimensions_reticulated <- function(self, definition=np$False_){

  # get x and y min and max from shapefile

  # dataset = ogr.Open(self$zonesShapeFName)
  dataset = ogr$Open(self$zonesShapeFName)
  # layer = dataset.GetLayer()
  layer = dataset$GetLayer()
  # print out definitions optional
  # if definition:
  #   getShapeLayerDefinition(layer)
  if(definition == np$True_) getShapeLayerDefinition(layer)
  # get dimensions
  # (xmin, xmax, ymin, ymax) = layer.GetExtent()
  xyminmax = layer$GetExtent()
  # del dataset
  # del layer
  rm(dataset, layer)

  # return xmin, xmax, ymin, ymax
  return(list(xyminmax[0], xyminmax[1],
              xyminmax[2], xyminmax[3]))
}

getGeoTrans_reticulated <- function(self){
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
  return(list(cols = cols, rows = rows, match_geotrans = bi$list(match_geotrans)))
}
