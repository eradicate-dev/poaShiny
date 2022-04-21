readGridSurveyData <- function(self, gridSurveyFname = NULL, params = poa$params){
  
  #-------------------------------------------------------------------------#
  # check self for missing entries (py crashes if missing)
  req <- c("zoneArray")
  noreq <- sapply(self[req], is.null)
  if(any(noreq)) stop("missing self entries:", paste(req[noreq], collapse = " "))
  #-------------------------------------------------------------------------#
  
  #- development inputs ----------------------------------------------------#
  # TODO: delete when in package
  params <- poa$params$POAParameters()
  gridSurveyFname <- bi$str("inst/python/Nutria example from Dean/poa/Nutria/Data/gridPublicSur7.csv")
  # self$zoneArray
  #- development inputs ----------------------------------------------------#
  
  # Read all the grid survey data and make sure it is converted to
  # files of the correct spatial reference and extent.
  
  rawGridSurvey = np$genfromtxt(gridSurveyFname, delimiter=",", names=TRUE,
                                dtype=list('S200', 'i4', 'f8', 'f8'))

  gridSurveyYears = rawGridSurvey['year']

  nGrids = bi$len(gridSurveyYears)
  #        print('gridyears', self.gridSurveyYears, 'type', type(self.gridSurveyYears),
  #            'is scalar', np.isscalar(self.gridSurveyYears), 'gridsize', nGrids)

  gridSurveyMeans = rawGridSurvey['mean']
  gridSurveySD = rawGridSurvey['sd']
  # get an array of powers of 2 so we can encode
  # each year in a raster as a bit
  gridSurveyCodes = np$power(2, np$arange(nGrids))
  dirName = os$path$dirname(gridSurveyFname)

  # work out the data type to use - find the minimum data type we can use
  # to save memory
  maxCode = gridSurveyCodes[-1]  # 2**number of grids-1
  npDType = bi$None
  for(dt in c(np$uint8, np$uint16, np$uint32, np$uint64)) {  # loop potential data types
    info = np$iinfo(dt)  # get min and max of datetype
    if (maxCode <= info$max){  # if the required integer < max of data type, keep
      npDType = dt
      break
    }
  }
  
  if (npDType == bi$None) {  # if have more than 61 grids, it will be error
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
  for(i in reticulate::py_to_r(np$arange(rawGridSurvey$size))){
    
    # get fname and code separately in for loop instead of zip() in py
    fname <- rawGridSurvey['gridName']$tolist()[i]
    code <- gridSurveyCodes[i]
    
    # input
    fname <- os$path$join(dirName, fname$decode())
    src_ds <- gdal$Open(fname)

    # temp file - maybe should be able to set directory?
    # original py lines:
        # handle, reprojFName = tempfile.mkstemp(suffix='.tif')
        # os.close(handle)
    # R equivalent? - TODO this affects deleting the file at the end of the
    # script. Might only be a problem in the reticulated version ...
    reprojFName = bi$str(tempfile(fileext = ".tif"))

    reproj_ds <- gdal$GetDriverByName('GTiff')$Create(reprojFName, self$cols,
                                                      self$rows, np$int(1), gdal$GDT_Byte)

    reproj_ds$SetGeoTransform(self$match_geotrans)
    # spatial reference from making extent mask
    reproj_ds$SetProjection(self$wkt)
    # Reproject the grid survey to the dimensions of the extent
    gdal$ReprojectImage(src_ds, reproj_ds, self$wkt, self$wkt, gdalconst$GRA_NearestNeighbour)

    reproj_ds$FlushCache()

    data <- reproj_ds$GetRasterBand(np$int(1))$ReadAsArray()
    # remove non-risk cells
    data <- np$where(np$equal(tmpExtMask, 0), 0, data)

    # is this the first one?
    # create empty 2d array of the right type for storing the codes
    if("python.builtin.NoneType" %in% class(gridSurveyData)){
      gridSurveyData <- np$zeros_like(data, dtype=npDType)
      gridSurveyData <- np$where(np$not_equal(data, 0), code, gridSurveyData)
    } else {
      # subsequent - bitwise or the code in
      # gridSurveyData[data != 0] |= code
      
      #------------------------------------------------------------------------#
      # converting to R matrix an and back to np.array
      # workaround for |=
      gridSurveyData <- reticulate::py_to_r(gridSurveyData)
      
      gridSurveyData[reticulate::py_to_r(data) != 0] <- 
        bitwOr(a = gridSurveyData[reticulate::py_to_r(data) != 0], reticulate::py_to_r(code))
      
      gridSurveyData <- np$array(gridSurveyData, dtype = npDType)
      #------------------------------------------------------------------------#
    }
     
    # del reproj_ds
    # del src_ds
    # os.remove(reprojFName)

  }
  return(reticulate::tuple(gridSurveyYears, gridSurveyMeans, gridSurveySD, gridSurveyCodes,
                           gridSurveyData))
}

  