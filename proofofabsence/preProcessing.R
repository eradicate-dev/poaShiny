#--------------------------------------#
#--------------------------------------#
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
#--------------------------------------#
#--------------------------------------#
# from __future__ import print_function, division


# IMPORT MODULES ----------------------------------------------------------
os        <- reticulate::import("os", convert = FALSE)
np        <- reticulate::import("numpy", convert = FALSE)
pickle    <- reticulate::import("pickle", convert = FALSE)
tempfile  <- reticulate::import("tempfile", convert = FALSE)
# gdal      <- reticulate::import("gdal", convert = FALSE)
# ogr       <- reticulate::import("ogr", convert = FALSE)
# osr       <- reticulate::import("osr", convert = FALSE)
# gdalconst <- reticulate::import("gdalconst", convert = FALSE)
# from numba import njit
bt <- reticulate::import_builtins(convert = FALSE)

TRAP_PARAM_DTYPE = bt$list(list(tuple('year', 'u4'), tuple('animal', 'u4'), tuple('detect', 'u4'), 
                                      tuple('easting', 'f8'), tuple('northing', 'f8'), tuple('age', 'f8'), tuple('sex', 'u1'), 
                                      tuple('trapnights', 'f8')))
# "data type for the array of animal/trap data"

USE_GDAL_FOR_BILINEAR = FALSE

# Change to True to use GDAL for bilinear interpolation. 
# Problems were found with GDAL 2.x so default is to use our own implementation.


ZONE_CODE_ATTRIBUTE = "zoneID"
# "Unique attribute to use when rasterising the file"
PU_CODE_ATTRIBUTE = "Pu_zone"
RRZONE_CODE_ATTRIBUTE = "RR_zone"
## CODE FOR NAME OF ZONES
NAMEZONE_CODE_ATTRIBUTE = "zoneName"


EXPECTED_SHP_ATTRIBUTES = list(c("integer","character"), c("character","numeric", "integer"), 
                               c("character","numeric", "integer"), "character")
names(EXPECTED_SHP_ATTRIBUTES) <- c(ZONE_CODE_ATTRIBUTE, PU_CODE_ATTRIBUTE, RRZONE_CODE_ATTRIBUTE, NAMEZONE_CODE_ATTRIBUTE)
# "Check that these attributes exist as the correct types"

RawData_R <- function(self = list(), zonesShapeFName,
                    relativeRiskFName,
                    zonesOutFName,
                    relRiskRasterOutFName,
                    resolution,
                    epsg,
                    surveyFName,
                    params, 
                    gridSurveyFname = NULL){
  
  # functions ---------------------------------------------------------------

  preProcessing <- reticulate::py_run_file("proofofabsence/preProcessing.py", convert = FALSE)
  
  self$getShapefileDimensions <- function(self, definition=bt$bool(0)){            
    
    # get x and y min and max from shapefile
    
    dataset = ogr$Open(self$zonesShapeFName)
    layer = dataset$GetLayer()
    # print out definitions optional
    if(definition){
      getShapeLayerDefinition(layer)
    }
    # get dimensions
    extent = py_to_r(layer$GetExtent())
    names(extent) <- c("xmin","xmax","ymin","ymax")
    rm(dataset)
    rm(layer)
    return(extent)
  }
  
  self$getGeoTrans <- function(self){
    
    #### get dimensions that incorporate both extent shape and farm boundaries
    
    cols = bt$int((self$xmax - self$xmin) / self$resol)
    rows = bt$int((self$ymax - self$ymin) / self$resol)
    match_geotrans = bt$list(list(self$xmin, self$resol, bt$int(0), self$ymax, bt$int(0),
                                        -self$resol))
    print(paste('cols', cols, 'rows', rows))
    return(list(cols, rows, match_geotrans))
  }
  
  self$makeMaskAndZones <-  function(self, multipleZones, params){
    
    # check required fields present
    reqFields <- names(EXPECTED_SHP_ATTRIBUTES)
    missingShpFields <- setdiff(reqFields,  names(self$zonesShape.sf))
    if(length(missingShpFields) > 0){
      stop(sprintf('Required Field \'%s\' not found in shape file\n', missingShpFields))
    }
    # check required fields
    reqClasses <- EXPECTED_SHP_ATTRIBUTES
    matchClasses <- sapply(self$zonesShape.sf, class)[names(reqClasses)]
    for(i in names(reqClasses)){
      if(!any(reqClasses[[i]] %in% matchClasses[[i]])){
        stop(
          sprintf('%s is of type %s. Expected: %s', i, matchClasses[[i]], paste(reqClasses[[i]], collapse = " or "))
        )
             
      }
    }
    
    # int64 values get imported as floating point using st_read
    # - st_read() has option to import these as strings
    # - following checks for numeric strings and converts to integer of fails w/error
    for(j in c(PU_CODE_ATTRIBUTE,RRZONE_CODE_ATTRIBUTE)){
      fieldvals <- self$zonesShape.sf[[j]]
      if(is.character(fieldvals) & all(grepl("^\\d*$", fieldvals))){
        self$zonesShape.sf[[j]] <- as.integer(self$zonesShape.sf[[j]])
      } else {
        sprintf('zoneID is a non-integer. Expected: integer')
      }
    }
      
    # Rasterize Extent and write to directory
    
    # make target raster using stored xy min-max and rows and columns
    rast.target <- 
      raster(raster::extent(self$xmin, self$xmax, self$ymin, self$ymax), 
             nrows = py_to_r(self$rows), ncols = py_to_r(self$cols),
             crs = sp::CRS(paste0("+init=epsg:", self$epsg)))
    
    # rasterise zone shapefile to target raster
    rast.zones <- rasterize(as(self$zonesShape.sf, "Spatial"), rast.target, background = 0)
    
    zoneCodes <- np_array(self$zonesShape.sf$zoneID, dtype = "int")
    Pu_zone <- np_array(self$zonesShape.sf$Pu_zone, dtype = "float")
    RR_zone <- np_array(self$zonesShape.sf$RR_zone, dtype = "float")
    Name_zone <- np_array(self$zonesShape.sf$zoneName, dtype = "str")
    
    if(!py_to_r(multipleZones)){
      
      # just burn 1 inside the polygon(s)
      rast.zones <- raster::clamp(rast.zones, upper = 1)
      zoneCodes = np$array(bt$list(list(bt$int(1))))
      Pu_zone = np$array(bt$list(list(params$pu)))
      RR_zone = np$array(bt$list(list(bt$int(1))))
      Name_zone = np$array(bt$list(list('oneZone')))
    }
    
    # read in the data so we can return it
    # zoneArray = zones_ds$GetRasterBand(bt$int(1))$ReadAsArray()
    zoneArray <- np_array(as.matrix(rast.zones))
    
    # write to file
    print(self$zonesOutFName)
    print(class(self$zonesOutFName))
    raster::writeRaster(rast.zones, self$zonesOutFName, overwrite = TRUE)
    
    rm(rast.target)
    rm(rast.zones)
    return(list(zoneArray, zoneCodes, Pu_zone, RR_zone, Name_zone))
    
  }
  
  self$makeRelativeRiskTif <- function(self, relativeRiskFName, relRiskRasterOutFName){
    
    # read in rel risk ascii, and write relative risk Tiff to directory
    # if RR not given, then it is derived from the zones data
    
    if(!is.null(relativeRiskFName)){
        RR_src = raster(relativeRiskFName)
        
        # copy to output folder 
        file.copy(relativeRiskFName, relRiskRasterOutFName)
        
        in_im <- as.matrix(RR_src)
        in_im[is.na(in_im)] <- 0
        
        # select rows and columns based on x-y min/max and resolution
        maxCol <- (self$xmax - self$xmin) / xres(RR_src)
        maxRow <- (self$ymax - self$ymin) / yres(RR_src)
        in_im <- as.matrix(in_im)[1:maxRow,1:maxCol]
        in_im = np_array(in_im, "float32")
        
        RelRiskExtent = np$empty(bt$tuple(list(self$rows, self$cols)), dtype=np$float32)
        
        preProcessing$bilinear(in_im, RelRiskExtent, bt$int(0))
        
    } else {
        
      rast.zones <- raster::raster(zonesOutFName)
      rast.zero <- raster::clamp(rast.zones, lower = -Inf, upper = 1, useValues = TRUE)
      
      RelRiskExtent <- np_array(as.matrix(!is.na(rast.zero)), dtype=np$float32)
    }
    
    print('finish makeRRTif')
    return(RelRiskExtent)
  }
  
  # Read in data and do one-time manipulations
  self$zonesShapeFName = bt$str(zonesShapeFName)
  self$relativeRiskFName = bt$str(relativeRiskFName)
  self$zonesOutFName = zonesOutFName
  self$relRiskRasterOutFName = bt$str(relRiskRasterOutFName)
  self$resol = resolution
  self$epsg = epsg

  # get spatial reference
  # sr = osr$SpatialReference()
  # sr$ImportFromEPSG(self$epsg)
  # self$wkt = sr$ExportToWkt()
  
  # load zone shape file
  
  self$zonesShape.sf <- sf::st_read(self$zonesShapeFName, crs = self$epsg, 
                                    int64_as_string = TRUE, stringsAsFactors = F, quiet = TRUE)
  # Get layer dimensions of extent shapefile
  # self[c("xmin","xmax","ymin","ymax")] = self$getShapefileDimensions(self, definition=FALSE)
  self[c("xmin","ymin","xmax","ymax")] = as.list(sf::st_bbox(self$zonesShape.sf))
  # get number of columns and rows and set transformation
  self[c("cols", "rows", "match_geotrans")] = self$getGeoTrans(self)
        
  #----------------------------------------#
  # RUN FUNCTIONS
  #----------------------------------------#
  self[c("zoneArray", "zoneCodes", "Pu_zone", "RR_zone", "Name_zone")] <-
    self$makeMaskAndZones(self = self, multipleZones = params$multipleZones, params)
  
  print(paste('Name_zone', self$Name_zone))
  
  self$RelRiskExtent = self$makeRelativeRiskTif(self, relativeRiskFName, relRiskRasterOutFName)
  
  print(paste('surveyFName:', surveyFName))

  # condition to use point survey data or not
  if(!is.null(surveyFName)){
    print(paste('params.animals', params$animals))
    self$survey = preProcessing$RawData$readSurveyData(self, surveyFName, params$animals)
  } else {
    # not present, but need an empty array for processing
    self$survey = np$empty(tuple(bt$int(0)), dtype=TRAP_PARAM_DTYPE)
  }
  
  if(!is.null(gridSurveyFname)){
    # (self.gridSurveyYears, self.gridSurveyMeans, self.gridSurveySD, 
    #         self.gridSurveyCodes, self.gridSurveyData) = self.readGridSurveyData(
    #         gridSurveyFname, params)
    
  } else {
    # not present
    self$gridSurveyYears = bt$None
    self$gridSurveyData = bt$None
    self$gridSurveyMeans = bt$None
    self$gridSurveySD = bt$None
    self$gridSurveyCodes = bt$None
  }
  
  # END RUN FUNCTIONS
  #----------------------------------------#
  return(self)
}
