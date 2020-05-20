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
gdal      <- reticulate::import("gdal", convert = FALSE)
ogr       <- reticulate::import("ogr", convert = FALSE)
osr       <- reticulate::import("osr", convert = FALSE)
gdalconst <- reticulate::import("gdalconst", convert = FALSE)
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


EXPECTED_SHP_ATTRIBUTES = bt$list(list(tuple(ZONE_CODE_ATTRIBUTE, bt$list(list(ogr$OFTInteger, ogr$OFTInteger64))),
                                             tuple(RRZONE_CODE_ATTRIBUTE, bt$list(list(ogr$OFTReal))),
                                             tuple(PU_CODE_ATTRIBUTE, bt$list(list(ogr$OFTInteger, ogr$OFTInteger64))),
                                             tuple(NAMEZONE_CODE_ATTRIBUTE, bt$list(list(ogr$OFTString)))))
# "Check that these attributes exist as the correct types"

RawData <- function(self = list(), zonesShapeFName,
                    relativeRiskFName,
                    zonesOutFName,
                    relRiskRasterOutFName,
                    resolution,
                    epsg,
                    surveyFName,
                    params, 
                    gridSurveyFname = NULL){
  
  # functions ---------------------------------------------------------------

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
    # create extent raster tiff
    dataset = ogr$Open(self$zonesShapeFName)
    zones_layer = dataset$GetLayer()
    
    # OK now check all the expected attributes are in the shape file
    # if we are doing the multiple zones thing
    if(py_to_r(multipleZones)){
      featDefn = zones_layer$GetLayerDefn()
      for(i in seq_along(EXPECTED_SHP_ATTRIBUTES)-1){
        name <- EXPECTED_SHP_ATTRIBUTES[i][0]
        types <- EXPECTED_SHP_ATTRIBUTES[i][1]
        idx = py_to_r(featDefn$GetFieldIndex(name))
        if(idx == -1){
          stop(sprintf('Required Field \'%s\' not found in shape file', name))
        }
        
        fieldDefn = featDefn$GetFieldDefn(bt$int(idx))
        ogrType = py_to_r(fieldDefn$GetType())
        if(!ogrType %in% py_to_r(types)){
          foundName = fieldDefn$GetTypeName()
          expectedNames = sapply(lapply(py_to_r(types), ogr$GetFieldTypeName), py_to_r)
          stop(sprintf('%s is of type %s. Expected: %s', name, foundName, 
                       paste0(expectedNames, collapse = ", ")))
        }
      }
    }
    
    zones_ds = gdal$GetDriverByName('GTiff')$Create(self$zonesOutFName, self$cols,
                                                    self$rows, bt$int(1), gdal$GDT_Byte)
    zones_ds$SetGeoTransform(self$match_geotrans)
    zones_ds$SetProjection(self$wkt)
    band = zones_ds$GetRasterBand(bt$int(1))
    NoData_value = bt$int(0)    #-9999
    band$SetNoDataValue(NoData_value)
    # Rasterize Extent and write to directory
    
    if(py_to_r(multipleZones)){
      # burn the value of the ZONE_CODE_ATTRIBUTE
      gdal$RasterizeLayer(zones_ds, bt$list(list(bt$int(1))), zones_layer, 
                          options=bt$list(list(sprintf('ATTRIBUTE=%s', ZONE_CODE_ATTRIBUTE))))
      
      # get the unique zones 
      # and create a mapping between the zone and RR_zone, Pu_zone
      zones = bt$set()
      zoneToRRDict = bt$dict()
      zoneToPuDict = bt$dict()
      ## GET NAME OF ZONE - FOR USE IN RESULTS TABLE
      zoneToNameDict = bt$dict()
      zones_layer$ResetReading()
      for(i in seq_along(zones_layer)-1){
        feature <- zones_layer[i]
        zone = feature$GetFieldAsInteger(ZONE_CODE_ATTRIBUTE)
        if(py_to_r(zone) == 0){
          stop("Can't use zero as a zone code")
        }
        zones$add(zone)
        pu = feature$GetFieldAsDouble(PU_CODE_ATTRIBUTE)
        zoneToPuDict[zone] = pu
        rr = feature$GetFieldAsDouble(RRZONE_CODE_ATTRIBUTE)
        zoneToRRDict[zone] = rr
        ## GET NAMES OF ZONES FOR RESULTS TABLE:
        nn = feature$GetFieldAsString(NAMEZONE_CODE_ATTRIBUTE)
        zoneToNameDict[zone] = nn
      }
      
      # ok turn zones into an array of unique values
      zoneCodes = np$array(bt$list(zones))
      zoneCodes$sort()
      rrlist = bt$list()
      pulist = bt$list()
      namelist = bt$list()
      # now ensure RR and Pu are in the same order as zoneCodes
      for(i in seq_along(zoneCodes)-1){
        zone <- zoneCodes[i]
        rrlist$append(zoneToRRDict[zone])
        pulist$append(zoneToPuDict[zone])
        namelist$append(zoneToNameDict[zone])
      }
      Pu_zone = np$array(pulist)
      RR_zone = np$array(rrlist)
      Name_zone = np$array(namelist)
      
    } else {
      # just burn 1 inside the polygon(s)
      gdal$RasterizeLayer(zones_ds, bt$list(list(bt$int(1))), zones_layer, burn_values=bt$list(list(bt$int(1))))
      zoneCodes = np$array(bt$list(list(bt$int(1))))
      Pu_zone = np$array(bt$list(list(params$pu)))
      RR_zone = np$array(bt$list(list(bt$int(1))))
      Name_zone = np$array(bt$list(list('oneZone')))
    }
    
    zones_ds$FlushCache()
    
    # read in the data so we can return it
    zoneArray = zones_ds$GetRasterBand(bt$int(1))$ReadAsArray()
    
    rm(dataset)
    rm(zones_ds)
    return(list(zoneArray, zoneCodes, Pu_zone, RR_zone, Name_zone))
  }

  self$makeRelativeRiskTif <- function(self, relativeRiskFName, relRiskRasterOutFName){
    # relativeRiskFName <- self.relativeRiskFName
    # relRiskRasterOutFName <- self.relRiskRasterOutFName
    
    # read in rel risk ascii, and write relative risk Tiff to directory
    # if RR not given, then it is derived from the zones data
    
    print(paste('rr name:', relativeRiskFName))
    
    dst = gdal$GetDriverByName('GTiff')$Create(relRiskRasterOutFName, self$cols, self$rows,
                                               bt$int(1), gdalconst$GDT_Float32)
    dst$SetGeoTransform(self$match_geotrans)
    # spatial reference from making extent mask
    dst$SetProjection(self$wkt)
    
    if(!is.null(relativeRiskFName)){
      # Source - Kmap
      RR_src = gdal$Open(relativeRiskFName, gdalconst$GA_ReadOnly)
      
      if(!py_to_r(preProcessing$equalProjection(self$wkt, RR_src$GetProjection()))){
        stop("Projection of relative risk raster not the same as EPSG given")
      }
      
      # write kmap array to tif in directory
      # temp kmap tif name and directory
      # Reproject the kmap to the dimensions of the extent
      if(USE_GDAL_FOR_BILINEAR){
        gdal$ReprojectImage(RR_src, dst, self$wkt, self$wkt, gdalconst$GRA_Bilinear)
        RelRiskExtent = dst$GetRasterBand(bt$int(1))$ReadAsArray()
      } else {
        inband = RR_src$GetRasterBand(bt$int(1))
        inband$SetNoDataValue(bt$int(0))
        in_im = inband$ReadAsArray()
        
        # subset to extent of shape file
        geoTrans = RR_src$GetGeoTransform()
        invGeoTrans = gdal$InvGeoTransform(geoTrans)
        tl = gdal$ApplyGeoTransform(invGeoTrans, self$xmin, self$ymax)
        br = gdal$ApplyGeoTransform(invGeoTrans, 
                                      self$xmin + (self$resol * py_to_r(self$cols)),
                                      self$ymax - (self$resol * py_to_r(self$rows)))
        tlx = bt$int(np$round(tl[0]))
        tly = bt$int(np$round(tl[1]))
        brx = bt$int(np$round(br[0]))
        bry = bt$int(np$round(br[1]))
        in_im = np_array(py_to_r(in_im)[py_to_r(tly):py_to_r(bry), py_to_r(tlx):py_to_r(brx)], "float32")
        
        nodata = inband$GetNoDataValue()
        if(is.null(py_to_r(nodata))){
          stop("No Data value must be set on RR raster")
        }
        RelRiskExtent = np$empty(bt$tuple(list(self$rows, self$cols)), dtype=np$float32)
        
        preProcessing$bilinear(in_im, RelRiskExtent, nodata)
        
        outband = dst$GetRasterBand(bt$int(1))
        outband$WriteArray(RelRiskExtent)
      }
      
      rm(dst)  # Flush
      rm(RR_src)
    } else {
      # Source - zonecodes - RR=1 where zone>0
      zoneArray = gdal$Open(self$zonesOutFName)$ReadAsArray()
      RelRiskExtent = np$where(py_to_r(zoneArray) > 0, bt$int(1), bt$int(0))$astype(np$float32)
      outband = dst$GetRasterBand(bt$int(1))
      outband$WriteArray(RelRiskExtent)
    }
    
    print('finish makeRRTif')
    return(RelRiskExtent)
  }
  
  # Read in data and do one-time manipulations
  self$zonesShapeFName = bt$str(zonesShapeFName)
  self$relativeRiskFName = bt$str(relativeRiskFName)
  self$zonesOutFName = bt$str(zonesOutFName)
  self$relRiskRasterOutFName = bt$str(relRiskRasterOutFName)
  self$resol = resolution
  self$epsg = epsg

  # get spatial reference
  sr = osr$SpatialReference()
  sr$ImportFromEPSG(self$epsg)
  self$wkt = sr$ExportToWkt()

  # Get layer dimensions of extent shapefile
  self[c("xmin","xmax","ymin","ymax")] = self$getShapefileDimensions(self, definition=FALSE)
  # get number of columns and rows and set transformation
  self[c("cols", "rows", "match_geotrans")] = self$getGeoTrans(self)
        
  #----------------------------------------#
  # RUN FUNCTIONS
  #----------------------------------------#
  self[c("zoneArray", "zoneCodes", "Pu_zone", "RR_zone", "Name_zone")] <-
    self$makeMaskAndZones(self = self, multipleZones = myParams$multipleZones, params)
  
  print(paste('Name_zone', self$Name_zone))
  
  self$RelRiskExtent = self$makeRelativeRiskTif(self, relativeRiskFName, 
                                                relRiskRasterOutFName)

  print(paste('surveyFName:', surveyFName))

  # condition to use point survey data or not
  if(!is.null(surveyFName)){
    print(paste('params.animals', params$animals))
    self$survey = preProcessing$RawData$readSurveyData(self, surveyFName, myParams$animals)
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
