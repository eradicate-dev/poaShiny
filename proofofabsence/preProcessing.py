#!/usr/bin/env python

########################################
########################################
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
########################################
########################################
from __future__ import print_function, division

# IMPORT MODULES
import os
import numpy as np
import pickle
import tempfile
# from osgeo import gdal
# from osgeo import ogr
# from osgeo import osr
# from osgeo import gdalconst
from numba import njit

from proofofabsence import params

TRAP_PARAM_DTYPE = [('year', 'u4'), ('animal', 'u4'), ('detect', 'u4'),
    ('easting', 'f8'), ('northing', 'f8'), ('age', 'f8'), ('sex', 'u1'),
    ('trapnights', 'f8')]
"data type for the array of animal/trap data"
# 
# USE_GDAL_FOR_BILINEAR = False
"""
Change to True to use GDAL for bilinear interpolation. 
Problems were found with GDAL 2.x so default is to use our own implementation.
"""

# ZONE_CODE_ATTRIBUTE = "zoneID"
# "Unique attribute to use when rasterising the file"
# PU_CODE_ATTRIBUTE = "Pu_zone"
# RRZONE_CODE_ATTRIBUTE = "RR_zone"
# ## CODE FOR NAME OF ZONES
# NAMEZONE_CODE_ATTRIBUTE = "zoneName"
# 
# 
# EXPECTED_SHP_ATTRIBUTES = [(ZONE_CODE_ATTRIBUTE, [ogr.OFTInteger, ogr.OFTInteger64]),
#                             (RRZONE_CODE_ATTRIBUTE, [ogr.OFTReal]),
#                             (PU_CODE_ATTRIBUTE, [ogr.OFTInteger, ogr.OFTInteger64]),
#                             (NAMEZONE_CODE_ATTRIBUTE, [ogr.OFTString])]
# "Check that these attributes exist as the correct types"



def decodeBytes(byteArray, nArray):
    """
    ## loop to decode byte to string
    """
    strArray = str(byteArray[0], 'utf-8')
    for i in range(1, nArray):
        strArr_i = str(byteArray[i], 'utf-8')
        strArray = np.append(strArray, strArr_i)
    return(strArray)

def equalProjection(this, other):
    """
    Returns True if the projection of self is the same as the 
    projection of other
        
    """
    srSelf = osr.SpatialReference(wkt=this)
    srOther = osr.SpatialReference(wkt=other)
    return bool(srSelf.IsSame(srOther))


class RawData():
    def __init__(self, zonesShapeFName, relativeRiskFName,
            zonesOutFName, relRiskRasterOutFName, resolution, epsg,
            surveyFName, params, gridSurveyFname=None):
        """
        Read in data and do one-time manipulations
        """
# 
#         self.zonesShapeFName = zonesShapeFName
#         self.relativeRiskFName = relativeRiskFName
#         self.zonesOutFName = zonesOutFName
#         self.relRiskRasterOutFName = relRiskRasterOutFName
#         self.resol = resolution
#         self.epsg = epsg
# 
#         # get spatial reference
#         sr = osr.SpatialReference()
#         sr.ImportFromEPSG(self.epsg)
#         self.wkt = sr.ExportToWkt()
# 
#         # Get layer dimensions of extent shapefile
#         self.xmin, self.xmax, self.ymin, self.ymax = self.getShapefileDimensions(definition=False)
#         # get number of columns and rows and set transformation
#         self.cols, self.rows, self.match_geotrans = self.getGeoTrans()
#         
#         ##########################################
#         # RUN FUNCTIONS
#         ##########################################
#         (self.zoneArray, self.zoneCodes, self.Pu_zone, self.RR_zone, self.Name_zone) = (
#                     self.makeMaskAndZones(params.multipleZones, params))
# 
#         print('Name_zone', self.Name_zone)
#         
#         self.RelRiskExtent = self.makeRelativeRiskTif(relativeRiskFName, 
#                                     relRiskRasterOutFName)
# 
#         print('surveyFName', surveyFName)
# 
#         # condition to use point survey data or not
#         if surveyFName is not None:
#             print('params.animals', params.animals)
#             self.survey = self.readSurveyData(surveyFName, params.animals)
#         else:
#             # not present, but need an empty array for processing
#             self.survey = np.empty((0,), dtype=TRAP_PARAM_DTYPE)
# 
# 
#         if gridSurveyFname is not None:
# 
#             (self.gridSurveyYears, self.gridSurveyMeans, self.gridSurveySD, 
#                     self.gridSurveyCodes, self.gridSurveyData) = self.readGridSurveyData(
#                     gridSurveyFname, params)
# 
#         else:
#             # not present
#             self.gridSurveyYears = None
#             self.gridSurveyData = None
#             self.gridSurveyMeans = None
#             self.gridSurveySD = None
#             self.gridSurveyCodes = None
# 
#         # END RUN FUNCTIONS
#         ##########################################
# 
# 
#         ##########################################
#         # RAWDATAFUNCTIONS
#         ##########################################
# 
# 
#     def makeMaskAndZones(self, multipleZones, params):
#         """
#         Use zone shapefile to make zone raster
#         """
#         # create extent raster tiff
#         dataset = ogr.Open(self.zonesShapeFName)
#         zones_layer = dataset.GetLayer()
#         
#         # OK now check all the expected attributes are in the shape file
#         # if we are doing the multiple zones thing
#         if multipleZones:
#             featDefn = zones_layer.GetLayerDefn()
#             for name, types in EXPECTED_SHP_ATTRIBUTES:
#                 idx = featDefn.GetFieldIndex(name)
#                 if idx == -1:
#                     msg = 'Required Field %s not found in shape file' % name
#                     raise ValueError(msg)
#                
#                 fieldDefn = featDefn.GetFieldDefn(idx)
#                 ogrType = fieldDefn.GetType()
#                 if ogrType not in types:
#                     foundName = fieldDefn.GetTypeName()
#                     expectedNames = [ogr.GetFieldTypeName(x) for x in types]
#                     msg = '%s is of type %s. Expected: %s' % (name, foundName, 
#                                         ','.join(expectedNames))
#                     raise ValueError(msg)
# 
#         zones_ds = gdal.GetDriverByName('GTiff').Create(self.zonesOutFName, self.cols,
#                             self.rows, 1, gdal.GDT_Byte)
#         zones_ds.SetGeoTransform(self.match_geotrans)
#         zones_ds.SetProjection(self.wkt)
#         band = zones_ds.GetRasterBand(1)
#         NoData_value = 0    #-9999
#         band.SetNoDataValue(NoData_value)
#         # Rasterize Extent and write to directory
#         
#         if multipleZones:
#             # burn the value of the ZONE_CODE_ATTRIBUTE
#             gdal.RasterizeLayer(zones_ds, [1], zones_layer, 
#                     options=['ATTRIBUTE=%s' % ZONE_CODE_ATTRIBUTE])
# 
#             # get the unique zones 
#             # and create a mapping between the zone and RR_zone, Pu_zone
#             zones = set()
#             zoneToRRDict = {}
#             zoneToPuDict = {}
#             ## GET NAME OF ZONE - FOR USE IN RESULTS TABLE
#             zoneToNameDict = {}
#             zones_layer.ResetReading()
#             for feature in zones_layer:
#                 zone = feature.GetFieldAsInteger(ZONE_CODE_ATTRIBUTE)
#                 if zone == 0:
#                     raise ValueError("Can't use zero as a zone code")
#                 zones.add(zone)
#                 pu = feature.GetFieldAsDouble(PU_CODE_ATTRIBUTE)
#                 zoneToPuDict[zone] = pu
#                 rr = feature.GetFieldAsDouble(RRZONE_CODE_ATTRIBUTE)
#                 zoneToRRDict[zone] = rr
#                 ## GET NAMES OF ZONES FOR RESULTS TABLE:
#                 nn = feature.GetFieldAsString(NAMEZONE_CODE_ATTRIBUTE)
#                 zoneToNameDict[zone] = nn
#                     
#             # ok turn zones into an array of unique values
#             zoneCodes = np.array(list(zones))
#             zoneCodes.sort()
#             rrlist = []
#             pulist = []
#             namelist = []
#             # now ensure RR and Pu are in the same order as zoneCodes
#             for zone in zoneCodes:
#                 rrlist.append(zoneToRRDict[zone])
#                 pulist.append(zoneToPuDict[zone])
#                 namelist.append(zoneToNameDict[zone])
#             Pu_zone = np.array(pulist)
#             RR_zone = np.array(rrlist)
#             Name_zone = np.array(namelist)
# 
#         else:
#             # just burn 1 inside the polygon(s)
#             gdal.RasterizeLayer(zones_ds, [1], zones_layer, burn_values=[1])
#             zoneCodes = np.array([1])
#             Pu_zone = np.array([params.pu])
#             RR_zone = np.array([1])
#             Name_zone = np.array(['oneZone'])
#         
#         zones_ds.FlushCache()
#         
#         # read in the data so we can return it
#         zoneArray = zones_ds.GetRasterBand(1).ReadAsArray()
#         
#         del dataset
#         del zones_ds
#         return zoneArray, zoneCodes, Pu_zone, RR_zone, Name_zone
#         
#     def readGridSurveyData(self, gridSurveyFname, params):
#         """
#         Read all the grid survey data and make sure it is converted to 
#         files of the correct spatial reference and extent.
#         """
#         rawGridSurvey = np.genfromtxt(gridSurveyFname,  delimiter=',', names=True,
#             dtype=['S200', 'i4', 'f8', 'f8'])
# 
#         gridSurveyYears = rawGridSurvey['year']
# 
#         nGrids = len(gridSurveyYears)
# #        print('gridyears', self.gridSurveyYears, 'type', type(self.gridSurveyYears),
# #            'is scalar', np.isscalar(self.gridSurveyYears), 'gridsize', nGrids)
# 
#         gridSurveyMeans = rawGridSurvey['mean']
#         gridSurveySD = rawGridSurvey['sd']
#         # get an array of powers of 2 so we can encode
#         # each year in a raster as a bit
#         gridSurveyCodes = 2 ** np.arange(nGrids)
#         dirName = os.path.dirname(gridSurveyFname)
# 
#         # work out the data type to use - find the minimum data type we can use
#         # to save memory
#         maxCode = gridSurveyCodes[-1]      # 2**number of grids-1
#         npDType = None
#         for dt in (np.uint8, np.uint16, np.uint32, np.uint64):  # loop potential data types
#             info = np.iinfo(dt)                 # get min and max of datetype        
#             if maxCode <= info.max:             # if the required integer < max of data type, keep
#                 npDType = dt
#                 break
#                 
#         if npDType is None:                     # if have more than 61 grids, it will be error
#             msg = 'Too many grid survey years to store in an integer'
#             raise ValueError(msg)
# 
#         print('chosen dtype', npDType)
# 
#         # have to create a mask to update the grid survey rasters
#         # this is not ideal because we do this in calculation.py
# 
# 
#         tmpExtMask = self.zoneArray.copy()
#         tmpExtMask[self.RelRiskExtent < params.minRR] = 0
# 
#         # so we know when we got to the first one        
#         gridSurveyData = None
#         
#         # reproject each dataset into a temproary file and
#         # then read it in.
#         for fname, code in zip(rawGridSurvey['gridName'], gridSurveyCodes):
#             # input
#             fname = os.path.join(dirName, fname.decode())
#             src_ds = gdal.Open(fname)
# 
#             # temp file - maybe should be able to set directory?
#             handle, reprojFName = tempfile.mkstemp(suffix='.tif')
#             os.close(handle)
# 
#             reproj_ds = gdal.GetDriverByName('GTiff').Create(reprojFName, self.cols,
#                             self.rows, 1, gdal.GDT_Byte)
# 
#             reproj_ds.SetGeoTransform(self.match_geotrans)
#             # spatial reference from making extent mask
#             reproj_ds.SetProjection(self.wkt)
#             # Reproject the grid survey to the dimensions of the extent
#             gdal.ReprojectImage(src_ds, reproj_ds, self.wkt, self.wkt, gdalconst.GRA_NearestNeighbour)
# 
#             reproj_ds.FlushCache()
# 
#             data = reproj_ds.GetRasterBand(1).ReadAsArray()
#             # remove non-risk cells
#             data[tmpExtMask == 0] = 0
# 
#             # is this the first one?
#             # create empty 2d array of the right type for storing the codes
#             if gridSurveyData is None:
#                 gridSurveyData = np.zeros_like(data, dtype=npDType)
#                 gridSurveyData[data != 0] = code
#             else:
#                 # subsequent - bitwise or the code in
#                 gridSurveyData[data != 0] |= code
# 
#             del reproj_ds
#             del src_ds
#             os.remove(reprojFName)
#             
#         return(gridSurveyYears, gridSurveyMeans, gridSurveySD, gridSurveyCodes, 
#                 gridSurveyData)
# 
#     def getShapefileDimensions(self, definition=False):
#         """
#         get x and y min and max from shapefile
#         """
#         dataset = ogr.Open(self.zonesShapeFName)
#         layer = dataset.GetLayer()
#         # print out definitions optional
#         if definition:
#             getShapeLayerDefinition(layer)
#         # get dimensions
#         (xmin, xmax, ymin, ymax) = layer.GetExtent()
#         del dataset
#         del layer
#         return xmin, xmax, ymin, ymax
# 
#     def getGeoTrans(self):
#         """
#         #### get dimensions that incorporate both extent shape and farm boundaries
#         """
#         cols = int((self.xmax - self.xmin) / self.resol)
#         rows = int((self.ymax - self.ymin) / self.resol)
#         match_geotrans = [self.xmin, self.resol, 0, self.ymax, 0,
#                                -self.resol]
#         print('cols', cols, 'rows', rows)
#         return cols, rows, match_geotrans
# 
# 
#     def makeRelativeRiskTif(self, relativeRiskFName, relRiskRasterOutFName):
#         """
#         read in rel risk ascii, and write relative risk Tiff to directory
#         if RR not given, then it is derived from the zones data
#         """
#         print('rr name', relativeRiskFName)
# 
#         dst = gdal.GetDriverByName('GTiff').Create(relRiskRasterOutFName, self.cols, self.rows,
#                                 1, gdalconst.GDT_Float32)
#         dst.SetGeoTransform(self.match_geotrans)
#         # spatial reference from making extent mask
#         dst.SetProjection(self.wkt)
# 
#         if relativeRiskFName is not None:
#             # Source - Kmap
#             RR_src = gdal.Open(relativeRiskFName, gdalconst.GA_ReadOnly)
# 
#             if not equalProjection(self.wkt, RR_src.GetProjection()):
#                 raise ValueError("Projection of relative risk raster not the same as EPSG given")
# 
#             # write kmap array to tif in directory
#             # temp kmap tif name and directory
#             # Reproject the kmap to the dimensions of the extent
#             if USE_GDAL_FOR_BILINEAR:
#                 gdal.ReprojectImage(RR_src, dst, self.wkt, self.wkt, gdalconst.GRA_Bilinear)
#                 RelRiskExtent = dst.GetRasterBand(1).ReadAsArray()
#             else:
#                 inband = RR_src.GetRasterBand(1)
#                 inband.SetNoDataValue(0)
#                 in_im = inband.ReadAsArray()
#                 
#                 # subset to extent of shape file
#                 geoTrans = RR_src.GetGeoTransform()
#                 invGeoTrans = gdal.InvGeoTransform(geoTrans)
#                 tlx, tly = gdal.ApplyGeoTransform(invGeoTrans, 
#                                 self.xmin, self.ymax)
#                 brx, bry = gdal.ApplyGeoTransform(invGeoTrans, 
#                                 self.xmin + (self.resol * self.cols),
#                                 self.ymax - (self.resol * self.rows))
#                 tlx = int(np.round(tlx))
#                 tly = int(np.round(tly))
#                 brx = int(np.round(brx))
#                 bry = int(np.round(bry))
#                 in_im = in_im[tly:bry, tlx:brx]
#                 
#                 nodata = inband.GetNoDataValue()
#                 if nodata is None:
#                     raise ValueError("No Data value must be set on RR raster")
#                 RelRiskExtent = np.empty((self.rows, self.cols), dtype=np.float32)
# 
#                 bilinear(in_im, RelRiskExtent, nodata)
# 
#                 outband = dst.GetRasterBand(1)
#                 outband.WriteArray(RelRiskExtent)
# 
#             del dst  # Flush
#             del RR_src
#         else:
#             # Source - zonecodes - RR=1 where zone>0
#             zoneArray = gdal.Open(self.zonesOutFName).ReadAsArray()
#             RelRiskExtent = np.where(zoneArray > 0, 1, 0).astype(np.float32)
#             outband = dst.GetRasterBand(1)
#             outband.WriteArray(RelRiskExtent)
#             
#         print('finish makeRRTif')
#         return RelRiskExtent
# 
# 
#     def writeTifToFile(self, fname, data, gdt_type):
#         """
#         write tif to directory
#         """
#         # write array to tif in directory
#         dst = gdal.GetDriverByName('GTiff').Create(fname, self.cols,
#                     self.rows, 1, gdt_type)
#         dst.SetGeoTransform(self.match_geotrans)
#         dst.SetProjection(self.wkt)
#         band = dst.GetRasterBand(1)
#         band.WriteArray(data)
#         del dst  # Flush
#         print('write RR to file')
# 
    def readSurveyData(self, surveyFName, animalTypes):
        """
        read in *.csv of survey data
        """
        # read in surveillance data
        surveyPathFName = surveyFName
        rawSurvey = np.genfromtxt(surveyPathFName,  delimiter=',', names=True,
            dtype=['u4', 'S32', 'f8', 'f8', 'f8', 'S10', 'f8'])
        survey = np.zeros(rawSurvey.shape, dtype=TRAP_PARAM_DTYPE)

        if np.sum(rawSurvey['Tnights'] < 0):
            raise ValueError('Negative trap nights found in point survey data')

        #TRAP_PARAM_DTYPE = [('u4', 'year'), ('u4', 'animal'), ('f8', 'easting'),
        #('f8', 'northing'), ('f8', 'age'), ('u1', 'sex'), ('u4', 'trapnights')]
        # copy the basic ones accross
        survey['year'] = rawSurvey['Year']
        survey['easting'] = rawSurvey['Easting']
        survey['northing'] = rawSurvey['Northing']
        survey['age'] = rawSurvey['Age']
        survey['trapnights'] = rawSurvey['Tnights']

        # the ones that require conversion from strings to constants
        survey['sex'][rawSurvey['Sex'] == b'M'] = params.MALE
        survey['sex'][rawSurvey['Sex'] == b'F'] = params.FEMALE

        # keep a track of all the animals that have been translated
        # from string to constants
        translatedAnimals = np.zeros(rawSurvey.shape, dtype=np.bool)

        for code in animalTypes.functionDict.keys():
            animal = animalTypes.functionDict[code]
            # have to be careful to match bytes, might need some
            # work for Python 2.x

            mask = (rawSurvey['Species'] == animal.name.encode())
            survey['animal'][mask] = code
            survey['detect'][mask] = animal.detect
            translatedAnimals[mask] = True

        # check if there are any that haven't been translated
        if not translatedAnimals.all():
            missedAnimals = np.unique(rawSurvey['Species'][mask == False])
            format = ','.join([str(x) for x in missedAnimals])
            msg = ('Was not able to understand the following species. ' +
                'Add these to the parameters: %s' % format)
            raise ValueError(msg)

        return survey

##################################################
# Pickle results to directory
##################################################


class PickleDat(object):
    """
    This object contains the fields from RawData that 
    are needed for running the model. This object is
    designed to be pickled.
    """
    def __init__(self, rawdata):
        # assemble objects to pickle
        self.wkt = rawdata.wkt
        self.match_geotrans = rawdata.match_geotrans
        self.resol = rawdata.resol
        self.cols = rawdata.cols
        self.rows = rawdata.rows
        self.relativeRiskRaster = rawdata.RelRiskExtent
        self.zoneArray = rawdata.zoneArray
        self.zoneCodes = rawdata.zoneCodes
        self.Pu_zone = rawdata.Pu_zone
        self.RR_zone = rawdata.RR_zone
        self.Name_zone = rawdata.Name_zone
        self.survey = rawdata.survey
        self.gridSurveyYears = rawdata.gridSurveyYears
        self.gridSurveyData = rawdata.gridSurveyData
        self.gridSurveyMeans = rawdata.gridSurveyMeans
        self.gridSurveySD = rawdata.gridSurveySD
        self.gridSurveyCodes = rawdata.gridSurveyCodes

# Workaround for errors in bilinear interpolation in GDAL 2.x.
# matches behaviour in GDAL 1.x which appears to give the correct answers.
@njit
def bilinear(in_im, out_im, nodata):

    xratio = in_im.shape[1] / out_im.shape[1]
    yratio = in_im.shape[0] / out_im.shape[0]

    for y in range(out_im.shape[0]):
        for x in range(out_im.shape[1]):
            out_im[y, x] = bilinear_interpolate(in_im, x * xratio, y * yratio, nodata)

@njit
def bilinear_interpolate(im, x, y, nodata):
    """
        Numba adapted bilinear interpolation

    :param im: 2D Image Matrix
    :param x: x pixel coordinate
    :param y: y pixel coordinate
    :return: Interpolated pixel from [x,y] coordinates.


    reference: http://stackoverflow.com/questions/12729228/simple-efficient-bilinear-interpolation-of-images-in-numpy-and-python

    """

    x0 = int(x)
    x1 = x0 + 1
    y0 = int(y)
    y1 = y0 + 1

    x_bound = im.shape[1] - 1
    y_bound = im.shape[0] - 1

    if x0 < 0:
        x0 = 0
    if x0 > x_bound:
        x0 = x_bound

    if x1 < 0:
        x1 = 0
    if x1 > x_bound:
        x1 = x_bound

    if y0 < 0:
        y0 = 0
    if y0 > y_bound:
        y0 = y_bound

    if y1 < 0:
        y1 = 0
    if y1 > y_bound:
        y1 = y_bound

    Ia = im[y0, x0]
    Ib = im[y1, x0]
    Ic = im[y0, x1]
    Id = im[y1, x1]

    if Ia == nodata or Ib == nodata or Ic == nodata or Id == nodata:
        # IMHO we should be returning nodata here, but we need
        # to match GDAL 1.x...
        return 0.0

    wa = (x1 - x) * (y1 - y)
    wb = (x1 - x) * (y - y0)
    wc = (x - x0) * (y1 - y)
    wd = (x - x0) * (y - y0)

    return wa * Ia + wb * Ib + wc * Ic + wd * Id

