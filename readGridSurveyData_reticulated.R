    """
    Read all the grid survey data and make sure it is converted to
    files of the correct spatial reference and extent.
    """
readGridSurveyData <- function(self, gridSurveyFname = NULL, params = poa$params){
    rawGridSurvey = np.genfromtxt(gridSurveyFname, delimiter=',', names=True,
                                  dtype=['S200', 'i4', 'f8', 'f8'])

    gridSurveyYears = rawGridSurvey['year']

    nGrids = len(gridSurveyYears)
    #        print('gridyears', self.gridSurveyYears, 'type', type(self.gridSurveyYears),
    #            'is scalar', np.isscalar(self.gridSurveyYears), 'gridsize', nGrids)

    gridSurveyMeans = rawGridSurvey['mean']
    gridSurveySD = rawGridSurvey['sd']
    # get an array of powers of 2 so we can encode
    # each year in a raster as a bit
    gridSurveyCodes = 2 ** np.arange(nGrids)
    dirName = os.path.dirname(gridSurveyFname)

    # work out the data type to use - find the minimum data type we can use
    # to save memory
    maxCode = gridSurveyCodes[-1]  # 2**number of grids-1
    npDType = None
    for dt in (np.uint8, np.uint16, np.uint32, np.uint64):  # loop potential data types
        info = np.iinfo(dt)  # get min and max of datetype
        if maxCode <= info.max:  # if the required integer < max of data type, keep
            npDType = dt
            break

    if npDType is None:  # if have more than 61 grids, it will be error
        msg = 'Too many grid survey years to store in an integer'
        raise ValueError(msg)

    print('chosen dtype', npDType)

    # have to create a mask to update the grid survey rasters
    # this is not ideal because we do this in calculation.py

    tmpExtMask = self.zoneArray.copy()
    tmpExtMask[self.RelRiskExtent < params.minRR] = 0

    # so we know when we got to the first one
    gridSurveyData = None

    # reproject each dataset into a temproary file and
    # then read it in.
    for fname, code in zip(rawGridSurvey['gridName'], gridSurveyCodes):
        # input
        fname = os.path.join(dirName, fname.decode())
        src_ds = gdal.Open(fname)

        # temp file - maybe should be able to set directory?
        handle, reprojFName = tempfile.mkstemp(suffix='.tif')
        os.close(handle)

        reproj_ds = gdal.GetDriverByName('GTiff').Create(reprojFName, self.cols,
                                                         self.rows, 1, gdal.GDT_Byte)

        reproj_ds.SetGeoTransform(self.match_geotrans)
        # spatial reference from making extent mask
        reproj_ds.SetProjection(self.wkt)
        # Reproject the grid survey to the dimensions of the extent
        gdal.ReprojectImage(src_ds, reproj_ds, self.wkt, self.wkt, gdalconst.GRA_NearestNeighbour)

        reproj_ds.FlushCache()

        data = reproj_ds.GetRasterBand(1).ReadAsArray()
        # remove non-risk cells
        data[tmpExtMask == 0] = 0

        # is this the first one?
        # create empty 2d array of the right type for storing the codes
        if gridSurveyData is None:
            gridSurveyData = np.zeros_like(data, dtype=npDType)
            gridSurveyData[data != 0] = code
        else:
            # subsequent - bitwise or the code in
            gridSurveyData[data != 0] |= code

        del reproj_ds
        del src_ds
        os.remove(reprojFName)

    return (gridSurveyYears, gridSurveyMeans, gridSurveySD, gridSurveyCodes,
            gridSurveyData)
}

  