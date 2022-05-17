---
editor_options: 
  markdown: 
    wrap: sentence
---

### Getting started

To get started using proof-of-absence calculator ensure your data meets the formats outlined in the [data requirements guide](ProofOfAbsence_data_req.pdf).

### Upload inputs

To start using the tool upload a shapefile of the area of interest boundaries.
Shapefiles are made up of files with .shp, .shx, .prj and .dbf extensions.

Some inputs require multiple files to be uploaded. Click the 'Browse' button to bring up the file upload dialogue. To select multiple files, hold down the Ctrl keys while clicking each file or hold down the shift key to select the first and last files. The files will upload and a progress bar will start and display 'Upload complete' when completed.

If successful, the associated EPSG code ([epsg.io](https://epsg.io/)) will be detected, the value in 'EPSG code' will be updated and the area of interest will be shown on the map.
If the EPSG code is not detected, the user will see an error, and will have the option to enter the code manually.
The uploaded shapefile sets the coordinate reference system (CRS) for all uploaded spatial files.
If these files differ in CRS, they will either fail to display, or will appear outside the area of interest.

Next, upload surveillance files.
Point surveillance infomation is stored in a single comma separated values (.csv) file.
Grid surveillance information are also stored as a .csv file, but include a raster file (img or tif extension) which shows where grid surveillance takes place.
Select the both grid surveillance information and associated raster files to upload.

Surveillance can be either point or grid-based, or a combination of both.
Once sucessfully uploaded, detection parameters for grid surveillance will populate the 'Grids' table in 'Set parameters' tab.
Uploaded point surveillance files will appear as points on the map and will populate the device types in the 'Points' table in the 'Set parameters' tab.
If the uploaded point surveillance file contains multiple years and/or device types, the devices and years to display can be selected using the 'Select year to display' and 'Select surveillance types to display' drop down menus below the map.

Finally, a raster file (optional) defining the relative risk can be uploaded.
Once uploaded, the relative risk values will be shown on the map.

### Set parameters

#### Detection parameters

Detection parameters for point surveillance data must be entered for each detection device type. The device type names are populated from the uploaded .csv file. To enter values, double-click the cells in the table to enter values.

The detection probabilities are summarised by two parameters g0 (g-naught) and sigma (σ). Sigma relates to an animals home range size and g0 summarises the probability of detecting an animal at the centre of its home range.  Both detection parameters require a mean value and a standard deviation for each device type. For g0, the mean value must between zero and one and the standard deviation must be greater than zero. Mean values for sigma must be greater than zero and the standard deviation must be greater than zero. 

Detection parameters for grid cell surveillance are contained in the uploaded .csv, but can be modified by double-clicking cells in the table and modifying values.

#### Run model

If all spatial files have been uploaded and parameters set, then the model can be run by pressing the 'Calculate proof of absence' button in the 'Run model' tab.
The tool allows the upload of complex scenarios, so run times may vary.
Performance slows as the number of spatial units (grid cells), point surveillance locations (rows in the uploaded .csv) and model iterations increase, so expect long run times for large examples.
If the 'Calculating proof of absence ...' notification in the lower right of the browser window is still displayed then the app is still working on calculations and will eventually either time out (currently at 15 mins) or complete.

Once completed tables of the probability of absence (PoA) and surveillance sensitivity (SSe) are shown and the user can also export results to a .zip file and view the spatial coverage of the surveillance sensitivity.

#### 
