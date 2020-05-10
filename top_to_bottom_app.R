#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Description: ------------------------------------------------------------
#
# Processing steps for proof-of-absence using reticulate
#  - based on steps in 'Kaitake_sc0'
#  - imports scripts for proofofabsence
#  - loads conda environment 'r_poa' containing required libraries
#
# Built under R version 3.6.2 (2019-12-12)
# Simon Howard; howards@landcareresearch.co.nz | si.w.howard@gmail.com
#-------------------------------------------------------------------------#

# packages ----------------------------------------------------------------

# devtools::install_github("rstudio/reticulate")
library(reticulate)

library(raster)

library(kableExtra)

library(leaflet)

library(sf)

library(shiny)


# file paths --------------------------------------------------------------

# import python packages
os <- reticulate::import("os", delay_load = TRUE)
pickle <- reticulate::import("pickle", delay_load = TRUE)

# app defaults ------------------------------------------------------------
defaults <- 
  list(zonesShapeFName = list(desc = "", label = "", value = "app\\www\\poa\\Kaitake\\Data\\extent.shp"),
       relativeRiskFName = list(desc = "", label = "", value = "app\\www\\poa\\Kaitake\\Data\\relRiskRaster.tif"),
       zonesOutFName = list(desc = "", label = "", value = "app\\www\\poa\\Kaitake\\Results\\Model_0\\zones.tif"),
       relRiskRasterOutFName = list(desc = "", label = "", value = "app\\www\\poa\\Kaitake\\Results\\Model_0\\relRiskRaster.tif"),
       resolution = list(desc = "", label = "Raster resolution", value = as.double(100)),
       epsg = list(desc = "", label = "epsg", value = as.integer(2193)),
       surveyFName = list(desc = "", label = "surveyFName", value = "app\\www\\poa\\Kaitake\\Data\\devices.csv"),
       gridSurveyFname = list(desc = "", label = "gridSurveyFname", value = NULL),
       prior_min = list(desc = "", label = "Prior min", value = 0.2),
       prior_mode = list(desc = "", label = "Prior mode", value = 0.4),
       prior_max = list(desc = "", label = "Prior max", value = 0.85),
       setNumIterations = list(desc = "", label = "Number of iterations", value = 100),
       setRRTrapDistance = list(desc = "", label = "Relative risk distance", value = 1),
       setMinRR = list(desc = "", label = "Minimum relative risk value", value = 1),
       setYears = list(desc = "", label = "setYears", value = 2),
       startPu = list(desc = "", label = "startPu", value = 1),
       sigmean = list(desc = "", label = "sigmean", value = 30),
       sigsd = list(desc = "", label = "sigsd", value = 10))

# make defaults input
input <- lapply(defaults, "[[", "value")

# server ------------------------------------------------------------------

# Define server logic required to draw a histogram

  

# server: load & format devices -------------------------------------------

# copy loaded device file to .tmp folder

# load devices as spatial object
devs <- st_sf(st_as_sf(read.csv("test_data/Input/devices.csv", stringsAsFactors = FALSE), 
                       coords = c("Easting", "Northing")), crs = input$epsg)
# get number devives types and sessions
ntypes <- length(unique(devs$Species))
nsession <- length(unique(devs$Year))

# message console when loaded
print(sprintf("device file loaded: %s rows - %s device types - %s sessions detected", nrow(devs), ntypes, nsession))
devices <- devs


spp <- sort(unique(devices$Species))
inputId.spp <- paste0("inputId.", spp)
n <- length(spp)
df <- data.frame(row.names = spp)
df$`Mean g0` <- as.numeric(0.1)
df$`Stdev g0` <- as.numeric(0.01)
df$`Mean sigma` <- as.numeric(90)
df$`Stdev sigma` <- as.numeric(10)
set.animal.params <- df
    
# server: zonesShape ------------------------------------------------------
  
path.ext <- "test_data/extent.shp"
if(file.exists(path.ext)){
  message(paste("loading .shp file:", path.ext))
  zonesShape.sf <- st_sf(st_read(path.ext), crs = input$epsg)
  zonesShape <- zonesShape.sf
}

# server: relative risk map -----------------------------------------------

# copy loaded relative risk raster file to .tmp folder
path.RRmap <- "test_data/relRiskRaster.tif"
# load any relative risk files in .tmp folder
if(file.exists(path.RRmap)){
  message(paste("loading relative risk .tif file:", path.RRmap))
  RRmap <- raster(path.RRmap)
  relRiskRaster <- RRmap
} 

# server: run py code -----------------------------------------------------

# import proofofabsence package
# params <- import_from_path(module = "params", path = "proofofabsence", convert = FALSE)
source_python("proofofabsence/params.py", convert = FALSE)
# preProcessing <- import_from_path(module = "preProcessing", path = "proofofabsence", convert = FALSE)
source_python("proofofabsence/preProcessing.py", convert = FALSE)
# calculation <- import_from_path(module = "calculation", path = "proofofabsence", convert = FALSE)
source_python("proofofabsence/calculation.py", convert = FALSE)

animals = AnimalTypes()                                              #

animal.params <-
  list(TYPENUM = as.integer(6:(6+nrow(set.animal.params-1))),    # start IDs at arbritary number
       TYPECHAR = row.names(set.animal.params),
       g0mean = set.animal.params$`Mean g0`,
       g0sd = set.animal.params$`Stdev g0`,
       sigmean = set.animal.params$`Mean sigma`,
       sigsd = set.animal.params$`Stdev sigma`)

print(animal.params)

for(i in seq_along(animal.params$TYPENUM)){
  print(i)
  animals$addAnimal(animal = animal.params$TYPENUM[i],
                    name = animal.params$TYPECHAR[i],
                    detect = DETECT_ANIMAL)
}

myParams = POAParameters(animals)

## USE MULTIPLE ZONES
myParams$setMultipleZones(TRUE)

for(i in seq_along(animal.params$TYPENUM)){
  TYPE <- animal.params$TYPENUM[i]
  
  myParams$setCapture(TYPE, animal.params$g0mean[i], animal.params$g0sd[i])
  myParams$setSigma(TYPE, animal.params$sigmean[i], animal.params$sigsd[i])
  myParams$addRRBufferAnimal(animalCode = TYPE)
}


# number of iterations
myParams$setNumIterations(as.integer(input$setNumIterations))
myParams$setRRTrapDistance(as.integer(input$setRRTrapDistance))
myParams$setYears(as.integer(1), as.integer(input$setYears))

## THE startPu WILL NOT BE USED IF USE zoneData FILE - TURN OFF
# starting Pu (GRID CELL PREVALENCE) and period rate of Pu increase
startPu = as.double(input$startPu)

## SET THE RATE OF INCREASE OF PU
PuIncreaseRate = as.double(0.0)
myParams$setPu(startPu, PuIncreaseRate)

# minimum RR value
myParams$setMinRR(as.double(input$setMinRR))

myParams$setPrior(input$prior_min, input$prior_mode, input$prior_max)
myParams$setIntro(0.00001, 0.00002, 0.00003)        # (min, mode, max)

rawdata <- # preProcessing$
      RawData(zonesShapeFName = "test_data/Input/extent.shp",
              relativeRiskFName = "test_data/Input/relRiskRaster.tif",
              zonesOutFName = "test_data/Results/zones.tif", 
              relRiskRasterOutFName = "test_data/Results/relRiskRaster.tif",
              resolution = as.double(input$resolution),
              epsg = as.integer(input$epsg),
              surveyFName = "test_data/Input/devices.csv",
              params = myParams, 
              gridSurveyFname = NULL)

    # load builtin py functions (required for open() below)
    py <- reticulate::import_builtins()
    # make object for pickling spatial data
    pickledat = PickleDat(rawdata)

    # pickle to output directory
    pickleName = "test_data/Results/spatialData.pkl"
    fileobj = py$open(pickleName, 'wb')
    pickle$dump(pickledat, fileobj, protocol=4)
    fileobj$close()
    
    
    result = # calculation$
      calcProofOfAbsence(myParams, pickledat$survey,
                         pickledat$relativeRiskRaster, pickledat$zoneArray, pickledat$zoneCodes,
                         pickledat$match_geotrans, pickledat$wkt, "test_data/Results",
                         pickledat$RR_zone, pickledat$Pu_zone, pickledat$Name_zone)

    
    
    # pickle results
    # pickleName = ".tmp/output/resultData.pkl"
    # fileobj <- py$open(pickleName, "wb")
    # pickle$dump(result, fileobj)
    # fileobj$close()
    
    
    # source_python("proofofabsence/postProcessing.py", convert = FALSE)
    # results = ResultsProcessing(".tmp/input", ".tmp/output", 
    #                             "spatialData.pkl", "resultData.pkl", ".tmp/input/extent.shp",
    #                             "PofSseResultTable.txt", 'PoF_SSe_Graph.png', 'zoneSeResultTable.txt')
    #  
    # results$makeTableFX("Sessions")
    # results$writeToFileFX('PofSseResultTable.txt', "Sessions")
    #  
    # try({
    #   results$makeZoneTableFX()
    #   results$writeZoneTableToFileFX("zoneSeResultTable.txt", "Sessions")
    # })
    
    return(result)

  })
  
  source("array_functions.R")
  
  
  
  
  
  # observe({try(print(pyPOA()$poFMatrix))})

  # server: PoF plot --------------------------------------------------------
  # observe({try(print(py_to_r(pyPOA()$poFMatrix)))})
  
  output$PoAtimeplot <- renderPlot({
    result <- pyPOA()
    PoFmat <- py_to_r(result$poFMatrix)
    poa_mean <- rowMeans(PoFmat)
    poa_low <- apply(PoFmat, 1, quantile, 0.05)
    poa_upp <- apply(PoFmat, 1, quantile, 0.95)
    years <- py_to_r(result$params$years)
    prior <- py_to_r(result$priorStore)
    prior_mean <- mean(prior)
    prior_low <- quantile(prior, 0.05)
    prior_upp <- quantile(prior, 0.95)
    plot(y = c(prior_mean, poa_mean), x = c(0, years), ylim = c(0, 1), type = "b", 
         xlab = "Session", ylab = "Probability of absence", axes = FALSE, frame.plot = TRUE)
    axis(side = 1, at = c(0, years), labels = c("Prior", years))
    axis(2, at = seq(0,1,0.2))
    arrows(y0 = c(prior_low, poa_low), x0 = c(0, years), y1 = c(prior_upp, poa_upp), x1 = c(0, years), code = 3, angle = 90)
  })
  

# server: add SSe raster --------------------------------------------------

    
  observe({
    input$GO
    pyPOA()
    
    library(raster)
    meanSeu <- raster(".tmp/output/meanSeuAllYears.tif")

    pal <- colorNumeric(palette = "viridis", domain = c(0,1.1),  # add 0.1 to upper limit
                        na.color = "transparent")
    
    leafletProxy(session = session, mapId = "baseMap") %>%
      
      # debugonce(pal); leaflet() %>%
      clearGroup(group = "SeU") %>% 
      addRasterImage(x = meanSeu, layerId = "SeU", group = "SeU", 
                     opacity = input$rasterOpacity, colors = pal) %>%
      addLegend(pal = pal, values = c(0,1), labels = c(0,1), layerId = "SeU") # %>%
      # addLayersControl(overlayGroups = c("SeU"), position = "bottomleft",
      #                  options = layersControlOptions(collapsed = FALSE))
    
    

  })
  
  
  # server: inputTable() ----------------------------------------------------
  inputTable <- reactive({

    input.ls <- reactiveValuesToList(input)
    input.ls <- unlist(input.ls)
    
    # print(lapply(input.ls, as.character))

    inputs.df <- data.frame(Description = names(input.ls),
                            Value = input.ls)
    return(kable(inputs.df, row.names = FALSE))
  })
  

  
  # server: output$inputTable -----------------------------------------------
  output$inputTable <- renderText(inputTable())
  
  validTable <- reactive({
    # return(valid())
    return(input$a)
  })
  output$validTable <- renderPrint(validTable())
  
  
  
  printRes <- eventReactive(eventExpr = input$runLine, {
    eval(str2expression(input$consoleIn))
  })
  
  output$consoleOut <- renderPrint({
    printRes()
  })
  
  output$pyinfo <- renderPrint({
    print("getwd()")
    print(getwd())
    print("py_config()")
    print(reticulate::py_discover_config())
    print("conda_list()")
    # print(reticulate::conda_list())
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
