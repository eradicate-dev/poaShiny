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

library(rgdal)

library(raster)

library(kableExtra)

library(leaflet)

library(sf)

library(shiny)

library(DT)

# Define any Python packages needed for the app here:
PYTHON_DEPENDENCIES = c("numpy==1.18.4", "llvmlite==0.31.0", "numba==0.47.0")

# file paths --------------------------------------------------------------

path.tmp <- paste0(getwd(), "/.tmp")
path.tmp.input <- paste0(path.tmp, "/input")
path.tmp.output <- paste0(path.tmp, "/output")
dir.create(path.tmp.input, recursive = T)
dir.create(path.tmp.output, recursive = T)

unlink(paste0(path.tmp.input, "/*"))
unlink(paste0(path.tmp.output, "/*"))

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
       prior_max = list(desc = "", label = "Prior max", value = 0.6),
       setNumIterations = list(desc = "", label = "Number of iterations", value = 100),
       setRRTrapDistance = list(desc = "", label = "Relative risk distance", value = 1),
       setMinRR = list(desc = "", label = "Minimum relative risk value", value = 1),
       setYears = list(desc = "", label = "setYears", value = 2),
       startPu = list(desc = "", label = "startPu", value = 1))


# Define UI
ui.troubleshooting <- tabPanel(title = "Troubleshooting",
                                    splitLayout(
                                      list(h4("Python & conda version info"),
                                           verbatimTextOutput("pyinfo")),
                                      list(h4("Run R commands"), 
                                           fluidRow(column(textInput(inputId = "consoleIn", label = "consoleIn", value = "getwd()"), width = 6), 
                                                    column(actionButton(inputId = "runLine", label = "runLine"), width = 6)),
                                           verbatimTextOutput("consoleOut")
                                      )
                                    ))

# UI: input ---------------------------------------------------------------

ui.inputs <- list(
  # Application title
  titlePanel("Shiny proof-of-absence interface"),
  # Sidebar with a slider input for number of bins 
  
  wellPanel(
    selectInput(inputId = "namedExample", label = "Select example set", choices = c("None", "Kaitake possums", "Mahia possums", "CK stoats"), multiple = FALSE),
    conditionalPanel(condition = "input.namedExample == 'None'",{
      list(fileInput(inputId = "zonesShapeFName", label = "zonesShapeFName", multiple = TRUE),
           fileInput(inputId = "surveyFName", label = "surveyFName", multiple = FALSE),
           fileInput(inputId = "relativeRiskFName", label = "relativeRiskFName", multiple = FALSE))
    }),
    numericInput(inputId = "setMinRR", label = defaults$setMinRR$label, value = defaults$setMinRR$value, min = 0, max = 1000),
    numericInput(inputId = "epsg", label = defaults$epsg$label, value = defaults$epsg$value)
  ),
  DT::DTOutput(outputId = "deviceUI"),
  actionButton(inputId = "runpy", label = "Run params script"),
  actionButton(inputId = "GO", label = "Run inputs")
)

ui.inputs.priors <- 
  fluidRow(column(numericInput(inputId = "prior_min", label = "Prior min", value = defaults$prior_min$value, min = 0, max = 1), width = 4),
       column(numericInput(inputId = "prior_mode", label = "Prior mode", value = defaults$prior_mode$value, min = 0, max = 1), width = 4),
       column(numericInput(inputId = "prior_max", label = "Prior max", value = defaults$prior_max$value, min = 0, max = 1), width = 4)
  )


# UI: advanced input ------------------------------------------------------
ui.advinputs <- 
  tabPanel(title = "Advanced inputs",
           splitLayout(
             list(
               sliderInput("rasterOpacity", label = "Raster opacity", min = 0, max = 1, value = 0.7),
               numericInput(inputId = "resolution", label = defaults$resolution$label, value = defaults$resolution$value),
               numericInput(inputId = "setNumIterations", label = defaults$setNumIterations$label, value = defaults$setNumIterations$value),
               numericInput(inputId = "setRRTrapDistance", label = defaults$setRRTrapDistance$label, value = defaults$setRRTrapDistance$value),
               numericInput(inputId = "setYears", label = defaults$setYears$label, value = defaults$setYears$value),
               numericInput(inputId = "startPu", label = defaults$startPu$label, value = defaults$startPu$value)
             )))


# UI: output --------------------------------------------------------------

ui.output <- 
  # list(
    # verbatimTextOutput(outputId = "table2"),
    fluidRow(column(width = 6, 
                    list(h3("Probability of absence")),
    plotOutput("PoAtimeplot"),
    plotOutput("PoAdensplot")),
    # h3("validTable"),
    # verbatimTextOutput("validTable"),
    column(width = 6, list(h3("baseMap"),
    leafletOutput("baseMap")),
    # h3("result"),
    # verbatimTextOutput("result"), 
    # h3("runpypress"),
    # verbatimTextOutput("runpypress"), 
    checkboxInput(inputId = "renderPts", label = "render map points", value = FALSE),
    checkboxInput(inputId = "renderRasts", label = "render map rasters", value = FALSE),
    h3("inputTable"),
    htmlOutput("inputTable"))
  )


# UI: layout page ---------------------------------------------------------
ui <- fluidPage(
  tabsetPanel(
    tabPanel(title = "Run model", 
             # splitLayout(list(ui.inputs, ui.inputs.priors), ui.output)
             fluidRow(column(4, list(ui.inputs, ui.inputs.priors)), column(8, list(ui.output)))
             ),  
    ui.advinputs,
    ui.troubleshooting
    )
  )


printLine <- NULL

options(shiny.reactlog = TRUE)   # press CTRL + F3 to view react log



# testing inputs ----------------------------------------------------------
# input <- lapply(defaults, "[[", "value")


# server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  runpypress <- reactive({input$runpy})
  
  output$runpypress <- renderPrint(runpypress())
  

  
  
  # manage python versions and modules --------------------------------------
  
  # import python packages
  os <- reticulate::import("os", delay_load = TRUE)
  pickle <- reticulate::import("pickle", delay_load = TRUE)
  
  # ------------------ App virtualenv setup (Do not edit) ------------------- #
  # adapted from - 'https://github.com/ranikay/shiny-reticulate-app'
  
  # check if on shiny server and install modules in 'PYTHON_DEPENDENCIES'
  if(Sys.info()[['user']] == 'shiny'){
    
    virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
    python_path = Sys.getenv('PYTHON_PATH')
  
    # Create virtual env and install dependencies
    reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
    reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES)
    reticulate::use_virtualenv(virtualenv_dir, required = T)
    
  } else if(grepl("conda.exe", Sys.getenv("CONDA_EXE"))){ 
    # check if conda available and use 'r_poa' environment
    use_condaenv(condaenv = "r_poa", required = TRUE)
  } else {
    # use python on system path
    if(Sys.getenv("PYTHON_PATH") != "") use_python(Sys.getenv("PYTHON_PATH"))
  }
  # ------------------ App server logic (Edit anything below) --------------- #
  
  # server: valid() ---------------------------------------------------------
  
  
  # observe({
  #   last.valid <- reactiveValuesToList(input)
  #   
  #   print(input$prior_min)
  #   print(valid()$prior_min)
  #   print(last.valid$prior_min)
  #   
  #   if(is.null(valid()$prior_min)) updateNumericInput(session, "prior_min", value = last.valid$prior_min)
  # })
  

  # server: set reactive values ---------------------------------------------
  setYears <- reactiveVal(NULL)
  
  
  # server: set file paths --------------------------------------------------

  paths <- reactiveValues(zonesShapeFName = NULL,
                          relativeRiskFName = NULL,
                          surveyFName = NULL)
  
  valid <- reactive({
    
    input.ls <- list(prior_min = if(is.numeric(input$prior_min) & input$prior_min > 0 & input$prior_min < 1) input$prior_min else NULL,
                     prior_mode = input$prior_mode,
                     prior_max = input$prior_max,
                     resolution = input$resolution,
                     setNumIterations = input$setNumIterations,
                     setRRTrapDistance = input$setRRTrapDistance,
                     setYears = input$setYears,
                     startPu = input$startPu,
                     sigmean = input$sigmean,
                     sigsd = input$sigsd
                     )
    return(input.ls)
  })
  
  
  
  observe({
    input$GO
    
    if(isolate(input$prior_min) >= isolate(input$prior_mode)){
    showNotification(ui = h5("Prior min > Prior mode"))
    }
    
    if(isolate(input$prior_min) < 0){
      updateNumericInput(session, inputId = "prior_min", value = 0)
    }
  })
  

  # server: base map --------------------------------------------------------
  output$baseMap <- 
    renderLeaflet({
      leaflet() %>% 
        addProviderTiles(group = "Topo", provider = providers$OpenTopoMap) %>% 
        addLayersControl(overlayGroups = c("Topo", "Zones", "Devices", "Relative risk", "SeU"), 
                         options = layersControlOptions(collapsed = FALSE), position = "bottomleft")
    })
  
  

  # server: copy input files to temp folder ---------------------------------
  
  # - copy device folders
  observe({
    if(input$namedExample == "Kaitake possums"){
      paths$zonesShapeFName <- "app/www/example_data/Kaitake_possums/extent.shp"
      paths$relativeRiskFName <- "app/www/example_data/Kaitake_possums/relRiskRaster.tif"
      paths$surveyFName <- "app/www/example_data/Kaitake_possums/devices.csv"
    } else if(input$namedExample == "Mahia possums"){
      paths$zonesShapeFName <- "app/www/example_data/Mahia_Audrey/extent_block1ABCD.shp"
      paths$relativeRiskFName <- "app/www/example_data/Mahia_Audrey/habDistRR_block1ABCD.tif"
      paths$surveyFName <- "app/www/example_data/Mahia_Audrey/Surveillance_location.csv"
    } else if(input$namedExample == "CK stoats"){
      paths$zonesShapeFName <- "app/www/example_data/CK_stoats/extent.shp"
      paths$relativeRiskFName <- "app/www/example_data/CK_stoats/relRisk.tif"
      paths$surveyFName <- "app/www/example_data/CK_stoats/devices.csv"
    } else {
      unlink(".tmp/input/*")
    }
  })
  
  

  # server: load & format devices -------------------------------------------
  devices <- reactiveVal(NULL)
  
  # copy loaded device file to .tmp folder
  observe({
    if(!is.null(input$surveyFName) & input$namedExample == "None"){
      paths.to <- paste0(path.tmp, "/input/", sub(".*(?=\\..*$)", "devices", normalizePath(input$surveyFName$name), perl = TRUE))
      file.copy(from = input$surveyFName$datapath, to = normalizePath(paths.to), overwrite = TRUE)
      paths$surveyFName <- normalizePath(".tmp/input/devices.csv")
    } 
    
    if(!is.null(paths$surveyFName)){
      # load devices as spatial object
      devs <- st_sf(st_as_sf(read.csv(paths$surveyFName, stringsAsFactors = FALSE), 
                             coords = c("Easting", "Northing")), crs = input$epsg)
      # get number devives types and sessions
      ntypes <- length(unique(devs$Species))
      nsession <- length(unique(devs$Year))
      
      # message console when loaded
      print(sprintf("device file loaded: %s rows - %s device types - %s sessions detected", nrow(devs), ntypes, nsession))
      
      # devs.4326 <- st_transform(devs, crs = 4326)
      
      devices(devs)
    }
    
  })
    
  # observe({input$namedExample; input$}, {  
  #   # convert to spatial object
  #   
  #   
  #   # setYears(length(unique(devs.4326$Year)))    # get number of years from devices
  #   
  #   # return(setYears)
  # })
  

  # server: add devices to map ----------------------------------------------
  observe({
    
    if("sf" %in% class(devices()) & input$renderPts){
      
      devs <- st_transform(devices(), crs = 4326)
      # set color palette
      pal.device <- colorFactor(palette = "Set2", domain = unique(devs$Species))
      
      leafletProxy(session = session, mapId = "baseMap") %>%
        # leaflet() %>%
        # addLayersControl(overlayGroups = c("Devices"), options = layersControlOptions(collapsed = FALSE)) %>% 
        clearGroup("Devices") %>% 
        addCircles(data = devs, group = "Devices",
                   radius = 20, weight = 1, opacity = 1, fillOpacity = 1, color = ~pal.device(devs$Species)) %>%
        addLegend(layerId = "Devices", pal = pal.device, values = unique(devs$Species), group = "Devices") 
    }
  })
  
  set.animal.params <- reactiveVal(NULL)
  observe({
    if(!is.null(devices())){
      
      spp <- sort(unique(devices()$Species))
      inputId.spp <- paste0("inputId.", spp)
      n <- length(spp)
      df <- data.frame(row.names = spp)
      df$`Mean g0` <- as.numeric(0.1)
      df$`Stdev g0` <- as.numeric(0.01)
      df$`Mean sigma` <- as.numeric(90)
      df$`Stdev sigma` <- as.numeric(10)
      
      set.animal.params(df)
    }
  })
  
  
  deviceUI <- reactive({
    dtOut <- DT::datatable(set.animal.params(), editable = TRUE, options = list(ordering=F, searching = F, paging = F))
    return(dtOut)
  })
  
  observe({
    if(!is.null(input$deviceUI_cell_edit)){
      print(set.animal.params())
      print(input$deviceUI_cell_edit)
      set.animal.params(DT::editData(data = set.animal.params(), 
                       info = input$deviceUI_cell_edit))
    }
  })
  
  
  output$deviceUI <- DT::renderDT(deviceUI())
  output$table2 <- renderPrint(deviceUI())
    
  
  # 
  # leafletProxy(session = session, mapId = "baseMap") %>%
  #   # leaflet() %>%
  #   addCircles(layerId = "Devices", data = isolate(devices())), radius = 10, weight = 1, opacity = 1, fillOpacity = 1, color = ~pal.device(isolate(devices()$Species))) %>%
  #   addLegend(layerId = "Devices", pal = pal.device, values = unique(devs.4326$Species))
  # 
  # observe(print(devices()))
  
  
  # server: zonesShape ------------------------------------------------------
  
  zonesShape <- reactiveVal(NULL)
  
  # copy loaded extent file to .tmp folder
  observe({
    
    if(!is.null(input$zonesShapeFName) & input$namedExample == "None"){
      paths.to <- paste0(path.tmp, "/input/", sub(".*(?=\\..*$)", "extent", normalizePath(input$zonesShapeFName$datapath), perl = TRUE))
      file.copy(from = input$zonesShapeFName$datapath, to = normalizePath(paths.to), overwrite = T)
      paths$zonesShapeFName <- normalizePath(".tmp/input/extent.shp")
    }
    
    path.ext <- paths$zonesShapeFName
    if(!is.null(paths$zonesShapeFName)){
      message(paste("loading .shp file:", paths$zonesShapeFName))
      zonesShape.sf <- st_sf(st_read(paths$zonesShapeFName), crs = input$epsg)
      zonesShape(zonesShape.sf)
    }
  })
  
  observe({
    
    # make reactive to selecting examples
    input$namedExample
    
    if("sf" %in% class(zonesShape())){
      
      # get zone polygons and convert
      zonesShape.4326 <- st_transform(zonesShape(), crs = 4326)
      
      # get bounds
      bb <- as.numeric(st_bbox(zonesShape.4326))
      
      # make popup text
      # try({zonesShape.4326$poptxt <- 
      #   sapply(split(zonesShape.4326, zonesShape.4326$zoneName), 
      #          function(x) kableExtra::kable(st_drop_geometry(x), escape = TRUE, format = "html", row.names = FALSE))
      # })
      # update base map
      leafletProxy(session = session, mapId = "baseMap") %>%
        # leaflet() %>% addProviderTiles(group = "Topo", provider = providers$OpenTopoMap) %>% 
        clearGroup(group = "Zones") %>% 
        addPolygons(data = zonesShape.4326, fill = T, weight = 2, # popup = ~poptxt, 
                    group = "Zones") %>% 
        fitBounds(lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])
    }
  })

  # server: relative risk map -----------------------------------------------

  relRiskRaster <- reactiveVal(NULL)
  
  # copy loaded relative risk raster file to .tmp folder
  observe({
    
    if(!is.null(input$relativeRiskFName) & input$namedExample == "None"){
      paths.to <- paste0(path.tmp, "/input/", 
                         sub(".*(?=\\..*$)", "relRiskRaster", 
                             normalizePath(input$relativeRiskFName$name), perl = TRUE))
      file.copy(from = input$relativeRiskFName$datapath, to = normalizePath(paths.to), overwrite = T)
      paths$relativeRiskFName <- normalizePath(list.files(".tmp/input", pattern = "relRiskRaster", full.names = T))
    }
    
    # load any relative risk files in .tmp folder
    if(!is.null(paths$relativeRiskFName)){
      message(paste("loading relative risk .tif file:", paths$relativeRiskFName))
      relRiskRaster(raster(paths$relativeRiskFName))
    }
  })
  
  # # crop relative risk layer to zones if available
  # isolate({!is.null(zonesShape()) & !is.null(relRiskRaster)})
  # 
  # isolate({plot(relRiskRaster()); plot(zonesShape(), col = NA, add = T)})
  # 
  # res <- raster::crop(isolate(relRiskRaster()), isolate(zonesShape()), snap = "out")
  # res <- raster::mask(res, isolate(zonesShape()))
  # 
  # tmp <- rasterize(isolate(zonesShape()), isolate(relRiskRaster()))
  # 
  # poly <- isolate(zonesShape())
  # plot(poly)
  # plot(st_cast(poly, "LINESTRING"))
  # 
  # rastLines <- rasterize(st_cast(poly, "LINESTRING"), isolate(relRiskRaster()))
  # plot(rastLines)
  # rastPoly <- rasterize(poly, isolate(relRiskRaster()))
  # plot(rastPoly)
  # overlay(rastPoly, rastLines, fun = function(x,y){!is.na(x)|!is.na(y)})
  # 
  # ?raster::calc
  # 
  # plot(tmp); plot(isolate(zonesShape()), color = NA, add = T)
  # 
  # plot(tmp); plot(isolate(zonesShape()), color = NA, add = T)
  # plot(res); plot(isolate(zonesShape()), color = NA, add = T)
  
  # server: add relative risk layer to map ----------------------------------
  observe({
    if("RasterLayer" %in% class(relRiskRaster()) & input$renderRasts){
      
      rasterOptions(# chunksize = 1e+06, 
        maxmemory = 2.5e+08)
      
      withProgress(message = 'Reprojecting raster', detail = 'This may take a while...',
                   value = 0, {
        # get layer and convert to WGS84
        RRmap.3857 <- 
          projectRaster(relRiskRaster(), crs = sp::CRS("+init=epsg:3857"),
                        method = "ngb")
      
      
      incProgress(amount = 0.5, message = 'Mapping raster')
      # get bounds (needs conversion to lat/long)
      bb <- as.vector(extent(
        raster::projectExtent(object = RRmap.3857, 
                              crs = sp::CRS("+init=epsg:4326"))))
      
      # set values above MinRR to NA
      RRmap.3857 <- raster::clamp(RRmap.3857, lower = input$setMinRR, useValues = FALSE)
      valrng <- c(input$setMinRR, max(values(RRmap.3857), na.rm = T))

      # make palette
      pal <- colorNumeric(palette = "viridis",
                          domain = valrng,
                          na.color = "transparent")
      
      # update base map
      leafletProxy(session = session, mapId = "baseMap") %>%
        # leaflet() %>%
        clearGroup(group = "Relative risk") %>%
        addRasterImage(x = RRmap.3857, group = "Relative risk",
                       opacity = 0.95, colors = pal, 
                       project = FALSE, method = "ngb") %>%
        fitBounds(lng1 = bb[1], lat1 = bb[3], lng2 = bb[2], lat2 = bb[4])
        
      })
      # add legend if relative risk values vary 
      if(diff(valrng) > 0){
        leafletProxy(session = session, mapId = "baseMap") %>%  
          addLegend(layerId = "RRlegend", pal = pal, values = valrng, bins = 5,
                    title = "Relative risk",
                    group = "Relative risk")
      }
      
    }
  })
  
    
  # server: run py code -----------------------------------------------------
  pyPOA <- eventReactive(input$runpy, {
    
    # import proofofabsence package
    params <- reticulate::py_run_file(file = "proofofabsence/params.py", convert = FALSE)
    preProcessing <- reticulate::py_run_file("proofofabsence/preProcessing.py", convert = FALSE)
    calculation <- reticulate::py_run_file("proofofabsence/calculation.py", convert = FALSE)
    
    source("proofofabsence/preProcessing.R")
    
    print("runpy press detected")
    
    # import proofofabsence package
    # params <- import_from_path(module = "params", path = "proofofabsence", convert = FALSE)
    source_python("proofofabsence/params.py", convert = FALSE)
    # preProcessing <- import_from_path(module = "preProcessing", path = "proofofabsence", convert = FALSE)
    source_python("proofofabsence/preProcessing.py", convert = FALSE)
    # calculation <- import_from_path(module = "calculation", path = "proofofabsence", convert = FALSE)
    source_python("proofofabsence/calculation.py", convert = FALSE)

    animals = AnimalTypes()                                              #
    # create parameter objects ------------------------------------------------

    # animal.params <-
    #    list(TYPENUM = as.integer(12:17),
    #         TYPECHAR = c("Leghold", "Sentinel", "PossMaster", "Camera", "CHEWDETECT", "AT220"),
    #         g0mean = c(0.06, 0.04, 0.04, 0.075, 0.06, 0.05),
    #         g0sd = c(0.03, 0.03, 0.03, 0.03, 0.03, 0.03),
    #         sigmean = rep(valid()$sigmean, 6),
    #         sigsd = rep(valid()$sigsd, 6))
    
    animal.params <-
      list(TYPENUM = as.integer(6:(6+nrow(set.animal.params())-1)),    # start IDs at arbritary number
           TYPECHAR = row.names(set.animal.params()),
           g0mean = set.animal.params()$`Mean g0`,
           g0sd = set.animal.params()$`Stdev g0`,
           sigmean = set.animal.params()$`Mean sigma`,
           sigsd = set.animal.params()$`Stdev sigma`)
    
    print(animal.params)

    for(i in seq_along(animal.params$TYPENUM)){
      print(i)
      animals$addAnimal(animal = animal.params$TYPENUM[i],
                        name = animal.params$TYPECHAR[i],
                        detect = params$DETECT_ANIMAL)
    }

    myParams = params$POAParameters(animals)

    ## USE MULTIPLE ZONES
    myParams$setMultipleZones(TRUE)

    for(i in seq_along(animal.params$TYPENUM)){

      TYPE <- as.integer(animal.params$TYPENUM[i])
      myParams$setCapture(TYPE, animal.params$g0mean[i], animal.params$g0sd[i])
      myParams$setSigma(TYPE, animal.params$sigmean[i], animal.params$sigsd[i])
      myParams$addRRBufferAnimal(animalCode = TYPE)
    }


    # number of iterations
    myParams$setNumIterations(as.integer(valid()$setNumIterations))
    #    myParams.setNumChewcardTraps(3)
    myParams$setRRTrapDistance(as.integer(valid()$setRRTrapDistance))

    myParams$setYears(as.integer(1), as.integer(valid()$setYears))

    ## THE startPu WILL NOT BE USED IF USE zoneData FILE - TURN OFF
    # starting Pu (GRID CELL PREVALENCE) and period rate of Pu increase
    startPu = as.double(valid()$startPu)

    ## SET THE RATE OF INCREASE OF PU
    PuIncreaseRate = as.double(0.0)
    myParams$setPu(startPu, PuIncreaseRate)

    # minimum RR value
    myParams$setMinRR(as.double(input$setMinRR))

    myParams$setPrior(valid()$prior_min, valid()$prior_mode, valid()$prior_max)
    myParams$setIntro(0.00001, 0.00002, 0.00003)        # (min, mode, max)


    # create rawdata using preProcessing.RawData() ----------------------------
    
    # debugonce(RawData_R)
    rawdata <- 
      RawData_R(zonesShapeFName = paths$zonesShapeFName,
              relativeRiskFName = paths$relativeRiskFName,
              zonesOutFName = ".tmp/output/zones.tif", #defaults$zonesOutFName$value, # "app\\www\\poa\\Kaitake\\Results\\Model_0\\zones.tif",
              relRiskRasterOutFName = ".tmp/output/relRiskRaster.tif", # "app\\www\\poa\\Kaitake\\Results\\Model_0\\relRiskRaster.tif",
              resolution = as.double(valid()$resolution),
              epsg = as.integer(input$epsg),
              surveyFName = paths$surveyFName,
              params = myParams, 
              gridSurveyFname = NULL)

    result <- calculation$calcProofOfAbsence(myParams, rawdata$survey,
                                               rawdata$RelRiskExtent, rawdata$zoneArray, rawdata$zoneCodes,
                                               rawdata$match_geotrans, rawdata$wkt, "test_data/Results",
                                               rawdata$RR_zone, rawdata$Pu_zone, rawdata$Name_zone)

    # browser()    # <- break into reactive object
    
    SeU.rast <- zone.rast <- raster(".tmp/output/zones.tif")
    raster::values(SeU.rast) <- py_to_r(result$sensitivityList)[[1]]
    raster::values(SeU.rast)[raster::values(SeU.rast) == 0L] <- NA
    SeU.rast <- mask(SeU.rast, zone.rast, maskvalue=0)
    
    writeRaster(SeU.rast, ".tmp/output/meanSeuAllYears.tif", overwrite = TRUE)
    
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
  

  # server: PoA density plot ------------------------------------------------

  output$PoAdensplot <- renderPlot({
    
    res <- pyPOA()
    # browser()
    PoFmat <- py_to_r(res$poFMatrix)
    row.names(PoFmat) <- paste("Session", 1:nrow(PoFmat))
    prior <- py_to_r(res$priorStore)
    PoFmat <- rbind(prior, PoFmat)
    
    lattice::densityplot(~val, group = d1, arr2df(PoFmat), auto.key = T, 
                xlab = "Probability of absence", ylab = "density")
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
    
    paths.ls <- unlist(reactiveValuesToList(paths))
    
    # print(lapply(input.ls, as.character))

    inputs.df <- data.frame(Description = c(names(input.ls), names(paths.ls)),
                            Value = c(input.ls, paths.ls))
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
