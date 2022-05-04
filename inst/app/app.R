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

library(proofofabsence)

# devtools::install_github(repo = "eradicate-dev/poaShiny", ref = "convert_to_R_package")

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
       startPu = list(desc = "", label = "startPu", value = 1),
       summaryCIs = list(desc = "", label = "Summary table credible intervals", value = 0.95))


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
    selectInput(inputId = "namedExample", label = "Select example set", choices = c("None", "Kaitake possums", "Mahia possums", "CK stoats", "Nutria"), multiple = FALSE),
    conditionalPanel(condition = "input.namedExample == 'None'",{
      list(fileInput(inputId = "zonesShapeFName", label = "zonesShapeFName", multiple = TRUE),
           fileInput(inputId = "surveyFName", label = "surveyFName", multiple = FALSE),
           fileInput(inputId = "relativeRiskFName", label = "relativeRiskFName", multiple = FALSE),
           fileInput(inputId = "gridSurveyFname", label = "gridSurveyFname", multiple = TRUE))
    }),
    radioButtons(inputId = "useMultiZone", label = "Use single or multiple zones?", 
                 choices = c("Single zone", "Multiple zones"), selected = "Multiple zones"),
    numericInput(inputId = "setMinRR", label = defaults$setMinRR$label, value = defaults$setMinRR$value, min = 0, max = 1000),
    numericInput(inputId = "epsg", label = defaults$epsg$label, value = defaults$epsg$value)
  ),
  h4("Device parameters - double-click to adjust"),
  DT::DTOutput(outputId = "deviceUI"),
  DT::DTOutput(outputId = "gridUI")
)

## UI: priors ----
ui.inputs.priors <- 
  list(
    div(title = "Values must be between 0 and 1 and min < mode < max",
        wellPanel(
          h5("Set parameters for prior distribution"),
          fluidRow(
            column(numericInput(inputId = "prior_min", label = "Prior min", value = defaults$prior_min$value, min = 0, max = 1), width = 4),
            column(numericInput(inputId = "prior_mode", label = "Prior mode", value = defaults$prior_mode$value, min = 0, max = 1), width = 4),
            column(numericInput(inputId = "prior_max", label = "Prior max", value = defaults$prior_max$value, min = 0, max = 1), width = 4)
          )
        )
    )
  )
  
## UI: introduction prob ----
ui.set.intro <-
  list(
    div(title = "Values must be between 0 and 1 and min < mode < max",
        wellPanel(
          h5("Set parameters for distribution of re-introduction probability"),
          fluidRow(
            column(numericInput(inputId = "intro_min", label = "Minimum", value = 0.00001), width = 4),
            column(numericInput(inputId = "intro_mode", label = "Mode", value = 0.00002), width = 4),
            column(numericInput(inputId = "intro_max", label = "Maximum", value = 0.00003), width = 4)
          )
        )
    )
  )

ui.inputs.yrs <- 
  fluidRow(column(numericInput(inputId = "startYear", label = "Start year", value = 1), width = 6),
           column(numericInput(inputId = "endYear", label = "End year", value = 1), width = 6))

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
               numericInput(inputId = "startPu", label = defaults$startPu$label, value = defaults$startPu$value),
               numericInput(inputId = "summaryCIs", label = defaults$summaryCIs$label, value = defaults$summaryCIs$value)
             )))


# UI: output --------------------------------------------------------------

ui.output <- 
  # list(
    # verbatimTextOutput(outputId = "table2"),
    fluidRow(column(width = 6, 
    list(h3("Probability of absence")),
    div(title = "Adjust credible from default (0.95) under the 'Advanced inputs' tab.", h4("Summary table")),
    tableOutput("POAsummary"),
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
    h3("----------DEBUGGING-----------"),
    checkboxInput(inputId = "renderPts", label = "render map points", value = FALSE),
    checkboxInput(inputId = "renderRasts", label = "render map rasters", value = TRUE),
    h3("inputTable"),
    htmlOutput("inputTable"))
  )


# UI: layout page ---------------------------------------------------------
ui <- fluidPage(
  tabsetPanel(
    tabPanel(title = "Run model", 
             # splitLayout(list(ui.inputs, ui.inputs.priors), ui.output)
             fluidRow(column(4, list(ui.inputs, ui.inputs.priors, ui.set.intro, ui.inputs.yrs,
                                     actionButton(inputId = "runpy", 
                                                  label = "Calculate PoA")
             )), 
             column(8, list(ui.output)))
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
    
  } else if(!is.null(reticulate::conda_binary())){ 
    
    # check if conda available and use 'proofofabsence' environment
    reticulate::use_condaenv(condaenv = "proofofabsence", required = TRUE)
    print(reticulate::py_config())
    
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
                          surveyFName = NULL,
                          gridSurveyFname = NULL)
  
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
  
  

  # server: copy example files to temp folder ---------------------------------
  
  # - copy device folders
  observe({
    if(input$namedExample == "Kaitake possums"){
      paths$zonesShapeFName <- system.file("example_data/Kaitake_possums/extent.shp", package = "proofofabsence")
      paths$relativeRiskFName <- system.file("example_data/Kaitake_possums/relRiskRaster.tif", package = "proofofabsence")
      paths$surveyFName <- system.file("example_data/Kaitake_possums/devices.csv", package = "proofofabsence")
    } else if(input$namedExample == "Mahia possums"){
      paths$zonesShapeFName <- system.file("example_data/Mahia_Audrey/extent_block1ABCD.shp", package = "proofofabsence")
      paths$relativeRiskFName <- system.file("example_data/Mahia_Audrey/habDistRR_block1ABCD.tif", package = "proofofabsence")
      paths$surveyFName <- system.file("example_data/Mahia_Audrey/Surveillance_location.csv", package = "proofofabsence")
    } else if(input$namedExample == "CK stoats"){
      paths$zonesShapeFName <- system.file("example_data/CK_stoats/extent.shp", package = "proofofabsence")
      paths$relativeRiskFName <- system.file("example_data/CK_stoats/relRisk.tif", package = "proofofabsence")
      paths$surveyFName <- system.file("example_data/CK_stoats/devices.csv", package = "proofofabsence")
    } else if(input$namedExample == "Nutria"){
      paths$zonesShapeFName <- system.file("example_data/Nutria/mngtZone_LowPu.shp", package = "proofofabsence")
      paths$surveyFName <- system.file("example_data/Nutria/AllNutriaSurvey_2024.csv", package = "proofofabsence", mustWork = TRUE)
      updateNumericInput(inputId = "epsg", value = 26918)
      
      # copy grid surveillance files to temporary folder
      #  - csv gets rewritten when selecting start and end years
      #  - unless the csv is copied the R system csvs are overwritten and the
      #    changes becomes permanent
      publicNutria <- system.file("example_data/Nutria/publicNutria.img", package = "proofofabsence")
      tmp.publicNutria <- file.path(tempdir(), basename(publicNutria))
      gridSurveyFname <- system.file("example_data/Nutria/gridPublicSur7.csv", package = "proofofabsence")
      tmp.gridSurveyFname <- file.path(tempdir(), basename(gridSurveyFname))
      file.copy(publicNutria, tmp.publicNutria)
      file.copy(gridSurveyFname, tmp.gridSurveyFname)
      paths$gridSurveyFname <- normalizePath(tmp.gridSurveyFname)
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
      devs <- read.csv(paths$surveyFName, stringsAsFactors = FALSE)
      # get number device types and sessions
      ntypes <- length(unique(devs$Species))
      nsession <- length(unique(devs$Year))
      
      # message console when loaded
      message("device file loaded: ", nrow(devs), " rows; ",
              ntypes, " device types; ",
              nsession, " sessions detected")
      
      # devs.4326 <- st_transform(devs, crs = 4326)
      
      # set devices reactive object
      devices(devs)
    }
    
  })

  # server: load & format grids -------------------------------------------
  gridinfo <- reactiveVal(NULL)

  observe({
    # if(!is.null(input$gridSurveyFname) & input$namedExample == "None"){
    #   paths$gridSurveyFname <- normalizePath(input$gridSurveyFname$datapath)
    # }
    input$gridSurveyFname
    
    if(!is.null(input$gridSurveyFname) && file.exists(input$gridSurveyFname$datapath)){
      
      # get paths for multiple uploaded files
      gridpaths <- input$gridSurveyFname$datapath
      gridnames <- input$gridSurveyFname$name
      # find csv file with grid info
      infopath <- gridpaths[grepl("\\.csv$", gridpaths)]
      infoname <- gridnames[grepl("\\.csv$", gridpaths)]
      # read csv
      gridcsv <- read.csv(infopath)
      # check for required rasters in grid info csv
      if(!all(gridcsv$gridName %in% gridnames)){
        stop("raster file '", setdiff(gridcsv$gridName, "a"), 
             "' specified in uploaded ", infoname, 
             " is missing. Make sure both grid information and grid raster files are uploaded.")
      }
      
      # fileInput drops file names renbame files back to original
      newpaths <- file.path(dirname(gridpaths), gridnames)
      file.rename(gridpaths, newpaths)
      
      # update reactives with new grid survey path
      newinfopath <- newpaths[basename(newpaths) == infoname]
      # update paths reactive object with path to renamed grid survey file
      paths$gridSurveyFname <- normalizePath(newinfopath)
      
    }
    
    # update reactive gridinfo object
    if(!is.null(paths$gridSurveyFname)){
      gridinfo(read.csv(paths$gridSurveyFname))
    }
    
  })
  

  # server: update start and end years ---------------------------------------

  observe({
    # get years from devices & grids
    deviceyrs <- devices()$Year
    gridyrs <- gridinfo()$year
    # combine years and update inputs
    allyrs <- c(deviceyrs, gridyrs)
    if(!is.null(allyrs) && is.numeric(allyrs)){
      updateNumericInput(session, inputId = "startYear", 
                         value = min(allyrs))
      updateNumericInput(session, inputId = "endYear", 
                         value = max(allyrs))
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
    
    req(input$renderPts)
    req(paths$zonesShapeFName)
    req(devices())
    
    devs_orig <- st_sf(st_as_sf(devices(), coords = c("Easting", "Northing")), crs = input$epsg)
    devs <- st_transform(devs_orig, crs = 4326)
    # set color palette
    pal.device <- colorFactor(palette = "Set2", domain = unique(devs$Species))
    
    leafletProxy(session = session, mapId = "baseMap") %>%
      # leaflet() %>%
      # addLayersControl(overlayGroups = c("Devices"), options = layersControlOptions(collapsed = FALSE)) %>% 
      clearGroup("Devices") %>% 
      addCircles(data = devs, group = "Devices",
                 radius = 20, weight = 1, opacity = 1, fillOpacity = 1, color = ~pal.device(devs$Species)) %>%
      addLegend(layerId = "Devices", pal = pal.device, values = unique(devs$Species), group = "Devices") 

  })
  
  # server: render and update device parameters -----------------------------
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
  
  # server: render and update grid parameters -------------------------------

  set.grid.params <- reactiveVal(NULL)
  observe({
    if(!is.null(gridinfo())){
      gridinfo()
      set.grid.params(gridinfo())
    }
  })
    
  gridUI <- reactive({
    # browser()
    dtOut <- DT::datatable(set.grid.params(), 
                           editable = TRUE, 
                           options = list(ordering=F, searching = F, paging = F))
    return(dtOut)
  })
  
  output$gridUI <- DT::renderDT(gridUI())
  
  observe({
    if(!is.null(input$gridUI_cell_edit)){
      # browser()
      print(set.grid.params())
      print(input$gridUI_cell_edit)
      set.grid.params(DT::editData(data = set.grid.params(), 
                                   info = input$gridUI_cell_edit))
    }
  })
  
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
    
    # set to use single zone if a single feature shape file loaded
    if(!is.null(zonesShape())) if(nrow(zonesShape()) == 1){
      updateRadioButtons(session = session, 
                         inputId = "useMultiZone", 
                         selected = "Single zone", choices = "Single zone")
    } else {
      updateRadioButtons(session = session, 
                         inputId = "useMultiZone", 
                         selected = "Multiple zones", 
                         choices = c("Single zone", "Multiple zones"))
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
    req(input$relativeRiskFName)

    # rename to original name
    newpath <- 
      file.path(dirname(input$relativeRiskFName$datapath), 
                input$relativeRiskFName$name)
    renameOK <- 
      file.rename(normalizePath(input$relativeRiskFName$datapath, mustWork = TRUE), 
                  normalizePath(newpath, mustWork = FALSE))
    # update paths reactive object with path to renamed relativeRisk
    if(renameOK){
      paths$relativeRiskFName <- newpath
    }
  })
  
  # load relative risk map given in paths$relativeRiskFName
  observe({   
    req(paths$relativeRiskFName)
    req(file.exists(paths$relativeRiskFName))
    
    # load relative risk map given in paths$relativeRiskFName
    message(paste("loading relative risk .tif file:", paths$relativeRiskFName))
    r <- terra::rast(paths$relativeRiskFName)
    terra::crs(r) <- paste0("EPSG:", input$epsg)
    relRiskRaster(r)
    rm(r)
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
    
    req(relRiskRaster(), input$renderRasts)
      
    withProgress(
      message = 'Reprojecting raster', detail = 'This may take a while...',
      value = 0, 
      {
        
        # get layer and convert to epsg:3857
        RRmap.3857 <- terra::project(relRiskRaster(), "epsg:3857")
        
        incProgress(amount = 0.5, message = 'Mapping raster')
        
        # get bounds
        # - bounds from pseudo-mercator (epsg:3857) dont't work with fitBounds below
        # - converting extent to WGS84 and storing bounds
        e <- terra::ext(relRiskRaster())
        re <- terra::rast(e, crs = crs(relRiskRaster()))
        re_4326 <- terra::project(re, "epsg:4326")
        bb <- as.vector(terra::ext(re_4326))
        
        # set values above MinRR to NA
        RRmap.3857 <- terra::clamp(RRmap.3857, lower = input$setMinRR, values = FALSE)
        valrng <- c(input$setMinRR, max(terra::values(RRmap.3857), na.rm = TRUE))
        
        # make palette
        pal <- colorNumeric(palette = "viridis",
                            domain = valrng,
                            na.color = "transparent")
        
        # update base map
        leafletProxy(session = session, mapId = "baseMap") %>%
          clearGroup(group = "Relative risk") %>%
          addRasterImage(x = raster::raster(RRmap.3857), 
                         group = "Relative risk",
                         opacity = 0.95, colors = pal, 
                         project = FALSE) %>%
          fitBounds(lng1 = bb[["xmin"]], lat1 = bb[["ymin"]], 
                   lng2 = bb[["xmax"]], lat2 = bb[["ymax"]])

      })
    
    # add legend if relative risk values vary 
    if(diff(valrng) > 0){
      leafletProxy(session = session, mapId = "baseMap") %>%  
        addLegend(layerId = "RRlegend", pal = pal, values = valrng, bins = 5,
                  title = "Relative risk",
                  group = "Relative risk")
    }
  })
  
    
  # server: run py code -----------------------------------------------------
  pyPOA <- eventReactive(input$runpy, {
    
    print("runpy press detected")
    
    proofofabsence::poa_paks(modules = "minimal")
    
    myParams <- proofofabsence::makeParams(setMultipleZones = input$useMultiZone %in% "Multiple zones",
                                           setNumIterations = input$setNumIterations,
                                           setRRTrapDistance = input$setRRTrapDistance,
                                           startYear = input$startYear, endYear = input$endYear,
                                           startPu = input$startPu, PuIncreaseRate = 0.0,
                                           setMinRR = input$setMinRR,
                                           setPrior = c(input$prior_min,input$prior_mode,input$prior_max),
                                           setIntro = c(input$intro_min,input$intro_mode,input$intro_max))
    
    myParams <- proofofabsence::addAnimalParams(myParams,
                                                deviceName = row.names(set.animal.params()),
                                                g0 = set.animal.params()$`Mean g0`,
                                                g0sd = set.animal.params()$`Stdev g0`,
                                                sig = set.animal.params()$`Mean sigma`, 
                                                sigsd = set.animal.params()$`Stdev sigma`)
    
    
    # create rawdata using preProcessing.RawData() ----------------------------
    
    dir.create(path = ".tmp/input", recursive = TRUE, showWarnings = FALSE)
    dir.create(path = ".tmp/output", recursive = TRUE, showWarnings = FALSE)
    
    # set temp files for zonesOutFName and relRiskRasterOutFName
    tmp.zonesOutFName <- tempfile(fileext = ".tif")
    tmp.relRiskRasterOutFName <- tempfile(fileext = ".tif")
    # write out modified (or not) grid parameters to temporary csv file
    if(!is.null(set.grid.params())){
      ind <- set.grid.params()$year %in% seq.int(input$startYear, input$endYear)
      write.csv(set.grid.params()[ind,], paths$gridSurveyFname, row.names = FALSE, quote = FALSE)
    }    
    
    # create poa.RawData
    rawdata <- 
      proofofabsence::RawData_R(zonesShapeFName = paths$zonesShapeFName,
                                relativeRiskFName = paths$relativeRiskFName,
                                zonesOutFName = tmp.zonesOutFName,
                                relRiskRasterOutFName = tmp.relRiskRasterOutFName,
                                resolution = as.double(valid()$resolution),
                                epsg = as.integer(input$epsg),
                                surveyFName = paths$surveyFName,
                                params = myParams, 
                                gridSurveyFname = paths$gridSurveyFname)
    
    # check if grid survey components are available as numpy.ndarray
    grids_available <- 
      sapply(rawdata[c("gridSurveyYears", "gridSurveyData", "gridSurveyMeans", "gridSurveySD", "gridSurveyCodes")], 
             function(x) "numpy.ndarray" %in% class(x))
    # if all available set useGrids and add grid surveys to poa.params object
    useGrids <- all(grids_available)
    if(useGrids){
      myParams$setGridSurvey(rawdata$gridSurveyYears, rawdata$gridSurveyData,
                             rawdata$gridSurveyMeans, rawdata$gridSurveySD, rawdata$gridSurveyCodes)
    }
    
    # use temporary output data path
    outputDataPath <- tempdir(check = TRUE)
    
    # calculate PoA using poa.calculation.calcProofOfAbsence
    result <- poa$calculation$calcProofOfAbsence(myParams, rawdata$survey,
                                                 rawdata$RelRiskExtent, rawdata$zoneArray, rawdata$zoneCodes,
                                                 rawdata$match_geotrans, rawdata$wkt, outputDataPath,
                                                 rawdata$RR_zone, rawdata$Pu_zone, rawdata$Name_zone)

    return(list(rawdata = rawdata,
                result = result))

  })
  
  source("array_functions.R")
  
  
  
  
  
  # observe({try(print(pyPOA()$poFMatrix))})


  # server: summary table ---------------------------------------------------
  output$POAsummary <- renderTable({
    
    # get credible interval from advanced input
    credint <- input$summaryCIs
    lowint <- (1-credint)/2
    uppint <- 1 - lowint
    
    # get results
    result <- pyPOA()$result
    
    # Prior
    prior <- py_to_r(result$priorStore)
    prior_mean <- mean(prior)
    prior_low <- quantile(prior, probs = lowint)
    prior_upp <- quantile(prior, probs = uppint)
    prior_string <- sprintf("%0.3f (%0.3f, %0.3f)", prior_mean, prior_low, prior_upp)
    
    # SSe
    SSe_mat <- py_to_r(result$sensitivityMatrix)
    SSe_mean <- rowMeans(SSe_mat)
    SSe_low <- apply(SSe_mat, 1, quantile, probs = lowint)
    SSe_upp <- apply(SSe_mat, 1, quantile, probs = uppint)
    SSe_string <- sprintf("%0.3f (%0.3f, %0.3f)", SSe_mean, SSe_low, SSe_upp)
    
    # PoF
    PoFmat <- py_to_r(result$poFMatrix)
    poa_mean <- rowMeans(PoFmat)
    poa_low <- apply(PoFmat, 1, quantile, lowint)
    poa_upp <- apply(PoFmat, 1, quantile, uppint)
    poa_string <- sprintf("%0.3f (%0.3f, %0.3f)", poa_mean, poa_low, poa_upp)
    
    years <- py_to_r(result$params$years)
    # py_to_r(result$sensitivityList)
    
    formatted_df <- 
      rbind(data.frame(Output = "Prior", Session = NA, 
                       Value = prior_string),
            data.frame(Output = c("SSe", rep("", length(SSe_string) - 1)), 
                       Session = sprintf("%d", years), Value = SSe_string),
            data.frame(Output = c("PoF", rep("", length(poa_string) - 1)), 
                       Session = sprintf("%d", years), Value = poa_string)) }, na = "")
  
  
  # server: PoF plot --------------------------------------------------------
  # observe({try(print(py_to_r(pyPOA()$poFMatrix)))})
  
  output$PoAtimeplot <- renderPlot({
    result <- pyPOA()$result
    PoFmat <- py_to_r(result$poFMatrix)
    poa_mean <- rowMeans(PoFmat)
    poa_low <- apply(PoFmat, 1, quantile, 0.05)
    poa_upp <- apply(PoFmat, 1, quantile, 0.95)
    years <- py_to_r(result$params$years)
    prior <- py_to_r(result$priorStore)
    prior_mean <- mean(prior)
    prior_low <- quantile(prior, 0.05)
    prior_upp <- quantile(prior, 0.95)
    ploty <- c(prior_mean, poa_mean)
    plotx <- c(min(years)-1, years)
    plot(y = ploty, x = plotx, ylim = c(0, 1), type = "b", 
         xlab = "Session", ylab = "Probability of absence", axes = FALSE, frame.plot = TRUE)
    axis(side = 1, at = plotx, labels = c("Prior", years))
    axis(2, at = seq(0,1,0.2))
    arrows(y0 = c(prior_low, poa_low), x0 = plotx, 
           y1 = c(prior_upp, poa_upp), x1 = plotx, code = 3, angle = 90)
  })
  

  # server: PoA density plot ------------------------------------------------

  output$PoAdensplot <- renderPlot({
    
    res <- pyPOA()$result
    # browser()
    PoFmat <- py_to_r(res$poFMatrix)
    row.names(PoFmat) <- paste("Session", 1:nrow(PoFmat))
    prior <- py_to_r(res$priorStore)
    PoFmat <- rbind(prior, PoFmat)
    
    lattice::densityplot(~val, group = d1, arr2df(PoFmat), auto.key = T, 
                xlab = "Probability of absence", ylab = "density")
  })


    
  # server: add meanSeU raster ----------------------------------------------
  observe({
    
    req(pyPOA())
    
    # get sensitivityList from calcProofOfAbsence result
    sensitivityList <- py_to_r(pyPOA()$result$sensitivityList)
    # get zone tif file path
    extZoneTifName <- pyPOA()$result$extZoneTifName
    
    # read zone raster as template
    rtemp <- terra::rast(pyPOA()$rawdata$zonesOutFName)
    
    # replace template values with cell mean SeU and replace zeroes with NA
    rlist <- lapply(sensitivityList, function(vals){
      r <- rtemp
      vals[vals == 0L] <- NA
      values(r) <- vals
      r
    })
    
    # stack rasters into layers
    meanSeu <- do.call("c", rlist)
    
    # terra::writeRaster(x = meanSeu, filename = "meanSeu_usingGDAL.tif")
    
    # project to pseudo-WGS84 using terra package
    meanSeu_project <- terra::project(meanSeu, "epsg:3857")
    # convert back to raster for use in leaflet
    meanSeu <- raster::raster(meanSeu_project)
    
    # set palette
    pal <- colorNumeric(palette = "viridis", domain = c(0,1.1),  # add 0.1 to upper limit
                        na.color = "transparent")
   
    # add to baseMap
    leafletProxy(session = session, mapId = "baseMap") %>%
      clearGroup(group = "SeU") %>%
      addRasterImage(x = meanSeu, layerId = "SeU", group = "SeU",
                     opacity = input$rasterOpacity, colors = pal, 
                     project = FALSE) %>% 
      addLegend(pal = pal, values = c(0,1), labels = c(0,1), layerId = "SeU")
    
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
