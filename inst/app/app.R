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


# R packages --------------------------------------------------------------

# devtools::install_github("rstudio/reticulate")
library(raster)

library(terra)

library(kableExtra)

library(leaflet)

library(sf)

library(shiny)

library(DT)

library(proofofabsence)

# install proofofabsence package from source before deploying to shinyapps.io
# devtools::install_github(repo = "eradicate-dev/poaShiny", ref = "master")
# devtools::install_github(repo = "eradicate-dev/poaShiny", ref = "app_improvements")

# print loaded package versions
loaded.paks <- as.character(.packages())
loaded.paks.vers <- sapply(sapply(loaded.paks, utils::packageVersion, simplify = F), as.character)
loaded.paks.vers <- mapply(paste, names(loaded.paks.vers), loaded.paks.vers, sep = ":", SIMPLIFY = F)
message(paste(loaded.paks.vers, collapse = "\n"))

# set shiny options -------------------------------------------------------

options(shiny.maxRequestSize=100*1024^2)

# UI: input defaults ------------------------------------------------------
defaults <- 
  list(zonesShapeFName = list(desc = paste0("Upload a shapefile by clicking the 'Browse' button and ",
                                            "selecting the shapefile's .shp, .shx, .prj and .dbf files. ",
                                            "To select multiple files hold down the Shift or Ctrl keys ",
                                            "while clicking the files"),
                              label = "Upload shapefiles", 
                              placeholder = "Select .shp, .shx, .prj and .dbf files"),
       relativeRiskFName = list(desc = "", label = "", value = "app\\www\\poa\\Kaitake\\Data\\relRiskRaster.tif"),
       zonesOutFName = list(desc = "", label = "", value = "app\\www\\poa\\Kaitake\\Results\\Model_0\\zones.tif"),
       relRiskRasterOutFName = list(desc = "", label = "", value = "app\\www\\poa\\Kaitake\\Results\\Model_0\\relRiskRaster.tif"),
         resolution = list(desc = "", label = "Spatial unit resolution (m)", value = as.double(50)),
         epsg = list(desc = "", label = "EPSG code", value = NULL),
         surveyFName = list(desc = "", label = "surveyFName", value = "app\\www\\poa\\Kaitake\\Data\\devices.csv"),
         gridSurveyFname = list(desc = "", label = "gridSurveyFname", value = NULL),
         prior_min = list(desc = "", label = "Prior min", value = 0.2),
         prior_mode = list(desc = "", label = "Prior mode", value = 0.4),
         prior_max = list(desc = "", label = "Prior max", value = 0.6),
         setNumIterations = list(desc = "", label = "Number of iterations", value = 2),
         setRRTrapDistance = list(desc = "", label = "Relative risk buffer", value = 100),
         setMinRR = list(desc = "", label = "Minimum relative risk value", value = 1),
         startPu = list(desc = "", label = "startPu", value = 1.00),
         PuIncreaseRate = list(desc = "", label = "PuIncreaseRate", value = 0.00))


# UI: input ---------------------------------------------------------------

## UI: troubleshooting ----
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

## UI: MWLR logo and title ----
ui.logotitle <-
  fluidRow(style = "height:75px",
    column(width = 6, 
           # Application title
           titlePanel("Proof-of-absence calculator")),
    column(width = 6, 
           div(style="height:75px",
             img(src="MW_LR_Landscape_lge_blk.png", align="right", hspace=20,vspace=10, style = "height:100%"),
             img(src="ciss_logo.jpg", align="right", hspace=20,vspace=10, style = "height:100%"),
             img(src="ari_logo.jpg", align="right", hspace=20,vspace=10, style = "height:100%")),
             style = "padding-left:35px; padding-right:35px;"))

## UI: about ----
ui.about <-
  list(br(),
       wellPanel(style = "padding:10px",
         h4("About"),
         h6(paste0("Version ", packageVersion("proofofabsence"), " (", packageDate("proofofabsence"), ")")),
         h6("Developed by Manaaki Whenua - Landcare Research."),
         h6("email: ", tags$a(href = "https://www.landcareresearch.co.nz/about-us/our-people/simon-howard", target="_blank", "Simon Howard"))
       ))

## UI: file uploads ----
ui.file.uploads <- 
  wellPanel(
    # selectInput(inputId = "namedExample", label = "Select example set", choices = c("None", "Kaitake possums", "Mahia possums", "CK stoats", "Nutria"), multiple = FALSE),
    # conditionalPanel(condition = "input.namedExample == 'None'",{
      list(
        div(title = defaults$zonesShapeFName$desc, 
            fileInput(inputId = "zonesShapeFName", 
                      label = "Area of interest shapefiles", 
                      multiple = TRUE, placeholder = defaults$zonesShapeFName$placeholder,
                      accept = ".SHP,.SHX,.DBF,.PRJ")),
        splitLayout(
          div(title = paste0("If the uploaded shape file contains multiple polygons, ",
                             "or zones, then they can be treated separately, with differing risk ",
                             "and design prevalence values. Select 'Multiple zones' ",
                             "to treat zones separately, or 'Single zone' to apply the ",
                             "same risk and prevalence to all zones."), 
              radioButtons(inputId = "useMultiZone", label = "Use single or multiple zones?", 
                           choices = c("Single zone", "Multiple zones"), selected = "Single zone",
                           inline = TRUE)),
          div(title = paste0("The EPSG code is detected automatically from information stored ",
                             "in the uploaded shapefile. The coordinate reference system associated ",
                             "with the  shape file is used for all other uploaded data. If incorrect, ",
                             "or not automatically detected, the code can be manually set here."),
          numericInput(inputId = "epsg", label = "EPSG code", value = defaults$epsg$value))),
        div(title = paste0(""),
            fileInput(inputId = "surveyFName", label = "Point surveillance file", 
                      accept = ".csv", placeholder = "Select .csv file", multiple = FALSE)),
        fileInput(inputId = "gridSurveyFname", label = "Grid surveillance files", 
                  accept = ".img,.tif,.tiff,.csv", placeholder = "Select .csv and associated .tif or .img raster files",
                  multiple = TRUE),
        fileInput(inputId = "relativeRiskFName", label = "Spatial relative risk raster", 
                  accept = ".img,.tif,.tiff", placeholder = "Select relative risk .tif or .img raster file",
                  multiple = FALSE),
        conditionalPanel(condition = "output.RRloaded", {
          splitLayout(
            numericInput(inputId = "setMinRR", label = defaults$setMinRR$label, 
                         value = defaults$setMinRR$value, min = 0, max = 1000),
            numericInput(inputId = "setRRTrapDistance", label = defaults$setRRTrapDistance$label, 
                         value = defaults$setRRTrapDistance$value)
          )
        })
      )
    # })  
  )

## UI: detection parameters ----
ui.survparams <- 
  wellPanel(
    h4("Detection parameters"),
    em("Double-click table cells to enter values. Use tab to move to next cell and Ctrl + Enter to confirm entries."),
    hr(),
    tabsetPanel(tabPanel(title = "Points",
                         DT::DTOutput(outputId = "deviceUI"),
                         hr(),
                         p(em("Enter the 1st row of sigma values and click the button below to fill down rows")),
                         actionButton(inputId = "filldownsigma", label = "Fill down sigma values")),
                tabPanel(title = "Grids",
                         DT::DTOutput(outputId = "gridUI"))),
  )

## UI: priors ----
ui.inputs.priors <- 
  wellPanel(
    div(title = "Values must be between 0 and 1 and min < mode < max",
        h4("Prior probability of eradication"),
        hr(),
        splitLayout(
          numericInput(inputId = "prior_min", label = "Prior min", value = 0.00001, min = 0, max = 1),
          numericInput(inputId = "prior_mode", label = "Prior mode", value = 0.00002, min = 0, max = 1),
          numericInput(inputId = "prior_max", label = "Prior max", value = 0.00003, min = 0, max = 1)
        )
    )
  )

## UI: introduction prob ----
ui.set.intro <-
  wellPanel(
    div(title = "Values must be between 0 and 1 and min < mode < max",
        h4("Re-introduction probability"),
        hr(),
        splitLayout(
          numericInput(inputId = "intro_min", label = "Minimum", value = 0.00001),
          numericInput(inputId = "intro_mode", label = "Mode", value = 0.00002),
          numericInput(inputId = "intro_max", label = "Maximum", value = 0.00003)
        )
    )
  )


# UI: simulation parameters ----
ui.simparams <- 
  wellPanel(
    h4("Simulation parameters"),
    hr(),
    splitLayout(numericInput(inputId = "startYear", label = "Start year", value = 1),
                numericInput(inputId = "endYear", label = "End year", value = 1)),
    numericInput(inputId = "setNumIterations", label = defaults$setNumIterations$label, value = defaults$setNumIterations$value, width = '50%'),
    numericInput(inputId = "resolution", label = defaults$resolution$label, value = defaults$resolution$value, width = '50%'),
    splitLayout(numericInput(inputId = "startPu", label = "Design prevalence (Pu)", value = defaults$startPu$value),
    numericInput(inputId = "PuIncreaseRate", label = "Prevalence increase rate", value = defaults$PuIncreaseRate$value))
  )

# UI: output --------------------------------------------------------------

ui.output <- 
  fluidRow(column(width=10,
                  leafletOutput("baseMap", height = '550px'), 
                  fluidRow(
                    column(width = 6,
                           selectInput(inputId = "selectYear", label = "Select year to display", 
                                       choices = NULL)),
                    column(width = 6,
                           selectInput(inputId = "selectDevices", label = "Select surveillance types to display", 
                                       choices = NULL, selectize = TRUE, multiple = TRUE))
                  )
                  
  )# , plotOutput("PoAdensplot", width="70%"))
  )


# UI: analytics script ----------------------------------------------------
ui.analytics <- tags$head(includeHTML("www/google-analytics.html"))

# UI: layout page ---------------------------------------------------------
ui <- fluidPage(title = "Proof-of-absence calculator",
                ui.analytics,
                ui.logotitle,
  tabsetPanel(id = "tabs",
    tabPanel(title = "Upload inputs",
             br(),
             # splitLayout(list(ui.inputs, ui.inputs.priors), ui.output)
             fluidRow(column(4, 
                             list(
                               ui.file.uploads, 
                               ui.about
             )), 
             column(8, list(ui.output)))
    ),  
    tabPanel(title = "Set parameters",
             br(),
             fluidRow(column(width = 4, ui.survparams), 
                      column(width = 4, 
                             ui.inputs.priors,
                             ui.set.intro), 
             column(width = 4, ui.simparams))
             ),
    tabPanel(title = "Run model",
             br(),
             fluidRow(
               column(width = 4, 
                      br(),
                      wellPanel(
                        actionButton(inputId = "runpoa", 
                                     label = strong("Calculate proof of absence"),
                                     width = '100%')
                      ),
                      br(),
                      wellPanel(
                        h4("Additional outputs"),
                        br(),
                        h5("Download results zip file"),
                        downloadButton("downloadData", "Download .zip file"),
                        hr(),
                        h5("Show raster cell sensitivities on map"),
                        actionButton(inputId = "renderSeU", 
                                     label = "Show on map")
                      )),
               column(width = 8,
                      h4("Proof of absence & sensitivity estimates"),
                      tabsetPanel(
                        tabPanel(title = "Results",
                                 tableOutput("SSePoFResultTable")),
                        tabPanel(title = "Plot", 
                                 h4("Probability of absence over time/session"),
                                 em("Points are means and error bands are 95% credible intervals"),
                                 plotOutput("PoAtimeplot", width="70%")),
                        tabPanel(title = "Zone results",
                                 tableOutput("zoneSeResultTable"))
                      )
               )
             )
    ),
    tabPanel(title = "Help",
             includeMarkdown("proofofabsence_help.md"))#, ui.troubleshooting
  )
)


printLine <- NULL

options(shiny.reactlog = TRUE)   # press CTRL + F3 to view react log



# testing inputs ----------------------------------------------------------
# input <- lapply(defaults, "[[", "value")


# server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  runpoapress <- reactive({input$runpoa})
  output$runpoapress <- renderPrint(runpoapress())


  # server: manage python versions and modules ------------------------------
  
  observeEvent({input$runpoa == 1L}, {
    
    # - Pre-processing steps don't require the python-dependent libraries
    # - Set up python environment after calculate button is pressed
    # - The order this observer and the event that does the calculations is
    #   controlled by setting the priority arguments in observeEvent() and
    #   reactiveEvent()
    
    req(input$runpoa == 1L)   # only run on first button press
    
    # ------------------ App virtualenv setup (Do not edit) ------------------- #
    # adapted from - 'https://github.com/ranikay/shiny-reticulate-app'
    
    # Define any Python packages needed for the app here:
    # - note: numpy, pip, wheel and setuptools are updated on shinyapps.io startup
    PYTHON_DEPENDENCIES = c("llvmlite", "numba")
    
    # check if on shiny server and install modules in 'PYTHON_DEPENDENCIES'
    if(Sys.info()[['user']] == 'shiny'){
      
      showNotification(id = "venvload", duration = NULL,
                       ui = strong("Loading python & installing dependencies ..."))
      
      virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
      python_path = Sys.getenv('PYTHON_PATH')
      
      # print python related messages to shinyapps.io logs
      message("PYTHON_PATH: \n", python_path)
      
      # Create virtual env and install dependencies
      venv_path <- reticulate::virtualenv_create(envname = virtualenv_dir)
      message("reticulate::virtualenv_create() COMPLETE - environment created in ", venv_path)
      reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES)
      reticulate::use_virtualenv(virtualenv_dir, required = T)
      
      removeNotification(id = "venvload")
      
    } else if(!is.null(reticulate::conda_binary())){ 
      
      showNotification(id = "condaload",  duration = NULL,
                       ui = strong("Loading python ..."))
      
      # check if conda available and use 'proofofabsence' environment
      reticulate::use_condaenv(condaenv = "proofofabsence", required = TRUE)
      print(reticulate::py_config())
      
      removeNotification(id = "condaload")
      
    } else {
      # use python on system path
      if(Sys.getenv("PYTHON_PATH") != "") use_python(Sys.getenv("PYTHON_PATH"))
    }
    # ------------------ App server logic (Edit anything below) --------------- #
    
    # python modules
    poa <<-
      reticulate::import_from_path(
        path = system.file("python", package = "proofofabsence"),
        module = "proofofabsence_min",
        convert = FALSE, delay_load = FALSE)
    
    cachedir <- system.file("python/proofofabsence_min/__pycache__", package = "proofofabsence")
    if(dir.exists(cachedir)) message("cached proofofabsence module functions found:\n", cachedir)
    
    np <<- reticulate::import(module = "numpy", convert = FALSE, delay_load = FALSE)
    bi <<- reticulate::import_builtins(convert = FALSE)
    
  }, ignoreInit = TRUE, priority = 2)
  
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
                     startPu = input$startPu,
                     sigmean = input$sigmean,
                     sigsd = input$sigsd
                     )
    return(input.ls)
  })
  
  
  
  observe({
    input$runpoa
    
    req(input$prior_min, input$prior_mode, input$prior_max)
    
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
      
      validate(need(zonesShape(), message = "\n\nUpload a shapefile of the geographic boundaries of the treatment area to get started"))
      
      leaflet() %>% 
        addProviderTiles(group = "Topo", provider = providers$OpenTopoMap, options = providerTileOptions(opacity = 0.4)) %>% 
        addLayersControl(overlayGroups = c("Topo", "Zones", "Devices", "Relative risk", "SeU"), 
                         options = layersControlOptions(collapsed = FALSE), position = "bottomleft")
    })
  
  

  # server: copy example files to temp folder ---------------------------------
  
  # - copy device folders
  # observe({
  #   if(input$namedExample == "Kaitake possums"){
  #     paths$zonesShapeFName <- system.file("example_data/Kaitake_possums/extent.shp", package = "proofofabsence")
  #     paths$relativeRiskFName <- system.file("example_data/Kaitake_possums/relRiskRaster.tif", package = "proofofabsence")
  #     paths$surveyFName <- system.file("example_data/Kaitake_possums/devices.csv", package = "proofofabsence")
  #   } else if(input$namedExample == "Mahia possums"){
  #     paths$zonesShapeFName <- system.file("example_data/Mahia_Audrey/extent_block1ABCD.shp", package = "proofofabsence")
  #     paths$relativeRiskFName <- system.file("example_data/Mahia_Audrey/habDistRR_block1ABCD.tif", package = "proofofabsence")
  #     paths$surveyFName <- system.file("example_data/Mahia_Audrey/Surveillance_location.csv", package = "proofofabsence")
  #   } else if(input$namedExample == "CK stoats"){
  #     paths$zonesShapeFName <- system.file("example_data/CK_stoats/extent.shp", package = "proofofabsence")
  #     paths$relativeRiskFName <- system.file("example_data/CK_stoats/relRisk.tif", package = "proofofabsence")
  #     paths$surveyFName <- system.file("example_data/CK_stoats/devices.csv", package = "proofofabsence")
  #   } else if(input$namedExample == "Nutria"){
  #     paths$zonesShapeFName <- system.file("example_data/Nutria/mngtZone_LowPu.shp", package = "proofofabsence")
  #     paths$surveyFName <- system.file("example_data/Nutria/AllNutriaSurvey_2024.csv", package = "proofofabsence", mustWork = TRUE)
  #     updateNumericInput(inputId = "epsg", value = 26918)
  #     
  #     # copy grid surveillance files to temporary folder
  #     #  - csv gets rewritten when selecting start and end years
  #     #  - unless the csv is copied the R system csvs are overwritten and the
  #     #    changes becomes permanent
  #     publicNutria <- system.file("example_data/Nutria/publicNutria.img", package = "proofofabsence")
  #     tmp.publicNutria <- file.path(tempdir(), basename(publicNutria))
  #     gridSurveyFname <- system.file("example_data/Nutria/gridPublicSur7.csv", package = "proofofabsence")
  #     tmp.gridSurveyFname <- file.path(tempdir(), basename(gridSurveyFname))
  #     file.copy(publicNutria, tmp.publicNutria)
  #     file.copy(gridSurveyFname, tmp.gridSurveyFname)
  #     paths$gridSurveyFname <- normalizePath(tmp.gridSurveyFname)
  #   }
  # })
  
  

  # server: load & format devices -------------------------------------------
  devices <- reactiveVal(NULL)
  
  # rename uploaded point surveillance file and set reactive paths$surveyFName
  observe({
    req(input$surveyFName)
    
    # rename to original name
    newpath <- 
      file.path(dirname(input$surveyFName$datapath), 
                input$surveyFName$name)
    renameOK <- 
      file.rename(normalizePath(input$surveyFName$datapath, mustWork = TRUE), 
                  normalizePath(newpath, mustWork = FALSE))
    # update paths reactive object with path to renamed survey file
    if(renameOK){
      paths$surveyFName <- newpath
    }
  })
  
  observe({
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
    req(input$gridSurveyFname)
    req(file.exists(input$gridSurveyFname$datapath))
    
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
      
      # copy grid survey and raster file to temporary folder
      copypaths <- file.path(tempdir(), "gridsurvey", gridnames)
      dir.create(normalizePath(dirname(copypaths[1]), mustWork = FALSE), showWarnings = FALSE)
      file.copy(from = gridpaths, to = copypaths, overwrite = TRUE)
      
      # update reactives with new grid survey path
      newinfopath <- copypaths[basename(copypaths) == infoname]
      # update paths reactive object with path to renamed grid survey file
      paths$gridSurveyFname <- normalizePath(newinfopath, mustWork = TRUE)
    
    # update reactive gridinfo object
    if(!is.null(paths$gridSurveyFname)){
      gridinfo(read.csv(paths$gridSurveyFname))
    }
    
  })
  

  # server: update start and end years ---------------------------------------

  allyrs <- reactiveVal()
  observe({
    
    # get years from devices & grids
    deviceyrs <- unique(devices()$Year)
    gridyrs <- unique(gridinfo()$year)
    # combine years and update inputs
    allyrs(c(deviceyrs, gridyrs))
    if(!is.null(allyrs()) && is.numeric(allyrs())){
      updateNumericInput(session, inputId = "startYear", 
                         value = min(allyrs()))
      updateNumericInput(session, inputId = "endYear", 
                         value = max(allyrs()))
    }
  })
    
  # server: add devices to map ----------------------------------------------
  
  # update inputs for selecting year and device types to display
  observe({
    req(allyrs())
    updateSelectInput(inputId = "selectYear", choices = unique(allyrs()))
  })
  observe({
    req(devices())
    choices <- sort(unique(devices()$Species))
    updateSelectInput(inputId = "selectDevices", choices = choices, selected = choices)
  })
  
  observe({
    
    req(paths$zonesShapeFName)
    req(devices())
    req(input$selectYear)
    req(input$selectDevices)
    req(input$epsg)
    
    # subset year and device
    devs_orig <- subset(devices(), Year %in% input$selectYear & Species %in% input$selectDevices)
    
    # show first n devices
    maxdevices <- 5000
    if(nrow(devs_orig) > maxdevices){
      showNotification(ui = list(strong("Displaying devices"), 
                                 p(paste(nrow(devs_orig), " selected, displaying first ", maxdevices))), 
                       id = "maxdevices", type = "default", duration = 7)
      devs_orig <- devs_orig[seq_len(maxdevices),]
    }
    
    devs_orig <- st_sf(st_as_sf(devs_orig, coords = c("Easting", "Northing")), crs = input$epsg)
    devs <- st_transform(devs_orig, crs = 4326)
    # set color palette
    pal.device <- colorFactor(palette = "Set1", domain = unique(devs$Species))
    
    leafletProxy(session = session, mapId = "baseMap") %>%
      # leaflet() %>%
      # addLayersControl(overlayGroups = c("Devices"), options = layersControlOptions(collapsed = FALSE)) %>% 
      clearGroup("Devices") %>% 
      addCircles(data = devs, group = "Devices", 
                 weight = 4, opacity = 1, fillOpacity = 1, color = ~pal.device(devs$Species)) %>%
      addLegend(layerId = "Devices", pal = pal.device, values = unique(devs$Species), group = "Devices") 

  })
  
  # server: render and update device parameters -----------------------------
  set.animal.params <- reactiveVal(NULL)
  
  observeEvent(devices(), {
    
    req(devices())
    
    spp <- sort(unique(devices()$Species))
    df <- data.frame(row.names = spp)
    df$`Mean g0` <- as.numeric(NA) # as.numeric(0.1)
    df$`Stdev g0` <- as.numeric(NA) # as.numeric(0.01)
    df$`Mean sigma` <- as.numeric(NA) # as.numeric(90)
    df$`Stdev sigma` <- as.numeric(NA) # as.numeric(10)
    
    set.animal.params(df)
    
  })
  
  
  deviceUI <- reactive({
    dtOut <- 
      DT::datatable(set.animal.params(), editable = list(target = "all", disable = list(columns = 0)),
                    options = list(ordering=F, searching = F, paging = F), 
                    selection = "none")
    return(dtOut)
  })
  
  # update set.animal.params reactiveValue when deviceUI table edited
  observeEvent(input$deviceUI_cell_edit, {
    
    # for testing/demonstration purposes fill table with placeholders when 9999 entered in first cell
    if(input$deviceUI_cell_edit$value[2] == 9999){
      .tmp <- set.animal.params()
      .tmp[,1] <- 0.1
      .tmp[,2] <- 0.02
      .tmp[,3] <- 100
      .tmp[,4] <- 10
      set.animal.params(.tmp)
    } else {
      print(input$deviceUI_cell_edit)
      set.animal.params(DT::editData(data = set.animal.params(), 
                                     info = input$deviceUI_cell_edit))
    }
    
  })
  
  # check values in set.animal.params
  observeEvent(set.animal.params(), {
    
    # copy parameter table
    d <- set.animal.params()
    
    # check g0 between zero and 1
    chk_g0 <- d$`Mean g0` > 1 | d$`Mean g0` < 0
    if(any(chk_g0, na.rm = TRUE)){
      showModal(ui = modalDialog(title = "Mean g0 must be between 0-1",
                                 div("Re-enter a valid value in the Detection parameters table")))
      d$`Mean g0`[chk_g0] <- NA
    }
    # check sigma > 0
    chk_sig <- d$`Mean sigma` < 0
    if(any(chk_sig, na.rm = TRUE)){
      showModal(ui = modalDialog(title = "Mean sigma must be > 0",
                                 div("Re-enter a valid value in the Detection parameters table")))
      d$`Mean sigma`[chk_sig] <- NA
    }
    
    # update set.animal.params with corrections from error checking above
    set.animal.params(d)
    
    
  })
  
  
  # fill down sigma values
  observeEvent(input$filldownsigma, {
    req(set.animal.params())
    d <- set.animal.params()
    d$`Mean sigma` <- d$`Mean sigma`[1]
    d$`Stdev sigma` <- d$`Stdev sigma`[1]
    set.animal.params(d)
  })
  
  output$deviceUI <- DT::renderDT({
    validate(need(set.animal.params(), label = "A point surveillance file"))
    deviceUI()
  })
  
  # server: render and update grid parameters -------------------------------

  # update reactive value set.grid.params() on gridinfo() update
  set.grid.params <- reactiveVal(NULL)
  observeEvent(gridinfo(), {
    req(gridinfo())  
    set.grid.params(gridinfo())
  })
    
  # datatable object with grid detection parameters
  gridUI <- reactive({
    dtOut <- 
      DT::datatable(set.grid.params(), editable = list(target = "all", disable = list(columns = 0)),
                    options = list(ordering=F, searching = F, paging = F), 
                    selection = "none")
    return(dtOut)
  })
  
  # render datatable in UI
  output$gridUI <- DT::renderDT({
    validate(need(set.grid.params(), label = "A grid surveillance file"))
    gridUI()
  })
  
  # update set.grid.params reactiveValue when gridUI table edited
  observeEvent(input$gridUI_cell_edit, {
    
    print(input$gridUI_cell_edit)
    set.grid.params(DT::editData(data = set.grid.params(), 
                                 info = input$gridUI_cell_edit))
    
  })
  
  # 
  # leafletProxy(session = session, mapId = "baseMap") %>%
  #   # leaflet() %>%
  #   addCircles(layerId = "Devices", data = isolate(devices())), radius = 10, weight = 1, opacity = 1, fillOpacity = 1, color = ~pal.device(isolate(devices()$Species))) %>%
  #   addLegend(layerId = "Devices", pal = pal.device, values = unique(devs.4326$Species))
  # 
  # observe(print(devices()))
  
  
  # server: zonesShape ------------------------------------------------------
  
  # rename uploaded shapefile and set reactive paths$zonesShapeFName
  observe({
    req(input$zonesShapeFName)
    
    # rename to original name
    newpath <- 
      file.path(dirname(input$zonesShapeFName$datapath), 
                input$zonesShapeFName$name)
    renameOK <- 
      file.rename(normalizePath(input$zonesShapeFName$datapath, mustWork = TRUE), 
                  normalizePath(newpath, mustWork = FALSE))
    # update paths reactive object with path to renamed shapefile
    if(all(renameOK)){
      paths$zonesShapeFName <- newpath[grepl("shp$", newpath)]
    }
  })
  
  # update zonesShape when paths$zonesShapeFName set/changed
  zonesShape <- reactiveVal(NULL)
  observe({
    if(!is.null(paths$zonesShapeFName)){
      message(paste("loading .shp file:", paths$zonesShapeFName))
      zonesShape.sf <- st_sf(st_read(paths$zonesShapeFName))
      zonesShape(zonesShape.sf)
    }
  })
  
  # check uploaded shapefile
  # 
  # set useMultiZone to FALSE (i.e. set useMultiZone to 'Single Zone'):
  #   if single polygon
  #   if multiple polygons but fields for multizone are missing
    observe({
    
      req(zonesShape())
      
      if(nrow(zonesShape()) == 1){
        updateRadioButtons(session = session, 
                           inputId = "useMultiZone", 
                           selected = "Single zone", choices = "Single zone")
        
      } else {
        
        # regex strings to identify zone shapefile fields
        # - matches logic in makeMaskAndZones()
        shp_fields <- c(zoneID = "zone.?ID", Pu_zone = "Pu.?zone",
                        RR_zone = "RR.?zone", zoneName = "zone.?Name")
        field_missing <- logical(length = length(shp_fields))
        # get shape file field names
        field_names <- names(zonesShape())
        
        # check for a match to required fields
        for(i in seq_along(shp_fields)){
          field_missing[i] <- all(!grepl(shp_fields[i], field_names))
        }
        
        # if missing fields show an informative modal dialog
        if(any(field_missing)){
          # prompt user
          showModal(
            modalDialog(title = "Shapefile missing required fields",
                        div(paste0("Required fields ",
                                   paste(names(shp_fields)[field_missing], collapse = ", "),
                                   " are missing from uploaded shapefile. Use single or multiple zones option set to 'Single zone'.")),
                        br(),
                        div("See the 'data requirements guide' linked to in the 'Getting started' section of the help tab for guidance on required fields and their values.")
            ))
          # set to 'Single zone'
          updateRadioButtons(session = session, 
                             inputId = "useMultiZone", 
                             selected = "Single zone", choices = "Single zone")
          
        } else {
          updateRadioButtons(session = session, 
                             inputId = "useMultiZone", 
                             selected = "Multiple zones", 
                             choices = c("Single zone", "Multiple zones"))
        }
      }
  })
    
  
  
  # manual EPSG code override
  observe({
    req(input$epsg)
    req(is.na(detected_epsg()))
    zonesShape.sf <- st_sf(st_read(paths$zonesShapeFName, crs = NA), crs = input$epsg)
    zonesShape(zonesShape.sf)
  })
  
  
  # server: update epsg input using uploaded shapefile ----------------------
  detected_epsg <- reactiveVal()
  observe({
    req(zonesShape())

    # try to get epsg using sf::st_crs()$epsg first
    sf_epsg_string <- st_crs(zonesShape())$epsg
    message("st_crs(zonesShape())$epsg result:\n", sf_epsg_string)
    
    if(!is.na(sf_epsg_string)){
      detected_epsg(sf_epsg_string)
    } else {
      # if no EPSG strings detected when loading shapefile then set to NA
      # - NA triggers check below and alerts user
      detected_epsg(NA)
    }
  })
  
  observe({
    req(!is.null(detected_epsg()))
    
    if(is.na(detected_epsg())){
      showModal(modalDialog(title = "Invalid coordinate reference system in uploaded shape file", 
                            p("Some common spatial libraries have been updated recently."),
                            p("Try updating your GIS software, re-export the file, then reuploading the shape file.")))
      # unset zonesShape and epsg input
      zonesShape(NULL)
      updateNumericInput(session, inputId = "epsg", value = NA)  
    } else {
      updateNumericInput(session, inputId = "epsg", value = detected_epsg())  
    }
  })
  
  # server: add zones to map ------------------------------------------------
  observe({
    
    req(input$epsg)
    
    # make reactive to selecting examples
    # input$namedExample
    
    if("sf" %in% class(zonesShape())){
      
      # get zone polygons and convert
      zonesShape.4326 <- st_transform(zonesShape(), crs = 4326)
      
      # get bounds
      bb <- as.numeric(st_bbox(zonesShape.4326))
      
      zonesShape.4326$poptxt <- 
        paste("zoneName: ", zonesShape.4326$zoneName, br(), 
              "zoneID:   ", zonesShape.4326$zoneID, br(),
              "RR_zone:  ", zonesShape.4326$RR_zone, br(),
              "Pu_zone:  ", zonesShape.4326$Pu_zone)
      
      # update base map
      leafletProxy(session = session, mapId = "baseMap") %>%
        # leaflet() %>% addProviderTiles(group = "Topo", provider = providers$OpenTopoMap) %>% 
        clearGroup(group = "Zones") %>% 
        addPolygons(data = zonesShape.4326, fill = T, weight = 2, 
                    popup = ~ poptxt, 
                    group = "Zones", opacity = 0.6, fillOpacity = 0.05) %>% 
        fitBounds(lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])
    }
  })

  # server: relative risk map -----------------------------------------------

  relRiskRaster <- reactiveVal(NULL)

  # rename uploaded relative risk raster and set reactive paths$relativeRiskFName
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
  
  # sets output.RRloaded to True when risk file loaded
  # used to make panel conditional on uploads
  output$RRloaded <- reactive({!is.null(input$relativeRiskFName)})
  outputOptions(output, 'RRloaded', suspendWhenHidden=FALSE)
  
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
    
    req(relRiskRaster())
      
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
  pyPOA <- eventReactive(input$runpoa, {
    
    message("runpoa press detected")
    
    # proofofabsence::poa_paks(modules = "minimal", delay_load = FALSE)
    
    showNotification("Processing data ...", id = "processing", duration = NULL)
    
    myParams <- proofofabsence::makeParams(setMultipleZones = input$useMultiZone %in% "Multiple zones",
                                           setNumIterations = input$setNumIterations,
                                           setRRTrapDistance = input$setRRTrapDistance,
                                           startYear = input$startYear, endYear = input$endYear,
                                           startPu = input$startPu, 
                                           PuIncreaseRate = input$PuIncreaseRate,
                                           setMinRR = input$setMinRR,
                                           setPrior = c(input$prior_min,input$prior_mode,input$prior_max),
                                           setIntro = c(input$intro_min,input$intro_mode,input$intro_max))
    message("makeParams complete")
    
    myParams <- proofofabsence::addAnimalParams(myParams,
                                                deviceName = row.names(set.animal.params()),
                                                g0 = set.animal.params()$`Mean g0`,
                                                g0sd = set.animal.params()$`Stdev g0`,
                                                sig = set.animal.params()$`Mean sigma`, 
                                                sigsd = set.animal.params()$`Stdev sigma`)
    message("addAnimalParams complete")
    
    # create rawdata using preProcessing.RawData()
    
    # set temp files for zonesOutFName and relRiskRasterOutFName
    tmp.zonesOutFName <- tempfile(fileext = ".tif")
    tmp.relRiskRasterOutFName <- tempfile(fileext = ".tif")
    
    # write out modified (or not) grid parameters to temporary csv file
    #  - otherwise grid surveys from all years are pre-processed
    if(!is.null(set.grid.params())){
      
      # subset grid survey dataframe by years
      ind <- set.grid.params()$year %in% seq.int(input$startYear, input$endYear)
      # write file with dropped rows
      write.csv(set.grid.params()[ind,], paths$gridSurveyFname, row.names = FALSE, quote = FALSE)
      
    } else { 
      tmp.gridSurveyFname <- NULL 
    }
    # write out modified (or not) point locations parameters to temporary csv file
    #  - otherwise grid surveys from all years are pre-processed
    if(!is.null(paths$surveyFName) && !is.null(devices())){
      tmp.surveyFName <- tempfile(fileext = ".csv")
      write.csv(subset(devices(), Year %in% seq.int(input$startYear, input$endYear)),
                tmp.surveyFName, row.names = FALSE, quote = FALSE)
    } else {
      tmp.surveyFName <- NULL
    }
    
    
    # create poa.RawData
    rawdata <- 
      proofofabsence::RawData_R(zonesShapeFName = paths$zonesShapeFName,
                                relativeRiskFName = paths$relativeRiskFName,
                                zonesOutFName = tmp.zonesOutFName,
                                relRiskRasterOutFName = tmp.relRiskRasterOutFName,
                                resolution = as.double(valid()$resolution),
                                epsg = as.integer(input$epsg),
                                surveyFName = tmp.surveyFName,
                                params = myParams, 
                                gridSurveyFname = paths$gridSurveyFname)
    message("RawData_R complete")
    
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
    
    removeNotification(id = "processing")
    showNotification("Processing data complete", duration = 2)
    showNotification("Calculating proof of absence ...", id = "poa", duration = NULL)
    
    # calculate PoA using poa.calculation.calcProofOfAbsence
    result <- poa$calculation$calcProofOfAbsence(myParams, rawdata$survey,
                                                 rawdata$RelRiskExtent, rawdata$zoneArray, rawdata$zoneCodes,
                                                 rawdata$match_geotrans, rawdata$wkt, outputDataPath,
                                                 rawdata$RR_zone, rawdata$Pu_zone, rawdata$Name_zone)
    
    removeNotification(id = "poa")
    showNotification("Calculating proof of absence complete", duration = 2)
    
    message("calcProofOfAbsence complete")
    
    return(list(rawdata = rawdata,
                result = result))

  })
  
  source("array_functions.R")
  
  
  
  
  
  # observe({try(print(pyPOA()$poFMatrix))})


  # server: summary table ---------------------------------------------------
  output$SSePoFResultTable <- renderTable({
    
    # credible intervals
    credint <- 0.95
    lowint <- (1-credint)/2
    uppint <- 1 - lowint
    
    # get results
    result <- pyPOA()$result
    
    # Prior
    prior <- reticulate::py_to_r(result$priorStore)
    prior_mean <- mean(prior)
    prior_low <- quantile(prior, probs = lowint)
    prior_upp <- quantile(prior, probs = uppint)
    prior_string <- sprintf("%0.3f (%0.3f, %0.3f)", prior_mean, prior_low, prior_upp)
    
    # SSe
    SSe_mat <- reticulate::py_to_r(result$sensitivityMatrix)
    SSe_mean <- rowMeans(SSe_mat)
    SSe_low <- apply(SSe_mat, 1, quantile, probs = lowint)
    SSe_upp <- apply(SSe_mat, 1, quantile, probs = uppint)
    SSe_string <- sprintf("%0.3f (%0.3f, %0.3f)", SSe_mean, SSe_low, SSe_upp)
    
    # PoF
    PoFmat <- reticulate::py_to_r(result$poFMatrix)
    poa_mean <- rowMeans(PoFmat)
    poa_low <- apply(PoFmat, 1, quantile, lowint)
    poa_upp <- apply(PoFmat, 1, quantile, uppint)
    poa_string <- sprintf("%0.3f (%0.3f, %0.3f)", poa_mean, poa_low, poa_upp)
    
    years <- reticulate::py_to_r(result$params$years)
    # reticulate::py_to_r(result$sensitivityList)
    
    formatted_df <- 
      rbind(data.frame(Output = "Prior", Session = NA, 
                       "Mean (upp, low 95% credible int.)" = prior_string, check.names = FALSE),
            data.frame(Output = c("SSe", rep("", length(SSe_string) - 1)), 
                       Session = sprintf("%d", years), 
                       "Mean (upp, low 95% credible int.)" = SSe_string, check.names = FALSE),
            data.frame(Output = c("PoA", rep("", length(poa_string) - 1)), 
                       Session = sprintf("%d", years), 
                       "Mean (upp, low 95% credible int.)" = poa_string, check.names = FALSE))
    }, na = "", width = "500px", align = "llr", rownames = FALSE)
  
  # server: zoneSeResultTable -----------------------------------------------
  
  zoneSeResultTable <- reactive({

    req(pyPOA())

    rawdata <- pyPOA()$rawdata
    result <- pyPOA()$result
    
    zoneSe3D <- as.array(result$zoneSeMatrix)
    zoneYears <- as.matrix(result$params$years)
    yrNames <- as.character(zoneYears)
    Name_zone <- as.matrix(result$Name_zone)
    
    proportionSearchedZone <- as.array(result$proportionSearchedZone)
    dimnames(proportionSearchedZone) <- list(zoneYears, Name_zone)
    zoneSeMatrix <- as.array(result$zoneSeMatrix)
    dimnames(zoneSeMatrix) <- list(zoneYears, NULL, Name_zone)
    
    meanSeZ <- apply(zoneSeMatrix, c(1,3), mean)
    LoCI_SeZ <- apply(zoneSeMatrix, c(1,3), quantile, prob = 0.025)
    HiCI_SeZ <- apply(zoneSeMatrix, c(1,3), quantile, prob = 0.975)
    
    d <- expand.grid(Zones = as.character(Name_zone),
                     "Year/session" = yrNames, 
                     stringsAsFactors = FALSE)[c(2,1)]
    d$Mean_SeZ <- as.vector(t(meanSeZ))
    d$Lo_CI_SeZ <- as.vector(t(LoCI_SeZ))
    d$HI_CI_SeZ <- as.vector(t(HiCI_SeZ))
    d$Prop_Searched <- as.vector(proportionSearchedZone)
    
    return(d)
    
  })
  
  output$zoneSeResultTable <- renderTable({
    zoneSeResultTable()
    
    d <- zoneSeResultTable()
    d$`Mean SeZ (upp, low 95% credible int.)` <- sprintf("%0.4f (%0.4f, %0.4f)", d$Mean_SeZ, d$Lo_CI_SeZ, d$HI_CI_SeZ)
    d$`Year/session` <- ifelse(duplicated(d$`Year/session`), "", d$`Year/session`)
    
    d[c("Year/session", "Zones", "Mean SeZ (upp, low 95% credible int.)")]
    
  }, width = "500px", align = "llr", rownames = FALSE)
  
  # server: PoF plot --------------------------------------------------------
  # observe({try(print(reticulate::py_to_r(pyPOA()$poFMatrix)))})
  
  output$PoAtimeplot <- renderPlot({
    result <- pyPOA()$result
    PoFmat <- reticulate::py_to_r(result$poFMatrix)
    poa_mean <- rowMeans(PoFmat)
    poa_low <- apply(PoFmat, 1, quantile, 0.025)
    poa_upp <- apply(PoFmat, 1, quantile, 0.975)
    years <- reticulate::py_to_r(result$params$years)
    prior <- reticulate::py_to_r(result$priorStore)
    prior_mean <- mean(prior)
    prior_low <- quantile(prior, 0.025)
    prior_upp <- quantile(prior, 0.975)
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
    PoFmat <- reticulate::py_to_r(res$poFMatrix)
    row.names(PoFmat) <- paste("Session", 1:nrow(PoFmat))
    prior <- reticulate::py_to_r(res$priorStore)
    PoFmat <- rbind(prior, PoFmat)
    
    lattice::densityplot(~val, group = d1, arr2df(PoFmat), auto.key = T, 
                xlab = "Probability of absence", ylab = "density")
  })

  # server: zip results download --------------------------------------------
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("poaresults_", format(Sys.time(), "%Y%m%d_%H%M"), ".zip", sep = "")
    },
    content = function(file) {
      
      showNotification(id = "zipid", ui = "Creating results zip file ...", 
                       duration = NULL, session = session)
      
      rawdata <- pyPOA()$rawdata
      result <- pyPOA()$result
      
      # R version of proofofabsence.postProcessing.makeTableFX()
      SSe2D <- as.matrix(result$sensitivityMatrix)
      years <- as.matrix(result$params$years)
      nyears <- length(years)
      yrNames <- as.character(years)
      PoF2D <- as.matrix(result$poFMatrix)
      prpSearched <- as.matrix(result$proportionSearchedExtent)
      meanPoFAllYr <- rowMeans(PoF2D)
      PoFQuantiles <- apply(PoF2D, 1, quantile, c(0.025, 0.975))
      meanSSeAllYr <- rowMeans(SSe2D)
      SSeQuantiles <- apply(SSe2D, 1, quantile, c(0.025, 0.975))
      resultTable <- 
        data.frame("Year/session" = yrNames, 
                   'Mean PoF' = sprintf("%0.3f", meanPoFAllYr), 
                   'Lo CI PoF' = sprintf("%0.3f", PoFQuantiles[1,]), 
                   'Hi CI PoF' = sprintf("%0.3f", PoFQuantiles[2,]),
                   'Mean SSe' = sprintf("%0.3f", meanSSeAllYr), 
                   'Lo CI SSe' = sprintf("%0.3f", SSeQuantiles[1,]), 
                   'Hi CI SSe' = sprintf("%0.3f", SSeQuantiles[2,]), 
                   'Prop. Searched' = sprintf("%0.3f", prpSearched), 
                   check.names = FALSE)
      
      # PofSseResultTable.txt
      PofSseResultTable.txt <- file.path(tempdir(), "PofSseResultTable.txt")
      write.csv(resultTable, PofSseResultTable.txt, row.names = FALSE, quote = FALSE)
      
      # zoneSeResultTable.txt
      d <- zoneSeResultTable()
      d$Mean_SeZ <- sprintf("%0.3f", d$Mean_SeZ)
      d$Lo_CI_SeZ <- sprintf("%0.3f", d$Lo_CI_SeZ)
      d$HI_CI_SeZ <- sprintf("%0.3f", d$HI_CI_SeZ)
      d$Prop_Searched <- sprintf("%0.3f", d$Prop_Searched)
      zoneSeResultTable.txt <- file.path(tempdir(), "zoneSeResultTable.txt")
      write.csv(d, zoneSeResultTable.txt, row.names = FALSE, quote = FALSE)
      
      # meanSeuAllYears.tif
      sensitivityList <- reticulate::py_to_r(result$sensitivityList)
      # use zones.tif as template
      rtemp <- terra::rast(rawdata$zonesOutFName)
      # replace values in zones.tif with meanSeU values
      meanSeuAllYears <- 
        do.call("c", lapply(sensitivityList, FUN = function(x){
          r <- rtemp
          values(r) <- x
          return(r)}))
      
      meanSeuAllYears.tif <- file.path(tempdir(), "meanSeuAllYears.tif")
      terra::writeRaster(x = meanSeuAllYears, filename = meanSeuAllYears.tif, 
                         datatype =  "FLT8S", overwrite = TRUE)
      rm(meanSeuAllYears, rtemp, sensitivityList)
      gc()
      message("meanSeuAllYears.tif created")
      
      # relRiskRaster.tif
      relRiskRaster.tif_tmp <- as.character(rawdata$relRiskRasterOutFName)
      relRiskRaster.tif <- file.path(dirname(relRiskRaster.tif_tmp), "relRiskRaster.tif")
      file.copy(relRiskRaster.tif_tmp, relRiskRaster.tif, overwrite = TRUE)
      message("relRiskRaster.tif copied")
      
      # zones.tif
      zones.tif_tmp <- as.character(rawdata$zonesOutFName)
      zones.tif <- file.path(dirname(zones.tif_tmp), "zones.tif")
      file.copy(zones.tif_tmp, zones.tif, overwrite = TRUE)
      message("zones.tif copied")

      removeNotification(id = "zipid")
      showNotification(ui = "Results zip file complete")
      
      # create zip
      utils::zip(zipfile = file, flags = "-rX9j",
                 files = c(PofSseResultTable.txt, zoneSeResultTable.txt, relRiskRaster.tif, zones.tif, 
                           meanSeuAllYears.tif))
      
    }
  )
  

    
  # server: add meanSeU raster ----------------------------------------------
  observeEvent(input$renderSeU, {
    
    req(pyPOA())
    
    # jump to 'Upload inputs' tab
    updateTabsetPanel(inputId = "tabs", session = session, 
                      selected = "Upload inputs")  
    showNotification(session = session, id = "SeUrender", 
                     ui = list(strong("Rendering cell surveillance sensitivities"),
                               p("Will jump to map in 'Upload inputs' tab when complete")), duration = NULL)
    
    # get result objects
    rawdata <- pyPOA()$rawdata
    result <- pyPOA()$result
    
    # find selected year and number of list element
    years <- as.vector(result$params$years)
    ind <- which(years %in% input$selectYear)
    
    # get sensitivity values from calcProofOfAbsence sensitivityList
    sensitivityMatrix <- result$sensitivityList[[ind-1]]
    sensitivityMatrix <- as.matrix(sensitivityMatrix)
    # sensitivityMatrix[sensitivityMatrix == 0L] <- NA
    
    # read zone raster as template
    rtemp <- terra::rast(rawdata$zonesOutFName)
    meanSeu_terra <- rtemp
    terra::values(meanSeu_terra) <- sensitivityMatrix
     
    # project to pseudo-WGS84 using terra package
    meanSeu_project <- terra::project(x = meanSeu_terra, y = "epsg:3857", 
                                      method = "bilinear", gdal = FALSE)
    terra::values(meanSeu_project)[terra::values(meanSeu_project) == 0] <- NA
    # convert back to raster for use in leaflet
    meanSeu <- raster(meanSeu_project)
    
    # set palette
    pal <- colorNumeric(palette = "viridis", domain = c(0,1.1),  # add 0.1 to upper limit
                        na.color = "transparent")
   
    # add to baseMap
    leafletProxy(session = session, mapId = "baseMap") %>%
      clearGroup(group = "SeU") %>%
      addRasterImage(x = meanSeu, layerId = "SeU", group = "SeU",
                     opacity = 0.7, colors = pal, 
                     project = FALSE) %>% 
      addLegend(pal = pal, values = c(0,1), labels = c(0,1), layerId = "SeU")
    
    removeNotification(session = session, id = "SeUrender")
    
  })
  
  

  # server: inputs table for debugging --------------------------------------

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
    print(try(reticulate::py_discover_config()))
    # print("conda_list()")
    # print(try(reticulate::conda_list()))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
