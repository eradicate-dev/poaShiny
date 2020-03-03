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

path.tmp <- ".tmp"
path.tmp.input <- paste0(path.tmp, "\\input\\")
path.tmp.output <- paste0(path.tmp, "\\output\\")
dir.create(path.tmp.input, recursive = T)
dir.create(path.tmp.output, recursive = T)

unlink(paste0(path.tmp, "\\input\\*"))
unlink(paste0(path.tmp, "\\output\\*"))


# manage python environments ----------------------------------------------

# conda_list()                               # list conda envts

use_condaenv("r_poa", required = TRUE)     # select conda envt

#-------------------------------------------------------------------------#
# source working Kaitake script
# source_python(file = "C:/Users/howards/OneDrive - MWLR/Projects/795208-0091 Ctr for Inv Spec Res-Erad Tool/shiny_PoA/Kaitake_sc0_Farm_app_paths.py",
#               convert = FALSE)
#-------------------------------------------------------------------------#

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
    selectInput(inputId = "namedExample", label = "Select example set", choices = c("None", "Kaitake possums", "Mahia possums"), multiple = FALSE),
    conditionalPanel(condition = "input.namedExample == 'None'",{
      list(fileInput(inputId = "zonesShapeFName", label = "zonesShapeFName", multiple = TRUE),
           fileInput(inputId = "surveyFName", label = "surveyFName", multiple = FALSE),
           fileInput(inputId = "relRiskRasterOutFName", label = "relRiskRasterOutFName", multiple = FALSE))
    }),
    numericInput(inputId = "setMinRR", label = defaults$setMinRR$label, value = defaults$setMinRR$value, min = 0, max = 1000),
    numericInput(inputId = "epsg", label = defaults$epsg$label, value = defaults$epsg$value)
  ),
  DT::DTOutput(outputId = "deviceUI"),
  actionButton(inputId = "runpy", label = "Run params script"),
  actionButton(inputId = "GO", label = "Run inputs")
)

ui.inputs.priors <- 
  list(numericInput(inputId = "prior_min", label = "Prior min", value = defaults$prior_min$value, min = 0, max = 1),
       numericInput(inputId = "prior_mode", label = "Prior mode", value = defaults$prior_mode$value, min = 0, max = 1),
       numericInput(inputId = "prior_max", label = "Prior max", value = defaults$prior_max$value, min = 0, max = 1),
       numericInput(inputId = "sigmean", label = defaults$sigmean$label, value = defaults$sigmean$value),
       numericInput(inputId = "sigsd", label = defaults$sigsd$label, value = defaults$sigsd$value)
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
                    list(h3("Probability of absence"),
    plotOutput("PoAtimeplot"))),
    # h3("validTable"),
    # verbatimTextOutput("validTable"),
    column(width = 6, list(h3("baseMap"),
    leafletOutput("baseMap"))) #,
    # h3("result"),
    # verbatimTextOutput("result"), 
    # h3("runpypress"),
    # verbatimTextOutput("runpypress"), 
    # h3("inputTable"),
    # htmlOutput("inputTable")
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
      
      paths <- list.files("app/www/example_data/Kaitake_possums/", pattern = "devices.csv|extent\\..*|relRiskRaster.tif", full.names = T)
      paths.to <- sub("app/www/example_data/Kaitake_possums/", ".tmp/input/", paths)
      
      unlink(".tmp/input/*")
      file.copy(paths, paths.to)
    } else if(input$namedExample == "Mahia possums"){
      
      paths <- list.files("app/www/example_data/Mahia_Audrey/", pattern = ".csv$|extent.*|.tif", full.names = T)
      paths.to <- sub("app/www/example_data/Mahia_Audrey/extent_block1ABCD", ".tmp/input/extent", paths)
      paths.to <- sub("app/www/example_data/Mahia_Audrey/habDistRR_block1ABCD.tif", ".tmp/input/relRiskRaster.tif", paths.to)
      paths.to <- sub("app/www/example_data/Mahia_Audrey/Surveillance_location.csv", ".tmp/input/devices.csv", paths.to)
      
      unlink(".tmp/input/*")
      file.copy(paths, paths.to)
    } else {
      unlink(".tmp/input/*")
    }
  })
  
  

  # server: load & format devices -------------------------------------------
  devices <- reactiveVal(NULL)
  
  # copy loaded device file to .tmp folder
  observe({
    if(input$namedExample == "None"){
      paths <- input$surveyFName
      print(paths)
      paths.to <- paste0(path.tmp, "/input/", sub(".*(?=\\..*$)", "devices", paths$name, perl = TRUE))
      file.copy(from = paths$datapath, to = paths.to, overwrite = TRUE)
    } 
    
    if(file.exists(".tmp/input/devices.csv")){
      # load devices as spatial object
      devs <- st_sf(st_as_sf(read.csv(".tmp/input/devices.csv", stringsAsFactors = FALSE), 
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
    
    if("sf" %in% class(devices())){
      
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
    dtOut <- DT::datatable(set.animal.params(), editable = TRUE)
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
    
    if(input$namedExample == "None"){
      
      paths <- input$zonesShapeFName
      paths.to <- paste0(path.tmp, "\\input\\", sub(".*(?=\\..*$)", "extent", paths$name, perl = TRUE))
      
      file.copy(from = paths$datapath, to = paths.to, overwrite = T)
    }
    
    path.ext <- paste0(path.tmp, "/input/extent.shp")
    if(file.exists(path.ext)){
      message(paste("loading .shp file:", path.ext))
      zonesShape.sf <- st_sf(st_read(path.ext), crs = defaults$epsg$value)
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
      zonesShape.4326$poptxt <- 
        sapply(split(zonesShape.4326, zonesShape.4326$zoneName), 
               function(x) kableExtra::kable(st_drop_geometry(x), escape = TRUE, format = "html", row.names = FALSE))
      
      # update base map
      leafletProxy(session = session, mapId = "baseMap") %>%
        # leaflet() %>% addProviderTiles(group = "Topo", provider = providers$OpenTopoMap) %>% 
        clearGroup(group = "Zones") %>% 
        addPolygons(data = zonesShape.4326, fill = T, weight = 2, popup = ~poptxt, group = "Zones") %>% 
        fitBounds(lng1 = bb[1], lat1 = bb[2], lng2 = bb[3], lat2 = bb[4])
    }
  })

  # server: relative risk map -----------------------------------------------

  relRiskRaster <- reactiveVal(NULL)
  
  # copy loaded relative risk raster file to .tmp folder
  observe({
    
    if(input$namedExample == "None"){
      
      paths <- input$relRiskRasterOutFName
      paths.to <- paste0(path.tmp, "\\input\\", sub(".*(?=\\..*$)", "relRiskRaster", paths$name, perl = TRUE))
      
      file.copy(from = paths$datapath, to = paths.to, overwrite = T)
    }
    
    path.RRmap <- paste0(path.tmp, "/input/relRiskRaster.tif")
    
    # load any relative risk files in .tmp folder
    if(file.exists(path.RRmap)){
      message(paste("loading relative risk .tif file:", path.RRmap))
      RRmap <- raster(path.RRmap)
      relRiskRaster(RRmap)
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
    if("RasterLayer" %in% class(relRiskRaster())){
      
      # get layer and convert to WGS84
      RRmap.4326 <- 
        projectRaster(relRiskRaster(), crs = st_crs(4326)[["proj4string"]],
                      method = "ngb")
      
      # get bounds
      bb <- as.vector(extent(RRmap.4326))                 
      
      # set values above MinRR to NA
      RRmap.4326@data@values[RRmap.4326@data@values < input$setMinRR] <- NA
      valrng <- c(input$setMinRR, max(values(RRmap.4326), na.rm = T))

      # make palette
      pal <- colorNumeric(palette = "viridis",
                          domain = valrng,
                          na.color = "transparent")
      
      # update base map
      leafletProxy(session = session, mapId = "baseMap") %>%
        # leaflet() %>%
        clearGroup(group = "Relative risk") %>%
        addRasterImage(x = RRmap.4326, group = "Relative risk",
                       opacity = 0.95, #input$rasterOpacity,
                       colors = pal) %>%
        fitBounds(lng1 = bb[1], lat1 = bb[3], lng2 = bb[2], lat2 = bb[4])
        
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
    print("runpy press detected")
    
    # import proofofabsence package
    # params <- import_from_path(module = "params", path = "proofofabsence", convert = FALSE)
    source_python("proofofabsence/params.py", convert = FALSE)
    # preProcessing <- import_from_path(module = "preProcessing", path = "proofofabsence", convert = FALSE)
    source_python("proofofabsence/preProcessing.py", convert = FALSE)
    # calculation <- import_from_path(module = "calculation", path = "proofofabsence", convert = FALSE)
    source_python("proofofabsence/calculation.py", convert = FALSE)

    animals = AnimalTypes()                                              #

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

    rawdata <- # preProcessing$
      RawData(zonesShapeFName = ".tmp/input/extent.shp", # "app\\www\\poa\\Kaitake\\Data\\extent_Kait.shp",
              relativeRiskFName = ".tmp/input/relRiskRaster.tif", # "app\\www\\poa\\Kaitake\\Data\\relRiskRaster.tif",
              zonesOutFName = ".tmp/output/zones.tif", #defaults$zonesOutFName$value, # "app\\www\\poa\\Kaitake\\Results\\Model_0\\zones.tif",
              relRiskRasterOutFName = ".tmp/output/relRiskRaster.tif", # "app\\www\\poa\\Kaitake\\Results\\Model_0\\relRiskRaster.tif",
              resolution = as.double(valid()$resolution),
              epsg = as.integer(2193),
              surveyFName = ".tmp/input/devices.csv",
              params = myParams, 
              gridSurveyFname = NULL)

    # load builtin py functions (required for open() below)
    py <- reticulate::import_builtins()
    # make object for pickling spatial data
    pickledat = PickleDat(rawdata)

    # pickle to output directory
    pickleName = ".tmp/output/spatialData.pkl"
    fileobj = py$open(pickleName, 'wb')
    pickle$dump(pickledat, fileobj, protocol=4)
    fileobj$close()
    
    
    result = # calculation$
      calcProofOfAbsence(myParams, pickledat$survey,
                         pickledat$relativeRiskRaster, pickledat$zoneArray, pickledat$zoneCodes,
                         pickledat$match_geotrans, pickledat$wkt, ".tmp/output",
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
