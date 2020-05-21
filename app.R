
# packages ----------------------------------------------------------------

# devtools::install_github("rstudio/reticulate")
library(shiny)
library(reticulate)
library(rgdal)
library(raster)
library(kableExtra)
library(leaflet)
library(sf)
library(DT)

# source('https://raw.githubusercontent.com/eradicate-dev/poaShiny/git-sourced-app/app_lines.R')
source("app_lines.R")

# Run the application 
shinyApp(ui = ui, server = server)
