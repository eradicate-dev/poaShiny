# Introduction

This draft shiny app aims to add a shiny interface to the proof-of-absence utility. Currently the [proof-of-absence utility](addlink) is written in python and calls local files and scripts to generate outputs to specified folders. This makes it difficult to run the utility in R, despite some clear upsides: advanced plotting, mapping (leaflet) and 'potentially' using shiny to wrap the utility in a web-interface.

**Update**: A draft version is up and running on shinyapps.io https://landcare.shinyapps.io/shiny_poa/. This version substitues the pre-processing python steps with R package equivalents. The steps for installing anaconda distrubutions below should no longer be necessary but are kept for reference below.
