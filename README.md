# Introduction

This draft shiny app aims to add a shiny interface to the proof-of-absence utility. Currently the [proof-of-absence utility](addlink) is written in python and calls local files and scripts to generate outputs to specified folders. This makes it difficult to run the utility in R, despite some clear upsides: advanced plotting, mapping (leaflet) and 'potentially' using shiny to wrap the utility in a web-interface.

**Update**: A draft version is up and running on shinyapps.io https://landcare.shinyapps.io/shiny_poa/. This version substitues the pre-processing python steps with R package equivalents. The steps for installing anaconda distrubutions below should no longer be necessary but are kept for reference below.

---

# Anaconda Installation

Anaconda manages python versions and package libraries installed on local machines. Anaconda can be used to create standalone environments which run specified verions of python (and R) and their packages. This makes it great for running scripts in particular in software environments which are version- or package-specific. Anaconda also works across Windows, Linux and macOS making it excellent for teams which work across these OS.

## 1. Install Anaconda distribution

The first step is to install Anaconda on your local machine. Select the download which matches the operating system on your machine vie the [download page](https://www.anaconda.com/distribution/).

## 2. Create Anaconda environment

The next step is to create an Anaconda environment which contains all the programs and packages needed to get the app up and running locally. After installing Anaconda in Step 1 a command `conda` should be added to your command line.

From command line run:

`conda create -n r_poa -c conda-forge -c r python=3.7 numba numpy rios gdal rstudio r-leaflet r-shiny r-reticulate r-rgdal r-raster r-sf r-kableextra`

The `-n r_poa` argument gives the name of the environment to be created, **r_poa** and the commands `-c conda-forge` and `-c r` tell Anaconda to look for the packages and libraries in the **conda-forge** and **r** channels. The remaining commands are the package and library names and version numbers.

Conda will then look for versions of those libraries which match all (or as many as possible) of the version numbers specified. Depending on the system and the versions this can take a while (`Solving environment \` may appear to hang).

If the command above completes then the environment has been installed correctly. Running `conda env list` lists the available conda environments and should now contain **r_poa**.

## 3. Activate r_poa environment

The created environment remains deactivated by default. This is by design and prevents the programs in the newer environment from over-riding existing ones. To activate the **r_poa** environment type `conda activate r_poa`.

The command line should change to show `(r_poa)` at the start of the current command line. If so, then the environment is sucessfully activated and programs installed in Step 2 can be accessed via the command line (i.e. `python`, `R`, `rstudio`).

## 4. Open RStudio

Programs installed into the environment become available once it's activated. The **r_poa** environment includes Rstudio which is what we will use to run the shiny app. To start RStudio run `rstudio` from the activated **r_poa** environment.

## 5. Create RStudio project
