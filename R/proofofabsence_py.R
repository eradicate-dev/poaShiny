
#' Check for 'proofofabsence' Anaconda environment on system
#'
#' Runs a check for installed Anaconda environment named 'proofofabsence'.
#' Required to load and run python proofofabsence module using reticulate.
#' @param envname Name of environment to check for (default = 'proofofabsence').
#' @export
#' @keywords proofofabsence
#' @examples
#'  conda_env_check()

conda_env_check <- function(envname = "proofofabsence"){
  any(grepl(envname, reticulate::conda_list()[["name"]]))
}

#' Build Anaconda environment for Proof of Absence python scripts
#'
#' Builds Anaconda environment with package load required to run the Proof of Absence scripts using python
#' @param force.overwrite Overwrite existing Anaconda environments named 'proofofabsence' (default = TRUE).
#' @examples
#'  conda_env_build(force.overwrite = T)  # build new Anaconda environment

conda_env_build_full <- function(envname = "proofofabsence", force.overwrite = TRUE){

  if(conda_env_check(envname)){
    message(sprintf("existing '%s' environment found", envname))
    if(!force.overwrite) switch(menu(choices = c("y","n"), title = "Proceed?") + 1, stop("Cancelling"),"",stop("Cancelling"))
  }

  # required python packages and versions
  paks <- c("python==3.7.1","numba==0.48.0","numpy==1.17.5","rios==1.4.10","gdal==2.3.2",
            "r==3.*", "rstudio","r-leaflet==2.0.3","r-shiny==1.4.0","r-reticulate==1.22",
            "r-rgdal==1.4_7","r-raster==3.0_7","r-sf==0.8_0","r-kableextra==1.1.0", 
            "r-devtools", "rtools")

  message(sprintf("Creating '%s' conda environment - this takes a while ....", envname))
  # create the 'envname' conda environment
  reticulate::conda_create(envname = envname, packages = paks, forge = TRUE, channel = "conda-forge")

  if(conda_env_check()) message(sprintf("'%s' conda environment built", envname))

}

conda_env_build_min <- function(envname = "proofofabsence", force.overwrite = TRUE){

  if(conda_env_check(envname)){
    message(sprintf("existing '%s' environment found", envname))
    if(!force.overwrite) switch(menu(choices = c("y","n"), title = "Proceed?") + 1, stop("Cancelling"),"",stop("Cancelling"))
  }

  #-------------------------------------------------------------------------#
  # conda create -n proofofabsence -c conda-forge python==3.7.1 numba==0.48.0
  # numpy==1.17.5 r==3.* rstudio r-leaflet==2.0.3 r-shiny==1.4.0
  # r-reticulate==1.14 r-rgdal==1.4_7 r-raster==3.0_7 r-sf==0.8_0
  # r-kableextra==1.1.0 r-devtools rtools
  #-------------------------------------------------------------------------#
  
  # required python packages and versions
  paks <- c("python==3.7.1","numba==0.48.0","numpy==1.17.5","r==3.*", "rstudio",
            "r-leaflet==2.0.3","r-shiny==1.4.0","r-reticulate==1.14",
            "r-rgdal==1.4_7","r-raster==3.0_7","r-sf==0.8_0","r-kableextra==1.1.0",
            "r-devtools", "rtools")

  message(sprintf("Creating '%s' conda environment - this takes a while ....", envname))
  # create the 'envname' conda environment
  reticulate::conda_create(envname = envname, packages = paks, forge = TRUE, channel = "conda-forge")

  if(conda_env_check()) message(sprintf("'%s' conda environment built", envname))

}


#' poa_py
#'
#' This function loads the python proof of absence functions in R using reticulate
#' @keywords python poa reticulate
#' @export
#' @examples
#' require(reticulate)
#' use_condaenv("proofofabsence")
#' poa <- poa_py()
#' poa$calculation             # show calculation proofofabsence module
#' py <- import_builtins()     # import built-in python functions
#' py$dir(poa)                 # display functions available with a module
#' py$dir(poa$calculation)
#' py$dir(poa$preProcessing)

poa_py <- function(){
  preProcessing <- reticulate::import("proofofabsence", convert = FALSE, delay_load = T)
  return(preProcessing)
}

