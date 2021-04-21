



#' Run proof of absence shiny app locally
#' 
#' Wrapper funtion to run app locally using app.R file stored in installed package folder
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' proofofabsence::poaApp()
#' }
poaApp <- function(){
  shiny::runApp(system.file("app/app.R", package = "proofofabsence"))
}
