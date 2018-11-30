#' mapMCDA Shiny web interface
#' 
#' Launches the graphical interface (GUI) of the package.
#' 
#' @param bg Logical. If TRUE, the GUI is launched in the background, leaving
#'   available the current session.
#' @export
mapMCDA_app <- function (bg = FALSE) {
  
  code_launch <- "shiny::runApp(system.file(\"interface\", package = \"mapMCDA\"), launch.browser = interactive())"
  
  if (bg) {
    system(paste("Rscript -e '", code_launch, "' &"))
  } else {
    eval(parse(text = code_launch))
  }
}

.onLoad <- function(libname, pkgname){
  # mapMCDA_app()
}