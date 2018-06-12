#' mapMCDA Shiny web interface
#' @export
#' @noRd
mapMCDA_app <- function (bg = FALSE) {
  
  code_launch <- "shiny::runApp(system.file(\"interface\", package = \"mapMCDA\"))"
  
  if (bg) {
    system(paste("Rscript -e '", code_launch, "' &"))
  } else {
    eval(parse(text = code_launch))
  }
}

.onLoad <- function(libname, pkgname){
  # mapMCDA_app()
}