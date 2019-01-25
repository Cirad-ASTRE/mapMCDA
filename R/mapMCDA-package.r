#' mapMCDA
#' 
#' Produce an epidemiological risk map by weighting multiple risk factors
#'
#' @name mapMCDA
#' @import methods
#' @docType package
NULL

.onLoad <- function(libname, pkgname) {
  mapMCDA_app()
}