#' Load a map layer
#'
#' Automatically detects whether it is a vector or raster map and uses the
#' appropriate read function.
#' 
#' @param x File name with full path
#'
#' @return A \code{Spatial*} object for vector maps and a \code{RasterLayer}
#'   object for raster maps.
#'   
#' @export
#' @import rgdal
#' @import raster
#'
#' @examples
#'  (pkg_layers <- list.files(
#'    system.file("cartography/CMR", package = "mapMCDA"),
#'    full.names = TRUE
#'  ))
#'  
#'  epiunits <- load_layer(pkg_layers[[2]])
load_layer <- function(x) {

  lt <- layer_type(basename(x))
  
  readOGR_silently <- function(x, ...) readOGR(x, verbose = FALSE, ...)
  
  load_f <- c(vector = "readOGR_silently", raster = "raster")
  
  return(do.call(load_f[lt], list(x)))
}
