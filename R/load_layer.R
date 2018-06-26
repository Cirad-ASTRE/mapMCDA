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
  if (is.na(lt)) return(invisible(NULL))
  
  readOGR_silently <- function(x, ...) {
    if (grepl("\\.shp$", x)) {
      readOGR(dirname(x), rmext(basename(x)), verbose = FALSE, ...)
    } else {
      readOGR(x, verbose = FALSE, ...)
    }
      
  }
  
  load_f <- c(vector = "readOGR_silently", raster = "raster")
  
  return(do.call(load_f[lt], list(x)))
}


#' Load all layers within a directory
#' 
#' Load all recognised vector and raster files within a given directory.
#'
#' @param path String. The path to the directory where spatial layers are
#'   stored.
#'   
#' @return list of layers in the form of Spatial* or RasterLayer objects
#' @export
#'
#' @examples
#'    pkg_layers <- system.file("cartography/CMR/", package = "mapMCDA")
#'    cmr <- load_dir(pkg_layers)
load_dir <- function(path) {
  
  filenames <- list.files(path, full.names = TRUE)
  names(filenames) <- rmext(basename(filenames))
  layers <- lapply(filenames, load_layer)
  
  ## remove unknown (NULL) layers
  return(layers[!vapply(layers, is.null, TRUE)])
  
}