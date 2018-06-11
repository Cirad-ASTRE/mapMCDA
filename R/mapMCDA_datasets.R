#' Load packaged Cameroon layers into memory
#' 
#' Loads a list of 4 geographical layers of Cameroon. Namely: the raster of
#' animal density, the administrative borders at the third level, the national
#' parks and the water bodies.
#'
#' Sources are the Gridded Livestock of the World database (ref?), the Global
#' Administrative Boundaries database (gadm.org), and ???.
#' 
#' @import raster
#' @import rgdal
#'
#' @return list of spatial layers
#' @export
#'
#' @examples
#'   cmr <- mapMCDA_datasets()
mapMCDA_datasets <- function() {
  pkg_files <- list.files(
    system.file(
      "cartography/CMR/",
      package = "mapMCDA"
    ),
    full.names = TRUE
  )
  
  layer_name <- basename(pkg_files)
  layer_noext <- gsub("\\.\\w{1,}$", "", layer_name)
  layer_type <- vapply(layer_name, layer_type, character(1))
  
  readOGR_silently <- function(x, ...) readOGR(x, verbose = FALSE, ...)
  
  load_f <- c(vector = "readOGR_silently", raster = "raster")
  
  ans <- mapply(function(x, y) do.call(x, list(y)), load_f[layer_type], pkg_files)
  names(ans) <- layer_noext
  
  return(ans)
}
