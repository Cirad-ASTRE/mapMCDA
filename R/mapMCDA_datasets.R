#' Load packaged Cameroon layers into memory
#' 
#' Loads a list of 4 geographical layers of Cameroon. Namely: the raster of
#' animal density, the administrative borders at the third level, the national
#' parks and the water bodies.
#'
#' Sources are the Gridded Livestock of the World database (ref?), the Global
#' Administrative Boundaries database (gadm.org), and ???.
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
  
  layer_name <- gsub("\\.\\w{1,}$", "", basename(pkg_files))

  ans <- structure(
    lapply(pkg_files, load_layer),
    names = layer_name
  )
  
  return(ans)
}
