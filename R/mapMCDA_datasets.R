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

  pkg_layers <- system.file(
    "cartography/CMR/",
    package = "mapMCDA"
  )
  
  ans <- load_dir(pkg_layers)
  
  return(ans)
}
