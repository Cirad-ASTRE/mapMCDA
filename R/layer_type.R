#' Detect type of geographic information
#' 
#' Either "vector", "raster" or "network" depending on the file extension.
#'
#' @param x string. A file name.
#'
#' @return character. Either "vector", "raster" or "network". NA for unknown formats.
#' @export
#'
#' @examples
#'   layer_type("myfile.tif")
layer_type <- function(x) {
  
  known_extensions <- list(
    vector  = c("\\.gpkg$", "\\.shp$"),
    raster  = c("\\.tif$", "\\.tiff$"),
    network = c("\\.csv$")
  )
  
  is_type <- 
    vapply(
      known_extensions,
      function(type) any(vapply(type, function(.) grepl(., x), TRUE)),
      TRUE
    )
  
  ## Check at most one layer-type is identified
  stopifnot( sum(is_type) < 2 )
  
  return(ifelse(any(is_type), names(is_type)[is_type], NA))
}
