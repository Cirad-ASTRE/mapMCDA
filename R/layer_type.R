#' Detect type of geographic information
#' 
#' Either "vector" or "raster" depending on the file extension
#'
#' @param x string. A file name.
#'
#' @return character. Either "vector" or "raster. NA for unknown formats.
#' @export
#'
#' @examples
#'   layer_type("myfile.tif")
layer_type <- function(x) {
  
  vectorExt <- c("\\.gpkg$", "\\.shp$")
  rasterExt <- c("\\.tif$", "\\.tiff$")
  
  isvec <- any(vapply(vectorExt, function(.) grepl(., x), TRUE))
  isras <- any(vapply(rasterExt, function(.) grepl(., x), TRUE))
  
  ## It should be either vector or raster, but not both
  if (!any(isvec, isras))
    return(NA)
  
  if (all(isvec, isras))
    stop("This is a bug and should not happen.")
  
  if (isvec) return("vector") else return("raster")
}
