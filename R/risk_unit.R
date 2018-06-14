#' Risk by epidemiological unit
#' 
#' Compute the risk for each of the epidemiological units.
#' 
#' Summarises the level of risk using a function such as the mean or median
#' value across the epidemiological unit.
#' 
#' @param r RasterLayer object
#' @param eu SpatialPolygons* object
#' @param fun A summary function. By default the mean.
#'
#' @return numeric vector of the same length as the number of epidemiological
#'   units.
#' 
#' @export
#' @import sp
#'
#' @examples
#'   cmr <- mapMCDA_datasets()
#'   summary(risk_unit(cmr$animal.density, cmr$cmr_admin3))
risk_unit <- function(r, eu, fun = mean) {
  rgrid <- as(r, "SpatialGridDataFrame")  # needed for overlay methods
  funrisk_poly <- over(eu, rgrid, fn = fun)[[1]]
  
  ## Small-polygon correction
  ## some values of funrisk_poly can be NA for very small polygons
  ## since over() first converts the raster in cell center points.
  ## Some polygons might not cover any of the raster cell centers.
  ## For that cases, we overlay the polygon centers with the raster.
  if (any(idx <- is.na(funrisk_poly))) {
    centroids <- SpatialPoints(
      coords = coordinates(eu)[idx, ],
      proj4string = CRS(proj4string(eu))
    )
    funrisk_poly[idx] <- over(centroids, rgrid)[[1]]
    
    stopifnot(!anyNA(funrisk_poly))
  }
  
  return(funrisk_poly)
}

