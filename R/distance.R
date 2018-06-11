#' Compute a distance map to spatial features
#' 
#' Returns a distance map (in meters) to spatial features.
#' 
#' Distances are computed up to the given boundaries and at given resolution.
#' 
#'
#' @param x Spatial* object with relevant spatial features.
#' @param boundaries SpatialPolygon* object with administrative borders.
#' @param res numeric. Resolution for the outcome distance map. The default divides
#'   the smallest dimension into 100 cells.
#'
#' @return
#'   A raster map with distances in meters to the closest spatial feature.
#' @export
#'
#' @examples
#'   cmr <- mapMCDA_datasets()
#'   raster::plot(distance_map(cmr$water_bodies, boundaries = cmr$cmr_admin3))
distance_map <- function(x, boundaries, res = resolution(boundaries, min_ncells = 100)) {
  
  ext_grid <- raster::raster(raster::extent(boundaries), resolution = res)
  msk <- raster::rasterize(boundaries, ext_grid, field = 1, background = NA, fun = "mean")
  ans <- distance_to_vector(x, msk)
  return(ans)
}



#' Distance to vector features
#' 
#' Compute a raster file with the distances to the nearest vector feature,
#' within the limits of a given mask.
#' 
#' The resolution of the mask determines that of the outcome.
#'
#' @param x Spatial* object
#' @param mask Raster* object
#'
#' @return Raster* object. Distance map in meters.
#' 
#' @import raster
#' @export
#'
#' @examples
#'   cmr <- mapMCDA_datasets()
#'   dist_wb <- distance_to_vector(cmr$water_bodies, cmr$animal.density)
#'   raster::plot(dist_wb)
distance_to_vector <- function(x, mask) {
  
  xr <- raster::rasterize(x, mask, field = 0, fun = "mean", background = NA)
  dx <- raster::mask(raster::distance(xr), mask)
  
}
