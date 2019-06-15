#' Compute a distance map to spatial features
#' 
#' Returns a distance map (in meters) to spatial features.
#' 
#' Distances are computed up to the given boundaries and at given resolution.
#' 
#'
#' @param x Spatial* object with relevant spatial features.
#' @param boundaries SpatialPolygons* object with administrative
#'   borders, or Raster* object from where to pick the resoltion.
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
distance_map <- function(x, boundaries, res = resolution(boundaries, max_ncells = 100)) {
  
  if (inherits(boundaries, "Spatial")) {
    max_res <- resolution(boundaries, max_ncells = 2)
    stopifnot(res > 0, res < max_res)
    ext_grid <- raster::raster(raster::extent(boundaries), resolution = res)
    msk <- raster::rasterize(boundaries, ext_grid, field = 1, background = NA, fun = "mean")
  } else if (inherits(boundaries, "Raster")) {
    ## Ignores res
    msk <- boundaries
  } else stop(paste(substitute(boundaries), "must be either a SpatialPolygon* or a Raster* object."))
  
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
#' @return Raster* object. Distance map in meters (even for unprojected maps in
#'   geographical coordinates).
#' 
#' @import raster
#' @export
#'
#' @examples
#'   cmr <- mapMCDA_datasets()
#'   dist_wb <- distance_to_vector(cmr$water_bodies, cmr$animal.density)
#'   raster::plot(dist_wb)
distance_to_vector <- function(x, mask) {
  
  xr <- raster::rasterize(x, mask, field = 0, fun = "last", background = NA)
  raster::mask(raster::distance(xr), mask)
  
}
