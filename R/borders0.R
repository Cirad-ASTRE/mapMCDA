#' Outer boundaries of a vector map
#'
#' @param x A SpatialPolygons* object
#'
#' @return A single polygon with the outermost boundary.
#'
#' @import maptools
#' @export
#' @examples
#'   cmr <- mapMCDA_datasets()
#'   sp::plot(cmr$cmr_admin3)
#'   sp::plot(borders0(cmr$cmr_admin3))
borders0 <- function(x) {
  maptools::unionSpatialPolygons(x, IDs = rep(1, nrow(x)))
}



