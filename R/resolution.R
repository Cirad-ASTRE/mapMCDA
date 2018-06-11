#' Compute a suitable resolution for a map
#' 
#' Compute a resolution in the original map units that conforms to given
#' criteria.
#' 
#' 
#' @param x a Raster* or Spatial* object
#' @param min_ncells Number of cells for the smallest dimension
#'
#' @return A number. The resolution that respects the desired number of cells.
#'
#' @import raster
#' @export
#' @examples
#'   r <- raster::raster(nrows = 5, ncols = 11, xmn = 0, xmx = 11, ymn = 0, ymx = 5)
#'   resolution(r, min_ncells = 10)
resolution <- function(x, min_ncells = 100) {
  ext <- raster::extent(x)
  span_x <- raster::xmax(x) - raster::xmin(x)
  span_y <- raster::ymax(x) - raster::ymin(x)
  
  min_span <- min(span_x, span_y)
  res <- min_span/min_ncells
  return(res)
}
