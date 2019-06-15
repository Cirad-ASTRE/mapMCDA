#' Compute a suitable resolution for a map
#' 
#' Compute a resolution in the original map units that conforms to given
#' criteria.
#' 
#' 
#' @param x a Raster* or Spatial* object
#' @param max_ncells Number of cells for the largest dimension
#'
#' @return A number. The resolution that respects the desired number of cells.
#'
#' @import raster
#' @export
#' @examples
#'   r <- raster::raster(nrows = 5, ncols = 11, xmn = 0, xmx = 11, ymn = 0, ymx = 5)
#'   resolution(r, max_ncells = 10)
resolution <- function(x, max_ncells = 100) {
  ext <- raster::extent(x)
  span_x <- raster::xmax(x) - raster::xmin(x)
  span_y <- raster::ymax(x) - raster::ymin(x)
  
  max_span <- max(span_x, span_y)
  res <- max_span/max_ncells
  return(res)
}
