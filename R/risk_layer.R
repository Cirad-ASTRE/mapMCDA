#' Compute risk layer
#' 
#' Rescale a raster with a linear relationship.
#' 
#' If you need an inverse relationship, just reverse the target scale.
#'
#' @param x a RasterLayer object
#' @param scale_target numeric vector of length 2. New scale.
#'
#' @return A RasterLayer object in the new scale.
#' @export
#' @import raster
#'
#' @examples
#'   ad <- mapMCDA_datasets()$animal.density
#'   raster::plot(ad)
#'   raster::plot(risk_layer(ad, scale_target = c(-1, 1)))
#'   raster::plot(risk_layer(ad, scale_target = c(1, -1)))
risk_layer <- function(x, boundaries, scale_target = c(0, 100)) {
  
  if (inherits(x, "Spatial")) {
    r <- distance_map(x, boundaries = boundaries)
  } else {
    if (inherits(x, "RasterLayer")) {
      r <- mask(extend(crop(x, boundaries), boundaries), boundaries)
    } else {
      stop("Can't compute risk layer from a ", class(x), " object.")
    }
  }
  
  scale_source <- range(raster::values(r), na.rm = TRUE)
  
  ## Linear function
  lin_fun <- function(r) {
    slope <- diff(scale_target)/diff(scale_source)
    ans <- scale_target[1] + slope * (r - scale_source[1])
    return(ans)
  }
  
  ans <- calc(r, lin_fun)
  
  return(ans)
}
