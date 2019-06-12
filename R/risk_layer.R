#' Compute risk layer
#' 
#' Compute a raster map from the input layer and rescale with a linear
#' relationship.
#' 
#' For Spatial* objects (geometries such as point, lines or polygons),
#' compute the \code{distance_map()}, which gives a RasterLayer. For
#' \code{igraph} objects (from network data), compute a RasterLayer 
#' with the relative importance of the nearest node. For a RasterLayer
#' mask, extend or crop to the \code{boundaries} as needed.
#' 
#' Finally, scale the RasterLayer outcome of any of the three input
#' types. If you need an inverse relationship, just reverse the target
#' scale.
#'
#' @param x a Spatial*, RasterLayer or igraph object
#' @param boundaries a Spatial* object, used to determine the boundaries of the
#'   computed risk layer.
#' @param scale_target numeric vector of length 2. New scale.
#'
#' @return A RasterLayer object in the new scale.
#' @export
#' @import raster
#'
#' @examples
#'   ad <- mapMCDA_datasets()$animal.density
#'   bd <- mapMCDA_datasets()$cmr_admin3
#'   raster::plot(ad)
#'   raster::plot(risk_layer(ad, bd, scale_target = c(-1, 1)))
#'   raster::plot(risk_layer(ad, bd, scale_target = c(1, -1)))
risk_layer <- function(x, boundaries, scale_target = c(0, 100)) {
  
  if (inherits(x, "Spatial")) {
    r <- distance_map(x, boundaries = boundaries)
  } else {
    if (inherits(x, "RasterLayer")) {
      r <- mask(extend(crop(x, boundaries), boundaries), boundaries)
    } else {
      if (inherits(x, "igraph")) {
        r <- rasterize(x, boundaries)
      } else {
        stop("Can't compute risk layer from a ", class(x), " object.")
      }
    }
  }
  
  scale_source <- range(raster::values(r), na.rm = TRUE)
  
  if (isTRUE(all.equal(diff(scale_source), 0))) {
    stop("This risk factor has a constant value and cannot be rescaled.\n",
         "Please correct or remove.")
  }
    
  ## Linear function
  lin_fun <- function(r) {
    slope <- diff(scale_target)/diff(scale_source)
    ans <- scale_target[1] + slope * (r - scale_source[1])
    return(ans)
  }
  
  ans <- calc(r, lin_fun)
  
  return(ans)
}
