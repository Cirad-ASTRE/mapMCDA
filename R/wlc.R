#' Weighted Linear Combination of risk factors
#'
#' @param x list of risk factors (RaterLayer objects)
#' @param w vector of weights
#'
#' @return A RasterLayer object.
#' @export
#' @import raster
#'
wlc <- function(x, w) {
  cmr <- mapMCDA_datasets()
  x <- lapply(cmr[-2], risk_layer, boundaries = cmr$cmr_admin3)
  w <- c(.5, .2, .3)
  
  stopifnot(
    sum(w) == 1,
    length(w) == length(x)
  )
  
  xc <- stack(align_layers(x))
  
  ans <- raster::weighted.mean(xc, w = w)
  return(ans)
}

#' Harmonise projection, resolution and extent of a list of rasters
#' 
#' Convert a list of rasters to a common projection, resolution and extent.
#' 
#' The final projection is the most common one, other than NA. If a raster has
#' unknown projection, it is assumed to be equal to the most common one.
#' 
#' All rasters are resampled to the same extent and resolution as the \emph{first}
#' raster in the list.
#' 
#' @param x list of RasterLayer objects
#'
#' @return Another list of RasterLayer objects
#' @export
#' @import sp
#' @import raster
#'
align_layers <- function(x) {
  
  ## Projection
  projs <- vapply(x, proj4string, character(1))
  if (length(unique(projs)) > 1) {
    nprojs <- table(projs)  # excludes NA
    commonest_proj <-  names(nprojs)[which.max(nprojs)]
    reflayer.idx <- head(which(projs %in% commonest_proj), 1)
    
    for (i in which(!projs %in% commonest_proj) ){
      if (is.na(projs[[i]])) {
        proj4string(x[[i]]) <- CRS(proj4string(x[[reflayer.idx]]))
      } else {
        x[[i]] <- projectRaster(x[[i]], x[[reflayer.idx]])
      }
    }
  }
        
  
  ## Resolution and extent
  ans <- lapply(x, resample, x[[1]])
  
  return(ans)
}
