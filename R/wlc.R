#' Weighted Linear Combination of risk factors
#' 
#' Combine a list of risk layers using a weighted linear approach:
#' \deqn{w_1 x_1 + w_2 x_2 + \cdots}
#' 
#' Missing values are by default removed from the combination, yielding a result
#' that relies on the available factors in each place. If \code{na.rm = FALSE},
#' then all pixels where at least one risk factor is missing get a missing value
#' as the result.
#' 
#' @param x list of risk factors (RaterLayer objects)
#' @param w vector of weights
#' @param na.rm logical. 
#'
#' @return A RasterLayer object.
#' @export
#' @import raster
#'
wlc <- function(x, w, na.rm = TRUE) {

  stopifnot(
    all.equal(sum(w), 1),
    length(w) == length(x)
  )
  
  xc <- raster::stack(align_layers(x))
  
  ans <- raster::weighted.mean(xc, w = w, na.rm = na.rm)
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
  ## Watch out! This rewrites pixels with NA, assigning a new value with the
  ## default method = "bilinear". This is fixed with "nearest neighbor"
  ans <- lapply(x, resample, x[[1]], method = "ngb")
  
  return(ans)
}
