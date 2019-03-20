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
#' @import methods
#'
#' @examples
#'   cmr <- mapMCDA_datasets()
#'   cmr$cmr_admin3$rv <- risk_unit(cmr$animal.density, cmr$cmr_admin3)
#'   sp::spplot(cmr$cmr_admin3[, "rv"], cuts = 3)
risk_unit <- function(r, eu, fun = mean) {
  rgrid <- methods::as(r, "SpatialGridDataFrame")  # needed for overlay methods
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
    
    # stopifnot(!anyNA(funrisk_poly))
  }
  
  return(funrisk_poly)
}


#' Produce final plot of risk
#' 
#' Represent the value of risk by epidemiological unit
#' categorised into n risk-levels
#'
#' @param x SpatialPolygons*. Epidemiological units.
#' @param v Numeric. Risk values.
#' @param n Number of risk levels.
#'
#' @return spplot
#' @export
#' @import sp
#'
#' @examples
#'   epi_units <- mapMCDA_datasets()$cmr_admin3
#'   risk_plot(epi_units, seq_along(epi_units), n = 4)
risk_plot <- function(x, v, n) {
  x$risk <- v
  spplot(x[, "risk"], cuts = n - 1)
}

#' Produce final table of risk levels by epidemiological unit
#' 
#' Represent the value of risk by epidemiological unit
#' categorised into n risk-levels
#'
#' @param x SpatialPolygons*. Epidemiological units.
#' @param v Numeric. Risk values.
#' @param n Number of risk levels.
#'
#' @return data.frame
#' @export
#' @import sp
#'
#' @examples
#'   epi_units <- mapMCDA_datasets()$cmr_admin3
#'   risk <- runif(nrow(epi_units), 0, 100)
#'   risk_table(epi_units, risk, n = 4)
risk_table<- function(x, v, n) {
  x$risk <- v
  x$risk_cat <- cut(v, n, labels = FALSE)
  
  return(data.frame(x))
}
