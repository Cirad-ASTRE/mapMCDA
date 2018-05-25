
#' Download Country Administrative Borders from GADM
#'
#' @param country_name character. A valid country name.
#' @param level integer 0:3.
#' @param path character. Where to store the downloaded object.
#'
#' @return A SpatioalPolygonsDataFrame object.
#' 
#' @export
#' 
#' @import raster
#'
#' @examples
#'   cmr <- download_country_admin("Cameroon")
download_country_admin <- function(country_name, level = 3, path = tempdir()) {
  country_iso3 <- getData("ISO3")
  iso3 <- country_iso3$ISO3[country_iso3$NAME == country_name]
  res <- try(
    suppressWarnings(
      getData('GADM', country = iso3, level = level, path = path)
    ),
    silent = TRUE
  )
    
  if (inherits(res, "try-error")) {
    if (grepl("cannot open URL", res))
      stop("Could not connect to gadm.org to download the cartography.\n",
           "Please check your internet connection.")
  }
}
