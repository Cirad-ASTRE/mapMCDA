#' Load packaged datasets into memory
#' @import raster, rgdal
#'
#' @return list of spatial layers
#' @export
#'
#' @examples
#'   cmr <- mapMCDA_datasets()
mapMCDA_datasets <- function() {
  pkg_files <- list.files(
    system.file(
      "cartography/CMR/",
      package = "mapMCDA"
    ),
    full.names = TRUE
  )
  
  layer_name <- basename(pkg_files)
  layer_noext <- gsub("\\.\\w{1,}$", "", layer_name)
  layer_type <- vapply(layer_name, layer_type, character(1))
  
  load_f <- c(vector = "readOGR", raster = "raster")
  
  ans <- mapply(function(x, y) do.call(x, list(y)), load_f[layer_type], pkg_files)
  names(ans) <- layer_noext
  
  return(ans)
}
