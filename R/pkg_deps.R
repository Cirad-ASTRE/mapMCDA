#' mapMCDA package dependencies
#' 
#' The list of packages has been recovered after the installation of the package
#' on a clean Windows 7 virtual machine. It also gathers some further packages
#' installed in Appveyor.
#'
#' @return character vector of package names
#'
pkg_deps <- function() {
  deps <- system.file(file.path("doc", "mapMCDA_deps"), package = "mapMCDA")
  deps <- readLines(deps)
  return(deps)
}