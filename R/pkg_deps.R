#' mapMCDA package dependencies
#' 
#' The list of packages has been recovered after the installation of the package
#' on a clean Windows 7 virtual machine. It also gathers some further packages
#' installed in Appveyor.
#'
#' @return character vector of package names
#'
pkg_deps <- function() {

  ## stored packages
  depsfile <- system.file(file.path("doc", "mapMCDA_deps"), package = "mapMCDA")
  deps <- readLines(depsfile)

  available <- available.packages()
  inst <- installed.packages()

  direct_deps <- dev_package_deps(dependencies = NA)$package

  indirect_deps <- tools::package_dependencies(direct_deps, db = available.packages(), recursive = TRUE)

  deps <- Reduce(union, c(list(mapMCDA = direct_deps), indirect_deps))

  base <- inst[inst[, "Priority"] %in% c("base", "recommended"), "Package"]
  
  deps <- setdiff(deps, base)

  return(deps)
}



# Download (recursive) dependencies
# specify where to store packages and type
download_deps <- function(dir, type = 'all', pkgs = pkg_deps()) {
  
  typelst <- c('source', 'win.binary', 'mac.binary', 'mac.binary.el-capitan')
  type <- match.arg(type, c('all', typelst), several.ok = TRUE)
  if ('all' %in% type) type <- typelst
  
  for (t in type) {
    dest <- file.path(dir, t)
    dir.create(dest)
    download.packages(pkgs, destdir = dest, type = t)
  }
  
  message('Done. ',
          'Don\'t forget to include also the ',
          'source and compiled packages for mapMCDA.')
}

# download_deps("../prodel/Rpkgs", type = "all", pkgs = pkg_deps())

