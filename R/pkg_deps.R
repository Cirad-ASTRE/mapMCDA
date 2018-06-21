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


# Build graph of dependencies
# plot(breedR.deps_graph())
# fname <- '../../doc/breedR-dev/dependencies.gml'
# write_graph(breedR.deps_graph(),
#             file = fname,
#             format = 'gml')
# # in the resulting file, change 'name' by 'label'
# # in order to open it with yEd
# system(paste("sed -i 's/name/label/g'", fname))
deps_graph <- function(which = c('Depends', 'Imports')) {
  if (!require(igraph))
    stop('This requires installing igraph.')
  
  inst <- installed.packages()
  base <- unname(inst[inst[, "Priority"] %in% c("base", "recommended"), 
                      "Package"])
  
  direct_deps <- dev_package_deps(dependencies = NA)$package
  
  deps <- setdiff(direct_deps, base)
  
  # available = available.packages()
  available = inst
  
  dep_list <- list(mapMCDA = deps)
  new_deps <- deps
  count = 0
  while (length(new_deps)>0 && count < 10) {
    all_deps <- tools::package_dependencies(new_deps, db = available)
    deps_clean <- sapply(all_deps, setdiff, base)
    deps_clean <- deps_clean[sapply(deps_clean, length) > 0]
    dep_list <- c(dep_list, deps_clean)
    new_deps <- unique(unname(unlist(deps_clean)))
    count = count + 1
  }
  
  dep_full <- 
    unname(
      unlist(
        sapply(names(dep_list),
               function(x) as.vector(sapply(dep_list[[x]], 
                                            function(y) c(x, y)))
        )
      )
    )
  
  return(make_graph(dep_full))
}

# require(visNetwork)
# visIgraph(deps_graph())
