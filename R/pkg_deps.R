#' mapMCDA package dependencies
#' 
#' The list of packages has been recovered after the installation of the package
#' on a clean Windows 7 virtual machine. It also gathers some further packages
#' installed in Appveyor.
#'
#' @return character vector of package names
#'
pkg_deps <- function(which = c('Depends', 'Imports')) {

  direct_deps <- devtools::dev_package_deps()$package
  inst <- utils::installed.packages()
  base <- unname(inst[inst[, "Priority"] %in% c("base", "recommended"), 
                      "Package"])
  
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
  
  Rver = gsub(
    "\\.\\d$", "",
    c(release = rversions::r_release()$version,
      oldrel = rversions::r_oldrel()$version)
  )

  contrib_version <- function(type, ver) {
    gsub(
      "\\d\\.\\d$", ver,
      contrib.url(getOption("repos")["CRAN"], type)
    )
  }
  
  contribs <- expand.grid(type = type, ver = Rver, stringsAsFactors = FALSE)
  contribs$conturl <- purrr::map2(contribs$type, contribs$ver, contrib_version)
  contribs <- contribs[!duplicated(contribs[, c("type", "conturl")]),]
  
  ans <- structure(vector("list", nrow(contribs)), names = contribs$type)
  for (i in seq_len(nrow(contribs))) {
    dest <- file.path(dir, paste(contribs[i, c("type", "ver")], collapse = "-"))
    dir.create(dest)
    ans[[i]] <- utils::download.packages(
      pkgs,
      destdir = dest,
      repos = getOption("repos")["CRAN"],
      contriburl = contribs[i, "conturl"],
      type = contribs[i, "type"]
    )
  }
  
  message('Done. ',
          'Don\'t forget to include also the ',
          'source and compiled packages for mapMCDA.')
  
  return(do.call("rbind", ans))
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

  direct_deps <- devtools::dev_package_deps()$package
  available <- utils::available.packages()

  base <- unname(available[available[, "Priority"] %in% c("base", "recommended"), 
                      "Package"])
  
  deps <- setdiff(direct_deps, base)
  
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
  
  return(igraph::make_graph(dep_full))
}

# require(visNetwork)
# visIgraph(deps_graph())


## Usage:
## repo <- file.path('file://', path.expand('~/t4f/pkgrepo'))
## available.packages(contrib.url(repo, 'source'))
## install.packages(pkgs[, 'Package'], repos = repo)
update_drat <- function(
  repodir  = '~/Work/Proyectos/prodel/pkgrepo',
  win.binary = NULL
) {
  file <- devtools::build(vignettes = TRUE)
  drat::insertPackage(file, repodir)
  
  srcdir <- tempdir()
  downloaded_pkgs <- download_deps(dir = srcdir, type = 'all')
  for (fn in downloaded_pkgs[, 2]) {
    drat::insertPackage(fn, repodir, action = "prune")
  }
  
  if (!is.null(win.binary)) {
    drat::insertPackage(win.binary, repodir)
  }
}
