#' Voronoi polygons
#' 
#' Compute a vornoi tesselation of a map for a set of points.
#' 
#' 
#' @param x SpatialPoints* or two-column matrix or data.frame with x and y coordinates.
#' @param ext Extent object.
#' @param eps  Numerical tolerance used in triangulation. See \code{\link[deldir]{deldir}}.
#' 
#' @author Borrowed from function \code{voronoi} from
#'   package \code{dismo} by R. Hijmans et al., which ultimately
#'   uses the \code{\link[deldir]{deldir}} function by Rolf Turner.
#' @return SpatialPolygonsDataFrame.
#' @import deldir
#' @importFrom raster intersect
#' @importFrom rgeos gUnaryUnion
#'
#' @export
#' @examples
#'   pts <- data.frame(x = c(1:2, 2), y = c(1, 1:2))
#'   region <- raster::extent(c(0, 3, 0, 3))
#'   sp::plot(voronoi(pts, region))
#'   points(pts)
voronoi <- function(x, ext, eps = 1e-09) {
  if (!requireNamespace("deldir")) {
    stop("you need to first install the deldir libary")
  }
  dat <- NULL
  sp <- FALSE
  if (inherits(x, "Spatial")) {
    if (.hasSlot(x, "data")) {
      dat <- slot(x, "data")
    }
    prj <- proj4string(x)
    sp <- TRUE
    xy <- coordinates(x)
    dups <- duplicated(xy)
    if (any(dups)) {
      xy <- xy[!dups, , drop = FALSE]
      dat <- dat[!dups, , drop = FALSE]
    }
  }
  else {
    xy <- stats::na.omit(x[, 1:2])
    xy <- unique(xy)
  }
  
  ext <- as.vector(ext)
  
  z <- deldir::deldir(xy[, 1], xy[, 2], rw = ext, eps = eps, 
                      suppressMsge = TRUE)
  index <- z$ind.orig
  w <- deldir::tile.list(z)
  polys <- vector(mode = "list", length = length(w))
  for (i in seq(along = polys)) {
    pc <- cbind(w[[i]]$x, w[[i]]$y)
    pc <- rbind(pc, pc[1, ])
    polys[[i]] <- Polygons(list(Polygon(pc)), as.character(index[i]))
  }
  if (sp) {
    polys <- SpatialPolygons(polys, proj4string = CRS(prj))
  }
  else {
    polys <- SpatialPolygons(polys)
  }
  if (is.null(dat)) {
    dat <- data.frame(id = index)
  }
  else {
    dat <- dat[index, , drop = FALSE]
  }
  rownames(dat) <- row.names(polys)
  polys <- SpatialPolygonsDataFrame(polys, data = dat)
  
  return(polys)
}
