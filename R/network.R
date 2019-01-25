#' Read and interpret network data
#' 
#' Read a csv file with a specific format (see Details) and intepret
#' it as network directed data with possibly weighted edges.
#' 
#' The file must be plain text with comma-separated columns and varaible
#' names in the first line.
#' There must be either 6 or 7 columns in the same order and of the
#' same types as follows:
#' \itemize{
#'   \item origin       character
#'   \item destination  character
#'   \item lon_orig     numeric (decimal degrees, WGS84)
#'   \item lat_orig     numeric (decimal degrees, WGS84)
#'   \item lon_dest     numeric (decimal degrees, WGS84)
#'   \item lat_dest     numeric (decimal degrees, WGS84)
#'   \item volume       Optional. directed flux in some consistent unit
#' }
#' 
#' Variable names can be different. If strings contain spaces they must
#' be quoted.
#' 
#' @param x character. Path of csv file
#'
#' @return object of class \code{igraph}
#' @importFrom igraph graph_from_data_frame edge_attr_names
#'   edge.attributes edge.attributes<-
#' @importFrom stats setNames
#' @importFrom utils read.csv
#' @export
#'
#' @examples
#'   d <- data.frame(from = "A", to = "B", fx = 0, fy = 0, tx = 1, ty = 1)
#'   tf <- tempfile()
#'   write.csv(d, tf, row.names = FALSE)
#'   read_network(tf)
read_network <- function(x) {
  
  ## Read file
  dat <- read.csv(x, stringsAsFactors = FALSE)
  
  ## Message on input format
  input_format <- c(
    "Input data format:",
    "Comma-separated plain text file with variable names in the first row.",
    "Decimal point symbol: dot.",
    "Variable names can be different, but the order and types
must be respected:",
    "",
    "origin       character",
    "destination  character",
    "lon_orig     numeric (decimal degrees, WGS84)",
    "lat_orig     numeric (decimal degrees, WGS84)",
    "lon_dest     numeric (decimal degrees, WGS84)",
    "lat_dest     numeric (decimal degrees, WGS84)",
    "volume       Optional. directed flux in some consistent unit."
  )

  ## Check number of columns
  if (ncol(dat) < 6 | ncol(dat) > 7) {
    stop("Expected 6 or 7 columns, observed ", ncol(dat),
         message(paste(input_format, collapse = "\n")))
    
  }
  
  ## Check type of columns
  ## transform any column of class 'integer' to 'numeric'
  class(dat[vapply(dat, is.integer, T)]) <- "numeric"
  col_classes <- sapply(dat, class)
  exp_classes <- c(rep("character", 2), rep("numeric", length(col_classes)-2))
  
  if( any(idx <- col_classes != exp_classes) ) {
    nms.idx <- paste(names(dat)[idx], collapse = ", ")
    stop(
      "Unexpected type in column(s) ", nms.idx,
      message(paste(input_format, collapse = "\n"))
    )
  }
  
  
  ## Check duplicated edges
  if ( any(idx <- duplicated(dat[, 1:2])) ) {
    print(dat[which(idx), 1:2])
    stop("The links above appear more than once")
  }
  
  ## Check graph is directed
  ## At least one edge contains a reciprocal
  if( !any(duplicated(rbind(dat[, 1:2], dat[, 2:1]))) ) {
    stop(
      "All the links are unidirectional.\n",
      "If you are sure that the network is directed, add at least one
      reciprocal link with a weight of 0 and proceed."
    )
  }
  
  ## Links
  e_list <- dat[, -(3:6)]
  
  ## Coordinates of vertices
  v_coord <- unique(
    rbind(
      setNames(dat[, c(1, 3:4)], c("Node", "Lon", "Lat")),
      setNames(dat[, c(2, 5:6)], c("Node", "Lon", "Lat"))
    )
  )
  v_coord <- v_coord[order(v_coord$Node),]
  v_coord$Node <- as.factor(v_coord$Node)
  
  ## Check consistency of coordinates
  if (any((idx <- tapply(v_coord$Node, v_coord$Node, length)) > 1)) {
    idx.nodes <- names(idx)[idx > 1]
    stop("Node(s) ", idx.nodes, " have inconsistent coordinates in the dataset.")
  }
  
  ans <- igraph::graph_from_data_frame(e_list, vertices = v_coord, directed = TRUE)
  
  ## If there are edge attributes, it must be only one, and must be
  ## the weight.
  if((natt <- length(edge_attr_names(ans))) > 0) {
    stopifnot(identical(natt, 1L))
    names(edge.attributes(ans)) <- "weight"
  }

  return(ans)
}

# ' @import igraph
setOldClass("igraph")


#' Rasterize a geographic network
#' 
#' Copmute a raster with the importance of the nearest network node.
#' 
#' This assumes that the network object has node attributes "Lon" "Lat"
#' in the WGS84 reference system.
#' 
#' If the graph is weighted, the weighted importance will be computed.
#' If you want the unweighted importance, remove the weight attribute
#' from the graph with \code{igraph::delete_edge_attr(x, "weight")}.
#' 
#' @param x a geographic network (a igraph object with node attributes
#'   "Lon" and "Lat" in the WGS84 reference system)
#' @param y a Raster* or a SpatialPolygons* object.
#' @param field character. The attribute of the network nodes to be
#'   assigned to the voronoi polygon associated with each node. By
#'   default it computes the \emph{importance} of each node in the
#'   network. But you can manually compute any other measure of
#'   interest, and pass it as a vertex attribute (see
#'   \code{?igraph::`vertex_attr<-`}).
#' @param ... Other arguments passed on to
#'   \code{\link[raster]{rasterize}}.
#' @importMethodsFrom raster rasterize
#' @importFrom stats setNames
#' @export
#' @name rasterize_geonetwork
#' @rdname rasterize_geonetwork
#' @aliases rasterize,igraph,Raster-method
setMethod(
  rasterize, c("igraph", "Raster"),
  {
    function(
      x, y, field = "importance", ...) {
      
      etx <- epidemic_threshold(x, beta = 1)
      
      ## If the graph is weighted, use weights
      if (is.null(epiR0 <- etx$weighted)) {
        epiR0 <- etx$unweighted
      }
      
      sna <- attr(epiR0, "sna")

      node_importance <- setNames(
        data.frame(sna[, 1], 100 * sna$R0k / epiR0["R0"]),
        c("name", "importance")
      )
      
      nodes <- as.data.frame(igraph::vertex.attributes(x))
      
      nodes <- merge(nodes, node_importance, by = "name", all.y=FALSE)
      
      ## Cast to SpatialPointsDataFrame
      ## Assume CRS WGS84
      coordinates(nodes) <- ~ Lon + Lat
      proj4string(nodes) <- CRS("+proj=longlat +datum=WGS84")
      
      vor <- voronoi(nodes, ext = extent(y))
    
      ans <- raster::mask(raster::rasterize(vor, y, field = field, ...), y)
      return(ans)
    }
  }
)

#' @importMethodsFrom raster rasterize
#' @param res numeric vector of length 2. Resolution in Lon/Lat of
#'   rasterisation.
#' @rdname rasterize_geonetwork
#' @aliases rasterize,igraph,SpatialPolygons-method
#' @export
setMethod(
  rasterize, c("igraph", "SpatialPolygons"),
  {
    function (x, y, res = resolution(y, min_ncells = 100), ...) {
      
      ext_grid <- raster::raster(raster::extent(y), resolution = res)
      msk <- raster::rasterize(y, ext_grid, field = 1, background = NA, fun = "mean")
      rasterize(x, msk, ...)
    }
  }
)

#' Compute the Epidemic Threshold of a graph
#' 
#' Weighted and unweighted Epidemic Threshold \eqn{q} of a graph.
#'
#' The Epidemic Threshold \eqn{q} quantifies the minimal expeced
#' transmission coefficient necessary for diffusing an epidemy in a
#' network. It is computed as the inverse of the \emph{Potential for
#' transmission} of the network: a measure of the expected number of
#' nodes affected by an infectious node, which is a generalisation of
#' the Basic Reproduction Number \eqn{R_0}{R₀} of an epidemy to the
#' context of a network. It thus quantifies the potential for
#' transmission of an infection throughout the contact network. It is
#' computed in terms of the incoming-outgoing rates from the network's
#' nodes: \deqn{R_0 = \beta \frac{\hat{k_\text{in}
#' k_\text{out}}}{\hat{k_\text{in}}},}{R₀ = \beta〈k_in*k_out〉/〈k_in〉,}
#' where \eqn{\beta} is the transmission coefficient among animals,
#' \eqn{k_\text{in/out}}{k_in/out} are the in/out-degrees of a node
#' and the \eqn{\hat{\cdot}}{〈·〉} symbol represents the average value
#' across all nodes in the graph.
#'
#' The unweighted value computed above is most appropriate for a
#' highly infectious epidemy with high animal-prevalence on nodes, as
#' it assumes that any contact is potentially infectious.
#'
#' In the weighted formulation, \eqn{k_\text{in/out}}{k_in/out} are
#' the weight values for the incoming/outgoing edges in each node. It
#' is more appropriate for low-prevalence diseases, where the
#' transmission probability is assumed proportional to the number of
#' contacts.
#'
#' The default value of 1 for the probability of transmission
#' \code{beta} implies that every infectious contact leads to
#' transmission.
#'
#' @param x an \code{igraph} object
#' @param beta numeric, between 0 and 1. Probability of transmission.
#'
#' @return a list the weighted and unweighted Potential for
#'   Transmission \eqn{R_0}{R₀} and its inverse, the Epidemic
#'   Threshold \eqn{q}. As an attribute named "sna", a data.frame with
#'   the in/out-degrees of each node and their individual contribution
#'   to R0.
#' 
#' If the input graph is unweighted, the weighted component is NULL.
#' 
#' @importFrom igraph decompose.graph degree gorder is.weighted
#'   as_data_frame
#' @importFrom stats setNames
#'   
#' @references 
#'   Volkova VV, Howey R, Savill NJ, Woolhouse MEJ (2010) Sheep
#'   Movement Networks and the Transmission of Infectious Diseases.
#'   PLoS ONE 5(6): e11185.
#'   https://doi.org/10.1371/journal.pone.0011185
#' @export
#' @examples
#'   g <- igraph::graph_from_literal(A --+ B --+C, A --+C, B --+D)
#'   epidemic_threshold(g)
#'   
#'   ## weighted graph
#'   igraph::E(g)$weight <- c(10, 1, 2, 5)
#'   epidemic_threshold(g)
epidemic_threshold <- function(x, beta = 1) {
  UseMethod("epidemic_threshold")
}

#' @export
epidemic_threshold.igraph <- function(x, beta = 1) {
  sc <- decompose.graph(x)
  n_v <- lapply(sc, gorder)

  ## work only with the largest network
  ## (in terms of number of nodes)  
  largest_x <- sc[[which.max(n_v)]]
  
  sna <- 
    data.frame(
      indeg = degree(largest_x, mode = "in"),
      outdeg = degree(largest_x, mode = "out")
    )
  sna <- cbind(
    node = rownames(sna),
    sna
  )
  rownames(sna) <- NULL
  sna[is.na(sna)] <- 0
  
  R0_contrib <- epidemic_threshold(sna[, c("indeg", "outdeg")])
  epidata <- setNames(sum(R0_contrib)^c(1, -1), c("R0", "q"))
  attr(epidata, "sna") <- cbind(sna, R0k = R0_contrib)
  
  epidataw <- NULL
  if (is.weighted(largest_x)) {
    
    lxdf <- as_data_frame(largest_x)
    in_w <- setNames(
      aggregate(lxdf$weight, by = list(lxdf$to), sum),
      c("node", "in_w")
    )
    out_w <- setNames(
      aggregate(lxdf$weight, by = list(lxdf$from), sum),
      c("node", "out_w")
    )
    snaw <- merge(in_w, out_w, by = "node", all=TRUE)
    snaw[is.na(snaw)] <- 0
    
    R0w_contrib <- epidemic_threshold(snaw[, c("in_w", "out_w")])
    epidataw <- setNames(sum(R0w_contrib)^c(1, -1), c("R0", "q"))
    attr(epidataw, "sna") <- cbind(snaw, R0k_w = R0w_contrib)
    }

  return(list(unweighted = epidata, weighted = epidataw))
}

#' @export
epidemic_threshold.data.frame <- function(x, beta) {
  stopifnot(ncol(x) == 2)
  ## Individual contributions to R0
  return( x[,1]*x[,2]/sum(x[,1]) )
}

