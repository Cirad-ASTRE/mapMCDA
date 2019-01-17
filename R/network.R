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
#' @importFrom igraph graph_from_data_frame
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
  
  return(ans)
}
