#' Check reciprocal matrix
#' 
#' Check whether a matrix is reciprocal. This is, whether the
#' entrywise product of itself with its transpose (Hadamard product)
#' is 1 for all its elements.
#' 
#' @param x square numeric matrix or number
#'
#' @return logical
#'
#' @examples
#'   Xf <- matrix(1:16, 4, 4)
#'   Xr <- Xf
#'   Xr[upper.tri(Xf)] <- 1/t(Xf)[upper.tri(Xf)]
#'   diag(Xr) <- 1
#'   is.reciprocal(Xf)  # FALSE
#'   is.reciprocal(Xr)  # TRUE
is.reciprocal <- function(x) {
  all(x * t(x) == 1)
}

