#' Compute MCDA weights
#' 
#' Given a square reciprocal-symmetric \emph{ratio} matrix from an Analytic
#' Hierarchy Process, calculate the vector of weights using the using the
#' Pairwise Comparison Method (Saaty 1987, Saaty 1996, Malczewski 1999)
#' 
#' @param x matrix of pair-wise assessment of relative importance of risk
#'   factors.
#'
#' @return numeric vector of weights
#' @export
#' 
#' @references 
#'   Saaty, R. W. (1987). The analytic hierarchy process--what it is and how it
#'   is used. Mathematical Modelling 9 (3-5), 161-176.
#'
#'   Saaty, T. L. (1996). Multicriteria decision making : the analytic hierarchy
#'   process : planning, priority setting, resource allocation. RWS
#'   Publications.
#'   
#'   Malczewski J. GIS and multicriteria decision analysis. Inc New York: John
#'   Wiley & Sons; 1999.
#'
#' @examples
#'   compute_weights(matrix(1, 5, 5))
compute_weights <- function(x) {
  
  is.reciprocal <- function(x) {
    all(diag(x) == 1) && (!any(lower.tri(x)) || x[lower.tri(x)]*x[upper.tri(x)] == 1)
  }
  
  stopifnot(
    is.numeric(x),
    is.matrix(x),
    diff(dim(x)) == 0,
    is.reciprocal(x)
  )
  
  # ## Dummy outcome
  # w_len <- dim(x)[1]
  # w_hat <- rep(1/w_len, w_len) 
  
  ## normalized pairwise comparison matrix
  npcm <- scale(x, center = FALSE, scale = apply(x, 2, sum))

  ## average of the elements in each row of the normalized matrix
  w_hat <- apply(npcm, 1, mean)

  ## What about the first eigenvector:
  # eigen(x)$vectors[,1]
  
  ## TODO: compute consistency index (see Malczewski 1999, p. 199)
  
  return(w_hat)
}