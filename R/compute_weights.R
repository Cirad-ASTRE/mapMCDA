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
  
  stopifnot(
    is.numeric(x),
    is.matrix(x),
    diff(dim(x)) == 0,
    is.reciprocal(x),
    all(x > 0)
  )
  
  # ## Dummy outcome
  # w_len <- dim(x)[1]
  # w_hat <- rep(1/w_len, w_len) 
  
  ## Algorithm from Malczewski, 1999:
  ## normalized pairwise comparison matrix
  # npcm <- scale(x, center = FALSE, scale = apply(x, 2, sum))
  # 
  # ## average of the elements in each row of the normalized matrix
  # w_hat <- apply(npcm, 1, mean)

  ## What about the first eigenvector (mentioned before in the book)?
  ev1 <-  eigen(x)$vectors[,1]
  w_hat <- Re(ev1)/sum(Re(ev1))
  
  ## MCDA::AHP uses the following iterative algorithm for the weights instead
  ## It gives the same results as the first normalised eigenvector
  # pairwisematrix <- x %*% x
  # sumrows <- rowSums(x)
  # sumtotal <- sum(sumrows)
  # normalisedsumrows <- sumrows/sumtotal
  # previous <- vector()
  # while (!identical(round(previous, digits = 10),
  #                   round(normalisedsumrows, digits = 10))) {
  #   previous <- normalisedsumrows
  #   pairwisematrix <- pairwisematrix %*% pairwisematrix
  #   sumrows <- rowSums(pairwisematrix)
  #   sumtotal <- sum(sumrows)
  #   normalisedsumrows <- sumrows/sumtotal
  # }
  # weights <- normalisedsumrows
  
  ## Looks like MCDA package is consistent with the normalised first
  ## eigenvector. So we are using it.
  
  ## TODO: compute consistency index (see Malczewski 1999, p. 199)
  
  return(w_hat)
}