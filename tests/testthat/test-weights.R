context("weights")

test_that("compute weights", {
  x <- matrix(1, 10, 10)
  expect_identical(
    compute_weights(x),
    rep(1/nrow(x), nrow(x))
  )
  
  x <- matrix(1, 1, 1)
  expect_identical(
    compute_weights(x),
    1
  )
  
  x <- matrix(1:9, 3, 3)
  expect_error(
    compute_weights(x),
    "not TRUE"
  )

  ## example from Malczewski 1999, p. 183
  x <- diag(3)  
  x[upper.tri(x)] <- c(4, 7, 5)
  x[lower.tri(x)] <- 1/c(4, 7, 5)
  exp_w <- c(0.675, 0.252, 0.073)
  expect_equal(compute_weights(x), exp_w, tol = .001)
})
