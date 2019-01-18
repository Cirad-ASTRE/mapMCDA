context("Network data")


## Input data format (assumed)
## variable names can be different, but the order and types
## must be respected:
## 
## origin       character
## destination  character
## lon_orig     numeric (decimal degrees, WGS84)
## lat_orig     numeric (decimal degrees, WGS84)
## lon_dest     numeric (decimal degrees, WGS84)
## lat_dest     numeric (decimal degrees, WGS84)
## volume       Optional. directed flux in some consistent unit.

test_that("Computation of the epidemic threshold", {
  
  ## Unweighted graph with two nodes
  g <- igraph::graph_from_literal(A --+ B --+ C)
  etg <- epidemic_threshold(g)
  sna <- attr(etg$unweighted, "sna")
  
  expect_true(is.null(etg$weighted))
  expect_equal(
    etg$unweighted,
    c(R0 = 0.5, q = 2),
    check.attributes = FALSE
  )
  expect_is(sna, "data.frame")
  expect_identical(dim(sna), c(length(igraph::V(g)), 4L))

  
  ## Weighted graph
  igraph::E(g)$weight <- c(10)
  etw <- epidemic_threshold(g)
  snw <- attr(etw$weighted, "sna")
  
  expect_true(!is.null(etw$weighted))
  expect_equal(
    etw$weighted,
    c(R0 = 5, q = .2),
    check.attributes = FALSE
  )
  expect_is(snw, "data.frame")
  expect_identical(dim(snw), c(length(igraph::V(g)), 4L))

  ## data.frame (2-col with in/out degrees)
  expect_identical(
    epidemic_threshold(sna[, 2:3]),
    sna[, 4]
  )
  expect_identical(sum(sna[, 4]), etg$unweighted[["R0"]])

  expect_identical(
    epidemic_threshold(snw[, 2:3]),
    snw[, 4]
  )
  expect_identical(sum(snw[, 4]), etw$weighted[["R0"]])
})
