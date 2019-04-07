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

test_that("Network rasterisation", {
  
  r <- raster(nrow = 10, ncol = 10, xmn = 0, xmx = 10, ymn = 0, ymx = 10, vals = 1)
  
  ## A geonetwork with a single source and several destinations:
  ## A -> B; A -> C; A -> D
  edges <- data.frame(
    O = rep("A", 3),
    D = LETTERS[2:4],
    stringsAsFactors = FALSE
  )
  nodes <- data.frame(
      node = LETTERS[1:4],
      x = rep(c(2, 8), times = 2),
      y = rep(c(2, 8), each = 2)
    )
  gn <- geonetwork::geonetwork(edges, nodes)
  
  # plot(r)
  # plot(gn, add = TRUE)
  
  gnr <- rasterize(gn, r)
  # plot(gnr)

  ## Constant value of 1/N_nodes * 100
  expect_equal(diff(range(values(gnr))), 0)
  expect_equal(values(gnr)[1], 100 / nrow(nodes))

  ## Add an outgoing link from B -> D
  gn2 <- geonetwork::geonetwork(rbind(edges, c("B", "D")), nodes)
  gnr2 <- rasterize(gn2, r)
  # plot(gnr2)
  
  ## Now, only B accounts for 100 of the contribution to the R0
  ## The value at B must be 100 while the values at the other nodes 0
  vals_at_B <- extract(gnr2, nodes[nodes$node == "B", -1])
  vals_not_B <- extract(gnr2, nodes[nodes$node != "B", -1])
  expect_equal(vals_at_B, 100)
  expect_true(all(vapply(vals_not_B, all.equal, TRUE, 0)))
})
