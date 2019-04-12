context("test-risk_plot")

polys <- as(GridTopology(c(0,0), c(1,1), c(3,2)), "SpatialPolygons")
# plot(polys)

test_that("Expected plot", {
  res <- expect_error(risk_plot(polys, seq_along(polys), n = 2), NA)
  expect_is(res, "trellis")
})

test_that("Some NA values are tolerated", {
  v <- seq_along(polys)
  v[1] <- NA
  
  res <- expect_error(risk_plot(polys, v, n = 2), NA)
  expect_is(res, "trellis")
})

test_that("All NA values raise error", {
  v <- NA[seq_along(polys)]
  
  expect_error(risk_plot(polys, v, n = 2), "missing")
})

