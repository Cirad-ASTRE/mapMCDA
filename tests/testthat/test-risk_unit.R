context("risk_unit")

## 10 x 10 raster with sequential values
r <- raster(nrow = 10, ncol = 10)
r[] <- seq.int(ncell(r))

## polygons covering the region by squares of 2x2 pixels
r0 <- r
res(r0) <- 2*res(r)
epiunits <- as(r0, "SpatialPolygons")

## index for the polygons (for each pixel, what block it belongs to)
block <- 
  do.call(
    paste0,
    expand.grid(x = rep(1:5, each = 2), y = rep(1:5, each = 2))
  )
block <- factor(block, levels = unique(block))

test_that("risk_unit returns a vector of length n_polygons", {

  riskvalues <- risk_unit(r, epiunits)  

  expect_length(riskvalues, length(epiunits))  
  expect_false(any(is.na(riskvalues)))

})

test_that("alternative risk summaries", {
  
  expect_summary <- function(fun) {
    expect_identical(
      risk_unit(r, epiunits, fun = fun),
      as.vector(tapply(getValues(r), block, fun))
    )
  }
  
  expect_summary(mean)
  expect_summary(median)
  expect_summary(min)
  expect_summary(max)
  expect_summary(sum)

})
