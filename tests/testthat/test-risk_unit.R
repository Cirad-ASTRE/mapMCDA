context("risk_unit")

## 10 x 10 raster with sequential values
r <- raster(xmn=-110, xmx=-90, ymn=40, ymx=60, nrow = 10, ncol = 10)
r <- setValues(r, seq.int(ncell(r)))

## polygons covering the region by squares of 2x2 pixels
r0 <- r
res(r0) <- 2*res(r)
epiunits <- as(r0, "SpatialPolygons")

test_that("risk_unit returns a vector of length n_polygons", {

  riskvalues <- risk_unit(r, epiunits)  

  expect_length(riskvalues, length(epiunits))  
  expect_false(any(is.na(riskvalues)))

})

test_that("alternative risk summaries", {

  ## index for the polygons (for each pixel, what block it belongs to)
  block <- 
    do.call(
      paste0,
      expand.grid(x = rep(1:5, each = 2), y = rep(1:5, each = 2))
    )
  block <- factor(block, levels = unique(block))
  

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


test_that("Missing CRS in epidemiological units", {
  
  eu_na <- epiunits
  proj4string(eu_na) <- CRS()

  ## If no CRS but possibly geographical coordinates, 
  ## assume them, and yield results with a Warning
  res1 <- expect_warning(risk_unit(r, eu_na), "CRS is NA")
  expect_true(!any(is.na(res1)))
  
  ## Also, reproject if necessary to match CRS of r
  # proj.4 projection description
  newproj <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
  rt <- suppressWarnings(projectRaster(r, crs = CRS(newproj)))
  
  res2 <- expect_warning(risk_unit(rt, eu_na), "CRS is NA")
  expect_true(!any(is.na(res2)))
  
  ## However, if the CRS is missing but coordinates are projected,
  ## fail.
  eu_pr <- epiunits
  eu_pr <- spTransform(eu_pr, CRS(newproj))
  proj4string(eu_pr) <- CRS()
  
  expect_error(risk_unit(rt, eu_pr), "Missing Coordinate Reference System")
})

