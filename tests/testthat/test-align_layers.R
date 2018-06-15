context("align_layers")

animald <- mapMCDA_datasets()$animal.density

test_that("handle rasters without projection information", {
  
  x <- list(animald, animald)
  proj4string(x[[1]]) <- NA
  expect_equal(length(unique(lapply(align_layers(x), proj4string))), 1)
  
})

test_that("harmonise rasters with different projections", {
  
  ## Albers Equal Area Africa 1 CRS
  ## http://spatialreference.org/ref/sr-org/8476/
  proj.srorg8476 <- "+proj=aea +lat_1=36.5 +lat_2=29.071428571429 +lat_0=32.7857142857145 +lon_0=-14.111111111111 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  adp <- suppressWarnings(projectRaster(animald, crs = CRS(proj.srorg8476)))
  aligned <- suppressWarnings(align_layers(list(animald, adp, animald)))
  expect_identical(
    unique(vapply(aligned, proj4string, character(1))),
    proj4string(animald)
  )
})

test_that("harmonise resolution and extents", {
  ## TODO
})