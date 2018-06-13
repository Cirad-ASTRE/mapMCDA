context("align_layers")

animald <- mapMCDA_datasets()$animal.density

test_that("handle rasters without projection information", {
  
  x <- list(animald, animald)
  proj4string(x[[1]]) <- NA
  expect_equal(length(unique(lapply(align_layers(x), proj4string))), 1)
  
})

test_that("harmonise rasters with different projections", {
  
  adp <- projectRaster(animald, crs = CRS("+init=epsg:26978"))
  aligned <- align_layers(list(animald, adp, animald))
  expect_identical(
    unique(vapply(aligned, proj4string, character(1))),
    proj4string(animald)
  )
})

test_that("harmonise resolution and extents", {
  ## TODO
})