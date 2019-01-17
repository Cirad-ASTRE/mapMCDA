context("Data loading")


test_that("layer_type() correctly identifies file extensions",  {
  file_names <- list(
    vector = "a.gpkg", 
    vector = "b.shp", 
    raster = "c.tif", 
    raster = "d.tiff", 
    network = "e.csv", 
    "NA"     = "z.zzz")

  file_types <- sapply(file_names, layer_type)
  
  expect_true(all(names(file_types[is.na(file_types)]) == "NA"))

  file_types <- file_types[!is.na(file_types)]
  expect_identical(names(file_types), as.character(file_types))
})

test_that("load_layer() reads known formats", {
  
  expect_load <- function(x) {
    eval(bquote(expect_error(load_layer(.(file.path(testdata, x))), NA)))
  }
  
  expect_load("points.shp")   # Shapefile
  expect_load("points.gpkg")  # GeoPackage
  expect_load("raster.tif")   # tiff
  expect_load("mobility.csv") # network
})


test_that("load_dir() skips unknnown file formats", {
  res <- load_dir(testdata)
  
  test_files <- c("mobility", "points", "points", "raster")
  
  expect_is(res, "list")
  expect_identical(names(res), test_files)
  
  expect_identical(
    vapply(res, class, "char")[test_files],
    setNames(
      c("igraph",  "SpatialPointsDataFrame", "SpatialPointsDataFrame", "RasterLayer"),
      test_files
    )
  )
  
})


test_that("Interpret network data", {
  nf <- system.file("testdata", "mobility.csv", package = "mapMCDA")
  expect_error(x <- read_network(nf), NA)
  expect_s3_class(x, "igraph")
  
})
