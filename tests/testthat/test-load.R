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
  exp_classes <- c("geonetwork",  "SpatialPointsDataFrame", "SpatialPointsDataFrame", "RasterLayer")
  
  expect_is(res, "list")
  expect_identical(names(res), test_files)
  
  expect_true(all(mapply(inherits, res, exp_classes)))
  
})


test_that("Interpret network data", {
  nf <- system.file("testdata", "mobility.csv", package = "mapMCDA")
  x <- expect_error(read_network(nf), NA)
  expect_s3_class(x, "igraph")
  
  ## Test data with integer and double numeric columns
  td <- data.frame(
    from = LETTERS[1:3],
    to = LETTERS[2:4],
    fx = 1:3 + .1,   # double
    fy = 1:3L,       # int
    tx = 2:4 + .1,
    ty = c(2:3, 1),
    hc = 10 * (1:3)
  )
  tf <- tempfile()
  write.csv(td, tf, row.names = FALSE)
  
  n1 <- expect_error(read_network(tf), NA)
  expect_s3_class(n1, "igraph")

  ## Test data with ";" field separator and "," decimal separator
  tf2 <- tempfile()
  writeLines(gsub(",", ";", readLines(tf)), file(tf2))
  writeLines(gsub("\\.", ",", readLines(tf2)), file(tf2))
  n2 <- expect_error(read_network(tf2), NA)
  expect_s3_class(n2, "igraph")

  # all.equal(n1, n2)

  close(file(tf))
  close(file(tf2))
})
