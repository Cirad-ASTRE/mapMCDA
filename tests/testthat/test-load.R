context("load")

tdir <- tempfile()
dir.create(tdir)

vector <- SpatialPointsDataFrame(
  cbind(1:5, 1:5),
  data.frame(a = 1:5)
)
writeOGR(vector, tdir, "points", driver = "ESRI Shapefile")
writeRaster(raster(matrix(1:4, 2, 2)), file.path(tdir, "raster.tif"))


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


test_that("load_layer() reads shapefiles", {

  expect_error(load_layer(file.path(tdir, "points.shp")), NA)
})


test_that("load_dir() skips unknnown file formats", {
  res <- load_dir(tdir)
  
  expect_is(res, "list")
  expect_identical(names(res), c("points", "raster"))
  expect_is(res[[1]], "SpatialPointsDataFrame")
  expect_is(res[[2]], "RasterLayer")
})
