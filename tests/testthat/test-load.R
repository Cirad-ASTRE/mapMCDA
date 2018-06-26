context("load")

tdir <- tempfile()
dir.create(tdir)

vector <- SpatialPointsDataFrame(
  cbind(1:5, 1:5),
  data.frame(a = 1:5)
)
writeOGR(vector, tdir, "points", driver = "ESRI Shapefile")
writeRaster(raster(matrix(1:4, 2, 2)), file.path(tdir, "raster.tif"))

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
