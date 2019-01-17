context("Network data")

admin <- mapMCDA_datasets()$cmr_admin3

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


test_that("Interpret network data", {
  nf <- system.file("testdata", "mobility.csv", package = "mapMCDA")
  expect_error(x <- read_network(nf), NA)
  expect_s3_class(x, "igraph")

})
