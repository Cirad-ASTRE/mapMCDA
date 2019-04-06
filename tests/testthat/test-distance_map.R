context("Distance maps")

bnd <-
  SpatialPolygons(list(Polygons(list(Polygon(
    cbind(c(0, 10, 10, 0, 0), c(0, 0, 10, 10, 0))
  )),
  "Boundary")),
  pO = 1L,
  proj4string = CRS("+proj=longlat"))

## Raster Mask
rmsk <- raster(
  nrows = 21,
  ncols = 51,
  xmn = 0,
  xmx = 10,
  ymn = 0,
  ymx = 10,
  crs = crs("+proj=longlat"),
  vals = 1
)

polyA <- Polygon(cbind(c(1, 2, 2, 1, 1), c(1, 1, 2, 2, 1)))
polyB <- Polygon(cbind(c(5, 6, 6, 5, 5), c(3, 3, 4, 4, 3)))
plg1 <-
  SpatialPolygons(list(Polygons(list(polyA),
                                "A")),
                  1L, proj4string = CRS("+proj=longlat"))
plg2 <-
  SpatialPolygons(list(Polygons(list(polyA), "A"),
                       Polygons(list(polyB), "B")),
                  1:2L, proj4string = CRS("+proj=longlat"))

pts1 <- SpatialPoints(cbind(c(3, 7), c(3, 3)), proj4string = CRS("+proj=longlat"))

ln1 <- SpatialLines(list(Lines(Line(cbind(c(1, 9), c(8, 2))), "Line")), proj4string = CRS("+proj=longlat"))

# plot(bnd)
# plot(plg1, col = "blue", add = TRUE)
# plot(plg2, col = "red", add = TRUE)
# plot(pts1, col = "black", add = TRUE)
# plot(ln1, col = "green", add = TRUE)

test_that("Distance from points", {
  ans <- expect_error(
    distance_to_vector(pts1, rmsk),
    NA
  )
  # plot(ans)
  
  expect_is(ans, "RasterLayer")  
  
  expect_true(all(values(ans) >= 0))
})

test_that("Distance from lines", {
  ans <- expect_error(
    distance_to_vector(ln1, rmsk),
    NA
  )
  # plot(ans)

  expect_is(ans, "RasterLayer")  
  
  expect_true(all(values(ans) >= 0))
})

test_that("Distance from polygons", {

  ## Single polygon
  ans1 <- expect_error(
    distance_to_vector(plg1, rmsk),
    NA
  )
  # plot(ans1)

  expect_is(ans1, "RasterLayer")  
  
  expect_true(all(values(ans1) >= 0))

  ## Multiple polygons
  ans2 <- expect_error(
    distance_to_vector(plg2, rmsk),
    NA
  )
  # plot(ans2)

  expect_is(ans2, "RasterLayer")  
  
  expect_true(all(values(ans2) >= 0))

  expect_true(all(values(ans2) <= values(ans1)))
})


test_that("distance_map() takes either a RasterLayer or a SpatialPolygons*", {
  expect_error(
    distance_map(pts1, bnd),
    NA
  )

  expect_error(
    distance_map(pts1, rmsk),
    NA
  )
})


test_that("distance_map() handles resolution spec", {
  expect_error(
    distance_map(pts1, bnd, res = 1),
    NA
  )

  expect_error(
    distance_map(pts1, bnd, res = 0),
    "res > 0 is not TRUE"
  )

  expect_error(
    distance_map(pts1, bnd, res = 20),
    "res < max_res"
  )
})
