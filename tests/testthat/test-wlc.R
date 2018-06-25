context("wlc")

test_that("Combining layers with missing values", {
  r1 <- raster(matrix(1, 2, 2))
  r2 <- r2_miss12 <- raster(matrix(2, 2, 2))
  r2_miss12[1, 2] <- NA
  
  ## If the NA in r2 is removed, the weighted combination must be equal to
  ## the value in r1. 
  expect_identical(1, getValues(wlc(list(r1, r2_miss12), w = c(1, 1)/2, na.rm = TRUE))[2])
  
  ## Otherwise, it must be NA
  expect_true(is.na(getValues(wlc(list(r1, r2_miss12), w = c(1, 1)/2, na.rm = FALSE))[2]))
})
