
## check existence of testdata directory and create it if necessary
if (
  !file.exists(
    tdp <- file.path(system.file(package = "mapMCDA"), "testdata")
  )
) {
  dir.create(tdp)
}

testdata <- tdp
