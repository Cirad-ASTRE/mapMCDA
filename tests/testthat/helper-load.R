## Fake vector, raster and network data
## For testing lading

if (!file.exists(file.path(testdata, "points.shp"))) {
  vector <- SpatialPointsDataFrame(
    cbind(1:5, 1:5),
    data.frame(a = 1:5)
  )

  
  writeOGR(vector, testdata, "points", driver = "ESRI Shapefile")
}

if (!file.exists(vf <- file.path(testdata, "points.gpkg"))) {
  vector <- SpatialPointsDataFrame(
    cbind(1:5, 1:5),
    data.frame(a = 1:5)
  )

  writeOGR(vector, vf, layer = "points", driver = "GPKG")
}

if (!file.exists(rf <- file.path(testdata, "raster.tif"))) {
  writeRaster(raster(matrix(1:4, 2, 2)), rf)
}

if (!file.exists(nf <- file.path(testdata, "mobility.csv"))) {
  set.seed(20190115)
  n_markets <- 5
  markets <- spsample(rgeos::gUnaryUnion(admin), n_markets, type = "random")
  
  mobility <- 
    subset(
      expand.grid(
        from = LETTERS[seq.int(n_markets)],
        to = LETTERS[seq.int(n_markets)],
        KEEP.OUT.ATTRS = FALSE
      ),
      from != to  # remove diagonals
    )
  
  mobility <- cbind(
    mobility,
    coordinates(markets)[factor(mobility$from), ]
  )
  mobility <- cbind(
    mobility,
    coordinates(markets)[factor(mobility$to), ]
  )
  mobility$volume <- round(rt(n_markets*(n_markets-1), df = 1, ncp = 10), 1)
  ## Set a few of the least important edges to zero
  n_zeros <- floor(nrow(mobility)/5)
  mobility$volume[sample(nrow(mobility), n_zeros, prob = 1/mobility$volume)] <- 0
  
  rownames(mobility) <- NULL
  
  write.csv(mobility, nf, row.names = FALSE, quote = FALSE)
}
