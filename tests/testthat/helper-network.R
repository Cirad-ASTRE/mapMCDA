## Fake animal mobility data
## For testing network data

if (!file.exists(system.file("testdata", "mobility.csv", package = "mapMCDA"))) {
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
  
  if (!file.exists(tdp <- file.path(system.file(package = "mapMCDA"), "testdata"))) {
    dir.create(tdp)
  }
  write.csv(mobility, file.path(tdp, "mobility.csv"), row.names = FALSE, quote = FALSE)
}
