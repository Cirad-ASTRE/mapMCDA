## ----packages------------------------------------------------------------
require(sp)
require(raster)
require(rasterVis)
require(mapMCDA)

## ----facteurs-risque-----------------------------------------------------
cmr <- mapMCDA_datasets()

# layers <- list.files(
#   system.file("cartography/CMR", package = "mapMCDA"),
#   full.names = TRUE
# )
# cmr <- lapply(layers, load_layer)
# names(cmr) <- gsub("\\.\\w{1,}$", "", basename(layers))


## ----unite-epidemiologique, fig.cap = "Unités épidémiologiques d'exemple pour le Cameroun."----
unites_epi <- cmr$cmr_admin3
par(mar = c(0, 0, 0, 0))
plot(unites_epi)

## ----mise-echelle, fig.width=4, fig.cap = "Mis en échelle directe ou inverse."----

plot(
  data.frame(x = c(0, 100), y = c(0, 100)),
  type = 'l',
  xaxs = "i",
  yaxs = "i",
  xaxt = "n",
  lab = c(1, 1, 7),
  xlab = c("Échelle originale"),
  ylab = c("Échelle de risque")
)
abline(100, -1)


## ----risk-layers---------------------------------------------------------

risques <- list(
  dens_animale = risk_layer(
    cmr$animal.density,
    boundaries = unites_epi
    # , scale_target = c(0, 100)  # échelle directe par défault
  ),
  points_eau = risk_layer(
    cmr$water_bodies,
    boundaries = unites_epi,
    scale_target = c(100, 0)  # échelle renversée
  ),
  parcs = risk_layer(
    cmr$national_parks,
    boundaries = unites_epi,
    scale_target = c(100, 0)  # échelle renversée
  )
)


## ----align-layers, fig.width = 6, fig.cap = "Niveaux de risque associé à chaque facteur."----
risques_alignes <- align_layers(risques)
levelplot(stack(risques_alignes))


## ----matrice-relations, echo = -1----------------------------------------

M <- matrix(c(
  1,     6, 4,
  1/6,   1, 3,
  1/4, 1/3, 1
), byrow = TRUE, 3, 3)
colnames(M) <- rownames(M) <- names(risques)
knitr::kable(M, digits = 2)

## ----compute-weights, fig.width=4, echo = 1, fig.cap = "Pondération des facteurs de risque."----
w <- compute_weights(M)
mapMCDA:::plot_weights(w, rownames(M))

## ----wlc, fig.width = 6, fig.height = 6, fig.cap = "Carte de risque combiné."----
risque_combine <- wlc(risques, w)
levelplot(risque_combine)

## ----risk-plot, fig.width = 6, fig.height = 6, fig.cap = "Carte de niveaux de risque par unité épidémiologique."----
risk_plot(unites_epi, risk_unit(risque_combine, unites_epi), n = 5)

