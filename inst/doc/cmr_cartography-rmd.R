## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages------------------------------------------------------------
require(raster)
require(rgdal)

if(!require(mapview, quietly = TRUE)) 
  cat("We suggest installing the pacakge mapview for interactive visualisation",
      "of cartography from within R")


## ----admin-borders-------------------------------------------------------

cmr_admin3 <- getData('GADM', country = "CMR", level=3)
# mapview(cmr_admin3, zcol = "NAME_3")


## ----water-bodies, eval = FALSE------------------------------------------
#  
#  ## This only works locally.
#  prodel_path <- "/home/facu/CmisSync/Cirad/Sites/PRODEL/documentLibrary/carto"
#  water_bodies <- readOGR(prodel_path, layer = "wb_cam.shp")
#  

## ----national-parks, eval = FALSE----------------------------------------
#  national_parks <- readOGR(
#    file.path(prodel_path, "WDPA_Mar2018_CMR-shapefile"),
#    "WDPA_Mar2018_CMR-shapefile-polygons"
#  )
#  

## ----production-systems, eval = FALSE------------------------------------
#  # Not using this for the moment
#  ps_cam <- raster("ps_cam.tif")
#  

## ----animal-density, eval = FALSE----------------------------------------
#  animal_density_world <- raster(file.path(prodel_path, "glw", "WdCt8k_vf_Mn_Rw_To.tif"))
#  animal_density <- mask(crop(animal_density_world, extent(cmr_admin3)), cmr_admin3)
#  
#  # plot(animal_density)
#  # summary(animal_density$WdCt8k_vf_Mn_Rw_To)
#  

## ----save-package-carto, eval = FALSE------------------------------------
#  cmr_dir <- "./inst/cartography/CMR"
#  dir.create(cmr_dir, recursive = TRUE)
#  
#  writeOGR(cmr_admin3, file.path(cmr_dir, "cmr_admin3.gpkg"), layer = "cmr_admin3", driver = "GPKG")
#  writeOGR(water_bodies, file.path(cmr_dir, "water_bodies.gpkg"), layer = "water_bodies", driver = "GPKG")
#  writeOGR(national_parks, file.path(cmr_dir, "national_parks.gpkg"), layer = "national_parks", driver = "GPKG")
#  
#  writeRaster(animal_density, file.path(cmr_dir, "animal.density.tif"))
#  
#  

