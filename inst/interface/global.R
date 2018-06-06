################### PURPOSE OF THE APP ###################
#Interface for PRODEL project
#May 2018, by Sylvain Falala, Unit CIRAD-INRA ASTRE

appTitle <- "PRODEL"

acceptLayerType <- c(".shp", ".jpg", ".tif")

vectorExt <- c("shp")

rasterExt <- c("tif", "tiff")

reExt <- "\\.\\w{1,}$" # regular expression to define file extension

#### LANGUAGE ####

# Language to use: 1 = english, 2 = french
indLang <- 2

#languages <- c("English" = "1", "Francais" = "2")

# Fichiers
langMenuFile <- c("Files","Fichiers")

langBoxFile <- langMenuFile

langBoxLayer <- c("Layers", "Couches")

# Vecteurs
langMenuVector <- c("Vectors","Vecteurs")

langBoxVMap <- c("Map", "Carte")

langBoxVDist <- c("Distances", "Distances")

langRBVector <- c("Select vector:", "S&#233;lectionnez le vecteur :")

# Rasters
langMenuRaster <- c("Rasters","Rasters")

langBoxRawMap <- c("Raw", "Brut")

langBoxProcMap <- c("Standardized", "Standardis&#233;")

langRBRaster <- c("Select raster:", "S&#233;lectionnez le raster :")

langABRasterInvert <- c("Invert", "Inverser")

# Weight
langMenuWeight <- c("Weight","Poids")

# Results
langMenuResult <- c("Results","R&#233;sultats")



# Type of layer
lVect <- c("Vector","Vecteur")

lRast <- c("Raster","Raster")


langTitleFileInput <- c("Select layers",
                        "S&#233;lectionnez les fichiers de vecteurs et rasters")

langButtonFileInput <- list(c("Browse...", "No file selected"), 
                            c("Parcourir...", "Pas de selection"))


langLayerRemove <- c("Remove", "Supprimer")

