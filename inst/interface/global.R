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

# Menu
langMenuFile <- c("Files","Fichiers")

langMenuVector <- c("Vectors","Vecteurs")

langMenuRaster <- c("Rasters","Rasters")

langMenuWeight <- c("Weight","Poids")

langMenuResult <- c("Results","R&#233;sultats")

# Type of layer
lVect <- c("Vector","Vecteur")

lRast <- c("Raster","Raster")


langTitleFileInput <- c("Select layers",
                        "S&#233;lectionnez les fichiers de vecteurs et rasters")

langButtonFileInput <- list(c("Browse...", "No file selected"), 
                            c("Parcourir...", "Pas de selection"))


langLayerRemove <- c("Remove", "Supprimer")