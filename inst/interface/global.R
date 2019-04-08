################### PURPOSE OF THE APP ###################
# Interface for mapMCDA project
# April 2019, by Sylvain Falala, Unit CIRAD-INRA ASTRE

################### LIBRARIES ################### 

library(rgdal) # to work with spatial vector
library(raster) # to work with spatial raster
library(igraph) # to work with igraph geonetwork

library(shiny) # to develop web interface
library(shinydashboard) # to create dashboard
library(rhandsontable) # to use an editable table
library(shinyFiles) # to load layer files locally


#### MAIN VARIABLES ####

appTitle <- "MapMCDA"


#### LANGUAGE ####

# Language to use: 1 = english, 2 = french
indLang <- 1

# Radio buttons to select language
langChoiceNames <- list(HTML("English"), HTML("Français"))
langChoiceVal <- list("1","2")


langDF <- data.frame(
  
  # SIDEBAR
  "MenuFile" = c("Files", "Fichiers"),
  "MenuUnit" = c("Epidemiological units", "Unités épidémiologiques"),
  "MenuRisk" = c("Risk factors", "Facteurs de risque"),
  "MenuWeight" = c("Weights", "Poids"),
  "MenuResult" = c("Results", "Résultats"),
  
  # Files box
  "BoxFile" = c("Files", "Fichiers"),
  "TitleFileInput" = c("Select layers", "Sélectionnez les fichiers de vecteurs et rasters"),
  "ButtonFileInput" = c("Browse...", "Parcourir..."),
  "BoxLayer" = c("Layers", "Couches"),
  
  # Epidemiological units box
  "BoxUnitMap" = c("Map", "Carte"),
  "BoxUnitStat" = c("Statistics", "Statistiques"), 
  
  # Risk factors box
<<<<<<< HEAD
  "BoxRiskRawMap" = c("Original scale", "Échelle originale"),
  "BoxRiskStandRaster" = c("Risk scale", "Échelle de risque"),
||||||| merged common ancestors
  "BoxRiskRawMap" = c("Raw", "Brut"),
  "BoxRiskStandRaster" = c("Standardized raster", "Raster standardisé"),
=======
  "BoxRiskRawMap" = c("Original scale", "Echelle originale"),
  "BoxRiskStandRaster" = c("Risk scale", "Echelle de risque"),
>>>>>>> 6c334642e14d50f808958cbec9df7c5bd190a662
  "ABRiskRasterInvert" = c("Invert", "Inverser"),
  "RBRiskLayer" = c("Scale risk factors",
                     "Mise en échelle des facteurs de risque"),
  
  # Weights box
  "BoxWeightMatrix" = c("Pairwise comparison Matrix", "Matrice de comparaison par paires"),
  "BoxWeightBar" = c("Weights Histogram", "Histogramme des poids"),
  
  # Results box
  "BoxResult" = c("Combined risk", "Risque combiné"),
  "BoxResultPerUnit" = c("Per Epidemiological unit","Par unité épidémiologique"),
  "SIRiskLevel" = c("# Risk categories", "# Catégories de risque"),
  "DownButton" = c("Export", "Exporter"),

stringsAsFactors = FALSE)

langDF <- t(langDF)


# Type of layer
lVect <- c("Vector", "Vecteur")

lRast <- c("Raster", "Raster")

lMob <- c("MobGraph","MobGraph")


langFileList <- c("Accepted files", "Fichiers acceptés")
 
# langLayerRemove <- c("Remove", "Supprimer")


# Columns to retrieve in data frame of layers for editable table
toEditLayerColNames <- c("originalName", "shortName", "layerType", "adminUnit")

# Column names for editable table of layers
langLayerList <- list(c("Name_orig", "Name_new", "Type", "Admin_unit"), 
                      c("Nom_orig", "Nom_modif", "Type", "Unite_admin"))


# Columns read only in editable table of layers
lockOrigNameCol <- 1
lockTypeCol <- 3

# Columns editable in editable table of layers
newNameCol <- 2
newAdminCol <- 4


# Help text on interface
langHelpFiles <- c(
  "Import files, including a vector map with epidemiological units.",
  "Importer les fichiers, y-compris une carte vectorielle avec des unités épidémiologiques."
)

langHelpLayers <- c(
  "Check the epidemiological units layer.",
  "Indiquer la couche d'unités épidémiologiques."
)

langHelpScale <- c(
  "Harmonise the original scales into a common risk-scale. Use the button to reverse the relationship.",
  "Harmoniser les échelles originales en une échelle de risque commune. Utiliser le bouton pour inverser la relation."
)

langHelpMatrix <- c(
  "The factor in row i is x[i,j] times more important than the factor in column j.",
  "Le facteur de la ligne i est x[i, j] fois plus important que le facteur de la colonne j."
)



