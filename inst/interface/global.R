################### PURPOSE OF THE APP ###################
#Interface for PRODEL project
#May 2018, by Sylvain Falala, Unit CIRAD-INRA ASTRE

appTitle <- "PRODEL"

acceptLayerType <- c(".shp", ".jpg", ".tif")


#### LANGUAGE ####

# Language to use: 1 = english, 2 = french
indLang <- 2

#languages <- c("English" = "1", "Francais" = "2")

langTitleFileInput <- c("Select layers",
                        "S&#233;lectionnez les couches")

langButtonFileInput <- list(c("Browse...", "No file selected"), 
                            c("Parcourir...", "Pas de fichiers s&#233;lectionn&#233;s"))

langLayerItem <- c("Layers", "Couches")

langLayerRemove <- c("Remove", "Supprimer")