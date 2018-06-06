################### PURPOSE OF THE APP ###################
#Interface for PRODEL project
#May 2018, by Sylvain Falala, Unit CIRAD-INRA ASTRE

################### LIBRARIES ################### 
library(stringr) # to work with character strings
library(rgdal) #to work with spatial vector
library(raster) #to work with spatial raster


# To avoid "Maximum upload size exceeded"
# Define upload limit as 30MB
options(shiny.maxRequestSize=30*1024^2)



server <- function(input, output, session) {
  
  #### Reactive values ####
  # uploadFileDF = data frame of information on all uploaded files
  # layerDF = data frame of information on layers
  # weightMatrix = matrix of weight
  rv <- reactiveValues(uploadFileDF = NULL,
                       layerDF = NULL,
                       weightMatrix = NULL)
  
  
  ##### Observer on data frame of layers ####
  
  observeEvent(rv$layerDF,{
    
    # If data frame is empty
    if(is.null(nrow(rv$layerDF)) || nrow(rv$layerDF)==0){
      
      
      #shinyjs::hide("abLayerRemove")
      
    } else {
        
      
      #shinyjs::show("abLayerRemove")
      
    }
    
    glLayerDF <<- rv$layerDF
    
  })
  
  
  ##### Observer on file input ####
  
  observeEvent(input$fiLayer,{
    
    # input$fiLayer will be NULL initially. After the user selects and uploads a file, 
    # it will be a data frame with 'name', 'size', 'type', and 'datapath' columns.
    # The 'datapath' column will contain the local filenames where the data can be found.
    
    layerFiles <- input$fiLayer
    
    if(is.null(layerFiles)) return()
    
    # Upload files are copied in a temporary folder 'datapath'and are renamed like '0.shp', '1.shx'
    # We need to rename the files with their original names to work with it
    oldNames = layerFiles$datapath
    newNames = file.path(dirname(layerFiles$datapath), layerFiles$name)
    file.rename(from = oldNames, to = newNames)
    layerFiles$datapath <- newNames
    
    # Update data frame of all uploaded files
    if(is.null(nrow(rv$uploadFileDF))) rv$uploadFileDF <- layerFiles else rv$uploadFileDF <- rbind(rv$uploadFileDF, layerFiles)
    
    # Create short name for the layer based on the file name
    # Remove file extension
    layerFiles$shortName <- gsub(reExt, "", layerFiles$name, ignore.case = TRUE, perl = TRUE)
    
    # Remove special characters
    layerFiles$shortName <- iconv(layerFiles$shortName, from = "UTF-8", to = "ASCII", sub = "")
    
    # Remove blanks  
    layerFiles$shortName <- gsub("\\s+", "", layerFiles$shortName, ignore.case = TRUE, perl = TRUE)
    
    layerFiles$originalName <- layerFiles$shortName
    
    
    # Retrieve file extension to define type of layer: vector or raster
    fileExt <- str_extract(layerFiles$name, reExt)
    fileExt <- tolower(str_replace(fileExt, "\\.", ""))
    
    layerFiles$layerType <- rep("Unknown", nrow(layerFiles))
    
    indVect <- which(fileExt %in% vectorExt)
    if(!is.na(indVect[1])) layerFiles[indVect,"layerType"] <- lVect[indLang]
    
    indRast <- which(fileExt %in% rasterExt)
    if(!is.na(indRast[1])) layerFiles[indRast,"layerType"] <- lRast[indLang]
    
    #Remove unknown file type
    indRem <- which(layerFiles$layerType=="Unknown")
    if(!is.na(indRem[1])) layerFiles <- layerFiles[-indRem,]
    
    nbLayer <- nrow(layerFiles)
    layerNames <- sort(layerFiles$shortName)
    
    # Load vectors and rasters in global environment
    for(k in 1:nbLayer){
      
      #If vector
      if(layerFiles[k,"layerType"]==lVect[indLang]){
      
        # Retrieve path of shp file to define dsn
        shpDir <- gsub(paste("/", layerFiles[k,"name"], sep = ""),"", layerFiles[k,"datapath"])
        
        # Extract shp file name without extension to define layer name
        shpLayer <- gsub(".shp","", layerFiles[k,"name"])
        
        curLay <- readOGR(dsn = shpDir, layer = shpLayer, verbose = FALSE)
      
      }
      
      
      #If raster
      if(layerFiles[k,"layerType"]==lRast[indLang]){
        
        curLay <- raster(layerFiles[k,"datapath"])

      }
      
      
      curLayerName <- paste("layer_", layerFiles[k,"shortName"], sep = "")
      
      #Save in global environment the current layer
      assign(x = curLayerName, 
             value = curLay, 
             envir = .GlobalEnv)
      
    }
    

    # Update layerDF
    if(is.null(nrow(rv$layerDF))) rv$layerDF <- layerFiles else rv$layerDF <- rbind(rv$layerDF, layerFiles)
    
    #Initialize weight matrix
    rv$weightMatrix <- matrix(data = 1, nrow = nbLayer, ncol = nbLayer, dimnames = list(layerNames, layerNames))
    
    
  })
  
  ##### Render for all files table ####
  
  output$allFileTable <- renderTable({
    
    # Retrieve short name of data frame of layers
    if(is.null(rv$uploadFileDF)) return(NULL)
    
    shortLayerDF <- subset(rv$uploadFileDF, select = "name")
    
    colnames(shortLayerDF) <- "Nom_fichier"
    
    shortLayerDF
    
  })
  
  
  ##### Render for layer editable table ####
  
  output$rhLayerTable <- renderRHandsontable({
    
    # Retrieve short name of data frame of layers
    if(!("shortName" %in% colnames(rv$layerDF))) return(NULL)
    
    shortLayerDF <- subset(rv$layerDF, select = c("originalName", "shortName", "layerType"))
    
    colnames(shortLayerDF) <- c("Nom_orig", "Nom_modif", "Type")
    
    rownames(shortLayerDF) <- 1:nrow(shortLayerDF)
    
    rhandsontable(shortLayerDF) %>%
      hot_col(c("Nom_orig", "Type"), readOnly = TRUE)
    
  })
  
  
  ##### Observer on layer editable table ####
  
  observeEvent(input$rhLayerTable,{

    #Retrieve data from editable table
    editLayerDF <- hot_to_r(input$rhLayerTable)

    newLayerNames <- editLayerDF$Nom_modif

    if(any(nchar(newLayerNames)==0)) return()
    
    # Rename variables of layers in global environment
    nbLayer <- nrow(rv$layerDF)
    
    for(k in 1:nbLayer){
      
      curLayerSN <- rv$layerDF[k, "shortName"]
      
      if(newLayerNames[k]!=curLayerSN){
        
        curLayerName <- paste("layer_", curLayerSN, sep = "")
        
        if(exists(curLayerName, envir = .GlobalEnv)) {
          
          curLay <- get(x = curLayerName, 
                            envir = .GlobalEnv)
        
          rm(list=curLayerName, envir = .GlobalEnv)
        
        curLayerNewName <- paste("layer_", newLayerNames[k], sep = "")
        
        #Save in global environment the current layer with new name
        assign(x = curLayerNewName, 
               value = curLay, 
               envir = .GlobalEnv)
        }
        
      }
      
    }

    # Update data frame of layers
    rv$layerDF$shortName <- newLayerNames

    

    # Upadate weight matrix
    dimnames(rv$weightMatrix) <- list(newLayerNames, newLayerNames)


  })
  
  
  ##### Render for the list of vectors ####
  
  output$uiVectorList <- renderUI({

      # Retrieve short name of data frame of layers
      if(!("layerType" %in% colnames(rv$layerDF))) return(NULL)

      indVect <- which(rv$layerDF$layerType==lVect[indLang])
      
      if(is.na(indVect[1])) return(NULL)
      
      shortNames <- sort(unique(rv$layerDF[indVect,"shortName"]))

      radioButtons(inputId = "rbVectorLayer", 
                   label = HTML(langRBVector[indLang]), 
                   choices = shortNames, 
                   inline = TRUE)

  })
  
  
  ##### Render for the list of rasters ####
  
  output$uiRasterList <- renderUI({
    
    # Retrieve short name of data frame of layers
    if(!("layerType" %in% colnames(rv$layerDF))) return(NULL)
    
    indRast <- which(rv$layerDF$layerType==lRast[indLang])
    
    if(is.na(indRast[1])) return(NULL)
    
    shortNames <- sort(unique(rv$layerDF[indRast,"shortName"]))
    
    radioButtons(inputId = "rbRasterLayer", 
                 label = HTML(langRBRaster[indLang]), 
                 choices = shortNames, 
                 inline = TRUE)
    
  })
  
  
  
  
  ##### Render for vector display ####
  
  output$vectorDisplay <- renderPlot({

    # Retrieve name of the current tab
    curLayer <- input$rbVectorLayer

    if(is.null(curLayer)) return(NULL)

    finalLayer <- NULL
    
    curLayerName <- paste("layer_", curLayer, sep = "")
    
    #If data frame of disease is loaded in global environment
    if(exists(curLayerName, envir = .GlobalEnv)) {
      
      finalLayer <- get(x = curLayerName, 
                        envir = .GlobalEnv)
    }
    
    plot(finalLayer)

  })

  
  ##### Render for raster display ####
  
  output$rawRasterDisplay <- renderPlot({
    
    # Retrieve name of the current tab
    curLayer <- input$rbRasterLayer
    
    if(is.null(curLayer)) return(NULL)
    
    finalLayer <- NULL
    
    curLayerName <- paste("layer_", curLayer, sep = "")
    
    #If data frame of disease is loaded in global environment
    if(exists(curLayerName, envir = .GlobalEnv)) {
      
      finalLayer <- get(x = curLayerName, 
                    envir = .GlobalEnv)
      }
    
    plot(finalLayer)
    
    
  })
  
  
  ##### Render for editable table of weight ####
  
  output$rhWeightTable <- renderRHandsontable({
    
    weightDF <- data.frame(rv$weightMatrix)
    
    rhandsontable(weightDF, rowHeaderWidth = 200)
    
  })
  
  
  ##### Observer on weight editable table ####
  
  observeEvent(input$rhWeightTable,{
    
    #Retrieve data from editable table
    weightDF <- hot_to_r(input$rhWeightTable)
    
    print(weightDF)
    
    
  })
  
  ##### Render for weight graph bar display ####
  
  output$weightBarDisplay <- renderPlot({
    
    
    
  })
  
  
  ##### Render for result display ####
  
  output$resultDisplay <- renderPlot({
    
    
    
  })
  
  
}
