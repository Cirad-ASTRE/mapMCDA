################### PURPOSE OF THE APP ###################
#Interface for PRODEL project
#May 2018, by Sylvain Falala, Unit CIRAD-INRA ASTRE

################### LIBRARIES ################### 
library(ggplot2)



# To avoid "Maximum upload size exceeded"
# Define upload limit as 20MB
options(shiny.maxRequestSize=20*1024^2)



server <- function(input, output, session) {
  
  #### Reactive values ####
  # uploadFileDF = data frame of information on all uploaded files
  # layerDF = data frame of information on layers
  # weightMatrix = matrix of weight
  rv <- reactiveValues(uploadFileDF = glUploadFileDF,
                       layerDF = glLayerDF,
                       weightMatrix = glWeightMatrix,
                       weightVect = NULL,
                       invert = FALSE)
  
  
  ##### Observer on data frame of layers ####
  
  observeEvent(rv$layerDF,{
    
    # If data frame is empty
    #if(is.null(nrow(rv$layerDF)) || nrow(rv$layerDF)==0){} else {}
    
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
    glUploadFileDF <<- rv$uploadFileDF
    
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
    
    
    
    # Create short name for the layer based on the file name
    # Remove file extension
    layerFiles$shortName <- gsub(reExt, "", layerFiles$name, ignore.case = TRUE, perl = TRUE)
    
    # Remove special characters
    layerFiles$shortName <- iconv(layerFiles$shortName, from = "UTF-8", to = "ASCII", sub = "")
    
    # Remove blanks  
    layerFiles$shortName <- gsub("\\s+", "", layerFiles$shortName, ignore.case = TRUE, perl = TRUE)
    
    layerFiles$originalName <- layerFiles$shortName
    
    
    nbLayer <- nrow(layerFiles)
    layerNames <- sort(layerFiles$shortName)
    
    # Load vectors and rasters in global environment
    for(k in 1:nbLayer){
      
      #If vector
      if(layerFiles[k,"layerType"]==lVect[indLang]){
        
        # If shape file
        if(str_detect(layerFiles[k,"name"], ".shp")){
      
          # Retrieve path of shp file to define dsn
          shpDir <- gsub(paste("/", layerFiles[k,"name"], sep = ""),"", layerFiles[k,"datapath"])
          
          # Extract shp file name without extension to define layer name
          shpLayer <- gsub(".shp","", layerFiles[k,"name"])
        }
        
        # If geopackage file
        if(str_detect(layerFiles[k,"name"], ".gpkg")){
          
          # Retrieve path of shp file to define dsn
          shpDir <- layerFiles[k,"datapath"]
          
          # Extract shp file name without extension to define layer name
          shpLayer <- gsub(".gpkg","", layerFiles[k,"name"])
        }
        
        
        curLay <- readOGR(dsn = shpDir, layer = shpLayer, verbose = FALSE)
      
      }
      
      
      #If raster
      if(layerFiles[k,"layerType"]==lRast[indLang]){
        
        curLay <- raster(layerFiles[k,"datapath"])

      }
      
      
      curLayerName <- paste("layer_", layerFiles[k,"shortName"], sep = "")
      
      # Pre-allocate list for layer
      curLayerList <- vector("list", nbLayIndex)
      curLayerList[[indRawLay]] <- curLay
      curLayerList[[indScale]] <- NA
      curLayerList[[indStandLay]] <- NA
      
      #Save in global environment the current layer
      assign(x = curLayerName, 
             value = curLayerList, 
             envir = .GlobalEnv)
      
    }
    
    # Add column if layer is administrative units
    layerFiles$adminUnit <- rep(FALSE, nbLayer)

    # Update layerDF
    if(is.null(nrow(rv$layerDF))) rv$layerDF <- layerFiles else rv$layerDF <- rbind(rv$layerDF, layerFiles)
    
    #Initialize weight matrix
    if(is.null(nrow(glWeightMatrix))){
    
      rv$weightMatrix <- matrix(data = 1.0, nrow = nbLayer, ncol = nbLayer, dimnames = list(layerNames, layerNames))
    
      } else {
      
      nbLines <- nrow(glWeightMatrix) + nbLayer
      newNames <- c(rownames(glWeightMatrix), layerNames) 
      
      rv$weightMatrix <- matrix(data = 1.0, nrow = nbLines, ncol = nbLines, 
                                dimnames = list(newNames, newNames))
        
    }
    glWeightMatrix <<- rv$weightMatrix
    
  })
  
  ##### Render for all files table ####
  
  output$allFileTable <- renderTable({
    
    # Retrieve short name of data frame of layers
    if(is.null(rv$uploadFileDF)) return(NULL)
    
    shortLayerDF <- subset(rv$uploadFileDF, select = "name")
    
    colnames(shortLayerDF) <- langFileList[indLang]
    
    shortLayerDF
    
  })
  
  
  ##### Render for layer editable table ####
  
  output$rhLayerTable <- renderRHandsontable({
    
    # Retrieve short name of data frame of layers
    if(!("shortName" %in% colnames(rv$layerDF))) return(NULL)
    
    shortLayerDF <- subset(rv$layerDF, select = toEditLayerColNames)
    
    colnames(shortLayerDF) <- langLayerList[[indLang]]
    
    rownames(shortLayerDF) <- 1:nrow(shortLayerDF)
    
    rhandsontable(shortLayerDF) %>%
      hot_col(c(lockOrigNameCol, lockTypeCol), readOnly = TRUE)
    
  })
  
  
  ##### Observer on layer editable table ####
  
  observeEvent(input$rhLayerTable,{

    #Retrieve data from editable table
    editLayerDF <- hot_to_r(input$rhLayerTable)
    
    # Update name of layers

    newLayerNames <- editLayerDF[,newNameCol]
    
    # Remove special characters
    newLayerNames <- iconv(newLayerNames, from = "UTF-8", to = "ASCII", sub = "")
    
    # Remove blanks  
    newLayerNames <- gsub("\\s+", "", newLayerNames, ignore.case = TRUE, perl = TRUE)

    # If at least one empty name
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
    
    rv$layerDF$adminUnit <- editLayerDF[,newAdminCol]

    # Upadate weight matrix
    rv$weightMatrix <- glWeightMatrix
    dimnames(rv$weightMatrix) <- list(newLayerNames, newLayerNames)


  })
  
  
  ##### reactive function to return name of epid. unit layer ####
  
  epidUnitName <- reactive({
    
    curLayerDF <- rv$layerDF
    
    if(!("adminUnit" %in% colnames(curLayerDF))) return(NULL)
    
    indAdmin <- which(curLayerDF$adminUnit==TRUE)
    if(is.na(indAdmin[1])) return(NULL)
    
    if(length(indAdmin)>1){
      
      print("Nb of epidemiological unit > 1")
      return(NULL)
      
    }
    
    curLayerDF[indAdmin, "shortName"]
    
    
  })
  
  
  ##### reactive function to return epid. unit layer ####
  
  curEpidUnitLayer <- reactive({
    
    curEpidUnitName <- epidUnitName()
    
    if(is.null(curEpidUnitName)) return(NULL)
    
    curLayerEpidUnitName <- paste("layer_", curEpidUnitName, sep = "")
    
    epidUnitLayer <- NULL
    
    #If admin layer is loaded in global environment
    if(exists(curLayerEpidUnitName, envir = .GlobalEnv)) {
      
      epidUnitLayer <- get(x = curLayerEpidUnitName, 
                           envir = .GlobalEnv)
    }
    
    epidUnitLayer
    
    
  })
  
  
  ##### Render for the layer name of epid. unit ####
  
  output$unitNameText <- renderUI({
    
    curEpidUnitName <- epidUnitName()
    
    if(is.null(curEpidUnitName)) return(NULL)
    
    h3(curEpidUnitName)
    
    
  })
  
  
  ##### Render for the map of epid. unit ####
  
  output$unitMapDisplay <- renderPlot({
    
    epidUnitLayer <- curEpidUnitLayer()
    
    if(is.null(epidUnitLayer)) return(NULL)

    
    plot(epidUnitLayer[[indRawLay]])
    
    
  })
  
  
  ##### Render for the list of layers in risk tab ####
  
  output$uiRiskLayerList <- renderUI({

    curLayerDF <- rv$layerDF

    # Retrieve short name of data frame of layers
    if(!("shortName" %in% colnames(curLayerDF))) return(NULL)

    shortNames <- sort(unique(curLayerDF$shortName))

    #Remove epid. unit
    indRem <- which(shortNames==isolate(epidUnitName()))
    if(!is.na(indRem[1])) shortNames <- shortNames[-indRem]

    radioButtons(inputId = "rbRiskLayer",
                 label = HTML(langRBRiskLayer[indLang]),
                 choices = shortNames,
                 inline = TRUE)

  })
  
  
  ##### reactive function to return selected layer in risk tab ####
  
  curRiskLayer <- reactive({
    
    # Retrieve name of the current tab
    curLayerSN <- input$rbRiskLayer
    
    if(is.null(curLayerSN)) return(NULL)
    
    finalLayer <- NULL
    
    curLayerName <- paste("layer_", curLayerSN, sep = "")
    
    #If data frame of disease is loaded in global environment
    if(exists(curLayerName, envir = .GlobalEnv)) {
      
      finalLayer <- get(x = curLayerName,
                        envir = .GlobalEnv)
      
    }
    
    finalLayer
    
  })
  
  
  ##### Render for raw layer display in risk tab ####
  
  output$rawLayerDisplay <- renderPlot({

    myLayer <- curRiskLayer()
    
    if(is.null(myLayer)) return(NULL)

    plot(myLayer[[indRawLay]])


  })
  
  
  
  ##### observer to compute standardized raster ####
  
  curStandRaster <- reactive({
    
    myLayer <- curRiskLayer()
    
    if(is.null(myLayer)) return(NULL)
    
    standRaster <- NULL
    
    #invertScale <- rv$invert
    
    invertScale <- FALSE
    
    if(inherits(myLayer[[indStandLay]], c("Spatial", "RasterLayer")) && invertScale==FALSE) {
      
      standRaster <- myLayer[[indStandLay]]
      
    } else {
      
      epidUnitLayer <- isolate(curEpidUnitLayer())
      
      if(is.null(epidUnitLayer)) return(NULL)
      
      scaleTarget <- c(0, 100)
      
      if(!is.na(myLayer[[indScale]][1]) && invertScale==TRUE){
        
        curScale <- myLayer[[indScale]]
        
        if(all(curScale==scaleTarget)) scaleTarget <- c(100, 0)
        
      }
      
      standRaster <- risk_layer(myLayer[[indRawLay]], epidUnitLayer[[indRawLay]], scaleTarget)
      myLayer[[indScale]] <- scaleTarget
      myLayer[[indStandLay]] <- standRaster
      
      curLayerName <- paste("layer_", isolate(input$rbRiskLayer), sep = "")
      
      
      #Save in global environment the standardized layer with new name
      assign(x = curLayerName,
             value = myLayer,
             envir = .GlobalEnv)
      
    }
    
    #isolate(rv$invert <- FALSE)
    
    standRaster
    
    
  })
  
  
  ##### Render for standardized raster in risk tab ####
  
  output$standRasterDisplay <- renderPlot({
    
    myStandRaster <- curStandRaster()
    
    if(is.null(myStandRaster)) return(NULL)
    
    plot(myStandRaster)
    
    
  })
  
  
  
  
  
  
   

  # 
  # ##### Render for processed raster display ####
  # 
  # output$processedRasterDisplay <- renderPlot({
  #   
  #   # Retrieve name of the current tab
  #   curLayer <- input$rbRasterLayer
  #   
  #   if(is.null(curLayer)) return(NULL)
  #   
  #   finalLayer <- NULL
  #   
  #   curLayerName <- paste("layer_", curLayer, sep = "")
  #   
  #   #If data frame of disease is loaded in global environment
  #   if(exists(curLayerName, envir = .GlobalEnv)) {
  #     
  #     finalLayer <- get(x = curLayerName, 
  #                       envir = .GlobalEnv)
  #   }
  #   
  #   invertScale <- rv$invert
  #   
  #   if(inherits(finalLayer[[indStandLay]], c("Spatial", "RasterLayer")) && invertScale==FALSE) {
  #     
  #     ansLayer <- finalLayer[[indStandLay]]
  #     
  #   } else {
  #   
  #     # Retrieve admin zone
  #     indAdmin <- which(glLayerDF$adminUnit==TRUE)
  #     if(is.na(indAdmin[1])) return(NULL)
  #     
  #     curLayerEpidUnitName <- paste("layer_", glLayerDF[indAdmin, "shortName"], sep = "")
  #     
  #     #If admin layer is loaded in global environment
  #     if(exists(curLayerEpidUnitName, envir = .GlobalEnv)) {
  #       
  #       epidUnitLayer <- get(x = curLayerEpidUnitName, 
  #                         envir = .GlobalEnv)
  #     }
  #     
  #     scaleTarget <- c(0, 100)
  #     
  #     if(!is.na(finalLayer[[indScale]][1]) && invertScale==TRUE){
  #     
  #       curScale <- finalLayer[[indScale]]
  #       
  #       if(all(curScale==scaleTarget)) scaleTarget <- c(100, 0)
  #     
  #     } 
  #     
  #     
  #     ansLayer <- risk_layer(finalLayer[[indRawLay]], epidUnitLayer[[indRawLay]], scaleTarget)
  #     finalLayer[[indScale]] <- scaleTarget
  #     finalLayer[[indStandLay]] <- ansLayer
  #     
  #     
  #     #Save in global environment the standardized layer with new name
  #     assign(x = curLayerName, 
  #            value = finalLayer, 
  #            envir = .GlobalEnv)
  #   
  #   }
  #   
  #   rv$invert <- FALSE
  #   
  #   plot(ansLayer)
  #   
  # })
  # 
  
  ##### Observer on action button invert ####
  
  observeEvent(input$abInvert,{ 
    
    rv$invert <- TRUE
    
    
    
    
  })
  
  
  
  ##### Render for editable table of weight ####
  
  output$rhWeightTable <- renderRHandsontable({
    
    weightDF <- data.frame(rv$weightMatrix)
    
    #Remove epid. unit
    indRem <- which(colnames(weightDF)==isolate(epidUnitName()))
    if(!is.na(indRem[1])) weightDF <- weightDF[-indRem, -indRem]
    
    rhandsontable(weightDF, rowHeaderWidth = 200)
    
    
  })
  
  
  ##### Observer on weight editable table ####
  
  observeEvent(input$rhWeightTable,{
    
    #Retrieve data from editable table
    weightMat <- as.matrix(hot_to_r(input$rhWeightTable))
    
    
    
    glWeightMatrix <<- weightMat
    
    rv$weightVect <- compute_weights(weightMat)
    
    
  })
  
  ##### Render for weight graph bar display ####
  
  output$weightBarDisplay <- renderPlot({
    

    # riskFactors <- colnames(glWeightMatrix)
    # 
    # plot_weights(rv$weightVect, riskFactors)
    
    
  })
  

  
  
  ##### Render for result display ####
  
  output$resultDisplay <- renderPlot({
    
    curWeightVect <- rv$weightVect
    
    if(is.null(curWeightVect)) return(NULL)
    
    riskFactors <- colnames(glWeightMatrix)
    
    standRasterList <- list()
    
    for(k in 1:length(riskFactors)){
      
      curLayerName <- paste("layer_", riskFactors[k], sep = "")
  
        
        curLayer <- get(x = curLayerName,
                          envir = .GlobalEnv)
        
        
        standRasterList[[k]] <- curLayer[[indStandLay]]
        
 
      
    }
    
    plot(wlc(standRasterList, curWeightVect))
    
    
  })
  
  
}
