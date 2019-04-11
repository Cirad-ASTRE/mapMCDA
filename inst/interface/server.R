################### PURPOSE OF THE APP ###################
# Interface for mapMCDA project
# April 2019, by Sylvain Falala, Unit CIRAD-INRA ASTRE

# For shiny basic fileInput
# To avoid "Maximum upload size exceeded"
# Define upload limit as 20MB
#options(shiny.maxRequestSize=20*1024^2)

server <- function(input, output, session) {
  
  #### Reactive values ####
  # layerInfoDF = data frame of information on layers
  # layerList = list of layers
  # weightMatrix = matrix of weight
  # weightVect = result of function compute_weights
  # invert = invert scale for standardized raster
  # finalRaster = result raster
  rv <- reactiveValues(layerInfoDF = NULL,
                       layerList = NULL,
                       weightMatrix = NULL,
                       weightVect = NULL,
                       invert = FALSE,
                       finalRaster = NULL)
  
  
  # For shinyFiles package
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyFileChoose(input, "file", roots = volumes, session = session) # use after input$file 
  
  
  #### Observer on language radio buttons ####
  
  observeEvent(input$rbLanguage,{
    
    curLang <- as.numeric(input$rbLanguage)
    
    # Update sidebar
    output$fileMenuTextUI <- renderUI({HTML(langDF["MenuFile", curLang])})
    
    output$unitMenuTextUI <- renderUI({HTML(langDF["MenuUnit", curLang])})
    
    output$riskMenuTextUI <- renderUI({HTML(langDF["MenuRisk", curLang])})
    
    output$weightMenuTextUI <- renderUI({HTML(langDF["MenuWeight", curLang])})
    
    output$resultMenuTextUI <- renderUI({HTML(langDF["MenuResult", curLang])})
    
    # Update boxes
    # Files
    output$fileBoxTextUI <- renderUI({HTML(langDF["BoxFile", curLang])})
    
    output$fileInputTextUI <- renderUI({HTML(langDF["TitleFileInput", curLang])})
    
    output$fileInputButtonLblTextUI <- renderUI({HTML(langDF["ButtonFileInput", curLang])})
    
    output$layerBoxTextUI <- renderUI({HTML(langDF["BoxLayer", curLang])})
    
    # Epidemiological units
    output$unitMapTextUI <- renderUI({HTML(langDF["BoxUnitMap", curLang])})
    
    output$unitStatTextUI <- renderUI({HTML(langDF["BoxUnitStat", curLang])})
    
    # Risk
    output$riskRawMapTextUI <- renderUI({HTML(langDF["BoxRiskRawMap", curLang])})
    
    output$riskStandRasterTextUI <- renderUI({HTML(langDF["BoxRiskStandRaster", curLang])})
    
    updateActionButton(session, "abInvert", label = HTML(langDF["ABRiskRasterInvert", curLang]))
    
    # Weights
    output$weightMatrixTextUI <- renderUI({HTML(langDF["BoxWeightMatrix", curLang])})
    
    output$weightBarTextUI <- renderUI({HTML(langDF["BoxWeightBar", curLang])})
    
    # Results
    output$resultTextUI <- renderUI({HTML(langDF["BoxResult", curLang])})
    
    output$resultPerUnitTextUI <- renderUI({HTML(langDF["BoxResultPerUnit", curLang])})
    
    output$levelRiskTextUI <- renderUI({HTML(langDF["SIRiskLevel", curLang])})
    
    output$resultExportTextUI <- renderUI({HTML(langDF["DownButton", curLang])})
    
    output$resultPUExportTextUI <- renderUI({HTML(langDF["DownButton", curLang])})
    
    # Help
    output$fileBoxHelpTextUI <- renderUI({HTML(langHelpFiles[curLang])})
    
    output$layerBoxHelpTextUI <- renderUI({HTML(langHelpLayers[curLang])})
    
    output$riskHelpTextUI <- renderUI({HTML(langHelpScale[curLang])})

    output$weightHelpTextUI <- renderUI({HTML(langHelpMatrix[curLang])})
    
  })
  
  
  ##### Observer on file input ####
  
  #observeEvent(input$fiLayer,{
  observe({ 
   
    # For shiny basic fileInput
    
    # input$fiLayer will be NULL initially. After the user selects and uploads a file,
    # it will be a data frame with 'name', 'size', 'type', and 'datapath' columns.
    # The 'datapath' column will contain the local filenames where the data can be found.

    # layerFiles <- input$fiLayer
  
    # if(is.null(layerFiles)) return()

    # Upload files are copied in a temporary folder 'datapath'and are renamed like '0.shp', '1.shx'
    # We need to rename the files with their original names to work with it
    
    # oldNames <- layerFiles$datapath
    # curDir <- dirname(oldNames)
    # newNames <- file.path(curDir, layerFiles$name)
    # file.rename(from = oldNames, to = newNames)
    # layerFiles$datapath <- newNames
    
    
    # For shinyFilesButton
    
    # input$file is a data frame with 'name', 'size', 'type', and 'datapath' columns.
    # input$file has nrow = 0 initially. After the user selects and uploads files, 
    # the 'datapath' column will contain the local filenames where the data can be found.
    layerFiles <- data.frame(
                      parseFilePaths(roots = volumes, 
                                     selection = input$file), 
                      stringsAsFactors = FALSE)
    
    if(nrow(layerFiles)==0) return()
    
    # Load layers
    layerFiles$name <- as.character(layerFiles$name)

    nbFiles <- nrow(layerFiles)
    
    layerFiles$isNull <- rep(FALSE, nbFiles)
    
    tempLayer <- list()
    
    indList <- 0
    
    for(k in 1:nbFiles){
      
      myLayer <- load_layer(layerFiles[k, "datapath"])
      
      if(is.null(myLayer)){
        
        layerFiles[k, "isNull"] <- TRUE
        
      } else {
      
        indList <- indList + 1
        
        tempLayer[[indList]] <- myLayer
        
      }

    }
    

    
    if(all(layerFiles$isNull==TRUE)) {
      print("Selected files are no layers")
      return()
      }
    
    acceptedFiles <- layerFiles
    
    indRem <- which(layerFiles$isNull==TRUE)
    
    if(!is.na(indRem[1])) acceptedFiles <- layerFiles[-indRem,]
    
    nbLayer <- nrow(acceptedFiles)
    
    # Update dataframe with info on layers
    
    layerInfo <- subset(acceptedFiles, select = name)
    
    # Retrieve class of layer
    if(nbLayer>1){
      layerInfo$layerType <- as.character(sapply(tempLayer, class))
    } else {
      layerInfo$layerType <- paste(class(tempLayer[[1]]), collapse = "_")
      
    }
    
    
    # Rename class name
    layerInfo$layerType <- gsub("SpatialPolygonsDataFrame", lVect[indLang], layerInfo$layerType, fixed = TRUE)
    layerInfo$layerType <- gsub("RasterLayer", lRast[indLang], layerInfo$layerType, fixed = TRUE)
    layerInfo$layerType <- gsub("c(\"geonetwork\", \"igraph\")", lMob[indLang], layerInfo$layerType, fixed = TRUE)
    layerInfo$layerType <- gsub("geonetwork_igraph", lMob[indLang], layerInfo$layerType, fixed = TRUE)
    
    # Create short name for the layers (based on the file name)
    layerInfo$shortName <- rmext(layerInfo$name)
    
    # Remove special characters
    layerInfo$shortName <- iconv(layerInfo$shortName, from = "UTF-8", to = "ASCII", sub = "")
    
    # Remove blanks  
    layerInfo$shortName <- gsub("\\s+", "", layerInfo$shortName, ignore.case = TRUE, perl = TRUE)
    
    layerInfo$originalName <- layerInfo$shortName
    
    # Add column to define layer as administrative units
    layerInfo$adminUnit <- rep(FALSE, nbLayer)
    
    
    # Prepare list of layers
    layerList <- vector("list", nbLayer) # pre-allocate
    
    # layerName = shortName of the layer
    # layerRaw = raw layer
    # layerScale = sclae, default is c(0, 100)
    # layerStand = standardized raster
    
    for(k in 1:nbLayer){
      
         layerList[[k]] <- list(layerName = layerInfo[k,"shortName"], 
                                layerRaw = tempLayer[[k]], 
                                layerScale = c(0, 100),
                                layerStand = NULL)
      
    }
    
    isolate({
    
      # Update list of layers and data frame with info on layers
      
      if(length(rv$layerList)==0){
        
        rv$layerList <- layerList
        
        rv$layerInfoDF <- layerInfo
  
      } else {
        
        rv$layerList <- c(rv$layerList, layerList)
        
        rv$layerInfoDF <- rbind(rv$layerInfoDF, layerInfo)
  
      }
    
    })
  
  })
  
  
  ##### Observer on layerInfoDF for weight matrix ####
  
  observeEvent(rv$layerInfoDF,{
    
    curLayerInfoDF <- rv$layerInfoDF
    
    if(is.null(rv$layerInfoDF)) return()
    
    # Remove epid. unit
    indAdmin <- which(curLayerInfoDF$adminUnit==TRUE)
    if(!is.na(indAdmin[1])) curLayerInfoDF <- curLayerInfoDF[-indAdmin,]
    
    nbLayer <- nrow(curLayerInfoDF)
    layerNames <- curLayerInfoDF$shortName 

    rv$weightMatrix <- matrix(data = 1.0, nrow = nbLayer, ncol = nbLayer, dimnames = list(layerNames, layerNames))
    
    resmat <- try(compute_weights(rv$weightMatrix))
    
    if (!inherits(resmat, 'try-error')) {
      
      rv$weightVect <- resmat}

  })
  
  
  ##### Render for accepted files ####
  
  output$accFileTable <- renderTable({
    
    # Retrieve filenames in data frame of info on layers
    if(is.null(rv$layerInfoDF)) return(NULL)
    
    shortLayerDF <- subset(rv$layerInfoDF, select = "name")
    
    colnames(shortLayerDF) <- langFileList[indLang]
    
    shortLayerDF
    
  })
  
  
  ##### Render for layer editable table ####
  
  output$rhLayerTable <- renderRHandsontable({
    
    # Retrieve short name of data frame of layers
    if(!("shortName" %in% colnames(rv$layerInfoDF))) return(NULL)
    
    shortLayerDF <- subset(rv$layerInfoDF, select = toEditLayerColNames)
    
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
    
    
    # Rename layers
    nbLayer <- nrow(rv$layerInfoDF)
    
    for(k in 1:nbLayer){
      
      curLayerSN <- rv$layerInfoDF[k, "shortName"]
      
      if(newLayerNames[k]!=curLayerSN){
        
        rv$layerList[[k]]$layerName <- newLayerNames[k]
        
        
      }
      
    }
    
    # Update data frame of info on layers
    rv$layerInfoDF$shortName <- newLayerNames
    
    rv$layerInfoDF$adminUnit <- editLayerDF[,newAdminCol]
    
  })
  
  
  
  ##### Reactive expression to return index of epid. unit layer ####
  
  epidUnitNum <- reactive({
    
    curLayerInfoDF <- rv$layerInfoDF
    
    if(!("adminUnit" %in% colnames(curLayerInfoDF))) return(NULL)
    
    indAdmin <- which(curLayerInfoDF$adminUnit==TRUE)
    if(is.na(indAdmin[1])) return(NULL)
    
    if(length(indAdmin)>1){
      
      print("Warning: Nb of epidemiological unit > 1")
      return(NULL)
      
    }
    
    indAdmin
    
    
  })
  
  
  ##### Reactive expression to return epid. unit layer ####
  
  curEpidUnitLayer <- eventReactive(epidUnitNum(),{
    
    curEpidUnit <- epidUnitNum()
    
    if(is.null(curEpidUnit)) return(NULL)
    
    rv$layerList[[curEpidUnit]]$layerRaw
    
    
  })
  
  
  
  ##### Render for the layer name of epid. unit ####
  
  output$unitNameText <- renderUI({
    
    curEpidUnit <- epidUnitNum()
    
    if(is.null(curEpidUnit)) return(NULL)
    
    h3(rv$layerList[[curEpidUnit]]$layerName)
    
    
  })
  
  
  ##### Render for the map of epid. unit ####
  
  output$unitMapDisplay <- renderPlot({
    
    epidUnitLayer <- curEpidUnitLayer()
    
    if(is.null(epidUnitLayer)) return(NULL)
    
    
    plot(epidUnitLayer)
    
    
  })
  
  
  ##### Summary statistics of epid. unit ####
  
  output$unitStatText <- renderText({
    
    epidUnitLayer <- curEpidUnitLayer()
    
    if(is.null(epidUnitLayer)) return(NULL)
    
    if( !isTRUE(is.projected(epidUnitLayer))) {
      warning("This map is not projected. This can lead to very
              inaccurate computations of distances and areas, depending
              on the location and size of the region of interest.
              Proceed with caution.")
    }
    
    paste("N. epidemiological units:", length(epidUnitLayer))
   
  })
  
  output$unitStatDisplay <- renderPlot({
    
    epidUnitLayer <- curEpidUnitLayer()
    
    if(is.null(epidUnitLayer)) return(NULL)
    
    hist(
      rgeos::gArea(epidUnitLayer, byid = TRUE),
      main = "Distribution of unit-areas",
      xlab = "Area (in map units, squared)"
    )
  })
  
  
  
  
  ##### Render for the list of layers in risk tab ####
  
  output$uiRiskLayerList <- renderUI({
    
    curLayerInfoDF <- rv$layerInfoDF
    
    # Retrieve short name of data frame of layers
    if(!("shortName" %in% colnames(curLayerInfoDF))) return(NULL)
    
    shortNames <- curLayerInfoDF$shortName
    
    # Remove epid. unit
    euNum <- isolate(epidUnitNum())
    if(!is.null(euNum)) shortNames <- shortNames[-euNum]
    
    
    radioButtons(inputId = "rbRiskLayer",
                 label = HTML(langDF["RBRiskLayer",indLang]),
                 choices = shortNames,
                 inline = TRUE)
    
  })
  
  
  ##### Render for raw layer display in risk tab ####
  
  output$rawLayerDisplay <- renderPlot({
    
    # Retrieve name of the current tab
    curLayerSN <- input$rbRiskLayer
    
    if(is.null(curLayerSN)) return(NULL)
    
    
    for(k in 1:length(rv$layerList)){
      
      if(curLayerSN==rv$layerList[[k]]$layerName) break
      
    }
    
    
    # Plot the epidemiological units lightly in the background
    epidUnitLayer <- isolate(curEpidUnitLayer())
    
    if(is.null(epidUnitLayer)) {
      addsetting = FALSE
    } else {
      plot(epidUnitLayer, border = "lightgray")
      addsetting = TRUE
    }
    
    
    plot(rv$layerList[[k]]$layerRaw, add = addsetting)
    
    
  })
  
  
  ##### Reactive expression which return standardized raster ####
  
  curStandRaster <- reactive({
    
    # Retrieve name of the current tab
    curLayerSN <- input$rbRiskLayer
    
    if(is.null(curLayerSN)) return(NULL)
    
    for(curInd in 1:length(isolate(rv$layerList))){
      
      if(curLayerSN==rv$layerList[[curInd]]$layerName) break
      
    }

    
    standRaster <- NULL
    
    invertScale <- rv$invert
    
    
    if(inherits(rv$layerList[[curInd]]$layerStand, c("Spatial", "RasterLayer", "igraph")) && invertScale==FALSE) {
      
      standRaster <- rv$layerList[[curInd]]$layerStand
      
    } else {
      
      epidUnitLayer <- isolate(curEpidUnitLayer())
      
      if(is.null(epidUnitLayer)) return(NULL)
      
      scaleTarget <- c(0, 100)
      
      
      if(!is.na(rv$layerList[[curInd]]$layerScale[1]) && invertScale==TRUE){
        
        curScale <- rv$layerList[[curInd]]$layerScale
        
        if(all(curScale==scaleTarget)) scaleTarget <- c(100, 0)
        
      }
      
      #print(scaleTarget)
      
      if (inherits(rv$layerList[[curInd]]$layerStand, c("Spatial", "RasterLayer", "igraph"))) {
        ## Already computed
        rv$layerList[[curInd]]$layerStand <-
          100 - rv$layerList[[curInd]]$layerStand
      } else {
        ## First time: compute risk scale
        rv$layerList[[curInd]]$layerStand <-
          risk_layer(rv$layerList[[curInd]]$layerRaw, epidUnitLayer, scaleTarget)
      }
      rv$layerList[[curInd]]$layerScale <- scaleTarget
      
    }
    
    rv$invert <- FALSE
    
    rv$layerList[[curInd]]$layerStand
    
    
  })
  
  
  ##### Render for standardized raster in risk tab ####
  
  output$standRasterDisplay <- renderPlot({
    
    myStandRaster <- curStandRaster()
    
    if(is.null(myStandRaster)) return(NULL)
    
    plot(myStandRaster)
    
    
  })
  
  
  ##### Observer on action button invert ####
  
  observeEvent(input$abInvert,{ 
    
    rv$invert <- TRUE
    
  })
  
  
  ##### Render for editable table of weight ####
  
  output$rhWeightTable <- renderRHandsontable({
    
    weightDF <- data.frame(rv$weightMatrix)
    
    if(nrow(weightDF)==0) return(NULL)
    
    nbCol <- ncol(weightDF)
    
    rht <- rhandsontable(weightDF, rowHeaderWidth = 200)
    
    # Format table
    rht <- hot_col(rht, c(1:nbCol), format = "0.00")
    
    for(k in 1:nbCol){
      
      rht <- hot_cell(rht, k, k, readOnly = TRUE)
      
    }
    
    rht
    
  })
  
  
  ##### Observer on weight editable table ####
  
  observeEvent(input$rhWeightTable$changes$changes,{
    
    # Row index with change. First Row start to index 0
    indRow <- input$rhWeightTable$changes$changes[[1]][[1]] + 1
    
    # Column index with change. First column start to index 0
    indCol <- input$rhWeightTable$changes$changes[[1]][[2]] + 1
    
    # Old value
    oldVal <- input$rhWeightTable$changes$changes[[1]][[3]]
    
    # New value
    newVal <- input$rhWeightTable$changes$changes[[1]][[4]]
    
    
    weightMat <- as.matrix(hot_to_r(input$rhWeightTable))
    
    tempMatrix <- rv$weightMatrix
    
    tempMatrix[indRow,indCol] <- weightMat[indRow,indCol]
    
    tempMatrix[indCol,indRow] <- 1.0/tempMatrix[indRow,indCol]
    
    #print(tempMatrix)
    
    rv$weightMatrix <- tempMatrix
    
    
    resmat <- try(compute_weights(rv$weightMatrix))
    
    if (!inherits(resmat, 'try-error')) {
      
      rv$weightVect <- resmat
      #print(cat("Weight vector: ", rv$weightVect))
      
      }
    
  })
  
  
  ##### Render for weight graph bar display ####
  
  output$weightBarDisplay <- renderPlot({
    
    isolate({
    
      if(is.null(nrow(rv$weightMatrix))) return(NULL)
      
      riskFactors <- colnames(rv$weightMatrix)
    
    })
    
    mapMCDA:::plot_weights(rv$weightVect, riskFactors)
    
    
  })
  
  
  
  ##### Render for result display ####
  
  output$resultDisplay <- renderPlot({
    
    curWeightVect <- rv$weightVect
    
    # cat("Weight vector: ", curWeightVect)
    
    if(is.null(curWeightVect)) return(NULL)
    
    epidNum <- isolate(epidUnitNum())
    
    if(is.null(epidNum)) return(NULL)
    
    
    # Retrieve standardized raster and remove units layer
    
    nbLayer <- length(rv$layerList)
    
    standRasterList <- list()
    
    ind <- 0
    
    for(k in 1:nbLayer){
      
      if(k!=epidNum){
        
        if(is.null(rv$layerList[[k]]$layerStand)){
          
          print(paste("Please standardize ", rv$layerList[[k]]$layerName, sep = ""))
          return(NULL)
          
          }
        
        ind <- ind + 1

        standRasterList[[ind]] <- rv$layerList[[k]]$layerStand
      
      }
      
    }
    
    #if(all(sapply(standRasterList, is.null))) return(NULL)
    
    finRast <- wlc(standRasterList, curWeightVect)
    
    rv$finalRaster <- finRast
    
    plot(finRast)
    
    
  })
  
  
  ##### Render for display of final unit raster ####
  
  output$resultUnitDisplay <- renderPlot({
    
    epidUnitLayer <- isolate(curEpidUnitLayer())
    
    if(is.null(epidUnitLayer)) return(NULL)
    
    finRast <- rv$finalRaster
    
    if(is.null(finRast)) return(NULL)
    
    risk_plot(
      epidUnitLayer,
      risk_unit(finRast, epidUnitLayer), 
      n = as.numeric(input$siLevelRisk)
    )

  })
  
  
  
  ##### To export raster 1 #####
  
  output$exportResultRaster <- downloadHandler(
    
    filename = paste0(Sys.Date(), '_final_risk.tif'),
    
    content = function(con) {
      isolate(
        raster::writeRaster(rv$finalRaster, con)
      )
    }
    
  )
  
  
  ##### To export vector 2 with epid unit #####
  
  output$exportResultVector <- downloadHandler(
    filename = paste0(Sys.Date(), '_final_risk_by_unit.gpkg'),
    content = function(con) {
      epidUnitLayer <- isolate(curEpidUnitLayer())
      if(is.null(epidUnitLayer)) return(NULL)
      ev <- epidUnitLayer
      ev$mapMCDA_risk <- risk_unit(rv$finalRaster, ev)
      rgdal::writeOGR(ev, con, layer = "risk", driver = "GPKG")
    },
    contentType = "application/vnd.opengeospatial.geopackage+sqlite3"
  )
  
  ##### To export csv with risk levels by epid unit #####
  
  output$exportResultCSV <- downloadHandler(
    filename = paste0(Sys.Date(), '_final_risk_category_by_unit.csv'),
    content = function(con) {
      epidUnitLayer <- isolate(curEpidUnitLayer())
      if(is.null(epidUnitLayer)) return(NULL)
      ev <- epidUnitLayer
      rt <- risk_table(
        ev,
        risk_unit(rv$finalRaster, ev), 
        n = as.numeric(input$siLevelRisk)
      )
      write.csv(rt, con, row.names = FALSE)
    }
  )
  
  
}