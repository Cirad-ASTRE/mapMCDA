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
  # layerDF = data frame of layers
  rv <- reactiveValues(layerDF = NULL)
  
  
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
    
    # Retrieve file extension to define type of layer: vector or raster
    fileExt <- str_extract(layerFiles$name, reExt)
    fileExt <- tolower(str_replace(fileExt, "\\.", ""))
    
    layerFiles$layerType <- rep("Unknown", nrow(layerFiles))
    
    indVect <- which(fileExt %in% vectorExt)
    if(!is.na(indVect[1])) layerFiles[indVect,"layerType"] <- "Vector"
    
    indRast <- which(fileExt %in% rasterExt)
    if(!is.na(indRast[1])) layerFiles[indRast,"layerType"] <- "Raster"
    
    # Create short name for the layer based on the file name
    # Remove file extension
    layerFiles$shortName <- gsub(reExt, "", layerFiles$name, ignore.case = TRUE, perl = TRUE) 
    
    # Remove blanks  
    layerFiles$shortName <- gsub("\\s+", "", layerFiles$shortName, ignore.case = TRUE, perl = TRUE) 

    # Update layerDF
    if(is.null(nrow(rv$layerDF))) rv$layerDF <- layerFiles else rv$layerDF <- rbind(rv$layerDF, layerFiles)
    
    
    
  })
  
  
  ##### Render for the list of vectors ####
  
  output$uiVectorList <- renderUI({

      # Retrieve short name of data frame of layers
      if(!("layerType" %in% colnames(rv$layerDF))) return(NULL)

      indVect <- which(rv$layerDF$layerType=="Vector")
      
      if(is.na(indVect[1])) return(NULL)
      
      shortNames <- sort(unique(rv$layerDF[indVect,"shortName"]))

      radioButtons(inputId = "rbVectorLayer", label = NULL, choices = shortNames, inline = TRUE)

  })
  
  
  ##### Render for the list of rasters ####
  
  output$uiRasterList <- renderUI({
    
    # Retrieve short name of data frame of layers
    if(!("layerType" %in% colnames(rv$layerDF))) return(NULL)
    
    indRast <- which(rv$layerDF$layerType=="Raster")
    
    if(is.na(indRast[1])) return(NULL)
    
    shortNames <- sort(unique(rv$layerDF[indRast,"shortName"]))
    
    radioButtons(inputId = "rbRasterLayer", label = NULL, choices = shortNames, inline = TRUE)
    
  })
  
  
  ##### Observer on sidebarMenu ####
  
  observeEvent(input$tabs,{

    print(input$tabs)
    
  })
  
  
  ##### Observer on button to remove a layer ####
  
  # observeEvent(input$abLayerRemove,{
  #   
  #   # Retrieve name of the current tab
  #   curLayer <- input$tabs
  #   
  #   if(is.null(curLayer)) return()
  #   
  #   # Search name in the layers data frame and remove it
  #   indRem <- which(rv$layerDF$shortName==curLayer)
  #   
  #   if(!is.na(indRem[1])) rv$layerDF <- rv$layerDF[-indRem,]
  #   
  # })
  # 
  
  ##### Render for vector display ####
  
  output$vectorDisplay <- renderPlot({

    # Retrieve name of the current tab
    curLayer <- input$rbVectorLayer

    if(is.null(curLayer)) return(NULL)

    # Search name in the layers data frame
    indLay <- which(rv$layerDF$shortName==curLayer)

    if(is.na(indLay[1])) return(NULL)

    curFile <- rv$layerDF[indLay,]


    # Retrieve line of shp file
    indShp <- grep(".shp", curFile$name)

    if(is.na(indShp[1])) return(NULL)

    shpFile <- curFile[indShp,]

    # Retrieve path of shp file to define dsn
    shpDir <- gsub(paste("/", shpFile$name, sep = ""),"", shpFile$datapath)

    # Extract shp file name without extention to define layer name
    shpLayer <- gsub(".shp","", shpFile$name)

    finalLayer <- readOGR(dsn = shpDir, layer = shpLayer, verbose = FALSE)

    plot(finalLayer)

  })

  
  ##### Render for raster display ####
  
  output$rasterDisplay <- renderPlot({
    
    # Retrieve name of the current tab
    curLayer <- input$rbRasterLayer
    
    if(is.null(curLayer)) return(NULL)
    
    # Search name in the layers data frame
    indLay <- which(rv$layerDF$shortName==curLayer)
    
    if(is.na(indLay[1])) return(NULL)
    
    curFile <- rv$layerDF[indLay,]
    
    finalLayer <- raster(curFile$datapath)
    
    plot(finalLayer)
    
    
  })
  
  
  ##### Render for editable table of weight ####
  
  output$rhWeightTable <- renderRHandsontable({
    
    # Retrieve short name of data frame of layers
    if(!("shortName" %in% colnames(rv$layerDF))) return(NULL)
    
    layerNames <- sort(unique(rv$layerDF$shortName))
    
    nbLayer <- length(layerNames)
    
    weightDF <- data.frame(diag(nrow = nbLayer, ncol = nbLayer))
    
    colnames(weightDF) <- layerNames
    
    rownames(weightDF) <- layerNames
    
    rhandsontable(weightDF, rowHeaderWidth = 200)
    
  })
  
  
  
  ##### Render for result display ####
  
  output$resultDisplay <- renderPlot({
    
    
    
  })
  
  
}
