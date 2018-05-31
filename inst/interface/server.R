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



server <- function(input, output) {
  
  #### Reactive values ####
  # layerDF = data frame of layers
  rv <- reactiveValues(layerDF = NULL)
  
  
  ##### Observer on data frame of layers ####
  
  observeEvent(rv$layerDF,{
    
    # If data frame is empty
    if(is.null(nrow(rv$layerDF)) || nrow(rv$layerDF)==0){
      
      shinyjs::hide("miLayers")
      shinyjs::hide("abLayerRemove")
      
    } else {
        
      shinyjs::show("miLayers")
      shinyjs::show("abLayerRemove")
      
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
  
  
  ##### Render for the menu of layers ####
  
  output$miLayers <- renderMenu({
    
      # Retrieve short name of data frame of layers
      if(!("shortName" %in% colnames(rv$layerDF))) return(NULL)
    
      allLayers <- sort(unique(rv$layerDF$shortName))
    
      # Create menuSubItem for each name
      # Return a list of menuSubItems
      allLayersItems <- lapply(allLayers, function(i){menuSubItem(i, tabName = i)})
      
      # Update menu
      menuItem(langLayerItem[indLang], allLayersItems, startExpanded = TRUE)

  })
  
  
  ##### Observer on sidebarMenu ####
  
  observeEvent(input$tabs,{
    
    #print(input$tabs)
    
  })
  
  
  ##### Observer on button to remove a layer ####
  
  observeEvent(input$abLayerRemove,{
    
    # Retrieve name of the current tab
    curLayer <- input$tabs
    
    if(is.null(curLayer)) return()
    
    # Search name in the layers data frame and remove it
    indRem <- which(rv$layerDF$shortName==curLayer)
    
    if(!is.na(indRem[1])) rv$layerDF <- rv$layerDF[-indRem,]
    
  })
  
  
  ##### Render for the plot ####
  
  output$layerDisplay <- renderPlot({
    
    # Retrieve name of the current tab
    curLayer <- input$tabs
    
    if(is.null(curLayer)) return(NULL)
    
    # Search name in the layers data frame
    indLay <- which(rv$layerDF$shortName==curLayer)
    
    if(is.na(indLay[1])) return(NULL)
    
    curFile <- rv$layerDF[indLay,]
    
    finalLayer <- NULL
    
    # If vector layer
    if(unique(curFile$layerType)=="Vector"){
      
      # Retrieve line of shp file
      indShp <- grep(".shp", curFile$name)
      
      if(is.na(indShp[1])) return(NULL)
      
      shpFile <- curFile[indShp,]
      
      # Retrieve path of shp file to define dsn
      shpDir <- gsub(paste("/", shpFile$name, sep = ""),"", shpFile$datapath)
      
      # Extract shp file name without extention to define layer name
      shpLayer <- gsub(".shp","", shpFile$name)
      
      finalLayer <- readOGR(dsn = shpDir, layer = shpLayer, verbose = FALSE)   
      
    }
    
    # If raster layer
    if(unique(curFile$layerType)=="Raster"){
      
      finalLayer <- raster(curFile$datapath)
      
    }
    
    plot(finalLayer)
    
  })
  
}
