################### PURPOSE OF THE APP ###################
#Interface for PRODEL project
#May 2018, by Sylvain Falala, Unit CIRAD-INRA ASTRE

################### LIBRARIES ################### 
library(shinydashboard) # to create dashboard

server <- function(input, output) {
  
  # Reactive values
  # layerDF = data frame of layers
  rv <- reactiveValues(layerDF = NULL)
  
  
  observeEvent(input$fiLayer, {
    
    # input$fiLayer will be NULL initially. After the user selects and uploads a file, 
    # it will be a data frame with 'name', 'size', 'type', and 'datapath' columns.
    # The 'datapath' column will contain the local filenames where the data can be found.
    
    layerFiles <- input$fiLayer
    
    if(is.null(layerFiles)) return()
    
    #print(layerFiles$datapath)
    
    # Create short name for the layer based on the file name
    # Remove file extension
    layerFiles$shortName <- gsub("\\.\\w{3,}$", "", layerFiles$name, ignore.case = TRUE, perl = TRUE) 
    
    # Remove blanks  
    layerFiles$shortName <- gsub("\\s+", "", layerFiles$shortName, ignore.case = TRUE, perl = TRUE) 

    # Update layerDF
    if(is.null(nrow(rv$layerDF))) rv$layerDF <- layerFiles else rv$layerDF <- rbind(rv$layerDF, layerFiles)
    
    shinyjs::show("abLayerRemove")
    
    print(rv$layerDF$shortName)
    
    
  })
  
  
  
  output$miLayers <- renderMenu({
    
      allLayers <- rv$layerDF$shortName
    
      if(is.null(allLayers)) return()
    
      # Create menuSubItem for each file name
      # Return a list of menuSubItems
      allLayersItems <- lapply(allLayers, function(i){menuSubItem(i, tabName = i)})
      
      menuItem(langLayerItem[indLang], allLayersItems, startExpanded = TRUE)

  })
  
  
  observeEvent(input$tabs,{
    
    print(input$tabs)
    
  })
  
  
  observeEvent(input$abLayerRemove,{
    
    curLayer <- input$tabs
    
    print(curLayer)
    
    if(is.null(curLayer)) return()
    
    indRem <- which(rv$layerDF$shortName==curLayer)
    
    if(is.na(indRem[1])) return()
    
    print(indRem)
    
    rv$layerDF <- rv$layerDF[-indRem,]
    
    if(nrow(rv$layerDF)==0) shinyjs::hide("abLayerRemove")
    
    print(rv$layerDF$shortName)
    
  })
  
  

}