################### PURPOSE OF THE APP ###################
#Interface for PRODEL project
#May 2018, by Sylvain Falala, Unit CIRAD-INRA ASTRE

################### LIBRARIES ################### 
library(shinydashboard) #to create dashboard
library(shinyjs) #for inputs enable/disable functions

################### UI ###################

sidebar <- dashboardSidebar(
  
  useShinyjs(),
  
  sidebarMenu(id = "tabs",
    
    # # To select language
    # selectInput(inputId = "siLanguage",
    #             label = NULL,
    #             choices = languages,
    #             selected = as.character(indLang)),
    
    # To upload shape and raster files
    fileInput(inputId = "fiLayer", 
              label = HTML(langTitleFileInput[indLang]),
              multiple = TRUE, 
              accept = acceptLayerType, 
              width = NULL,
              buttonLabel = HTML(langButtonFileInput[[indLang]][1]),
              placeholder = HTML(langButtonFileInput[[indLang]][2])),
    
    
    # Items list for layers (1 layer = 1 item)
    menuItemOutput(outputId = "miLayers"),
    
    shinyjs::hidden(actionButton(inputId = "abLayerRemove", label = langLayerRemove[indLang] ,
                                 icon = NULL, width = NULL))
    
  )
)


body <- dashboardBody(
  
  plotOutput("layerDisplay")
  
)


dashboardPage(
  dashboardHeader(title = appTitle),
  sidebar,
  body
)

