################### PURPOSE OF THE APP ###################
#Interface for PRODEL project
#May 2018, by Sylvain Falala, Unit CIRAD-INRA ASTRE

################### LIBRARIES ################### 
library(shinydashboard) # to create dashboard
#library(shinyjs) # for inputs show/hide functions
library(rhandsontable) # editable table

################### UI ###################

sidebar <- dashboardSidebar(
  
  sidebarMenu(id = "tabs",
    
    # Menu for input files
    menuItem(HTML(langMenuFile[indLang]), tabName = "fileTab"),
    
    # Menu for vectors
    menuItem(HTML(langMenuVector[indLang]), tabName = "vectorTab"),
    
    # Menu for rasters
    menuItem(HTML(langMenuRaster[indLang]), tabName = "rasterTab"),
    
    # Menu for weight table
    menuItem(HTML(langMenuWeight[indLang]), tabName = "weightTab"),
    
    # Menu for results
    menuItem(HTML(langMenuResult[indLang]), tabName = "resultTab")
    
  )
)


body <- dashboardBody(
  
  tabItems(
    
    tabItem("fileTab", 
            
            # To upload shape and raster files
            fileInput(inputId = "fiLayer", 
                      label = HTML(langTitleFileInput[indLang]),
                      multiple = TRUE, 
                      accept = acceptLayerType, 
                      width = NULL,
                      buttonLabel = HTML(langButtonFileInput[[indLang]][1]),
                      placeholder = HTML(langButtonFileInput[[indLang]][2])),
            
            tableOutput("allFileTable"),
            
            
            # List of upload files. Name of layer is editable
            rHandsontableOutput("rhFileTable")),
    
            
    
    tabItem("vectorTab", 
            
            h1("Onglet vecteurs"),
            uiOutput("uiVectorList"),
            plotOutput("vectorDisplay")),
    
    tabItem("rasterTab", 
            
            h1("Onglet raster"),
            uiOutput("uiRasterList"),
            plotOutput("rasterDisplay")),
    
    tabItem("weightTab", 
            
            h1("Onglet poids"),
            rHandsontableOutput("rhWeightTable")),
    
    tabItem("resultTab", 
            
            h1("Onglet resultats"),
            plotOutput("resultDisplay"))
    
  )
  
  
  
)


dashboardPage(
  dashboardHeader(title = appTitle),
  sidebar,
  body
)

