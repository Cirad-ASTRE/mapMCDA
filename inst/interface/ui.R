################### PURPOSE OF THE APP ###################
#Interface for PRODEL project
#May 2018, by Sylvain Falala, Unit CIRAD-INRA ASTRE

################### LIBRARIES ################### 
library(shinydashboard) # to create dashboard
#library(shinyjs) # for inputs show/hide functions
library(rhandsontable) # editable table

################### UI ###################

sidebar <- dashboardSidebar(
  
  #useShinyjs(),
  
  sidebarMenu(id = "tabs",
    
    # To upload shape and raster files
    fileInput(inputId = "fiLayer", 
              label = HTML(langTitleFileInput[indLang]),
              multiple = TRUE, 
              accept = acceptLayerType, 
              width = NULL,
              buttonLabel = HTML(langButtonFileInput[[indLang]][1]),
              placeholder = HTML(langButtonFileInput[[indLang]][2])),
    
    
    # Items list for layers (1 layer = 1 item)
    menuItem("Vecteurs", tabName = "vectorTab"),
    
    # Items list for layers (1 layer = 1 item)
    menuItem("Rasters", tabName = "rasterTab"),
    
    # Items list for layers (1 layer = 1 item)
    menuItem("Poids", tabName = "weightTab"),
    
    # Items list for layers (1 layer = 1 item)
    menuItem("Resultats", tabName = "resultTab")
    
  )
)


body <- dashboardBody(
  
  tabItems(
    
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

