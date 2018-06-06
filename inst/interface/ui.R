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
            
            fluidRow(
            
              box(title = HTML(langBoxFile[indLang]), status = "primary", width = 6, solidHeader = TRUE,
              
                # To upload shape and raster files
                fileInput(inputId = "fiLayer", 
                          label = HTML(langTitleFileInput[indLang]),
                          multiple = TRUE, 
                          accept = acceptLayerType, 
                          width = NULL,
                          buttonLabel = HTML(langButtonFileInput[[indLang]][1]),
                          placeholder = HTML(langButtonFileInput[[indLang]][2])),
                
                # List of all upload files
                tableOutput("allFileTable")
            
                 ),
            
              
              box(title = HTML(langBoxLayer[indLang]), status = "success", width = 6, solidHeader = TRUE,
            
                  # List of layers. Name is editable
                  rHandsontableOutput("rhLayerTable")
            
                 )
            
            )
    
    ),
    
    tabItem("vectorTab", 
            
            uiOutput("uiVectorList"),
            
           
              
              box(title = HTML(langBoxVMap[indLang]), status = "primary", width = 6, solidHeader = TRUE,
                  
                  plotOutput("vectorDisplay")
                  
                 ),
              
              
              box(title = HTML(langBoxVDist[indLang]), status = "success", width = 6, solidHeader = TRUE,
                  
                  plotOutput("distanceDisplay")
                  
                 )

            ),
    
    tabItem("rasterTab", 
            
            uiOutput("uiRasterList"),
            
            box(title = HTML(langBoxRawMap[indLang]), status = "primary", width = 6, solidHeader = TRUE,
                
                plotOutput("rawRasterDisplay")
                
               ),
            
            
            box(title = HTML(langBoxProcMap[indLang]), status = "success", width = 6, solidHeader = TRUE,
                
                
                plotOutput("processedRasterDisplay")
                
               ),
            
            actionButton(inputId = "abRasterInvert", 
                         label = langABRasterInvert[indLang] ,
                         icon = NULL, width = NULL)
            
          ),
            
            
            
    
    tabItem("weightTab", 
            
            h1("Poids"),
            rHandsontableOutput("rhWeightTable"),
            plotOutput("weightBarDisplay")
            
    
          ),
            
    tabItem("resultTab", 
            
            h1(HTML(langMenuResult[indLang])),
            plotOutput("resultDisplay")
            
            
            )
    
  )
  
  
  
)


dashboardPage(
  dashboardHeader(title = appTitle),
  sidebar,
  body
)

