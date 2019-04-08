################### PURPOSE OF THE APP ###################
# Interface for mapMCDA project
# April 2019, by Sylvain Falala, Unit CIRAD-INRA ASTRE


################### UI ###################

#### SIDEBAR ####

sidebar <- dashboardSidebar(
  
  # Radio buttons to select language
  radioButtons(inputId = "rbLanguage",
               label = NULL,
               choiceNames = langChoiceNames,
               choiceValues = langChoiceVal,
               selected = as.character(indLang),
               inline = FALSE),
  
  sidebarMenu(id = "tabs",
              
              # Menu for input files
              menuItem(uiOutput("fileMenuTextUI"), tabName = "fileTab"),
              
              # Menu for epidemiological units
              menuItem(uiOutput("unitMenuTextUI"), tabName = "unitTab"),
              
              # Menu for risk factors
              menuItem(uiOutput("riskMenuTextUI"), tabName = "riskTab"),
              
              # Menu for weight table
              menuItem(uiOutput("weightMenuTextUI"), tabName = "weightTab"),
              
              # Menu for results
              menuItem(uiOutput("resultMenuTextUI"), tabName = "resultTab")
              
  )
)

#### BODY ####

body <- dashboardBody(
  
  tabItems(
    
    # Files
    tabItem("fileTab", 
            
            fluidRow(
              
              box(title = uiOutput("fileBoxTextUI"), status = "primary", width = 5, solidHeader = TRUE,
                  footer = uiOutput("fileBoxHelpTextUI"),
                  
                  # Button from shinyFiles package
                  # To upload vector, raster and igraph files
                  shinyFilesButton(id = "file", 
                                   label = uiOutput("fileInputButtonLblTextUI"),
                                   title = "Please select a file",
                                   multiple = TRUE),
                  
                  # # Shiny basic fileInput
                  # # To upload vector, raster and igraph files
                  # fileInput(inputId = "fiLayer", 
                  #           label = uiOutput("fileInputTextUI"),
                  #           multiple = TRUE, 
                  #           accept = NULL, #acceptLayerType, 
                  #           width = NULL,
                  #           buttonLabel = uiOutput("fileInputButtonLblTextUI"),
                  #           placeholder = "No file"),
                  
                  # List of accepted files
                  tableOutput("accFileTable")
                  
              ),
              
              
              box(title = uiOutput("layerBoxTextUI"), status = "success", width = 7, solidHeader = TRUE,
                  footer = uiOutput("layerBoxHelpTextUI"),
                  
                  # List of layers. Name is editable
                  rHandsontableOutput("rhLayerTable")
                  
              )
              
            )
            
    ),
    
    # Epidemiological units
    tabItem("unitTab", 
            
            uiOutput("unitNameText"),
            
            box(title = uiOutput("unitMapTextUI"), status = "primary", width = 6, solidHeader = TRUE,
                
                # Display map
                plotOutput("unitMapDisplay")
                
            ),
            
            
            box(title = uiOutput("unitStatTextUI"), status = "success", width = 6, solidHeader = TRUE,
                
                # Display statistics
                textOutput("unitStatText"),
                
                plotOutput("unitStatDisplay")
                
            )
            
    ),
    
    # Risk factors
    tabItem("riskTab", 
            
            uiOutput("uiRiskLayerList"),
            
            box(title = uiOutput("riskRawMapTextUI"), status = "primary", width = 6, solidHeader = TRUE,
                
                # Original raw layer
                plotOutput("rawLayerDisplay")
                
            ),
            
            
            box(title = uiOutput("riskStandRasterTextUI"), status = "success", width = 6, solidHeader = TRUE,
                
                # Standardized raster
                plotOutput("standRasterDisplay")
                
            ),
            
            
            box(status = "info", width = 6, solidHeader = FALSE,
              
              uiOutput("riskHelpTextUI")
              
            ),
            
            actionButton(inputId = "abInvert", label = langDF["ABRiskRasterInvert", indLang])
            
    ),
    
    
    # Weight table
    tabItem("weightTab", 
            
            fluidRow(
              
              box(title = uiOutput("weightMatrixTextUI"), status = "primary", width = 12, solidHeader = TRUE,
                  footer = uiOutput("weightHelpTextUI"),
                  
                  # Editable table of weights
                  rHandsontableOutput("rhWeightTable")

              )
              
            ),
            
            fluidRow(
              
              box(title = uiOutput("weightBarTextUI"), status = "success", width = 12, solidHeader = TRUE,
                  
                  # Weights bar plot
                  plotOutput("weightBarDisplay")
                  
              )
              
            )
            
            
            
    ),
    
    # Results
    tabItem("resultTab",
            
            box(title = uiOutput("resultTextUI"), status = "primary", width = 6, solidHeader = TRUE,
                
                plotOutput("resultDisplay"),
                
                #Button to generate and download raster
                downloadButton(outputId = "exportResultRaster", 
                               label = uiOutput("resultExportTextUI"))
                
            ),
            
            box(title = uiOutput("resultPerUnitTextUI"), status = "success", width = 6, solidHeader = TRUE,
                
                selectInput(inputId = "siLevelRisk", 
                            label = uiOutput("levelRiskTextUI"),
                            choices = 1:12,
                            selected = 4),
                
                plotOutput("resultUnitDisplay"),
                
                #Button to generate and download raster
                downloadButton(outputId = "exportResultVector", 
                               label = uiOutput("resultPUExportTextUI")),
                
                #Button to generate and download csv table
                downloadButton(outputId = "exportResultCSV", 
                               label = "Export table")
                
            )
            
            
    )
    
  )
  
  
  
)

#### PAGE ####

dashboardPage(
  dashboardHeader(title = appTitle),
  sidebar,
  body
)

