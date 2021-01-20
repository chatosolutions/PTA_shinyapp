#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Pressure Transient Analysis"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            tags$a(href = "https://www.chatosolutions.com/", "Home", 
                   target="_blank"),
          selectInput("type_WT","Type", c("Drowdown", "Build-up")),
          h3("Input data"),
          
          fileInput('filePTA', label=NULL, multiple = FALSE, accept = NULL, width =NULL, buttonLabel = "Browse...", placeholder = "No file selected"),
          
          #conditionalPanel(
              #condition = "input.type_WT == 'Drowdown'",
              column(
                  6,
                  numericInput("qo", "Oil rate (STB/D)", value = ""),
                  numericInput("poro", "Porosity (fraction)", value = ""),
                  numericInput("h", "Thickness (ft)", value = ""),
                  numericInput("rw", "Well redius (ft)", value = "")
              ),
              
              column(
                  6,
                  conditionalPanel(
                     condition = "input.type_WT == 'Drowdown'", 
                     numericInput("pi", "Initial pressure (psi)", value = "")
                     ),
                  
                    conditionalPanel(
                    condition = "input.type_WT == 'Build-up'", 
                    numericInput("tp", "Production time (hour)", value = "")
                    )
                  ,
                  numericInput("bo", "Oil Formation Volume (Bo, bbl/STB)", value = ""),
                  numericInput("vis", "Oil Viscosity (cp)", value = ""),
                  numericInput("ct", "Total compressibility (psi^-1)", value = "")
              ),
          #), 
          
          

          
          actionButton("loadex", "Load Example"),
          
          #selectInput("type_WT","Type", c("Drowdown"))
          h3("Results"),
          
          numericInput("k","Permeability (md)", value = ""),
          numericInput("s","Skin", value = ""),
          numericInput("C","WBS coefficient (bbl/psi)", value = ""),
          actionButton("gen_model", "Generate model"),
          actionButton("fit_model", "Fit model")
          

          
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            tabsetPanel(id = "tabs_PVT",
                        
                        tabPanel(title="Production data",
                                 
                                 plotlyOutput("plot_rate", height = "30vh"),
                                 plotlyOutput("plot_pressure", height = "30vh")
                                 
                        ),
                        
                        tabPanel(title="log-log analysis",
                                 
                                 plotlyOutput("p", height = "70vh"),
                                 
                                 column(4,
                                        numericInput("tdpr","t\u0394p'r", value = ""),
                                        numericInput("dtr","\u0394tr", value = "")
                                 ),
                                 
                                 column(4,
                                        numericInput("dpr","\u0394pr", value = ""),
                                        numericInput("dtw","\u0394tw", value = "")
                                 ),
                                 
                                 column(4,
                                        numericInput("dpw","\u0394pw", value = "")
                                 )
                                 
                        )#,
                        
                        #tabPanel(title="Semi-log analysis"
                                      
                        #)

                        
            )
            
            

            
            
            
        )
    )
))
