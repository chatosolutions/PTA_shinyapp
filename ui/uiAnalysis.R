# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  #titlePanel("Pressure Transient Analysis"),
  
  navbarPage("Analysis",theme = shinytheme("flatly"),
             tabPanel("Production Data",
                      plotlyOutput("plot_rate", height = "30vh"),
                      plotlyOutput("plot_pressure", height = "30vh")
                      ),
             
             tabPanel("Semi-Log",
                     source(file.path("ui", "uiSemiLog.R"),  local = TRUE)$value
                     
             ),
             
             tabPanel("Log-Log analysis",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          h3("Results"),
                          
                          numericInput("k","Permeability (md)", value = ""),
                          numericInput("s","Skin", value = ""),
                          numericInput("C","WBS coefficient (bbl/psi)", value = ""),
                          actionButton("gen_model", "Generate model"),
                          actionButton("fit_model", "Fit model"),
                          
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          
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
                        )
                      )
                    )
             
  )
  
  # Sidebar with a slider input for number of bins

)

