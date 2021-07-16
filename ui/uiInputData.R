fluidRow(
  
  
  box(width=12,
          title =span( icon("fas fa-table"), "Pressure data"), 
          closable = FALSE,
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE, 
          enable_sidebar = TRUE,
          
          tags$a(href = "https://brave.com/es/download/", "Open PTA webapp on Brave Browser to support it", 
                 target="_blank"),
          h1("\n"),
          
          column (6,
                  
                  rHandsontableOutput("tablePressure", height = 400)
                  
          ),
          
          column (6,
                  h3("\n"),
                  selectInput("type_WT","Type", c("Drowdown", "Build-up")),
                  h3("Input data"),
                  
                  fileInput('filePTA', label=NULL, multiple = FALSE, accept = NULL, 
                            width =NULL, buttonLabel = "Browse...", placeholder = "No file selected"),
                  
                  column (6, 
                          #Pressure Data format
                          pickerInput("time_Pre", span("Time"),choices=c("Elapse")),
                          #pickerInput("timeU_Pre", span("Unit"),choices=c("sec", "min", "hr","day","month", "year"), selected = "day"),
                          pickerInput("timeU_Pre", span("Unit"),choices=c("hr"), selected = "day"),
                          
                          awesomeCheckbox(inputId = 'header', label = 'Header', value = TRUE),
                          
                          radioGroupButtons(
                            inputId = "sep",
                            label = "Separator",
                            choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''),
                            selected = ',',
                            status = "default",
                            individual = FALSE,
                            direction = "vertical",
                            checkIcon = list(
                              yes = tags$i(class = "fa fa-circle", 
                                           style = "color: steelblue"),
                              no = tags$i(class = "fa fa-circle-o", 
                                          style = "color: steelblue"))
                          ),
                          
                          actionButton("loadex", "Load Example"),
                          h1("\n")
                          
                  )
                  
                  
          )
          

          
  ),
  
  box(width=12,
          title =span( icon("fas fa-table"), "Reservoir and fluid data"), 
          closable = FALSE,
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE, 
          enable_sidebar = TRUE,
          
          h1("\n"),
          column(
            6,
            conditionalPanel(
              condition = "input.type_WT == 'Drowdown'", 
              numericInput("pi", "Initial pressure (psi)", value = "")
            ),
            
            conditionalPanel(
              condition = "input.type_WT == 'Build-up'", 
              numericInput("tp", "Production time (hour)", value = "")
            ),
            numericInput("qo", "Oil rate (STB/D)", value = ""),
            numericInput("h", "Thickness (ft)", value = ""),
            numericInput("rw", "Well redius (ft)", value = ""),
            h1("\n")
          ),
          
          column(
            6,

            numericInput("poro", "Porosity (fraction)", value = ""),
            numericInput("bo", "Oil Formation Volume (Bo, bbl/STB)", value = ""),
            numericInput("vis", "Oil Viscosity (cp)", value = ""),
            numericInput("ct", "Total compressibility (psi^-1)", value = ""),
            h1("\n")
          )
          
          
          
          
  )
  
)