
sidebarLayout(
  sidebarPanel(
    
    h3("Results",
       actionBttn("eq_sl","",  icon = icon("far fa-question-circle"), color = "primary",style = "stretch")), 
    #actionBttn("eq_sl","",  icon = icon("far fa-question-circle"), color = "primary",style = "stretch"),
    
    conditionalPanel(
      condition = "input.type_WT == 'Build-up'", 
      selectInput("BU_methods","Methods", c("MDH", "Horner", "Agarwal")),
      #numericInput("pi", "Initial pressure (psi)", value = "")
    ),
    
    numericInput("m_sl","m", value = ""),
    numericInput("p1hr_sl","P (1 hr)", value = ""),
    numericInput("k_sl","Permeability (md)", value = ""),
    numericInput("s_sl","Skin", value = ""),
    
    conditionalPanel(
      condition = "input.type_WT == 'Drowdown'", 
      numericInput("ri_sl","Radius of investigation (ft)", value = "")
    ),
   
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    
    plotlyOutput("p_sl", height = "70vh"),
    
    # column(4,
    #        numericInput("tdpr","t\u0394p'r", value = ""),
    #        numericInput("dtr","\u0394tr", value = "")
    # ),
    # 
    # column(4,
    #        numericInput("dpr","\u0394pr", value = ""),
    #        numericInput("dtw","\u0394tw", value = "")
    # ),
    # 
    # column(4,
    #        numericInput("dpw","\u0394pw", value = "")
    # )
  )
)


