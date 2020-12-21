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
          
          selectInput("type_WT","Type", c("Drowdown")),
          h3("Input data"),
          
          fileInput('filePTA', label=NULL, multiple = FALSE, accept = NULL, width =NULL, buttonLabel = "Browse...", placeholder = "No file selected"),
          
          
          column(6,
                 numericInput("qo","Oil rate (STB/D)", value = ""),
                 numericInput("poro","Porosity (fraction)", value = ""),
                 numericInput("h", "Thickness (ft)", value = ""),
                 numericInput("rw", "Well redius (ft)", value = "")
          ),
          
          column(6,
                 numericInput("pi","Initial pressure (psi)", value = ""),
                 numericInput("bo","Oil Formation Volume (Bo, bbl/STB)", value = ""),
                 numericInput("vis", "Oil Viscosity (cp)", value = ""),
                 numericInput("ct", "Total compressibility (psi^-1)", value = "")
          ),
          
          actionButton("loadex", "Load Example"),
          
          #selectInput("type_WT","Type", c("Drowdown"))
          h3("Results"),
          
          numericInput("k","Permeability (md)", value = ""),
          numericInput("s","Skin", value = ""),
          numericInput("C","WBS coefficient (bbl/psi)", value = "")
          

        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("p", height = "70vh")
        )
    )
))
