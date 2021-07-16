
data_origin <- reactiveVal(NULL)

observeEvent(input$filePTA,{
  data_origin(input$filePTA)
  #print(data_origin())
})


observe({
  if(!is.null(data_origin())){
    
    pwf.data <- read.csv(file=data_origin()$datapath, sep=input$sep, header = input$header) #read.csv("E:/SHINY_APP/pruebas/wt.csv")
    validate(need(ncol(pwf.data) == 2, "You need two column dataset"))
    colnames(pwf.data) <- c("t", "pwf")
    
    if(input$type_WT == "Drowdown"){
      pwf.data$dp <- pwf.data$pwf[1] - pwf.data$pwf
      pwf.data$Ddp <- -derivative.Bourdet(log(pwf.data$t),pwf.data$pwf)
    }
    
    if(input$type_WT == "Build-up"){
      pwf.data$dp <- pwf.data$pwf - pwf.data$pwf[1]
      pwf.data$HTR <- (input$tp + pwf.data$t)/pwf.data$t
      pwf.data$time_eq <- (input$tp*pwf.data$t)/(input$tp + pwf.data$t)
      pwf.data$Ddp <- derivative.Bourdet(log(pwf.data$time_eq),pwf.data$pwf)
    }
    
    last_Dpd <-  dplyr::last(pwf.data$Ddp)
    v$yint1 <- last_Dpd
    y_escala <- lseq(0.000001,1000000, 13)
    
    val_y <-  c(pwf.data$dp, pwf.data$Ddp)
    val_y <- val_y[val_y > 0]
    first_t <-  pwf.data$t[2]
    v$xint2.1 <- first_t
    v$yint2.1 <- max(y_escala[y_escala < min(val_y, na.rm = TRUE)], na.rm = TRUE)
    yint2.1 <- max(y_escala[y_escala < min(val_y, na.rm = TRUE)], na.rm = TRUE)
    
    v$yint2.2 <- min(y_escala[y_escala > max(c(pwf.data$dp, pwf.data$Ddp), na.rm = TRUE)], na.rm = TRUE)
    yint2.2 <- min(y_escala[y_escala > max(c(pwf.data$dp, pwf.data$Ddp), na.rm = TRUE)], na.rm = TRUE)
    v$xint2.2 <- 10^((log10(yint2.2)-log10(yint2.1)) + log10(first_t))
    updateNumericInput(session, "pi","Initial pressure (psi)", value = pwf.data$pwf[1])
    v$datatest <- pwf.data
  }
})


#Load examples button
observeEvent(input$loadex,{
  
  v$datamodel <- NULL
  v$datamodelDD_BU <- NULL
  if(input$type_WT == "Drowdown"){ 
    file_example <- data.frame(name = "Example 1", datapath = "https://raw.githubusercontent.com/chatosolutions/PTA_shinyapp/main/data/Example_4.3_AWTI.csv", stringsAsFactors = FALSE)
    data_origin(file_example)
    
    updateNumericInput(session,"qo","Oil rate (STB/D)", value = "125")
    updateNumericInput(session,"poro","Porosity (fraction)", value = "0.22")
    updateNumericInput(session,"h", "Thickness (ft)", value = "32")
    updateNumericInput(session,"rw", "Well redius (ft)", value = "0.25")
    updateNumericInput(session,"bo","Oil Formation Volume (Bo, bbl/STB)", value = "1.152")
    updateNumericInput(session,"vis", "Oil Viscosity (cp)", value = "2.122")
    updateNumericInput(session,"ct", "Total compressibility (psi^-1)", value = "0.0000109")
  }
  
  if(input$type_WT == "Build-up"){ 
    file_example <- data.frame(name = "Example 1", datapath = "https://raw.githubusercontent.com/chatosolutions/PTA_shinyapp/main/data/BU_Ex_7.6.csv", stringsAsFactors = FALSE)
    data_origin(file_example)
    
    updateNumericInput(session,"qo","Oil rate (STB/D)", value = "150")
    updateNumericInput(session,"poro","Porosity (fraction)", value = "0.2")
    updateNumericInput(session,"h", "Thickness (ft)", value = "50")
    updateNumericInput(session,"rw", "Well redius (ft)", value = "0.33")
    updateNumericInput(session,"bo","Oil Formation Volume (Bo, bbl/STB)", value = "1.5")
    updateNumericInput(session,"vis", "Oil Viscosity (cp)", value = "0.5")
    updateNumericInput(session,"ct", "Total compressibility (psi^-1)", value = "0.00005")
    updateNumericInput(session,"tp", "Production time (hour)", value = "1000")
  }
  
  #v$yint1 <- 25.6
  #v$xint2.1 <- 0.00086
  #v$yint2.1 <- 1
})


#pressure data table
output$tablePressure <- renderRHandsontable({
  
  if(is.null(data_origin())){
    #if(is.null(vDAC$prodData)){
    #return()
    #m <- matrix(NA_integer_, nrow = 6, ncol = 2)
    c1 <- c(NA_real_,NA_real_,NA_real_,NA_real_,NA_real_,NA_real_,NA_real_,NA_real_)
    c2 <- c(NA_real_,NA_real_,NA_real_,NA_real_,NA_real_,NA_real_,NA_real_,NA_real_)
    
    data <- data.frame(Time = c1, Pressure = c2)
    
    rhandsontable(data, stretchH = "all")
  }else{
    # rhandsontable(head(read.table(file=input$fileFS$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)), stretchH = "all")
    

      data <- read.table(file=data_origin()$datapath, sep=input$sep, header = input$header) #v$datatest 
      validate(need(ncol(data) == 2, "You need two column dataset"))
      
      if (ncol(data)==2){
        colnames(data) <- c("Time", "Pressure")
        
        rhandsontable(data, stretchH = "all",height = 400) %>%
          hot_col("Time", format = "0.0000") %>%
          hot_col("Pressure", format = "0.0000")
      }else{
        rhandsontable(data, stretchH = "all",height = 400)
      }
      
    
    
    
  }
  
  
})
