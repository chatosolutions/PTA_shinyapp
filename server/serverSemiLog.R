
line_x <- reactiveVal(0.1)
line_x2 <- reactiveVal(1)
#Semi-Log plot


output$p_sl <- renderPlotly({
  
  if(!is.null(v$datatest)){
    
    pwf.data <- v$datatest
    
    #x-axis plot
    if(input$type_WT == "Drowdown") {
      t_plot <- pwf.data$t
      
    }else{
      if(input$BU_methods == "MDH") {
        t_plot <- pwf.data$t
      }
      
      if(input$BU_methods == "Horner") {
        t_plot <- pwf.data$HTR
      }
      
      if(input$BU_methods == "Agarwal") {
        t_plot <- pwf.data$time_eq
      }
      
    }
    
    
    #lineas
    line1 <- list(
      type = "line", 
      line = list(color = "gray", dash = "dot"),
      x0 = line_x(), 
      x1 = line_x(),
      y0 = 0,
      y1 = 1,
      yref = "paper"
    )
    
    line2 <- list(
      type = "line", 
      line = list(color = "gray", dash = "dot"),
      x0 = line_x2(), 
      x1 = line_x2(),
      y0 = 0,
      y1 = 1,
      yref = "paper"
    )
    
    plot_pre <- plot_ly(color = I("red"), source = "trajectory_sl") %>%
      add_markers(x = t_plot, y = pwf.data$pwf, marker = list(color = "blue"), name = ~"\u0394p") %>%
      layout(
        xaxis = list(type = "log", title = "time, hr"),
        yaxis = list(title = "Pwf, psi"),
        shapes = list(line1, line2)
      ) %>%
      config(editable = TRUE)
    
    if("fit" %in% colnames(pwf.data)){
      plot_pre <- plot_pre %>% 
      add_lines(x = t_plot, y = pwf.data$fit, line = list(color = "red"), name = "fit")
    }
    
    if(!is.null(v$datamodel)){
      datamodel <- v$datamodel
      plot_pre <- plot_pre %>%
        add_lines(x = t_plot, y = datamodel$pwf , line = list(color = "blue")  , name = ~"Model \u0394p")
   
    }
    
    plot_pre
  }else{
    p <- ggplot() +
      xlab("time, hr") +
      ylab("Pwf, psi") +
      scale_x_continuous(limits = c(0, 100)) +
      scale_y_continuous(limits = c(0, 100)) 
    p <- ggplotly(p)
  }
  
  
})




#Lines, fit and calculous
observeEvent(event_data("plotly_relayout", source = "trajectory_sl"), {
  
  
  if (!is.null(event_data("plotly_relayout", source = "trajectory_sl")[["shapes[0].x0"]])) {
    xint <- event_data("plotly_relayout", source = "trajectory_sl")[["shapes[0].x0"]]
    #xpt <- x[which.min(abs(x - xint))]
    line_x(xint)
  } else {
    #line_x()
  }
  
  if (!is.null(event_data("plotly_relayout", source = "trajectory_sl")[["shapes[1].x0"]])) {
    xint2 <- event_data("plotly_relayout", source = "trajectory_sl")[["shapes[1].x0"]]
    #xpt <- x[which.min(abs(x - xint))]
    line_x2(xint2)
  } else {
    #line_x()
  }
  
  data <- v$datatest
  
  
  
  if(nrow(data) > 0){  
  
  if(input$type_WT == "Drowdown") {
    data_filter <- v$datatest %>%
      filter(t>=line_x(), t<=line_x2())
    
    fit_sl <- lm(pwf ~ log10(t), data_filter)
    
    P_hr <- as.numeric(coefficients(fit_sl)[2]*log10(1)+coefficients(fit_sl)[1])
    
    data$fit <- predict(fit_sl, data)
    
    m <- as.numeric(abs(coefficients(fit_sl)[2]))
    k_sl <- (162.6*input$qo*input$bo*input$vis)/(m*input$h)
    s_sl <- 1.151*((input$pi-P_hr)/m-log10((k_sl)/(1688*input$poro*input$vis*input$ct*input$rw^2))+3.23)
    ri_sl <- sqrt((k_sl*max(data$t))/(948*input$poro*input$vis*input$ct))
    
    updateNumericInput(session, "m_sl","m", value = m)
    updateNumericInput(session, "p1hr_sl","P (1 hr)", value = P_hr)
    updateNumericInput(session, "k_sl","Permeability (md)", value = k_sl)
    updateNumericInput(session, "s_sl","Skin", value = s_sl)
    updateNumericInput(session, "ri_sl","Radius of investigation (ft)", value = ri_sl)
  }else{
    if(input$BU_methods == "MDH") {
      
      data_filter <- v$datatest %>%
        filter(t>=line_x(), t<=line_x2())
      
      fit_sl <- lm(pwf ~ log10(t), data_filter)
      P_hr <- as.numeric(coefficients(fit_sl)[2]*log10(1)+coefficients(fit_sl)[1])
      data$fit <- predict(fit_sl, data)
      
      m <- as.numeric(abs(coefficients(fit_sl)[2]))
      k_sl <- (162.6*input$qo*input$bo*input$vis)/(m*input$h)
      s_sl <- 1.151*((P_hr-data$pwf[1])/m-log10((k_sl)/(1688*input$poro*input$vis*input$ct*input$rw^2))+3.23)
     
      updateNumericInput(session, "m_sl","m", value = m)
      updateNumericInput(session, "p1hr_sl","P (1 hr)", value = P_hr)
      updateNumericInput(session, "k_sl","Permeability (md)", value = k_sl)
      updateNumericInput(session, "s_sl","Skin", value = s_sl)
      
    }
    
    if(input$BU_methods == "Horner") {
      
      data_filter <- v$datatest %>%
        filter(HTR>=line_x(), HTR<=line_x2())
      
      fit_sl <- lm(pwf ~ log10(HTR), data_filter)
      P_hr <- as.numeric(coefficients(fit_sl)[2]*log10(input$tp+1)+coefficients(fit_sl)[1])
      data$fit <- predict(fit_sl, data)
      
      m <- as.numeric(abs(coefficients(fit_sl)[2]))
      k_sl <- (162.6*input$qo*input$bo*input$vis)/(m*input$h)
      s_sl <- 1.151*((P_hr-data$pwf[1])/m-log10((k_sl)/(1688*input$poro*input$vis*input$ct*input$rw^2))+3.23)
      
      updateNumericInput(session, "m_sl","m", value = m)
      updateNumericInput(session, "p1hr_sl","P (1 hr)", value = P_hr)
      updateNumericInput(session, "k_sl","Permeability (md)", value = k_sl)
      updateNumericInput(session, "s_sl","Skin", value = s_sl)
    }
    
    if(input$BU_methods == "Agarwal") {
      
      data_filter <- v$datatest %>%
        filter(time_eq>=line_x(), time_eq<=line_x2())
      
      fit_sl <- lm(pwf ~ log10(time_eq), data_filter)
      P_hr <- as.numeric(coefficients(fit_sl)[2]*log10(1)+coefficients(fit_sl)[1])
      data$fit <- predict(fit_sl, data)
      
      m <- as.numeric(abs(coefficients(fit_sl)[2]))
      k_sl <- (162.6*input$qo*input$bo*input$vis)/(m*input$h)
      s_sl <- 1.151*((P_hr-data$pwf[1])/m-log10((k_sl)/(1688*input$poro*input$vis*input$ct*input$rw^2))+3.23)
      
      updateNumericInput(session, "m_sl","m", value = m)
      updateNumericInput(session, "p1hr_sl","P (1 hr)", value = P_hr)
      updateNumericInput(session, "k_sl","Permeability (md)", value = k_sl)
      updateNumericInput(session, "s_sl","Skin", value = s_sl)
    }
    
    
    
  }
    
    

  
  v$datatest <- data
  
  }
  
  
  
})

#inicio primer linea
observe(     
        if(!is.null(v$datatest)){
          if(!("fit" %in% colnames(v$datatest))){
            line_x(min(v$datatest$t[v$datatest$t!=0]))
          }
          
        }
)

#inicio segunda linea
observe(
        if(!is.null(v$datatest)){
          if(!("fit" %in% colnames(v$datatest))){
            line_x2(max(v$datatest$t))
          }
          
        }
)

#Info button
observeEvent(input$eq_sl, {
  
  showModal(EquationsSemi_log())
  
})


EquationsSemi_log <- function(failed = FALSE) {
  modalDialog(
    includeHTML(file.path("help", "Semi_log_info.html"))
  )
} 
