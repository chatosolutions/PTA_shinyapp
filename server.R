#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)
library(emdbook)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    v <- reactiveValues(datatest = NULL, xint1 = 1, yint1 = 0, xint2.1 = 1, yint2.1 = 0, xint2.2 = 1, yint2.2 = 0)
    
    observe({
      if(!is.null(input$filePTA)){
        
        pwf.data <- read.csv(file=input$filePTA$datapath) #read.csv("E:/SHINY_APP/pruebas/wt.csv")
        colnames(pwf.data) <- c("t", "pwf")
        pwf.data$dp <- pwf.data$pwf[1] - pwf.data$pwf
        pwf.data$Ddp <- -derivative.Bourdet(log(pwf.data$t),pwf.data$pwf)
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

    
    
    observe({ 
      
      q <- input$qo
      poro <- input$poro
      h <- input$h
      rw <- input$rw
      Bo <- input$bo
      pi <- input$pi
      vis <- input$vis
      ct <- input$ct
      
      dpr <- dplyr::last(v$datatest$dp)
      tdp <- dplyr::last(v$datatest$t)
      
      k <- (70.6*q*Bo*vis)/(h*v$yint1)
      s <- 0.5*(dpr/v$yint1-log((k*tdp)/(1688*poro*vis*ct*rw^2)))
      C <- (q*Bo*v$xint2.1)/(24*v$yint2.1)
      updateNumericInput(session, "k","Permeability (md)", value = k)
      updateNumericInput(session, "s","Skin", value = s)
      updateNumericInput(session, "C","WBS coefficient (bbl/psi)", value = C)
      
    })
    
    observeEvent(input$loadex,{
      
    })
    
    output$p <- renderPlotly({
      
      if(!is.null(v$datatest)){
        d <- event_data("plotly_relayout", source = "trajectory")
        pwf.data <- v$datatest
        selected_point <- if (!is.null(d[["shapes[0].x0"]])) {
          v$yint1 <- d[["shapes[0].y0"]]
          list(x = 1, y = v$yint1)
        } else {
          list(x = 1, y = v$yint1)
        }
        
        selected_point2.1 <- if (!is.null(d[["shapes[1].x0"]])) {
          xpt <- d[["shapes[1].x0"]]
          #selected_point2.2$x <- 
          v$xint2.2 <- 10^((log10(v$yint2.2)-log10(v$yint2.1)) + log10(xpt))
          list(x = xpt, y = v$yint2.1)
        } else {
          list(x = v$xint2.1, y = v$yint2.1)
        }
        
        selected_point2.2 <- if (!is.null(d[["shapes[1].x1"]])) {
          xpt <- d[["shapes[1].x1"]]
          v$xint2.1 <- 10^(log10(xpt) - (log10(v$yint2.2)-log10(v$yint2.1)))
          list(x = xpt, y = v$yint2.2)
        } else {
          list(x = v$xint2.2, y = v$yint2.2)
        }
        
        line1 <- list(
          type = "line", 
          line = list(color = "gray", dash = "dot"),
          x0 = 0, #selected_point$x, 
          x1 = 1, #selected_point$x,
          y0 = selected_point$y,
          y1 = selected_point$y,
          xref = "paper"
        )
        
        line2 <- list(
          type = "line", 
          line = list(color = "gray", dash = "dot"),
          x0 = selected_point2.1$x, 
          x1 = selected_point2.2$x,
          y0 = selected_point2.1$y, #selected_point$y,
          y1 = selected_point2.2$y #selected_point$y,
          #yref = "paper"
        )
        
        
        plot_pre <- plot_ly(color = I("red"), source = "trajectory") %>%
          add_markers(x = pwf.data$t, y = pwf.data$dp, marker = list(color = "blue"), name = ~"\u0394p") %>%
          add_markers(x = pwf.data$t, y = pwf.data$Ddp, name = ~"\u0394p'") %>%
          layout(
            shapes = list(line1, line2),
            xaxis = list(type = "log", title = "time, hr"),
            yaxis = list(type = "log", title = "\u0394p, \u0394p', psi")
          ) %>%
          config(editable = TRUE)
        
        plot_pre
      }else{
        p <- ggplot() +
          xlab("time, hr") +
          ylab("\u0394p, \u0394p', psi") +
          scale_x_continuous(limits = c(0, 100)) +
          scale_y_continuous(limits = c(0, 100)) 
        p <- ggplotly(p)
      }
        

        
    })

})


finite.differences.FW <- function(x, y) {
  
    if (length(x) != length(y)) {
        stop('x and y vectors must have equal length')
    }
    
    n <- length(x)
    
    fdx <- vector(length = n)
    
    for (i in 2:n) {
        fdx[i-1] <- (y[i-1] - y[i]) / (x[i-1] - x[i])
    }
    
    fdx[n] <- (y[n] - y[n - 1]) / (x[n] - x[n - 1])
    
    return(fdx)
}

finite.differences.BW <- function(x, y) {
    if (length(x) != length(y)) {
        stop('x and y vectors must have equal length')
    }

    n <- length(x)

    fdx <- vector(length = n)

    fdx[1] <- (y[2] - y[1]) / (x[2] - x[1]) #(y[n] - y[n - 1]) / (x[n] - x[n - 1])

    for(i in 2:n) {
        fdx[i] <- (y[i] - y[i-1]) / (x[i] - x[i-1])
    }



    return(fdx)
}


derivative.Bourdet <-  function(x, y){

    if (length(x) != length(y)) {
        stop('x and y vectors must have equal length')
    }
    #print(x)
    dev.L <- finite.differences.BW(x, y)
    dev.R <- finite.differences.FW(x, y)

    n <- length(x)
    dev <- vector(length = n)

    dev[1] <- (x[1]*dev.R[1])/x[2]

    for(i in 2:(n-1)){
        #print(i)
        dev[i] <- (((x[i+1] - x[i])*dev.L[i]) + ((x[i] - x[i-1])*dev.R[i]))/(x[i+1] - x[i-1])
    }

    dev[n] <- (-x[n]*dev.L[n])/(-x[n-1])

    return(dev)
}
