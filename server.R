#
# PTAwebapp
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
library(minpack.lm)

#comentario
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    v <- reactiveValues(datatest = NULL, 
                        datamodel = NULL,
                        datamodelDD_BU = NULL,
                        xint1 = 1, 
                        yint1 = 0, 
                        xint2.1 = 1, yint2.1 = 0, xint2.2 = 1, yint2.2 = 0)
    
    observe({
      if(!is.null(data_origin())){
        
        pwf.data <- read.csv(file=data_origin()$datapath) #read.csv("E:/SHINY_APP/pruebas/wt.csv")
        colnames(pwf.data) <- c("t", "pwf")
        
        if(input$type_WT == "Drowdown"){
          pwf.data$dp <- pwf.data$pwf[1] - pwf.data$pwf
          pwf.data$Ddp <- -derivative.Bourdet(log(pwf.data$t),pwf.data$pwf)
        }
        
        if(input$type_WT == "Build-up"){
          pwf.data$dp <- pwf.data$pwf - pwf.data$pwf[1]
          time_eq <- (input$tp*pwf.data$t)/(input$tp + pwf.data$t)
          pwf.data$Ddp <- derivative.Bourdet(log(time_eq),pwf.data$pwf)
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
      dtr <- dplyr::last(v$datatest$t)
      
      k <- (70.6*q*Bo*vis)/(h*v$yint1)
      s <- 0.5*(dpr/v$yint1-log((k*dtr)/(1688*poro*vis*ct*rw^2)))
      C <- (q*Bo*v$xint2.1)/(24*v$yint2.1)
      
      updateNumericInput(session,"tdpr","t\u0394p'r", value = v$yint1)
      updateNumericInput(session,"dtr","\u0394tr", value = dtr)
      updateNumericInput(session,"dpr","\u0394pr", value = dpr)
      updateNumericInput(session,"dtw","\u0394tw", value = v$xint2.1)
      updateNumericInput(session,"dpw","\u0394pw", value = v$yint2.1)
      updateNumericInput(session, "k","Permeability (md)", value = k)
      updateNumericInput(session, "s","Skin", value = s)
      updateNumericInput(session, "C","WBS coefficient (bbl/psi)", value = C)
      
    })
    
    data_origin <- reactiveVal(NULL)
    
    observeEvent(input$filePTA,{
      data_origin(input$filePTA)
      #print(data_origin())
    })
    
    #Boton Load de ejemplos
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
        
        if(!is.null(v$datamodel)){
          datamodel <- v$datamodel
          plot_pre <- plot_pre %>%
            add_lines(x = datamodel$t, y = datamodel$dp, line = list(color = "blue")  , name = ~"Model \u0394p") %>%
            add_lines(x = datamodel$t, y = datamodel$Ddp, name = ~"Model \u0394p'")
            
        }
        
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
    
    #generate model with analytical solution
    observeEvent(input$gen_model,{
      
      q <- input$qo
      poro <- input$poro
      h <- input$h
      rw <- input$rw
      Bo <- input$bo
      pi <- input$pi
      vis <- input$vis
      ct <- input$ct
      k <- input$k
      s <- input$s
      C <- input$C


      if(!is.null(v$datatest)){
        pwf.data <- v$datatest
        
        # if(pwf.data$t[1] == 0){
        #   ti = pwf.data$t[2]
        #   logt <- c(0,lseq(ti, dplyr::last(pwf.data$t),500))
        # }else{
        #   ti = pwf.data$t[1]
        #   logt <- lseq(ti, dplyr::last(pwf.data$t),500)
        # }
        
        if(input$type_WT == "Drowdown"){
          logt <- pwf.data$t
          pwD <- pwD.calDD(poro,ct,rw,Bo,vis,q,h,k,logt,C,s)
          pwf <- pi - (pwD*141.2*Bo*vis*q)/(h*k)
          
          if(pwf.data$t[1] == 0){
            pwf[1] = pi
          }
          
          dp <- pwf[1] - pwf
          Ddp <- -derivative.Bourdet(log(logt),pwf)
          
          v$datamodel <- data.frame(t = logt, pwf = pwf, dp = dp, Ddp = Ddp) 
        }
        
        if(input$type_WT == "Build-up"){
          tp <- input$tp
          dt <- pwf.data$t #(input$tp*pwf.data$t)/(input$tp + pwf.data$t)
          te <- (input$tp*pwf.data$t)/(input$tp + pwf.data$t)
          tp.dt <- pwf.data$t + input$tp
          pwf <-  pwf.data$pwf[1]
          
          pwD.dt <- pwD.calDD(poro,ct,rw,Bo,vis,q,h,k,dt,C,s)
          pwD.dpdt <- pwD.calDD(poro,ct,rw,Bo,vis,q,h,k,tp.dt,C,s)
          pwD.BU <- pwD.dpdt[1] + pwD.dt - pwD.dpdt
          pwD.BU[1] <- 0
          pws <- pwf + (pwD.BU*141.2*Bo*vis*q)/(h*k)
          
          dp <- pws - pws[1]
          Ddp <- derivative.Bourdet(log(te),pws)
          
          v$datamodel <- data.frame(t = pwf.data$t, pwf = pws, dp = dp, Ddp = Ddp)
          
          #Drowdown section in BU
          DD_t <- c(0.0000001,lseq(0.001, input$tp,nrow(pwf.data)))
          DD_t_sort <- sort(DD_t, decreasing = TRUE)
          pwD.DDdp <- pwD.calDD(poro,ct,rw,Bo,vis,q,h,k,DD_t_sort,C,s)
          pwf_DD <- pwf + (pwD.DDdp*141.2*Bo*vis*q)/(h*k)
          
          v$datamodelDD_BU <- data.frame(t = DD_t, pwf = pwf_DD)
        }
        
        
        head(v$datamodel) 
      }
      
    })
    
    #Boton fit model
    observeEvent(input$fit_model,{
      
      qp <- input$qo
      porop <- input$poro
      hp <- input$h
      rwp <- input$rw
      Bop <- input$bo
      pip <- input$pi
      visp <- input$vis
      ctp <- input$ct
      kp <- input$k
      sp <- input$s
      Cp <- input$C
      
      if(!is.null(v$datatest)){
        pwf.data <- v$datatest
        
        if(input$type_WT == "Drowdown"){
          logt <- pwf.data$t
          
          residFun <- function(parS,observed,logt,pi = pip ,poro = porop,ct = ctp,rw = rwp,Bo = Bop,vis = visp,q = qp,h = hp){
            
            pwD <- pwD.calDD(poro,ct,rw,Bo,vis,q,h,parS$k,logt,parS$C,parS$s)
            pwf_cal <- pip - (pwD*141.2*Bo*vis*q)/(h*parS$k)
            
            if(logt[1] == 0){
              pwf_cal[1] = pip
            }
            
            observed - pwf_cal
          }
          
          parStart <- list(k = kp, C = Cp, s = sp)
          fit_model <- nls.lm(par = parStart, fn = residFun, observed =  pwf.data$pwf,
                              logt =  logt, control = nls.lm.control(nprint=1))
          
          fit_coeff <- as.list(coef(fit_model))
          pwD <- pwD.calDD(porop,ctp,rwp,Bop,visp,qp,hp,fit_coeff$k,logt,fit_coeff$C,fit_coeff$s)
          pwf_cal <- pip - (pwD*141.2*Bop*visp*qp)/(hp*fit_coeff$k)
          
          if(pwf.data$t[1] == 0){
            pwf_cal[1] = pip
          }
          
          dp <- pwf_cal[1] - pwf_cal
          Ddp <- -derivative.Bourdet(log(logt),pwf_cal)
          
          v$datamodel <- data.frame(t = logt, pwf = pwf_cal, dp = dp, Ddp = Ddp) 
          
          }

        if(input$type_WT == "Build-up"){
          
          tp <- input$tp
          dtp <- pwf.data$t #(input$tp*pwf.data$t)/(input$tp + pwf.data$t)
          te <- (input$tp*pwf.data$t)/(input$tp + pwf.data$t)
          tp.dtp <- pwf.data$t + input$tp
          pwfp <-  pwf.data$pwf[1]
          
          residFun <- function(parS,observed,dt = dtp,tp.dt = tp.dtp,pwf = pwfp,poro = porop,ct = ctp,rw = rwp,Bo = Bop,vis = visp,q = qp,h = hp){
            
            pwD.dt <- pwD.calDD(poro,ct,rw,Bo,vis,q,h,parS$k,dt,parS$C,parS$s)
            pwD.dpdt <- pwD.calDD(poro,ct,rw,Bo,vis,q,h,parS$k,tp.dt,parS$C,parS$s)
            pwD.BU <- pwD.dpdt[1] + pwD.dt - pwD.dpdt
            pwD.BU[1] <- 0
            pws_cal <- pwf + (pwD.BU*141.2*Bo*vis*q)/(h*parS$k)
            
            (observed - pws_cal)/observed
          }
          
          parStart <- list(k = kp, C = Cp, s = sp)
          fit_model <- nls.lm(par = parStart, fn = residFun, observed =  pwf.data$pwf,
                              dt =  dtp, control = nls.lm.control(nprint=1))
          
          
          #calculate with fit coef
          fit_coeff <- as.list(coef(fit_model))
          pwD.dt <- pwD.calDD(porop,ctp,rwp,Bop,visp,qp,hp,fit_coeff$k,dtp,fit_coeff$C,fit_coeff$s)
          pwD.dpdt <- pwD.calDD(porop,ctp,rwp,Bop,visp,qp,hp,fit_coeff$k,tp.dtp,fit_coeff$C,fit_coeff$s)
          pwD.BU <- pwD.dpdt[1] + pwD.dt - pwD.dpdt
          pwD.BU[1] <- 0
          pws_cal <- pwfp + (pwD.BU*141.2*Bop*visp*qp)/(hp*fit_coeff$k)

          dp <- pws_cal - pws_cal[1]
          Ddp <- derivative.Bourdet(log(te),pws_cal)
          
          v$datamodel <- data.frame(t = pwf.data$t, pwf = pws_cal, dp = dp, Ddp = Ddp)
          
          #Drowdown section in BU
          DD_t <- c(0.0000001,lseq(0.001, input$tp,nrow(pwf.data)))
          DD_t_sort <- sort(DD_t, decreasing = TRUE)
          pwD.DDdp <- pwD.calDD(porop,ctp,rwp,Bop,visp,qp,hp,fit_coeff$k,DD_t_sort,fit_coeff$C,fit_coeff$s)
          pwf_DD <- pwfp + (pwD.DDdp*141.2*Bop*visp*qp)/(hp*fit_coeff$k)
          
          v$datamodelDD_BU <- data.frame(t = DD_t, pwf = pwf_DD)
          
        }
        
        updateNumericInput(session, "k","Permeability (md)", value = fit_coeff$k)
        updateNumericInput(session, "s","Skin", value = fit_coeff$s)
        updateNumericInput(session, "C","WBS coefficient (bbl/psi)", value = fit_coeff$C)
        
        }
    })
    
    
    #Plot production data
    output$plot_rate <- renderPlotly({ 
      
      if(!is.null(v$datatest)){
        
        pwf.data <- v$datatest
        
        if(input$type_WT == "Drowdown"){
          time <- c(0,pwf.data$t)
          q <- c(0, rep(input$qo,length(pwf.data$t)))
          
          plot_pre <- plot_ly() %>%
            add_lines(x = time, y = q, line = list(color = "green", width = 5), name = ~"rate") %>%
            layout(
              xaxis = list(title = "time, hr"),
              yaxis = list(title = "rate, STB")
            )
        }
        
        if(input$type_WT == "Build-up"){
          
          time <- c(0,0,input$tp,input$tp, input$tp+max(pwf.data$t))
          q <- c(0,input$qo,input$qo,0,0)
          
          plot_pre <- plot_ly() %>%
            add_lines(x = time, y = q, line = list(color = "green", width = 5), name = ~"rate") %>%
            layout(
              xaxis = list(title = "time, hr"),
              yaxis = list(title = "rate, STB")
            )
        }

        plot_pre
      }else{
        p <- ggplot() +
          xlab("time, hr") +
          ylab("rate, STB") +
          scale_x_continuous(limits = c(0, 100)) +
          scale_y_continuous(limits = c(0, 100)) 
        p <- ggplotly(p)
      }
      
      })
    
    #Plot pressure data
    output$plot_pressure <- renderPlotly({ 
      
      if(!is.null(v$datatest)){
       
        pwf.data <- v$datatest
        
        if(input$type_WT == "Drowdown"){ 
          plot_pre <- plot_ly() %>%
            add_markers(x = pwf.data$t, y = pwf.data$pwf,marker = list(color = "green"), name = ~"pressure") %>%
            layout(
              xaxis = list(title = "time, hr"),
              yaxis = list(title = "pressure, psia")
            )
          
          if(!is.null(v$datamodel)){
            datamodel <- v$datamodel
            time <- datamodel$t
            plot_pre <- plot_pre %>%
              add_lines(x = time, y = datamodel$pwf, line = list(color = "blue")  , name = ~"Model pressure")
          }
        }
        
        if(input$type_WT == "Build-up"){
          
          time <- input$tp + pwf.data$t
          plot_pre <- plot_ly() %>%
            add_markers(x = time, y = pwf.data$pwf,marker = list(color = "green"), name = ~"pressure") %>%
            layout(
              xaxis = list(title = "time, hr", range = c(0,input$tp+max(pwf.data$t))),
              yaxis = list(title = "pressure, psia")
            )
          
          
          if(!is.null(v$datamodel)){
            datamodel <- v$datamodel
            datamodel.pwfDD <-v$datamodelDD_BU
            time <- input$tp + datamodel$t
            plot_pre <- plot_pre %>%
              add_lines(x = time, y = datamodel$pwf, line = list(color = "blue")  , name = ~"Model pressure") %>%
              add_lines(x = datamodel.pwfDD$t, y = datamodel.pwfDD$pwf, line = list(color = "blue")  , name = ~"Model pressure")

          }
          
        }
        
        plot_pre
      }else{
        p <- ggplot() +
          xlab("time, hr") +
          ylab("pressure, psia") +
          scale_x_continuous(limits = c(0, 100)) +
          scale_y_continuous(limits = c(0, 100)) 
        p <- ggplotly(p)
      }
      
    })

})

#Drawdown
pwD.calDD <- function(poro,ct,rw,Bo,vis,q,h,k,logt,C,s){
  
  td <- (0.0002637*k*(logt))/(poro*vis*ct*rw^2)
  cD <- (0.8936*C)/(h*poro*ct*rw^2)
  pwD <- Stehfest_inversion(td,cD,s)
  #pwf <- pi - (pwD*141.2*Bo*vis*q)/(h*k)
  
  return(pwD)
}


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




#Stehfest inversion
Stehfest_inversion <- function(t, cD, s){
  
  #t <- 1
  PwD <- 0
  m <- length(t)
  #s <- 0
  #cD <- 1
  V <- c(-0.3333,48.3333,-906,5464.6667,-14376.6667,18730,-11946.6667,2986.6667)
  
  for(j in 1:m){
    
    a <- log(2)/t[j]
    i <- c(1:8)
    u <- (i*a)
    ru <- sqrt(u)
    aux1 <- besselK(ru,0)+s*ru*besselK(ru,1)
    aux2 <- ru*besselK(ru,1)+cD*u*(besselK(ru,0)+s*ru*besselK(ru,1))
    PwDL <- 1/u*(aux1/aux2)
    PwD[j] <- a*sum(V*PwDL)
    
  }
  
  return(PwD)
  
}




