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