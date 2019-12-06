rm(list=ls())
MSECalc <- function(yvar, yvarname, xvar, xvarname, maxorder){
  ###Get some data properties
  n <- length(yvar)
  if(n!=length(xvar)) {
    print("Vectors must be equal in length")
  } else {
    TSS <- sum((yvar-mean(yvar))^2)  #Total Sum of Squares - same for all degrees
    ###Set up some empty data structures
    RSS <- rep(0, maxorder)                 
    MSE <- rep(0, maxorder)
    Rsqr <- rep(0, maxorder)
    for(i in 1:maxorder){
      # Fit required number of polynomial models
      lm.fit <- lm(yvar~poly(xvar, i))
      ### Compute RSS's
      RSS[i] <- sum(lm.fit$residuals^2)
      ### Compute MSE's
      MSE[i] <- RSS[i]/n
      ###Compute R-Squared's
      Rsqr[i] <- 1-RSS[i]/TSS
    }
    ### Plot MSE
    par(mfrow=c(2, 1))
    plot(1:maxorder, MSE, 
         type="b", 
         xlab="Degree of Polynomial", 
         main=paste(yvarname, "as a function of", xvarname))
    plot(1:maxorder, Rsqr, 
         type="b", 
         xlab="Degree of Polynomial", 
         main=paste(yvarname, "as a function of", xvarname))
    par(mfrow=c(1, 1))
    ### Return MSE's and R-Squared's
    #result <- list(RSquareds=Rsqr, MSEs=MSE)
    return(data.frame(d=1:maxorder, RSquareds=Rsqr, MSEs=MSE))
  }
}

require(ISLR)
###Call the function
MSECalc(Auto$mpg, "MPG", Auto$horsepower, "Horsepower", 20)
MSECalc(Auto$mpg, "MPG", Auto$weight, "Weight", 20)
