rm(list=ls())
######################################################
### Functions
######################################################
installIfAbsentAndLoad  <-  function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
##############################
### Load required packages ###
##############################
needed <- c('ISLR', 'boot')      
installIfAbsentAndLoad(needed)
############################################################
# The Bootstrap Exercise #1
# Repeat the example of two stocks from the text to estimate
# Var(aX +(1 -a)Y) #
############################################################
rm(list=ls())
# Here we are using the Portfolio data frame (part of the
# ISLR package). Each of the 100 rows contains returns for
# the two investments of interest: X and Y
str(Portfolio)
head(Portfolio)
n <- nrow(Portfolio)
# Begin by creating a function to compute the population
# statistic of interest - here alpha. This function is named
# alpha.fn (our choice) and takes 2 paramaters: the first is
# the dataframe to use for sampling, the second is a vector
# of (random) indices which define the rows in the data
# frame to be used to estimate alpha (this function, with
# these two parameters, is required by the boot() function
# used later). In this case, we know that the dataframe
# contains the variables X and Y, so we can refer to them by
# name within the function.

alpha.fn <- function(data, index){
  X <- data$X[index]
  Y <- data$Y[index]
  #Compute alpha, the optimal amount to invest in X
  return((var(Y)-cov(X, Y))/(var(X)+var(Y)-2*cov(X, Y)))
}

# As an example, call the alpha.fn() function using all the
# rows of the X and Y values in Portfolio to estimate alpha.
alpha.fn(Portfolio, 1:n)
# As another example, we apply the alpha.fn() function to a
# random sample (with replacement) of size n from the rows
# in the Portfolio data frame
alpha.fn(Portfolio, sample(n, n, replace=T))

# Finally, use the boot() function to generate 10,000
# bootstrap replicates of alpha and report the standard
# error of those replicates
set.seed(5072, sample.kind="Rejection")
boot.out <- boot(Portfolio, alpha.fn, R=10000)
# Results of alpha.fn() applied to the entire Portfolio
# dataset
alpha.fn(Portfolio, 1:n)
# Same value as a boot named element
boot.out$t0
# Bias 
mean(boot.out$t)-boot.out$t0
# Std. error
sd(boot.out$t)
sqrt(sum((boot.out$t-mean(boot.out$t))^2)/(length(boot.out$t)-1))
# All 3 elements available by displaying boot output
boot.out
##############################################################################
# The Bootstrap Exercise #2  
# Estimating the Accuracy of the coefficients in a Linear
# Regression Model   
##############################################################################
# Create a function to create linear regression coefficients
# for mpg~horsepower in the Auto data frame.
boot.fn <- function(data, index) {
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
}
# Use this function to estimate B0 and B1 using the full 
# data frame as the training set - these are just the values
# obtained by using lm() on the full sample
n <- nrow(Auto)
boot.fn(Auto, 1:n)
# Check...
coef(lm(mpg~horsepower, data=Auto))
# Example 2.1: use our boot.fn() function to produce two 
# bootstrap estimates of the intercept and slope

boot.fn(Auto, sample(n, n, replace=T))
boot.fn(Auto, sample(n, n, replace=T))

# Example 2.2: use the boot() function to compute the
# standard errors of 10,000 bootstrap estimates for the
# intercept and slope terms
set.seed(5072, sample.kind="Rejection")
boot(Auto, boot.fn, R=10000)

# Compare to results of formulae in Section 3.1.2 (pp. 66)
summary(lm(mpg~horsepower, data=Auto))$coef

# Which is better? The bootstrap is a more reliable estimate
# of the standard error since the values reported by the
# lm() function (and the formulae) rely on an estimate (RSE)
# of the true value of the population sigma (the population
# noise variance).

# Recall why we care: by underestimating the std. error, the
# lm() output causes the t-value to be larger than it should
# be, which increases the liklihood of a Type I error
# (falsely rejecting a true null hypothesis).

# Do the same for the quadratic model - discrepency is less
# since fit is closer
boot.fn <- function(data, index) {
  return(coef(lm(mpg~horsepower+I(horsepower^2), data=data, subset=index)))
}
  
set.seed(5072, sample.kind="Rejection")
boot(Auto, boot.fn, 10000)
summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef
