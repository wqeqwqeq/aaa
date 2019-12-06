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
# The boot package contains the cv.glm() function
needed <- c('ISLR', 'boot')        
installIfAbsentAndLoad(needed)
############################################################## 
### Leave-One-Out Cross-Validation  ##########################
############################################################## 
# We use the cv.glm() (part of the boot package) function to
# perform LOOCV. This function is used in conjunction with
# the glm() generalized linear model builder used earlier for
# logistic regression (using the family=binomial parameter -
# when this parameter is omitted, glm() performs OLS linear 
# regression).

# Fit a linear regression model of mpg as a function of
# horsepower using glm() with the full dataset as the
# training set, then use the cv.glm() function to compute
# the LOOCV-based estimate of the true MSE.
glm.fit <- glm(mpg~horsepower, data=Auto)
cv.err <- cv.glm(Auto, glm.fit)
# The cv.glm() function produces several outputs - the one 
# we're most interested in is the element named delta, which
# provides an estimate of the test MSE using LOOCV.
cv.err$delta   
# The estimate of the true MSE using LOOCV. The first value
# in delta is the basic CV estimate - just the mean of the
# individual MSE's. The second value is an adjusted value 
# which corrects for the bias that is introduced when k>1
# (more later).

# Repeat this process for increasingly complex polynomials...
# initialize a vector 
cv.error <- rep(0, 10)
# loop through the process 10 times 
for (i in 1:10){
 glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
 cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
 }
cv.error
plot(cv.error, 
     type="b",
     xlab='degree',
     ylab='LOOCV MSE',
     main ='Leave-One-Out Cross Validation')
print(paste('Best degree was ', which.min(cv.error), ' which produced a cross-validation error of ', cv.error[which.min(cv.error)], sep=''))
?cv.glm
