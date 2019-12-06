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
######################################
### k-Fold Cross-Validation  #########
######################################
set.seed(5072, sample.kind="Rejection")
#Create an error vector
cv.error=rep(0,12)
# Loop through the integers from 1 to 10, creating
# increasingly flexible polynomial regression models and
# calculating their 10-fold CV MSE's
for (i in 1:12){
 glm.fit=glm(mpg~poly(horsepower,i), data=Auto)
 cv.error[i]=cv.glm(Auto,glm.fit, K=10)$delta[1]
 }
# Display the results
cv.error
plot(cv.error,type="b",
     xlab='Degree',
     ylab='10-Fold Cross Validation Error',
     main='k-Fold Cross Validation ')
