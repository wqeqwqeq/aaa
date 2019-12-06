rm(list=ls())
##########################################################################################
### Functions
##########################################################################################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
##############################
### Load required packages ###
##############################
needed <- c("ISLR")  #contains Auto data
installIfAbsentAndLoad(needed)
# Get data into a data frame
mydf <- Auto
n <- nrow(mydf)
# Randomly shuffle the data frame - this is a cautionary (and
# almost always necessary) step to prevent bias if the data
# is sorted somehow
set.seed(5072, sample.kind="Rejection")
mydf <- mydf[sample(n, n),]
# Create 10 equally size folds
numfolds <- 10
fold.indices <- cut(1:n, breaks=numfolds, labels=F)
#Perform 10 fold cross validation
mse <- rep(0, numfolds)
# Build the model with the full data frame (this is the
# point - don't need to withhold rows for validation/test)
my.final.model <- glm(mpg ~ poly(horsepower, 2), data=mydf)
summary(my.final.model)
# Estimate the expected value of the true MSE
for(i in 1:numfolds){
  #Segement your data by fold using the which() function 
  test.indices <- which(fold.indices == i)
  test.data <- mydf[test.indices, ]
  train.data <- mydf[-test.indices, ]
  glm.fit=glm(mpg ~ poly(horsepower,2),data=train.data)
  mse[i] <- mean((predict.glm(glm.fit, test.data) - test.data$mpg) ^ 2)
}
# The following value is the final estimate the expected
# value of the true MSE
mean(mse)  
# The following value is a measure of its variability 
sd(mse)    
# 
# Now compare to cv.glm()...
glm.fit <- glm(mpg ~ poly(horsepower,2), data=mydf)
cv <- cv.glm(mydf, glm.fit, K = 10)
cv$delta
