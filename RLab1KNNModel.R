rm(list=ls())
###############################
### Functions
###############################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    library(thispackage, character.only = T)
  }
}
##############################
### Load required packages ###
##############################
needed  <-  c("ISLR", "class")  # class contains the knn() function
installIfAbsentAndLoad(needed)
#########################################
### K-Nearest Neighbors (ISLR pp. 164)
#########################################
?Smarket
str(Smarket)
# We will use the Smarket data to predict the direction of
# the market's movement in 2005 based on a training set
# composed of market behavior during the period from
# 2001-2004. Only the Lag1 and Lag2 variables will be used
# for this purpose.
#
# Create a training set of observations from 2001 to 2003, a
# validate set for 2004 observations and a test set of
# observations from 2005
train <- Smarket$Year < 2003
validate <- Smarket$Year == 2003  
test <- Smarket$Year == 2004  
# these vectors are T's and F's of length equal to the
# Smarket nrow - the rows that are T will be used to select
# the rows from smarket for each subset - these subsets will
# be mutually exclusive and collectively exhaustive.
sum(train)   
sum(validate)
sum(test)
sum(train && validate)  
sum(train && test)
sum(validate && test)
# The knn() function requires 4 parameters: 
#
# 1. A matrix or dataframe containing the predictors (the X's) 
#    associated with the training data, here train.x
#
# 2. A matrix or dataframe containing the predictors (the X's) 
#    associated with the data for which we wish to make predictions, 
#    here train.x for the training set, validate.x for the validate 
#    set, and test.x for the test set.
#
# 3. A factor containing the class labels (the Y's) for the
#    training observations, here train.y
#
# 4. A value for k, the number of nearest neighbors to be 
#    used by the classifier.
used.vars <- c('Lag1', 'Lag2')
train.x <- Smarket[train, used.vars]
validate.x <- Smarket[validate, used.vars]
test.x <- Smarket[test, used.vars]
train.y <- Smarket$Direction[train]
validate.y <- Smarket$Direction[validate]
test.y <- Smarket$Direction[test]
# We need to set a seed because ties in nearest neighbors
# are possible when k is even and these ties are broken
# randomly
set.seed(123)       
# Fit a knn model with k=1 and produce predictions for the
# validate X's
k <- 1
knn.pred <- knn(train.x, validate.x, train.y, k=k)
# In the table() function, always put Actuals first so they 
# appear in the rows, predictions next so they appear in the
# columns
mytable <- table(validate.y, knn.pred)   
mytable
validate.error <- (mytable["Up", "Down"] + mytable["Down", "Up"]) / sum(mytable)
print(paste('The validate error rate is ', round(validate.error, 4)*100, '% when k=', k, sep='')) 
# Since the error rate with k=1 is so high, try with a less
# flexible model, say k=3. 
k <- 3
knn.pred <- knn(train.x, validate.x, train.y, k=k)
mytable <- table(validate.y, knn.pred)
mytable
(validate.error <- (mytable["Down", "Up"] + mytable["Up", "Down"]) / sum(mytable))
print(paste('The validate error rate is ', round(validate.error, 4)*100, '% when k=', k, sep='' )) 
# Since the error rate with k=3 is still high, try with a
# still-less flexible model with k=10. 
k <- 10
knn.pred <- knn(train.x, validate.x, train.y, k=k)
mytable <- table(validate.y, knn.pred)
mytable
validate.error <- (mytable["Down", "Up"] + mytable["Up", "Down"]) / sum(mytable)
print(paste('The validate error rate is ', round(validate.error, 4)*100, '% when k=', k, sep='' )) 

# Among the 3 values of k we analyzed, it looks like k=1 has
# the lowest validate error, so it would become our "best"
# model for the knn learning algorithm.

# As a final assessment of our best model's accuracy, we
# evaluate it using the test set:
k <- 1
knn.pred <- knn(train.x, test.x, train.y, k=k)
mytable <- table(test.y, knn.pred)
mytable
test.error <- (mytable["Down", "Up"] + mytable["Up", "Down"]) / sum(mytable)
print(paste('The test error rate is ', round(test.error, 4)*100, '% when k=', k, sep='' )) 

# Just for interest sake, let's see how this model would
# have done on the training data...
k <- 1
knn.pred <- knn(train.x, train.x, train.y, k=k)
mytable <- table(train.y, knn.pred)   
mytable
training.error <- (mytable["Down", "Up"] + mytable["Up", "Down"]) / sum(mytable)
print(paste('The training error rate is ', round(training.error, 4)*100, '% when k=', k, sep='' )) 
