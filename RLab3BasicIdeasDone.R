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
needed  <-  c("ISLR", 'FNN')  #FNN contains the knn.reg() function
installIfAbsentAndLoad(needed)
################################################
######### Part 1: Predicting Graduation Rate: 
######### A Regression Example 
################################################
college <- read.table('College.csv', header=T, sep=',', quote='')
rownames(college) <- college$Name
## We need to omit the Private variable as it is a factor
## and nearest-neighbor reasoning functions only like
## numeric variables. We also need to omit the first
## variable since we have used it as a rowname.
college <- college[!names(college) %in% c('Name', 'Private')]
n <- nrow(college)
p <- ncol(college)-1
print(paste('MSE of predicting the mean:', 
            mean((college$Grad.Rate-mean(college$Grad.Rate))^2)))
print(paste('Variance of the population:', 
            (var(college$Grad.Rate)*(n-1)/n)))
print('The MSE calculated by always predicting the mean (called the Null Model) measures the mean of the squared deviations around the predictions. Since the predictions are the mean in the case of the Null Model, this is precisely the calculation required to calculate the variance, which is also the mean of the squared deviations around the mean')
## Scale the numeric data now rather than after it has #
## been partitioned - better centering/rescaling with the #
## entire set of observations
#
college[1:p] <- scale(college[1:p])   
set.seed(50721, sample.kind = "Rejection")
# Say we want a 70/20/10 split of rows for training,
# validation and test respectively
trainprop <- 0.70  
validateprop <- 0.2
train  <-  sample(n, trainprop * n)
validate  <-  sample(setdiff(1:n, train), validateprop * n) 
test <- setdiff(1:n, union(train, validate))
# Create the data frames using the indices created in the
# three vectors above
train.set <- college[train,]
validate.set <- college[validate,]
test.set <- college[test,]
# Build subsets for knn
y.name='Grad.Rate'
x.names <- setdiff(names(train.set), y.name)
train.x <- train.set[x.names]
validate.x <- validate.set[x.names]
test.x <- test.set[x.names]
train.y <- train.set$Grad.Rate
validate.y <- validate.set$Grad.Rate
test.y <- test.set$Grad.Rate
# The knn.reg() function requires 4 parameters: 
#
# 1. A matrix or dataframe containing the predictors (the X's) 
#    associated with the training data, here train.x
#
# 2. A matrix or dataframe containing the predictors (the X's) 
#    associated with the data for which we wish to make predictions. 
#
# 3. A vector containing the response variable values for the
#    training observations, here train.y
#
# 4. A value for k, the number of nearest neighbors to be 
#    used by the classifier.
biggestk=50
validateMSEs <- rep(0, biggestk)
trainMSEs <- rep(0, biggestk)
kset <- seq(1, biggestk, by=1)  
set.seed(50722, sample.kind = "Rejection")
for(k in kset) {
  knn.regression.model <- knn.reg(train.x, validate.x, train.y, k=k)
  validateMSEs[k] <- mean((validate.y-knn.regression.model$pred)^2)
  knn.regression.model <- knn.reg(train.x, train.x, train.y, k=k)
  trainMSEs[k] <- mean((train.y-knn.regression.model$pred)^2)
}
print(cbind(k=kset, validateMSEs, trainMSEs))
# 
plot(kset, validateMSEs, 
     xlim=c(biggestk, 1), 
     ylim=c(0,max(c(validateMSEs, trainMSEs))),      
     type='n',  
     xlab='Increasing Flexibility (Decreasing k)', 
     ylab='Mean Squared Errors', 
     main='Training and Validate MSEs as a Function of \n Flexibility for KNN Prediction')
lines(seq(biggestk, 1, by=-1), 
      validateMSEs[order(length(validateMSEs):1)], 
      type='b', 
      col=2, 
      pch=16)
lines(seq(biggestk, 1, by=-1), 
      trainMSEs[order(length(trainMSEs):1)], 
      type='b', 
      col=1, 
      pch=16)
legend("topleft", legend = c("Validate MSEs", "Training MSEs"), 
       col=c(2, 1), 
       cex=.75, 
       pch=16)
# Produce a summary of the results so far
best.validate.k <- kset[which.min(validateMSEs)]
best.train.k <- kset[which.min(trainMSEs)]
best.validate.mse <- validateMSEs[which.min(validateMSEs)]
best.train.mse <- trainMSEs[which.min(trainMSEs)]
# 
print(paste("Best validate k is", best.validate.k, "with a validate MSE of", best.validate.mse))
print(paste("Best training k is", best.train.k, "with training MSE of", best.train.mse))

# Produce a test MSE for the 'best' model 
knn.regression.model <- knn.reg(train.x, test.x, train.y, k=best.validate.k)
test.mse <- mean((test.y-knn.regression.model$pred)^2)
print(paste("The test MSE for k=", best.validate.k, "is", test.mse))

# Now repeat with only the odd k's
biggestk=49
validateMSEs <- rep(0, biggestk %/% 2 + 1)
trainMSEs <- rep(0, biggestk %/% 2 + 1)
kset <- seq(1, biggestk, by=2)  
set.seed(50723, sample.kind = "Rejection")
for(k in kset) {
  knn.regression.model <- knn.reg(train.x, validate.x, train.y, k=k)
  validateMSEs[k %/% 2 + 1] <- mean((validate.y-knn.regression.model$pred)^2)
  knn.regression.model <- knn.reg(train.x, train.x, train.y, k=k)
  trainMSEs[k %/% 2 + 1] <- mean((train.y-knn.regression.model$pred)^2)
}
print(cbind(kset, validateMSEs, trainMSEs))
# 
plot(kset, validateMSEs, 
     xlim=c(biggestk, 1), 
     ylim=c(0,max(c(validateMSEs, trainMSEs))),      
     type='n',  
     xlab='Increasing Flexibility (Decreasing k)', 
     ylab='Mean Squared Errors', 
     main='Training and Validate MSEs as a Function of \n Flexibility for KNN Prediction')
lines(seq(biggestk, 1, by=-2), 
      validateMSEs[order(length(validateMSEs):1)], 
      type='b', 
      col=2, 
      pch=16)
lines(seq(biggestk, 1, by=-2), 
      trainMSEs[order(length(trainMSEs):1)], 
      type='b', 
      col=1, 
      pch=16)
legend("topleft", legend = c("Validate MSEs", "Training MSEs"), 
       col=c(2, 1), 
       cex=.75, 
       pch=16)
# Produce a summary of the results so far
best.validate.k <- kset[which.min(validateMSEs)]
best.train.k <- kset[which.min(trainMSEs)]
best.validate.mse <- validateMSEs[which.min(validateMSEs)]
best.train.mse <- trainMSEs[which.min(trainMSEs)]
# 
print(paste("Best validate k is", best.validate.k, "with a validate MSE of", best.validate.mse))
print(paste("Best training k is", best.train.k, "with training MSE of", best.train.mse))

# Produce a test MSE for the 'best' model 
knn.regression.model <- knn.reg(train.x, test.x, train.y, k=best.validate.k)
test.mse <- mean((test.y-knn.regression.model$pred)^2)
print(paste("The test MSE for k=", best.validate.k, "is", test.mse))