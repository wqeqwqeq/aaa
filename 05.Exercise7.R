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
############################################
# Question 7 in ISLR Ch. 5 (pp. 199-200) 
############################################
set.seed(5072, sample.kind="Rejection")
# 1. Using the Weekly data frame in the ISLR package, Fit a 
#    logistic regression model that predicts the probability of
#    Direction using Lag1 and Lag2.
# 2. Display the model summary
# 3. Display the error rate
# 4. Display the confusion matrix
# 5. Compute and display the power of the model
n <- nrow(Weekly)
glm.fit <- glm(Direction~Lag1 + Lag2, 
               data=Weekly, 
               family='binomial')
summary(glm.fit)$coef
# Compute the training error rate.
glm.probs <- predict(glm.fit, Weekly, type='response')
glm.pred <- ifelse(glm.probs>.5, 'Up', 'Down')
mytable <- table(Weekly$Direction, glm.pred)
mytable
print(paste('Power:', mytable['Up','Up']/sum(mytable['Up', ])))
print(paste('Error rate:', mean(glm.pred!=Weekly$Direction)))
# 6. Repeat using all but the first observation.
glm.fit.L1O <- update(glm.fit, subset=-1)
# 6. Repeat using all but the first observation. Then
#    predict with the single withheld observation and compute
#    the associated error rate.
glm.probs <- predict(glm.fit.L1O, Weekly[1, ], type='response')
glm.probs
glm.pred <- ifelse(glm.probs>.5, 'Up', 'Down')
print(paste('Error rate:', 
            as.numeric(glm.pred!=Weekly[1, ]$Direction)))
table(Weekly[1, ]$Direction, glm.pred)
# Do the following to do a LOOCV manually...
# For each observation i, fit a logistic regression model
# using all but the ith observation to predict probability
# of Direction using Lag1 and Lag2. Compute the posterior 
# probability of Direction for the ith observation, use it
# predict whether or not the observation was Up or Down, and
# record the error rate (0 or 1) for each i. Then use this
# to compute the LOOCV estimate for the test error rate.
T1errors <- 0
T2errors <- 0
for(i in 1:n) {
  glm.fit.LOO <- update(glm.fit, subset=-i)
  glm.probs <- predict(glm.fit.LOO, 
                       Weekly[i, ], 
                       type='response')
  glm.pred <- ifelse(glm.probs>.5, 'Up', 'Down')
  if(glm.pred=='Up' && Weekly[i, ]$Direction=='Down') {
    T1errors <- T1errors+1
  } else {
    if(glm.pred=='Down' && Weekly[i, ]$Direction=='Up') {
      T2errors <- T2errors+1
    }
  }
}
# Compute the LOOCV estimate as the mean of error rates
(T1errors+T2errors)/n
# Compute the LOOCV estimate using the cv.glm() function
#################
### IMPORTANT ###
#################
# When using the cv.glm() function on methods that return 
# probabilities (e.g.: logistic regression), the optional 
# parameter cost must be included to transform the 
# probabilities into 0/1 predictions. The following simple
# function will work in most cases (note the presence of the
# hard-coded cutoff of 0.5)
cost <- function(actuals, predictions) mean(abs(actuals-predictions) > 0.5)
cv.err <- cv.glm(Weekly, 
                 glm.fit, 
                 cost)
cv.err$delta   
# This LOOCV estimate of the test MSE is the same as the
# manual value computed above.
# 
# Calculate a 10-fold cross validation error
cv.err <- cv.glm(Weekly, 
                 glm.fit, 
                 cost, 
                 K=10)
cv.err$delta   #The 10-fold CV estimate of the test MSE



