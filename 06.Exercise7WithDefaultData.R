########################################################
# Question 7 in ISLR Ch. 5 (pp. 199-200)
# Revised to use Default dataset (10,000 Observations)
########################################################
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
set.seed(5072, sample.kind="Rejection")
# Using the first 1,000 rows of the Default data frame in
# the ISLR package, fit a logistic regression model that
# predicts the probability of Default using balance. Display
# the model summary, the error rate, the confusion matrix
# and the power
myDefault <- Default[1:1000,  ]
n <- nrow(myDefault)
glm.fit <- glm(default~balance,
               data=myDefault,
               family='binomial')
summary(glm.fit)$coef
# Compute the training error rate.
glm.probs <- predict(glm.fit,
                     myDefault,
                     type='response')
glm.pred <- ifelse(glm.probs>.5, 'Yes', 'No')
mytable <- table(myDefault$default, glm.pred)
mytable
print(paste('Power:', mytable['Yes', 'Yes']/sum(mytable['Yes', ])))
print(paste('Error rate:', mean(glm.pred!=myDefault$default)))
# Repeat using all but the first observation.
glm.fit.L1O <- update(glm.fit, subset=-1)
glm.probs <- predict(glm.fit.L1O,
                     myDefault[1, ],
                     type='response')
glm.probs
glm.pred <- ifelse(glm.probs>.5,'Yes','No')
print(paste('Error rate:', as.numeric(glm.pred!=myDefault[1, ]$default)))
table(myDefault[1, ]$default, glm.pred)
# Do the following to do a LOOCV manually...
# For each observation i, fit a logistic regression model 
# using all but the ith observation to predict probability 
# of Default using balance. Compute the posterior 
# probability of Default for the ith observation, use it 
# predict whether or not the observation was Yes or No, and 
# record the error rate (0 or 1) for each i. Then use this 
# to compute the LOOCV estimate for the test error rate.
T1errors<-0
T2errors<-0
for(i in 1:n) {
  glm.fit.LOO <- update(glm.fit, subset=-i)
  glm.probs <- predict(glm.fit.LOO,
                       myDefault[i, ],
                       type='response')
  glm.pred<-ifelse(glm.probs>.5, 'Yes', 'No')
  if(glm.pred=='Yes' && myDefault[i, ]$default=='No') {
    T1errors <- T1errors+1
  } else {
    if(glm.pred=='No' && myDefault[i, ]$default=='Yes') {
      T2errors <- T2errors+1
    }
  }
}
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
# hard-coded cutoff)
cost <- function(actuals, predictions) mean(abs(actuals-predictions) > 0.5)
cv.err <- cv.glm(myDefault, 
                 glm.fit, 
                 cost)
cv.err$delta   #The LOOCV estimate of the test MSE
# Here's how we would do the same thing using the cost
# function directly:
error.rates <- rep(0,n)
for(i in 1:n) {
  glm.fit.LOO <- update(glm.fit, subset=-i)
  #Predistion must be a probability
  prediction <- predict(glm.fit.LOO,
                       myDefault[i, ],
                       type='response')
  #Actual must be a 0/1 variable
  actual <- ifelse(myDefault[i, 1]=="Yes",1,0)
  error.rates[i] <- cost(prediction, actual)
}
mean(error.rates)
# Finally, do a 10-fold CV
cv.err <- cv.glm(myDefault, 
                 glm.fit, 
                 cost, 
                 K=10)
cv.err$delta   #The 10-fold CV estimate of the test MSE
