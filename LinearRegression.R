rm(list=ls())
#######################################
#### Linear Regression ################
#######################################

#######################
### Functions
#######################
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
## MASS contains the Boston data, car contains the vif()
## function
needed  <-  c("ISLR", "MASS", "car")      
installIfAbsentAndLoad(needed)
####################################################
### ISLR Lab Part 1: Simple Linear Regression ######
####################################################
### Investigate the Boston data frame
?Boston
str(Boston)
## Use the lm() function to produce a linear model called
## lm.fit that regresses median house value (medv) onto
## percentage of households with low socioeconomic status
## (lstat)
lm.fit <- lm(medv ~ lstat, data = Boston)
## Investigate the structure of lm.fit
str(lm.fit)
## Display a summary of the regression model
summary(lm.fit)
# Use the names() function in order to find out what other
# pieces of information are stored in lm.fit
names(lm.fit)
head(lm.fit$model)         ### Retrieve via col names
lm.fit$coefficients
head(lm.fit$residuals)
## Use extractor functions to obtain the same info
coef(lm.fit)
# In order to obtain a confidence interval for the
# coefficient estimates, we can use the confint() function.
confint(lm.fit)   ## For CIs for parameter estimates
confint(lm.fit, level=.99)
# The predict() function can be used to produce confidence
# intervals and prediction intervals for the prediction of
# medv for a given value of lstat.
predict(lm.fit, data.frame(lstat=(c(5,10,15))), 
        interval="confidence")    ### For CIs of points
predict(lm.fit, data.frame(lstat=(c(5,10,15))), 
        interval="prediction")    ### For PIs of points
# plot medv and lstat and the least squares regression line
# using the plot() and abline() functions.
plot(Boston$lstat, Boston$medv)
abline(lm.fit, lwd=3, col="red")
# Some examples of manipulating the appearance of these
# plots
plot(Boston$lstat, Boston$medv, col="red", pch=20)
plot(Boston$lstat, Boston$medv, col="red", pch="+")
plot(1:20, 1:20, pch=1:20)
# Examine some diagnostic plots, some of which will be 
# discussed more fully when we address the topic of Multiple
# Regression.
# 
# Four diagnostic plots are automatically produced by
# applying the plot() function directly to the output from
# lm().
par(mfrow=c(2, 2))   ### Divide plot window into 2x2
plot(lm.fit) 
par(mfrow=c(1,1))
### Plot 4 standard diagnostic plots 
plot(predict(lm.fit), residuals(lm.fit))       ### Suggests non-linearity
plot(predict(lm.fit), rstudent(lm.fit))        ### Ditto - scale is in num t-StdDevs
plot(predict(lm.fit), cooks.distance(lm.fit))  ### 
plot(hatvalues(lm.fit))        ### Plots leverage of points (much bigger than (p+1)/n is bad)
head(hatvalues(lm.fit)[order(hatvalues(lm.fit), decreasing = T)],10)   ### ID highest-leverage points
# Another measure of interesting points - Cook's Distance (>1 is bad)
plot(cooks.distance(lm.fit))
head(cooks.distance(lm.fit)[order(cooks.distance(lm.fit), decreasing = T)],10)   ### ID highest Cook's Distance points
#############################################################
## Assessing the accuracy of the coefficient estimates 
#############################################################
## compute the least squares estimators for a linear
## regression model that regresses median house value (medv)
## on percentage of households with low socioeconomic status
## (lstat)
lm.fit <- lm(medv ~ lstat, data=Boston)    #For comparison
my.summary <- summary(lm.fit)   
my.summary$sigma     # RSE - more later
## Using the expressions in equations on Slide 7...
beta1hat <- (sum(
                (Boston$lstat - mean(Boston$lstat)) 
                * (Boston$medv - mean(Boston$medv))) 
          / sum((Boston$lstat - mean(Boston$lstat)) ^ 2))  
beta1hat
beta0hat <- mean(Boston$medv) - beta1hat * mean(Boston$lstat)
beta0hat
# Alternative calculation of beta1hat
cov(Boston$lstat, Boston$medv)/var(Boston$lstat)
## Calculate the RSS - Slide 15
n <- nrow(Boston)
RSS <- sum((lm.fit$residuals) ^ 2)
RSS
RSSalt <- sum((Boston$medv - (beta0hat + beta1hat * Boston$lstat)) ^ 2)
RSSalt
## Calculate the RSE (Residual Standard Error)  Slide 15
RSE <- sqrt(RSS / (n - 1 - 1))
RSE
## Calculating the standard error of the intercept using RSE
## as an estimate for sigma-squared - Slide 16
se0 <- sqrt(RSE ^ 2 * (1 / n + mean(Boston$lstat) ^ 2 /
                         sum((Boston$lstat - mean(Boston$lstat)) ^ 2)))
se0
## Calculating the standard error of the slope - Slide 16
se1 <- sqrt(RSE ^ 2 / sum((Boston$lstat - mean(Boston$lstat)) ^ 2))
se1
## Construct a 95% confidence interval for the slope parameter Slide 17
confint(lm.fit, level=.95)
# Now do it manually - slide 17
myconfint <- c(beta1hat - qt(.975, n - 2) * se1, beta1hat + qt(.975, n - 2) * se1)
myconfint
## Conduct a hypothesis of H0:beta1=0 H1:beta1<>0 - Slide 20
(tstat <- (beta1hat - 0) / se1)
(pvalue <- 2 * pt(tstat, n - 2))    # p-value for 2-tailed test
## Reject H0 since pvalue is VERY small
## Calculate TSS (Slide 25) and r-squared Slide 28
TSS <- sum((Boston$medv - mean(Boston$medv)) ^ 2)
(rsquared <- 1 - RSS / TSS)
# For simple regression, R-squared is the square of the
# correlation coefficient between x and y
cor(Boston$lstat, Boston$medv) ^ 2   
######################################################
### ISLR Lab Part 2: Multiple Linear Regression ######
######################################################
lm.fit <- lm(medv ~ lstat + age, data = Boston)  # Basic syntax is lm(y ~ a + b + c)
summary(lm.fit)
lm.fit <- lm(medv ~ ., data = Boston) # the "." is shorthand for "everything"
summary(lm.fit)
lm.fit1 <- lm(medv ~ . -tax, data=Boston)     # Excludes tax
summary(lm.fit1)
# This achieves the same result using the update() function         
lm.fit1 <- update(lm.fit, ~ . -tax)         
y <- Boston$medv
x <- Boston[-grep("medv", names(Boston))]   # Exclude the response variable
p <- ncol(Boston)-1
# For comparison later
lm.fit <- lm(medv ~ ., data=Boston)
(lm.fit.summary <- summary(lm.fit))
## Compute the Multiple Regression Coefficients by Solving 
## the Regression Equation Y = XB - slide 7
## 
## Recall that beta = Inverse(x'x)x'Y. We first construct a 
## 'model matrix' using the model.matrix() function, then
## compute this matrix product to get the beta's. 
x.mm <- model.matrix(~ ., Boston[, -which(names(Boston) %in% 'medv')])
(beta.hat <- solve(t(x.mm) %*% x.mm) %*% t(x.mm) %*% y)
##
## Use the regression equation on slide 9 to create predictions y-hat
y.hat <- x.mm %*% solve(t(x.mm) %*% x.mm) %*% t(x.mm) %*% y
## now using predict() function
preds <- predict(lm.fit)
## biggest difference is tiny - only differences is due to round-off
max(preds-y.hat)
## Matrix expression for RSS 
# In algebraic form 
(RSS <- sum((Boston$medv - x.mm %*% betas) ^ 2))
RSE <- sqrt(RSS / (n - p -1))
# In matrix form (slide 10) using the hat matrix
H <- x.mm %*% solve(t(x.mm) %*% x.mm) %*% t(x.mm)
M <- diag(nrow(x.mm)) - H
(RSS.matrix <- t(Boston$medv) %*% M %*% Boston$medv)
## Compute confidence and prediction intervals for a new observation:
# Recall: Confidence Intervals coefficients
head(confint(lm.fit, level=.95))
# Also need confidence intervals for predictions (the y-hats).
# 
# Choose a random observation from Boston predictors to act as a new observation
new.obs.df <- Boston[100, -which(names(Boston) %in% 'medv')] 
# Create a model matrix (recall: adds a 1, creates dummy
# variables for categoric variables)
new.obs.mm <- model.matrix(~ ., new.obs.df)
# Method 1: Use extractor function:
predict(lm.fit, 
         newdata = new.obs.df,
         interval='confidence', se.fit=T)
# Method 2: Use formulae  
# Slides 23 
x.mm <- model.matrix(~ ., Boston[, -which(names(Boston) %in% 'medv')])
(fit <- new.obs.mm %*% betas)
(se <- RSE * sqrt((new.obs.mm %*% solve(t(x.mm) %*% x.mm) %*% t(new.obs.mm))))
(lwr <- fit - qt(.975, n-p-1) * se)
(upr <- fit + qt(.975, n-p-1) * se)

# Recall: Prediction Intervals coefficients 
# Slide 26
# Method 1: Use extractor function:
predict(lm.fit, 
        newdata = new.obs.df,
        interval='prediction', se.fit=T)
# Method 2: 
(fit <- new.obs.mm %*% betas)
(se <- RSE * sqrt((1+(new.obs.mm %*% solve(t(x.mm) %*% x.mm) %*% t(new.obs.mm)))))
(lwr <- fit - qt(.975, n-p-1) * se)
(upr <- fit + qt(.975, n-p-1) * se)

##############################################
### ISLR Lab Part 3: Regression Extensions  ##
##############################################
# Interaction Terms

# The syntax lstat : age tells R to include an interaction
# term between lstat and age.
summary(lm(medv ~ lstat : age, data=Boston))
# The syntax lstat * age simultaneously includes lstat, age, 
# and the interaction term lstat × age as predictors; it is a
# shorthand for lstat + age + lstat:age.
summary(lm(medv ~ lstat * age, data=Boston))
# Non-linear Transformations of the Predictors
# 
# The function I() is needed since the ^ has a special 
# meaning in a formula; wrapping as we do allows the 
# standard usage in R, which is to raise X to the power 2.
lm.fit2 <- lm(medv ~ lstat 
              + I(lstat ^ 2), 
              data=Boston)
summary(lm.fit2)
# The near-zero p-value associated with the quadratic term 
# suggests that it leads to an improved model. We use the 
# anova() function to further quantify the extent to which
# the quadratic fit is superior to the linear fit.
lm.fit1 <- lm(medv ~ lstat, 
             data=Boston)
anova(lm.fit1, lm.fit2)
# 
# Here Model 1 represents the linear submodel containing
# only one predictor, lstat, while Model 2 corresponds to
# the larger quadratic model that has two predictors, lstat
# and lstat2. 

# The anova() function performs a hypothesis test comparing
# the two models. The null hypothesis is that the two models
# fit the data equally well, and the alternative hypothesis
# is that the full model is superior. 

# Here the F-statistic is 135 and the associated p-value is
# virtually zero. This provides very clear evidence that the
# model containing the predictors lstat and lstat2 is far
# superior to the model that only contains the predictor
# lstat. 

# This is not surprising, since earlier we saw evidence for
# non-linearity in the relationship between medv and lstat.

par(mfrow=c(2, 2))
plot(lm.fit2)
# In this plot, we see that when the lstat-squared term is
# included in the model, there is little discernible pattern
# in the residuals.

# In order to create a cubic fit, we can include a predictor
# of the form I(X^3). 

# However, this approach can start to get cumbersome for
# higher-order polynomials. A better approach involves using
# the poly() function to create the polynomial within lm().
# For example, the following command produces a fifth-order
# polynomial fit:
  
lm.fit5 <- lm(medv ~ poly(lstat, 5), 
              data=Boston)
summary(lm.fit5)

# This suggests that including additional polynomial terms,
# up to fifth order, leads to an improvement in the model
# fit! 

# However, further investigation of the data reveals that no
# polynomial terms beyond fifth order have significant
# p-values in a regression fit. Of course, we are in no way
# restricted to using polynomial transformations of the
# predictors. Here we try a log transformation.

summary(lm(medv ~ log(rm), data=Boston))

# Qualitative Predictors

# We will now examine the Carseats data, which is part of
# the ISLR library. We will attempt to predict Sales (child
# car seat sales) in 400 locations based on a number of
# predictors.

names(Carseats)
head(Carseats)

# The Carseats data includes qualitative predictors such as
# Shelveloc, an indicator of the quality of the shelving
# location—that is, the space within a store in which the
# car seat is displayed—at each location. The predictor 
# Shelveloc takes on three possible values, Bad, Medium, and
# Good.

# Given a qualitative variable such as Shelveloc, R
# generates dummy variables automatically. Below we fit a
# multiple regression model that includes some interaction
# terms.

lm.fit <- lm(Sales ~ . + Income : Advertising 
             + Price : Age, 
             data=Carseats)
summary(lm.fit)

# The contrasts() function returns the coding that R uses 
# for the dummy variables.

contrasts(Carseats$ShelveLoc)

# R has created a ShelveLocGood dummy variable that takes on
# a value of 1 if the shelving location is good, and 0
# otherwise. 

# It has also created a ShelveLocMedium dummy variable that
# equals 1 if the shelving location is medium, and 0
# otherwise. 

# A bad shelving location corresponds to a zero for each of
# the two dummy variables. 

# The fact that the coefficient for ShelveLocGood in the
# regression output is positive indicates that a good
# shelving location is associated with high sales (relative
# to a bad location). And ShelveLocMedium has a smaller
# positive coefficient, indicating that a medium shelving
# location leads to higher sales than a bad shelving
# location but lower sales than a good shelving location.
# 
# Variance Inflation Factor
lm.fit2 <- lm(medv ~ ., data=Boston)     
summary(lm.fit2)
vif(lm.fit2)        # slide 38 - displays variance inflation factors. VIF > 5 to 10 implies multicollinearity
lm.fit2 <- lm(medv ~ . -tax, data=Boston)     # Excludes tax
vif(lm.fit2)
summary(lm.fit2)
