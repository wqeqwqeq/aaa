rm(list=ls())
##########################################################################################
### Functions
##########################################################################################
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
# 
needed <- c('ISLR')      
installIfAbsentAndLoad(needed)

n <- nrow(Auto)
n
# Set the seed for a single split
set.seed(5072, sample.kind="Rejection")
# Randomly select half the indices of the Auto data frame
train <- sample(n,.5*n)
length(train)
head(train)
# Create five linear regression models with increasing flexibility
# and calculate their MSE's
MSEs <- data.frame(degree=rep(0, 10), MSE=rep(0, 10))
for(i in 1:10) {
  lm.fit1 <- lm(mpg ~ poly(horsepower, i), 
                data = Auto,
                subset = train)
  MSEs[i,1:2] <- c(i, 
                   mean((Auto$mpg - predict(lm.fit1, Auto))[-train] ^ 2))
}
MSEs
# Observe that this analysis suggests that the minimum MSE
# occurs with polynomial of degree 7

# Create a different split of the training data, build three
# more linear regression models identical to those above,
# and calculate their MSE's

## Set the seed again, this time before multiple splits
set.seed(1, sample.kind="Rejection")
# Randomly select 10 different splits of the indices of the
# Auto data frame and for each create five linear regression
# models with increasing flexibility
MSEs <- data.frame(sample=rep(0,100), degree=rep(0,100), MSE=rep(0,100))
for(j in 1:10) {
  train=sample(n, 0.5*n)
  for(i in 1:10) {
    lm.fit1 <- lm(mpg~poly(horsepower,i),
                  data=Auto,
                  subset=train)
    MSEs[(j-1)*10+i,] <- c(j, i, mean((Auto$mpg-predict(lm.fit1,Auto))[-train]^2))
  }
}
head(MSEs, 12)
par(mfrow=c(1,1))
plot(1:10, 
     ylim=range(MSEs$MSE),
     type='n',
     xlab="Degree",
     ylab='MSE',
     main='Validation Set Approach')
for(j in 1:10) {
  lines(MSEs[MSEs$sample==j,]$degree,
        MSEs[MSEs$sample==j,]$MSE,
        type='b')
}
 
# Observe that this analysis seems to suggests that the
# minimum MSE occurs with a polynomial of degree 5, but note
# also that there is no agreement between the ten different
# training sets as to the  value of the true MSE.

# Compute the means of the MSEs for each polynomial fit, and
# plot these
colmeans <- rep(0, 10)
for(i in 1:10) {
  colmeans[i] <- mean(MSEs[MSEs$degree==i,]$MSE)
}
plot(1:10, colmeans,
     type='b',
     xlab="Degree",
     ylab='MSE',
     main='Validation Set Approach')
par(mfrow=c(1,1))

