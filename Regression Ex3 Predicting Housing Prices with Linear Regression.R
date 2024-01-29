## 1
library(mlbench)
data(BostonHousing)

## 2
boxplot(BostonHousing$medv)

## 3
for(var in c("crim","rm","age","rad","tax","lstat")){
  plot(x=BostonHousing[,var],y=BostonHousing$medv,xlab=var)
  abline(lm(BostonHousing$medv ~ BostonHousing[,var]))
}

## 4
library(caret)
set.seed(123)
test <- sample(c(0,1),506,replace=TRUE,prob=c(.75,.25))
# "test" being 1 means that the row is meant for testing (not training)
BostonHousing$test <- test

testSet <- BostonHousing[test == 1,]
trainSet <- BostonHousing[test == 0,]

## 5
Ex5MDL <- lm(medv ~ crim + rm + tax + lstat,data=trainSet)

## 6
summary(Ex5MDL)
#Observation: R^2 =.6506

par(mfrow=c(2,2))
plot(Ex5MDL)

## 7

# log transform
trainSet$logMedv <- log(trainSet$medv)
Ex7logMDL <- lm(logMedv ~ crim + rm + tax + lstat,data=trainSet)

# box-cox transform
library(MASS)

Ex7bc <- boxcox(medv ~ crim + rm + tax + lstat,data=trainSet)
lambda <-  Ex7bc$x[which.max(Ex7bc$y)]

Ex7bcMDL <- lm( ((medv^lambda - 1)/lambda) ~ crim + rm + tax + lstat,data=trainSet)

## 8 

#log transform
summary(Ex7logMDL)
par(mfrow=c(2,2))
plot(Ex7logMDL)
mean(Ex7logMDL$residuals)

# box-cox transform
summary(Ex7bcMDL)
par(mfrow=c(2,2))
plot(Ex7bcMDL)
mean(Ex7bcMDL$residuals)

## 9
# using box-cox model due  to higher r-squared

testSet$medvPredictions <- (lambda * predict(Ex7bcMDL,testSet) + 1)^(1/lambda)

## 10
par(mfrow=c(1,1))
plot(x=testSet$medvPredictions,y=testSet$medv,xlab="predictions",ylab="actual")
abline(a=0,b=1)
