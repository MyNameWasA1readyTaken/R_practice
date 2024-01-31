## 1
library(lars)
library(glmnet)
data(diabetes)

## 2
par(mfrow=c(3,4))
for(name in colnames(diabetes$x)){
  plot(diabetes$y ~ diabetes$x[,name],xlab=name)
  abline(lm(diabetes$y ~ diabetes$x[,name]))
}

par(mfrow=c(1,1))
## 3
Ex3MDL <- lm(y ~ x , data = diabetes)

## 4
Ex4MDL <- glmnet(diabetes$x,diabetes$y)
plot(Ex4MDL)

## 5
Ex5Data <- cv.glmnet(diabetes$x,diabetes$y)
plot(Ex5Data)
lambda <- Ex5Data$lambda.min

## 6
Ex6MDL <- glmnet(diabetes$x,diabetes$y,lambda = lambda)
Ex6betaMatrix <- Ex6MDL$beta

## 7
Ex7MDL <- glmnet(diabetes$x, diabetes$y, lambda = Ex5Data$lambda.1se)
Ex7betaMatrix <- Ex7MDL$beta

## 8
Ex8MDL <- lm(y ~ x2 , data = diabetes)
summary(Ex8MDL)

#9
Ex9MDL <- glmnet(diabetes$x2,diabetes$y)
plot(Ex9MDL)

#10
Ex10Data <- cv.glmnet(diabetes$x2,diabetes$y)
plot(Ex10Data)
Ex10Lambda <- Ex10Data$lambda.min

Ex10MDL <- glmnet(diabetes$x2,diabetes$y,lambda = Ex10Lambda)
Ex10betaMatrix <- Ex10MDL$beta
Ex10MDL$beta
