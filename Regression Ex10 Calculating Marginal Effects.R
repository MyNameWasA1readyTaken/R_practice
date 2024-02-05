## 1
data("mtcars")
Ex1MDL <- lm(mpg ~ wt + qsec + am + hp,data = mtcars)

## 2
print(Ex1MDL$coefficients)

## 3
library(margins)
marginal_effects(Ex1MDL,mtcars)[1,]

## 4
# Observation: the marginal effect of any variable in a linear model will be the same as its coefficient

## 5
Ex5MDL <- lm(mpg ~ wt + qsec + am + hp + am * hp ,data = mtcars)

## 6
library(glmnet)
data("BinomialExample")
Ex6Data <- data.frame(BinomialExample$x[,1:4])
Ex6Data$y <- BinomialExample$y

Ex6MDL <- glm(y ~ ., data = Ex6Data  ,family = binomial)

## 7
# Observation: while a linear model's derivative is the same at all points, a the derivative of a logistical model will have some term which is affected by the independant variables


## 8
margins(Ex6MDL,at = list(X1 = c(mean(Ex6Data$X1)),X2 = c(mean(Ex6Data$X2)),X3 = c(mean(Ex6Data$X3)),X4 = c(mean(Ex6Data$X4))))

## 9
margins(Ex6MDL)

## 10
# It is different to find the average marginal effect than to find the marginal effect at the mean
