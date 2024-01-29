## 1

data(cars)

## 2
plot(x=cars$dist,y=cars$speed)

## 3
Ex3MDL <- lm(dist ~ speed, data = cars)

## 4
summary(Ex3MDL)
#Observation:65.11% of variation is explained by the model

## 5
par(mfrow=c(2,2))
plot(Ex3MDL)

## 6
library(car)
ncvTest(Ex3MDL)
#observation: The P-value for a Breusch-Pagan test suggests the data is not Homoscedastic


## 7

shapiro.test(Ex3MDL$residuals)
#observation: The P-value for a Shapiro-wilk test suggests that the residuals are not normally distributed

## 8 
summary(lm(Ex3MDL$residuals ~ cars$speed))
##observation: the residuals are not correlated with the explanitory value

## 9
##Observation: no points exist outside the Cook's Distance

## 10
df10 <- cars[-c(49),]
Ex10MDL <- lm(dist ~ speed, data = df10)
summary(Ex3MDL)
summary(Ex10MDL)

#observation: after removing datapoint #49, the slope of the model is reduced by ~.3