## 1
library(AER)
data(PSID1976)
Ex1Data <- PSID1976[PSID1976$wage != 0,]
library(gmm)

## 2
Ex2MDL <- lm(log(wage) ~ education, data=Ex1Data)

Ex2GMM <- gmm(log(Ex1Data$wage) ~ Ex1Data$education,x=Ex1Data$education)

## 3
Ex3MDL <- ivreg(log(wage) ~ education | feducation, data = Ex1Data)

Ex3GMM <- gmm(log(Ex1Data$wage) ~ Ex1Data$education,Ex1Data$feducation)

## 4
Ex4MDL <- lm(log(wage) ~ education + experience + I(experience^2),data = Ex1Data)

Ex4GMM <- gmm(log(Ex1Data$wage) ~ Ex1Data$education + Ex1Data$experience + I(Ex1Data$experience^2), cbind(Ex1Data[,c("education","experience")],I(Ex1Data$experience^2)))

## 5
Ex5MDL <- ivreg(log(wage) ~ education + experience + I(experience^2) | experience + I(experience^2) +feducation,data = Ex1Data)

Ex5GMM <- gmm(log(Ex1Data$wage) ~ Ex1Data$education + Ex1Data$experience + I(Ex1Data$experience^2), cbind(Ex1Data[,c("feducation","experience")],I(Ex1Data$experience^2)))

## 6
Ex6MDL <- ivreg(log(wage) ~ education | feducation + meducation, data=Ex1Data)

Ex6GMM <- gmm(log(Ex1Data$wage) ~ Ex1Data$education,x=Ex1Data[,c("feducation","meducation")])

## 7
Ex7MDL <- ivreg(log(wage) ~ education + experience + I(experience^2)| experience + I(experience^2) + meducation + feducation,data = Ex1Data)

Ex7GMM <- gmm(log(Ex1Data$wage) ~ Ex1Data$education + Ex1Data$experience + I(Ex1Data$experience^2), cbind(Ex1Data[,c("meducation","feducation","experience")],I(Ex1Data$experience^2)))

## 8

summary(Ex7GMM)
summary(Ex6GMM)

## 9
Ex9GMM <- gmm(log(Ex1Data$wage) ~ Ex1Data$education + Ex1Data$experience + I(Ex1Data$experience^2), cbind(Ex1Data[,c("meducation","feducation","experience")],I(Ex1Data$experience^2)),type = "iter", traceIter = TRUE)

## 10
plot(Ex7GMM, 3)