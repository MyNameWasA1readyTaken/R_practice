## 1
setwd("/home/marcus/Documents/Rwork/R exercises/")
Data <- read.csv("mussel.csv")

plot(INDIV ~ AREA, data = Data)

## 2
Ex2MDL <- lm(INDIV ~ AREA, data = Data)
summary(Ex2MDL)
library(car)
linearHypothesis(Ex2MDL,c(Ex2MDL$coefficients[1],Ex2MDL$coefficients[2]))


## 3

Ex3MDL <- lm(INDIV ~ AREA + I(AREA^2) + I(AREA^3), data = Data)

## 4

Ex4_2nd <- lm(INDIV ~ I(AREA^2), data = Data)
summary(Ex4_2nd)
par(mfrow = c(2,2))
plot(Ex4_2nd)
Ex4_3rd <- lm(INDIV ~ I(AREA^3), data = Data)
summary(Ex4_3rd)
plot(Ex4_3rd)

Ex4_1st_and_2nd <- lm(INDIV ~ AREA + I(AREA^2), data = Data)
summary(Ex4_1st_and_2nd)
plot(Ex4_1st_and_2nd)
Ex4_1st_and_3rd <- lm(INDIV ~ AREA +  I(AREA^3), data = Data)
summary(Ex4_1st_and_3rd)
plot(Ex4_1st_and_3rd)


summary(Ex3MDL)
plot(Ex3MDL)

## 5
AIC(Ex4_1st_and_2nd, Ex4_1st_and_3rd, Ex3MDL, Ex2MDL)

## Observation, the 2nd order model has the lowest AIC by a small margin, with the 3rd order model which excudes a second order variable in a close second.

linearHypothesis(Ex4_1st_and_2nd,Ex4_1st_and_2nd$coefficients)
linearHypothesis(Ex4_1st_and_3rd,Ex4_1st_and_3rd$coefficients)
linearHypothesis(Ex3MDL, Ex3MDL$coefficients)