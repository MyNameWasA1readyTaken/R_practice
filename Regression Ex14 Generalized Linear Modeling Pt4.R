## 1
setwd("/home/marcus/Documents/Rwork/R exercises/")
Data <- read.csv("polis w9.csv")

plot(PA ~ RATIO, data = Data)
plot(y =Data$PA,  x = factor(Data$ISLAND))
boxplot(RATIO ~ PA, data = Data)

## 2
Ex2MDL <- glm(PA ~ RATIO, data= Data, family=binomial)

## 3

testDisp<- function(binom_model){
  ## function takes a glm with binomial family and prints to console results of test for overdispersion
  print(sum(residuals(binom_model, type = "deviance")^2)/binom_model$df.residual)
}

testDisp(Ex2MDL)

## 4
crPlots(Ex2MDL)

## 5
plot(cooks.distance(Ex2MDL))

## 6
plot(Ex2MDL, 4)
summary(Ex2MDL)

## 7
par(mfrow = c(2, 2))
plot(Ex2MDL)
par(mfrow = c(1, 1))

## 8
library(viridis)

prediction <- predict(Ex2MDL, data.frame( RATIO = (1:600 * .1) ),type = "response",se.fit = TRUE)

par(bg = "grey")
plot(1:600 * .1, prediction$fit, col= viridis(2)[2], type="l")

## 9

lines(1:600 * .1, prediction$fit - prediction$se, col= viridis(2)[2], type="l", lty = 4)
lines(1:600 * .1, prediction$fit + prediction$se, col= viridis(2)[2], type="l", lty = 4)
points(Ex2MDL$data$RATIO,Ex2MDL$data$PA, col = viridis(2)[1])
legend("bottom", legend = c("actual", "predicted"), col = viridis(2), pch = 16, horiz = TRUE)
par(bg = "white")


## 10

exp(Ex2MDL$coefficients[2])

## 11

log_one <- glm(PA ~ 1, data= Data, family=binomial)

R_Sq_estimate <- 1 - logLik(Ex2MDL)/logLik(log_one)
