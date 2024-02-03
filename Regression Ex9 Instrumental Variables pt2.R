## 1
library(AER)
data(PSID1976)
Ex1Data <- PSID1976[PSID1976$wage != 0,]

## 2
Ex2MDL <- lm(log(wage) ~ education + experience + I(experience^2),data = Ex1Data)

## 3
Ex3MDL <- lm(education ~ experience + I(experience^2) + feducation + meducation,data = Ex1Data)
## Observation the effects of feducation and meducation are significant, but experience isn't

## 4

linearHypothesis(Ex3MDL,c("meducation = 0", "feducation = 0"))

## 5

Ex5MDL <- lm(log(wage) ~ experience + I(experience^2) + fitted.values(Ex3MDL),data=Ex1Data)

## 6
Ex6MDL <- ivreg(log(wage) ~ education + experience + I(experience^2) | experience + I(experience^2) + feducation + meducation,data = Ex1Data)

## 7
Ex7MDL <- ivreg(log(wage) ~ education + experience + I(experience^2) | .-education + feducation + meducation,data = Ex1Data)


## 8
Ex8MDL <- lm(log(wage) ~ education + experience + I(experience^2) + Ex3MDL$residuals, data= Ex1Data)

## 9
Ex9MDL <- lm(Ex7MDL$residuals ~ experience + I(experience^2) + feducation + meducation, data = Ex1Data)
sargan_test <- 1 - pchisq(summary(Ex9MDL)$r.squared *nrow(Ex1Data),1)

## 10

# for 9
summary(object = Ex7MDL, diagnostics = TRUE)
