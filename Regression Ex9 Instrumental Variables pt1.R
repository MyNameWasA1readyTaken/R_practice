## 1
library(AER)
data(PSID1976)

## 2
summary(PSID1976)

## 3

#As part of the exercise, the following line is not supposed to run
#Ex3MDL <- lm(log(wage) ~ education,data=PSID1976)
## ovservation: some values in wage = 0. since log(0) is undefined we may not use log(wage)

## 4
Ex4Data <- PSID1976[PSID1976$wage != 0,]


Ex4MDL <- lm(log(wage) ~ education,data=Ex4Data)

## 5
plot(x=Ex4Data$education,y=log(Ex4Data$wage))
abline(Ex4MDL)

## 6
Ex6MDL <- lm(education ~ feducation,data = Ex4Data)
# Observation 1: there is significant evidence to reject the null hypothesis that education and feducation are not correlated
# observation 2: education and feducation are positively correlated

## 7
Ex7MDL <- lm(log(wage) ~ fitted.values(Ex6MDL),data = Ex4Data)


## 8
Ex8MDL <- ivreg(log(wage) ~ education | feducation, data = Ex4Data)
# Observation: the beta values in Ex8MDL and Ex4MDL are markedly different

## 9
confint(Ex4MDL,"education", level = .90)
confint(Ex8MDL,"education", level = .90)

## 10
Ex10_IV_finder <-Ex6MDL <- lm(education ~ meducation,data = Ex4Data)
Ex10MDL <-Ex8MDL <- ivreg(log(wage) ~ education | meducation, data = Ex4Data)