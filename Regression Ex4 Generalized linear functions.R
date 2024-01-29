## Ex 1

# 1
library(titanic)
DATA <- titanic_train[,-c(1,4,9,11)]
lm_reg <- lm(Survived~ Age + Fare,data=DATA)

# 2
glm_reg <- glm(Survived~ Age + Fare,data=DATA)

## Ex 2

summary(lm_reg)
summary(glm_reg)

## Ex 3

bin_model <- glm(Survived~ Age + Fare,data=DATA,family=binomial)

## Ex 4

# 1
#Observation: the default link associated to the binomial family is logit

#2
bin_probit_model <- glm(Survived~ Age + Fare,data=DATA,family=binomial(link="probit"))

## Ex 5
bin_model_NoIntercept <- glm(Survived~ 0 + Age + Fare,data=DATA,family=binomial)

## Ex 6
#1
DATA$Age[is.na(DATA$Age)] <-median(na.omit(DATA$Age))

#2
glm_model <- glm(Survived~ 0 + Age + Fare,data=DATA,family=binomial,na.action='na.fail')

## Ex 7
glm_model<- glm(Survived~ 0 + Age + poly(Fare,2),data=DATA,family=binomial,na.action='na.fail')

## Ex 8

DATA$Sex <- as.factor(DATA$Sex)
glm_model<- glm(Survived~ 0 + Age + poly(Fare,2) + Sex,data=DATA,family=binomial,na.action='na.fail')

## Ex 9

Pred.default <- predict(glm_model,DATA)

## Ex 10

Pred.specified <- predict(glm_model,DATA,type="response")

#Extra
DATA$prediction <- round(Pred.specified)

sum(DATA$prediction == DATA$Survived) /length(DATA$prediction)
## Observation: the model is 78.22671% accurate