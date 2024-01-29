## 1
library(MASS)
train <- rbind(Pima.tr,Pima.tr2)
test <- Pima.te
train$type <- as.integer(train$type == "Yes")
test$type <- as.integer(test$type == "Yes")

## 2
library(viridis)
pairs(type~.,data=train,col=viridis(2)[unclass(train$type+1)])

plot(jitter(type) ~ age, data=train)
predicted <- data.frame(age=20:75)
predicted$type <- predict(glm(type ~ age, family=binomial,data=train),predicted,type="response")
lines(type~age,data=predicted)


## 3
train$bmi[is.na(train$bmi)] <- median(train$bmi)
Ex3MDL <- glm(type ~ age + bmi,data=train,family="binomial")
summary(Ex3MDL)

## 4
# the probability for 35 year old with a bmi of 32 to have diabetes is 0.3470908
predict(Ex3MDL,data.frame(bmi=c(32),age=c(35)),type="response")

# the probability for 35 year old with a bmi of 22 to have diabetes is 0.1600962 
predict(Ex3MDL,data.frame(bmi=c(22),age=c(35)),type="response")

## 5
odds <- predict(Ex3MDL,data.frame(bmi=c(37),age=c(55)),type="response")/ (1-predict(Ex3MDL,data.frame(bmi=c(37),age=c(55)),type="response"))

## 6
library(caret)
Ex6prediction <- round(predict(Ex3MDL,train,type="response"))
confusionMatrix(as.factor(train$type),as.factor(Ex6prediction))

## 7
Ex7prediction <- round(predict(Ex3MDL,test,type="response"))
confusionMatrix(as.factor(test$type),as.factor(Ex7prediction))


## 8
library(ROCR)
Ex8prediction <- predict(Ex3MDL,test)
Ex8MDLPred <- prediction(Ex8prediction,test$type)

#draws ROC curve
plot(performance(Ex8MDLPred,"tpr","fpr"))
Ex8AUC <- performance(Ex8MDLPred,"auc")

#finds AUC
Ex8AUC@y.values

## 9
Ex9MDL <- glm(type ~ age + bmi + npreg + I(age^2),data=train,family="binomial")

Ex9prediction <- predict(Ex9MDL,test)
Ex9MDLPred <- prediction(Ex9prediction,test$type)

#draws ROC curve
plot(performance(Ex9MDLPred,"tpr","fpr"))
Ex9AUC <- performance(Ex9MDLPred,"auc")

#finds AUC
Ex9AUC@y.values

predict(Ex9MDL,data.frame(age=c(35),bmi=c(35),npreg=c(2)),type="response")
predict(Ex9MDL,data.frame(age=c(35),bmi=c(25),npreg=c(2)),type="response")

#Observation, according to the model, a 35 year old mother of two increases her probability of having diabeties from 25.94182% to  47.51591% when increasing her bmi to 35 from 25 

##to find the marginal effect of bmi, for the woman in question at bmi = 25, we may use the following functions
## P(diabeties) = 1/(1+e^(-.095 * bmi + beta0 + beta1+...))
## d/d(bmi) P(diabeties) = 1/(1+e^-(.095 * bmi + beta0 + beta1+...)) d/d(bmi)
## d/d(bmi) P(diabeties) = 1/(1+e^-(.095 * bmi -8.804622434 + 0.234143632 * 35 -0.002355526  * 35^2 +0.036123777 * 2)) d/d(bmi)
## d/d(bmi) P(diabeties) = 1/(1+e^-(.095 * bmi -8.804622434 + 0.234143632 * 35 -0.002355526  * 35^2 +0.036123777 * 2)) d/d(bmi)
## P'(diabeties) = (0.0949549 e^(3.42287 - 0.0949549 * bmi))/(1 + e^(3.42287 - 0.0949549 * bmi))^2
##thus, when bmi = 25
##P'(diabeties| bmi = 25) = 0.01824275
