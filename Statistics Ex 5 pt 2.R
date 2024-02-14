# Statistics section
# Exercise 5: Model evauation part 2

## problem 1

library(ROCR)
library(caTools)
library(caret)
data("GermanCredit")
df1=GermanCredit
df1$Class=ifelse(df1$Class=="Bad",1,0)
set.seed(100)
spl=sample.split(df1$Class,SplitRatio = 0.7)
Train1=df1[spl==TRUE,]
Test1=df1[spl==FALSE,]
model1=glm(Class~.,data=Train1,family = binomial)
pred1=predict(model1,Test1)
table(Test1$Class,pred1>0.5)

## problem 2
# solution generalized to work with any seed
Test1$pred <- pred1

nrow(Test1[Test1$Class == (Test1$pred >.5),]) / nrow(Test1)

## problem 3

ROCRpred1=prediction(pred1,Test1$Class)
ROCRperf1=performance(ROCRpred1,"tpr","fpr")

plot(ROCRperf1)
#observation: the rate of true positives is higher than the rate of false positives.

## problem 4

summary(model1)

## problem 5
auc= performance(ROCRpred1,measure="auc")
auc=auc@y.values[[1]]

## problem 6

model2 <- glm(Class~ OtherInstallmentPlans.Bank + EmploymentDuration.4.to.7 + SavingsAccountBonds.lt.100 + CreditHistory.PaidDuly + CreditHistory.ThisBank.AllPaid + CreditHistory.NoCredit.AllPaid + CheckingAccountStatus.gt.200 + CheckingAccountStatus.0.to.200 + CheckingAccountStatus.lt.0 + InstallmentRatePercentage + Duration,data=Train1,family = binomial)

## problem 7

pred2 <- predict(model2,Test1)

## problem 8
Test1$pred <- pred2

table(Test1$Class,pred2>0.5)
nrow(Test1[Test1$Class == (Test1$pred >.5),]) / nrow(Test1)

## problem 9
ROCRpred2=prediction(pred2,Test1$Class)
ROCRperf2=performance(ROCRpred2,"tpr","fpr")
auc2= performance(ROCRpred2,measure="auc")
auc2=auc2@y.values[[1]]
#Observation: the AUC of model2 is 0.73806878...

## problem 10
## Observation: the AUC of model2 is lower than that of model1