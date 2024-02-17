##### Part 1 #####
## 1
setwd("/home/marcus/Documents/Rwork/R exercises/")
Data <- read.csv("adult.csv")
str(Data)

## 2
colnames(Data) <- c("age","workclass","fnlwgt","education","education-num","marital-status","occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country","class")

## 3
table(Data$class)

## 4
Data$class <- as.integer(Data$class == " >50K")

## 5
cor(Data[,c("age","fnlwgt","education-num","capital-gain","capital-loss","hours-per-week","class")])

## 6
set.seed(1000)

Data$test <- sample(c(rep(TRUE,ceiling(length(Data[,1])*.3)),rep(FALSE,floor(length(Data[,1])*.7))))
Train <- Data[!(Data$test),-c(16)]
Test <- Data[(Data$test),-c(16)]

## 7
nrow(Test)
nrow(Train)

## 8
library(rpart)
library(rpart.plot)

## 9
dec <- rpart(class ~.,data=Train, method="class")

## 10
prp(dec)

##### Part 2 #####
## 1
pred_dec <- predict(dec,Test,type="class")

## 2
Ex2_Tbl <- table(pred_dec,Test$class)
Ex2_Tbl

## 3
Ex3_Accuracy <- sum(diag(Ex2_Tbl))/length(Test[,1])
Ex3_Accuracy

## 4
mean(pred_dec != Test$class)

## 5
base <- rep(1,length(Test$class))
table(base,Test$class)

## 6
Ex6_dif <- abs(mean(pred_dec != Test$class) - mean(base != Test$class))
Ex6_dif

## 7
pred_dec_reg <-  predict(dec,Test,type="prob")

## 8
library(ROCR)
ROCRpred1=prediction(pred_dec_reg[,2],Test$class)
ROCRperf1=performance(ROCRpred1,"tpr","fpr")

plot(ROCRperf1)

## 9
plot(ROCRperf1,colorize=TRUE)

## 10

# Observation: the model is well above the diagnal at all times