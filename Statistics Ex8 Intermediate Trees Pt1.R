## 1
setwd("/home/marcus/Documents/Rwork/R exercises/")
Data <- read.csv("adult.csv")
str(Data)

## 2
colnames(Data) <- c("age","workclass","fnlwgt","education","education-num","marital-status","occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country","class")

## 3
table(Data$class)

## 4
Data$class <- Data$class == " >50K"

## 5
cor(Data[,c("age","fnlwgt","education-num","capital-gain","capital-loss","hours-per-week","class")])

## 6
set.seed(1000)

Data$test <- sample(c(rep(TRUE,ceiling(length(Data[,1])*.3)),rep(FALSE,floor(length(Data[,1])*.7))))
Train <- Data[!(Data$test),]
Test <- Data[(Data$test),]

## 7
nrow(Test)
nrow(Train)

## 8
library(rpart)
library(rpart.plot)

## 9
dec <- rpart(class ~.,data=Data)

## 10
prp(dec)