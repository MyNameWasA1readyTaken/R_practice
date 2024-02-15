## 1
setwd("/home/marcus/Documents/Rwork/R exercises/")
Data <- read.csv("lenses.csv")

## 2
colnames(Data) <- c("index","age","spec_pres","astigmatic","tpr","class")

## 3
Data$age <- c("young","pre-presbyopic","presbyopic")[Data$age]
Data$spec_pres <- c("myope","hypermetrope")[Data$spec_pres]

Data$class <- c("patient needs hard contact Data","patient needs soft contact Data","patient does not need contact Data")[Data$class]

## 4
str(Data)

## 5
Data$astigmatic <- c("no","yes")[(as.integer(Data$astigmatic))]

## 6
Data$tpr <- c("reduced","normal")[Data$tpr]

## 7
# Note: this step is unneeded due to a more eficient method being used in #6

## 8
table(Data)

## 9
Data <- Data[-c(10),]

## 10
Data <- Data[,-c(1)]

## Exporting data

write.csv(Data,"for_Ex7_pt2.csv",row.names = FALSE)