## 1
setwd("/home/marcus/Documents/Rwork/R exercises/")
Data <- read.csv("moth-trap-experiment.csv")

library(dplyr)
library(viridis)
library(car)
library(pwr)


## 2
aggregate(Data[,c("location","number.of.moths")],list(Data$location),summary)

## 3
aggregate(Data[,c("type.of.lure","number.of.moths")],list(Data$type.of.lure),summary)

## 4
boxplot(number.of.moths~location+type.of.lure,data=Data,col=viridis(4),xlab="Lure",ylab="Number of Moths",names=rep(levels(factor(Data$type.of.lure)),each=4))
legend(x="topleft",legend = levels(factor(Data$location)),fill=viridis(4))


## 5
shapiro.test(Data$number.of.moths)


## 6
leveneTest(number.of.moths ~ location * type.of.lure,data = Data)


## 7
log.number.of.moths <- log(Data$number.of.moths)
shapiro.test(log.number.of.moths)
leveneTest(log.number.of.moths ~ location * type.of.lure,data = Data)

## 8
pwr.f2.test(u = 3, v = 4, f2=.3^2)

## 9
Ex9MDL <- aov(log.number.of.moths ~ location * type.of.lure, data = Data)

##10
plot(Ex9MDL,1)