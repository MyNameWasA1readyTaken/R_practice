setwd("/home/marcus/Documents/Rwork/R exercises/")
Data <- read.csv("moth-trap-experiment.csv")

library(dplyr)
library(viridis)
library(car)
library(pwr)

aggregate(Data[,c("location","number.of.moths")],list(Data$location),summary)

aggregate(Data[,c("type.of.lure","number.of.moths")],list(Data$type.of.lure),summary)

boxplot(number.of.moths~location+type.of.lure,data=Data,col=viridis(4),xlab="Lure",ylab="Number of Moths",names=rep(levels(factor(Data$type.of.lure)),each=4))
legend(x="topleft",legend = levels(factor(Data$location)),fill=viridis(4))

shapiro.test(Data$number.of.moths)

leveneTest(number.of.moths ~ location * type.of.lure,data = Data)

log.number.of.moths <- log(Data$number.of.moths)
shapiro.test(log.number.of.moths)
leveneTest(log.number.of.moths ~ location * type.of.lure,data = Data)

pwr.f2.test(u = 3, v = 4, f2=.3^2)

Model <- aov(log.number.of.moths ~ location * type.of.lure, data = Data)

plot(Model)