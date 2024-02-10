## 1
setwd("/home/marcus/Documents/Rwork/R exercises/")
Data <- read.csv("RoadKills.csv")

plot(Data$D.PARK,Data$TOT.N)

## 2
Ex2MDL <- glm(TOT.N ~ D.PARK, data = Data, family = quasipoisson)
summary(Ex2MDL)

## 3

# removing data related to the index of the information
test <- cov(Data[,c(-1 * 1:3, -5)])


useNames <- c()
for(name in colnames(test)){
  if(sum(test[,c(name)]) >= 0){
    useNames <- c(useNames, name)
  }
}

Ex3MDL <- glm(reformulate(useNames,"TOT.N"), data = Data, family = quasipoisson)
summary(Ex3MDL)

## 4
library(car)
vif(Ex3MDL)

## 5
library(MuMIn)
Ex5MDL <- glm( reformulate(useNames,"TOT.N") , data = Data, family = poisson, na.action = na.fail )
vif(Ex5MDL)
Ex5test <- dredge(Ex5MDL)
summary(model.avg(Ex5test))

## 6
library(performance)
check_overdispersion(Ex3MDL)

par(mfrow = c(2,2))
plot(Ex3MDL)
par(mfrow = c(1,1))

## 7
Ex7MDL <- glm(reformulate(useNames[c(1:3,6)],"TOT.N"), data = Data, family = quasipoisson)
summary(Ex7MDL)

## 8
par(mfrow = c(2,2))
plot(Ex7MDL)
par(mfrow = c(1,1))