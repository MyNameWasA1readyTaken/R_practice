## 1
setwd("/home/marcus/Documents/Rwork/R exercises/")
Data <- read.csv("RoadKills.csv")
## NOTE: this exercise only concerns itself with 9 explanatory variables: TOT.N, OPEN.L, MONT.S, POLIC, SHRUB, WAT.RES, L.WAT.C, L.P.ROAD, D.WAT.COUR, D.PARK
##       thus, we shall prune the other variables from the dataset

Data <- Data[,c("TOT.N","OPEN.L", "MONT.S", "POLIC", "SHRUB", "WAT.RES", "L.WAT.C", "L.P.ROAD", "D.WAT.COUR", "D.PARK")]

plot(Data$D.PARK,Data$TOT.N)

## 2
Ex2MDL <- glm(TOT.N ~ D.PARK, data = Data, family = quasipoisson)
summary(Ex2MDL)

## 3

# removing data related to the index of the information

Ex3MDL <- glm(TOT.N ~. , data = Data, family = quasipoisson)
summary(Ex3MDL)

## 4
library(car)
vif(Ex3MDL)

## 5
library(MuMIn)
Ex5MDL <- glm( TOT.N ~. , data = Data, family = poisson, na.action = na.fail )
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
Ex7MDL <- glm(TOT.N ~ MONT.S + SHRUB + L.WAT.C+ D.PARK, data = Data, family = quasipoisson)
summary(Ex7MDL)

## 8
par(mfrow = c(2,2))
plot(Ex7MDL)
par(mfrow = c(1,1))

## Part 3 Starts Here ##

## 9
library(MASS)
Ex9MDL <- glm.nb(TOT.N ~. , data = Data)

## 10
summary(Ex9MDL)

## 11

Ex11MDL <- glm.nb( TOT.N ~. , data = Data, na.action = na.fail )
vif(Ex11MDL)
Ex11test <- dredge(Ex11MDL)
summary(model.avg(Ex11test))

## 12
Ex12MDL <- glm.nb(TOT.N ~ OPEN.L + SHRUB + L.WAT.C, data = Data)

## 13

par(mfrow = c(2,2))
plot(Ex12MDL)
par(mfrow = c(1,1))

