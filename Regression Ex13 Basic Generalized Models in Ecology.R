## 1
setwd("/home/marcus/Documents/Rwork/R exercises/")
Data <- read.csv("Vegetation2.csv")
library(mgcv)

## 2
plot( x = Data$ROCK, y = Data$SR, ylab = "species richness")

## 3
Ex3MDL <- gam(SR ~ s(ROCK),data = Data)

## 4
summary(Ex3MDL)

## observation: It seems as if  the mode fit is poor given a relatively poor R squared value

## 5
plot(Ex3MDL)
par(mfrow = c(2,2))
gam.check(Ex3MDL)
par(mfrow = c(1,1))

## 6
plot( x = Data$ROCK, y = Data$SR, ylab = "species richness")

## 7 & 8
library(viridis)
index <- 0:600 * .1
points(index,predict(Ex3MDL,data.frame(ROCK = index)), type = "l", col =viridis(1)[1], lwd = 3)


