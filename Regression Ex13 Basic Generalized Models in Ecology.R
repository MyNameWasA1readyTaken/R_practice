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

Data <- Data[,-6]

createTestSet <- function(i,limit=TRUE){
  i2 <- (i*(!limit) - 1) + limit * 5
  returnV <- c()
  for(number in 1:i2){
    returnV <- c(returnV, combn(2:i,number,simplify=FALSE))
  }
  return(returnV)
}

toTest <- createTestSet(length(Data[,c(-1)]), limit=FALSE)

aics <- c()
for(vars in toTest){
  print(vars)
  print(colnames(as.data.frame(Data[,c(vars)])))
  print(reformulate( paste("s(",colnames(Data)[vars],")" ), "Data$SR"))
  aics <- c(aics, AIC(gam(reformulate( paste("s(",colnames(as.data.frame(Data[,c(vars)])),")" ), "Data$SR"), data = Data)))
}

MinAICModel <- gam(reformulate( paste("s(",colnames(as.data.frame(Data[,toTest[which.min(aics)][[1]]])), ")"), "Data$SR"), data = Data)