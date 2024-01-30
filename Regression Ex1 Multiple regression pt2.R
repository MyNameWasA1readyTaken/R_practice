## 1
stateData <- data.frame(state.x77)
cMatrix <- cor(stateData)
plot(stateData[,c("Life.Exp","HS.Grad","Murder","Frost")],pch=20, cex=1.5,col="Blue")

## 2
Ex2MDL <- lm(Life.Exp ~ HS.Grad + Murder,data=stateData)
Ex2Residuals <- Ex2MDL$residuals
Ex2FitValues <- Ex2MDL$fitted.values

## 3
plot(Ex2FitValues, Ex2Residuals, ylab="Residuals",xlab="Fitted Values")
plot(Ex2MDL,1)

## 4
plot(main="HS Grad vs residuals",x=stateData$HS.Grad,y=Ex2Residuals)
plot(main="Murder vs residuals",x=stateData$Murder,y=Ex2Residuals)

## 5
qqnorm(Ex2Residuals)
qqline(Ex2Residuals)
plot(Ex2MDL,2)

## 6
library(MASS)
studentized <- data.frame(studres(Ex2MDL))

## 7
leverages <- data.frame(hatvalues(Ex2MDL))
plot(1:50, order(leverages$hatvalues.Ex2MDL))
influentialObs <- data.frame(leverages[leverages$hatvalues.Ex2MDL. > 2* (length(Ex2MDL$coefficients)-1)/length(leverages$hatvalues.Ex2MDL.),])


## 8
dffits <- dffits(Ex2MDL)
convThresh <- 2*sqrt((length(Ex2MDL$coefficients)-1)/length(leverages$hatvalues.Ex2MDL.))
infOBSDFIT <- dffits[dffits>convThresh]
dfBeta <- dfbetas(Ex2MDL)
convThreshBetas <-2/sqrt(length(leverages$hatvalues.Ex2MDL))
infOBSBeta <- dfBeta[dfBeta > convThreshBetas]

## 9
cookD <- cooks.distance(Ex2MDL)
plot(cookD)
ThresholdCook <- 4/length(cookD)
abline(h=4/length(cookD)) 
plot(Ex2MDL,4)

## 10 
library(car)
influencePlot(Ex2MDL)