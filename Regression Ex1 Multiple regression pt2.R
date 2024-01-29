stateData <- data.frame(state.x77)
cMatrix <- cor(stateData)
plot(stateData[,c("Life.Exp","HS.Grad","Murder","Frost")],pch=20, cex=1.5,col="Blue")

Ex2MDL <- lm(Life.Exp ~ HS.Grad + Murder,data=stateData)
Ex2Residuals <- Ex2MDL$residuals
Ex2FitValues <- Ex2MDL$fitted.values

plot(Ex2FitValues, Ex2Residuals, ylab="Residuals",xlab="Fitted Values")
plot(Ex2MDL,1)

plot(main="HS Grad vs residuals",x=stateData$HS.Grad,y=Ex2Residuals)
plot(main="Murder vs residuals",x=stateData$Murder,y=Ex2Residuals)

qqnorm(Ex2Residuals)
qqline(Ex2Residuals)
plot(Ex2MDL,2)

library(MASS)
studentized <- data.frame(studres(Ex2MDL))

leverages <- data.frame(hatvalues(Ex2MDL))
plot(1:50, order(leverages$hatvalues.Ex2MDL))
influentialObs <- data.frame(leverages[leverages$hatvalues.Ex2MDL. > 2* (length(Ex2MDL$coefficients)-1)/length(leverages$hatvalues.Ex2MDL.),])

dffits <- dffits(Ex2MDL)
convThresh <- 2*sqrt((length(Ex2MDL$coefficients)-1)/length(leverages$hatvalues.Ex2MDL.))
infOBSDFIT <- dffits[dffits>convThresh]
dfBeta <- dfbetas(Ex2MDL)
convThreshBetas <-2/sqrt(length(leverages$hatvalues.Ex2MDL))
infOBSBeta <- dfBeta[dfBeta > convThreshBetas]

cookD <- cooks.distance(Ex2MDL)
plot(cookD)
ThresholdCook <- 4/length(cookD)
abline(h=4/length(cookD)) 
plot(Ex2MDL,4)

library(car)
influencePlot(Ex2MDL)