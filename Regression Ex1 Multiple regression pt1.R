## 1
stateData <- data.frame(state.x77)

## 2
Ex2MDL <- lm(Life.Exp ~.,data = stateData)

## 3
Ex3MDL <- update(Ex2MDL, Life.Exp ~. -Income -illiteracy -Area)

## 4
Ex4MDL <- lm(Life.Exp ~ HS.Grad + Murder, data=stateData)

## 5
Ex5MDL1 <- lm(Life.Exp ~ HS.Grad + Murder + HS.Grad * Murder, data=stateData)
Ex5MDL2 <- lm(Life.Exp ~ HS.Grad + Murder + (HS.Grad * Murder)^2, data=stateData)
Ex5MDL3 <- lm(Life.Exp ~ HS.Grad + Murder + (HS.Grad * Murder)^3, data=stateData)

## 6
CI.HS.Grad <- confint(Ex4MDL,"HS.Grad")
CI.Murder <- confint(Ex4MDL,"Murder")

## 7
Ex7 <- predict(Ex4MDL,list(HS.Grad=c(55),Murder=c(8)))

## 8
Ex8 <- predict(Ex4MDL,list(HS.Grad=c(55),Murder=c(8)), interval = "confidence", level = .98 )

## 9
Ex9 <- predict(Ex4MDL,list(HS.Grad=c(55),Murder=c(8)), interval = "prediction", level = .98 )

##  10
library(rgl)

open3d()

plot3d(Ex4MDL,plane.col="blue")
rglwidget()