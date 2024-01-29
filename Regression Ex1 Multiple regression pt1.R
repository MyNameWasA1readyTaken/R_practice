stateData <- data.frame(state.x77)

Ex2MDL <- lm(Life.Exp ~.,data = stateData)

Ex3MDL <- update(Ex2MDL, Life.Exp ~. -Income -illiteracy -Area)

Ex4MDL <- lm(Life.Exp ~ HS.Grad + Murder, data=stateData)

Ex5MDL1 <- lm(Life.Exp ~ HS.Grad + Murder + HS.Grad * Murder, data=stateData)
Ex5MDL2 <- lm(Life.Exp ~ HS.Grad + Murder + (HS.Grad * Murder)^2, data=stateData)
Ex5MDL3 <- lm(Life.Exp ~ HS.Grad + Murder + (HS.Grad * Murder)^3, data=stateData)

CI.HS.Grad <- confint(Ex4MDL,"HS.Grad")
CI.Murder <- confint(Ex4MDL,"Murder")

Ex7 <- predict(Ex4MDL,list(HS.Grad=c(55),Murder=c(8)))

Ex8 <- predict(Ex4MDL,list(HS.Grad=c(55),Murder=c(8)), interval = "confidence", level = .98 )

Ex9 <- predict(Ex4MDL,list(HS.Grad=c(55),Murder=c(8)), interval = "prediction", level = .98 )

library(rgl)

open3d()

plot3d(Ex4MDL,plane.col="blue")
rglwidget()