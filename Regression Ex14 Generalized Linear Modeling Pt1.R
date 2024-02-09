## 1
library(car)
setwd("/home/marcus/Documents/Rwork/R exercises/")
Data <- read.csv("gotelli.csv")
Data$Habitat <- factor(Data$Habitat)
scatterplotMatrix(Data[,c("Srich","Habitat","Latitude","Elevation")])

## 2
Ex2MDL <- glm( Srich ~Habitat * Latitude * Elevation , data = Data,family = poisson)
vif(Ex2MDL)
# Observation: all factors have issues with colinearity

## 3
library(misty)
Data <- cbind(Data,center(Data[,c("Latitude","Elevation")]))

## 4
Ex4MDL <- glm( Srich ~ Habitat * Latitude.c * Elevation.c, data = Data, family = poisson)
vif(Ex4MDL)

## 5
plot(cooks.distance(Ex4MDL))
abline(h=1)
# Observation: there are no outliers among the data

## 6
library(performance)
check_overdispersion(Ex4MDL)

## 7
summary(Ex4MDL)
# Observation: it can be inferred from the model that: 1) forests have greater species richness than bogs, 2) species richness is negatively correlated with latitude

## 8
library(MuMIn)
Ex8test <- dredge(glm(Srich ~ Habitat * Latitude.c * Elevation.c, data = Data, family = poisson, na.action = na.fail))
summary(model.avg(Ex8test))

Ex8MDL <- glm( Srich ~Habitat + Latitude + Elevation , data = Data,family = poisson)

## 9
par(mfrow = c(2,2))
plot(Ex8MDL)
par(mfrow = c(1,1))

## 10
library(rgl)
options(rgl.printRglwidget = TRUE)
library(viridis)

colors <- c(viridis(2)[c(as.integer(Data$Habitat))])
plot3d(z = Data$Srich, y= Data$Latitude, x = Data$Elevation, col = colors , radius = 5)
legend3d("bottom", legend = c("Bog","Forest"), col = viridis(2), pch = 16, horiz = TRUE)

EIndex <- 0:60 *10
LIndex <- 410:450 *.1
indexMatrixBog <- expand.grid(Elevation = EIndex, Latitude = LIndex)
indexMatrixForest <- indexMatrixBog

indexMatrixBog$Habitat <- factor(x = rep("Bog",times = length(indexMatrixForest$Elevation)))
indexMatrixForest$Habitat <- factor(x = rep("Forest",times = length(indexMatrixForest$Elevation)))

zBog <- predict(Ex8MDL,indexMatrixBog)
zForest <- predict(Ex8MDL,indexMatrixForest)

surface3d(EIndex,LIndex,zBog, alpha = 0.4, front = "lines", back = "lines", col = viridis(1))
surface3d(EIndex,LIndex,zForest, alpha = 0.4, front = "lines", back = "lines", col = viridis(2)[2])
