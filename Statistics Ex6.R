## 1
data(attitude)

Ex1_MDL_all <- lm(rating ~. ,data = attitude)
Ex1_MDL_CPLA <- lm(rating ~ complaints + privileges + learning + advance ,data = attitude)
Ex1_MDL_CLA <- lm(rating ~ complaints + learning + advance ,data = attitude)

summary(Ex1_MDL_all)
summary(Ex1_MDL_CPLA)
summary(Ex1_MDL_CLA)

## 2
RSME <- function(lm.model){
  sqrt( sum( (lm.model$residuals)^2) / length(lm.model$residuals))
}

RSME(Ex1_MDL_all)
RSME(Ex1_MDL_CPLA)
RSME(Ex1_MDL_CLA)

## 3
MAE <- function(lm.model){
  sum(abs(lm.model$residuals))/ length(lm.model$residuals)
}

MAE(Ex1_MDL_all)
MAE(Ex1_MDL_CPLA)
MAE(Ex1_MDL_CLA)

## 4
RSMLE <- function(lm.model){
  sqrt(sum((log(lm.model$fitted.values + 1) - log(lm.model$model$rating + 1))^2)/length(lm.model$residuals))
}

RSMLE(Ex1_MDL_all)
RSMLE(Ex1_MDL_CPLA)
RSMLE(Ex1_MDL_CLA)

## 5
data(iris)

#Note: the Lloyd algorithm will produce different models with different seeds
set.seed(111)
Ex5MDL <- kmeans(iris[,c(1:4)],3, iter.max = 50,algorithm = "Lloyd")

## 6
iris$prediction <- as.factor(c("setosa","virginica","versicolor")[Ex5MDL$cluster])
table(iris$Species,iris$prediction)

## 7

accuracy <- function(real, prediction){
  sum(real == prediction)/length(real)
}

recall <- function(real, prediction){
  returnObj <- c()
  for(level in levels(real)){
    returnObj <- c(returnObj, (sum(real[prediction == level] == prediction[prediction == level])/(length(prediction[real == level])) ) )
  }
  return(data.frame(levels = levels(real), recall = returnObj))
}

precision <- function(real, prediction){
  returnObj <- c()
  for(level in levels(real)){
    returnObj <- c(returnObj, (sum(real[prediction == level] == prediction[prediction == level])/(length(prediction[prediction == level])) ) )
  }
  return(data.frame(levels = levels(real), precision = returnObj))
}

accuracy(iris$Species,iris$prediction)
recall(iris$Species,iris$prediction)
precision(iris$Species,iris$prediction)

## 8
F_measure <- function(real, prediction,param = 2){
  rec <- data.frame(recall(real,prediction))$recall
  prec <-data.frame(precision(real,prediction))$precision
  return(param * (rec * prec)/(rec + prec))
}

F_measure(iris$Species,iris$prediction)
F_measure(iris$Species,iris$prediction,param = .5)

## 9
 
purity <- function(real,prediction){
  cMatrix <- as.matrix(table(real,prediction))
  numer <- 0
  for(i in levels(real)){
    numer <- numer + max(cMatrix[,i])
  }
  return(numer / length(real))
}


## 10
library(clValid)
dunn(distance =dist(iris),clusters = Ex5MDL$cluster)



