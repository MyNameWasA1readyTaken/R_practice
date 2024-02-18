## 1
library(rpart)
library(rpart.plot)
data("kyphosis")
Ex1_MDL <- rpart(Kyphosis ~., data= kyphosis, method = "class")


## 2
# part 1
prp(Ex1_MDL)
# Observation: start and age are used to describe kyphosis

# part 2
# Observation: four observations contain terminal nodes


## 3
# part 1
Ex3_MDL <- rpart(Kyphosis ~., data= kyphosis[1:60,], method = "class")

# part 2
Ex3_predictions <- predict(Ex3_MDL,kyphosis[61:81,],type="class")

# part 3
Ex3_misclass <- mean(Ex3_predictions != kyphosis[61:81,]$Kyphosis)


## 4
# part 1
data("iris")
Ex4_MDL <- rpart(Species ~., data=iris,method="class")

# part 2
prp(Ex4_MDL,type=1,extra=1)


## 5
Ex5_prunedTree <- prune(Ex4_MDL,cp=median(Ex4_MDL$cptable[,1]))
par(mfrow=c(1,2))
prp(Ex4_MDL,type=1,extra=1)
prp(Ex5_prunedTree,type=1,extra=1)
par(mfrow=c(1,1))


## 6
# part 1
Ex3_MDL$where

# part 2
predict(Ex4_MDL,data.frame(Petal.Length = c(2.45), Petal.Width=c(1.75),Sepal.Length = c(1),Sepal.Width=c(1) ),type="class")


## 7
# part 1
data("car90")
Ex7_MDL <- rpart(Price ~., data = car90)

# part 2
prp(Ex7_MDL,type=1,extra=1)


## 8
# part 1
# Observation: Rim, Tires, Tank, HP, and Height are used to explain price

# part 2
# Observation: the terminal nodes which have a mean Price less than mean(car90$price) (which is 15805.22), are nodes 11,13, and 7 (by their placement in Ex7_MDL$frame)

## 9
# part 1
data("car.test.frame")
Ex9_MDL <- rpart(Mileage ~., data= car.test.frame)

# part 2
Ex9_snip2 <- snip.rpart(Ex9_MDL,2)

# part 3
par(mfrow=c(1,2))
prp(Ex9_MDL,type=1,extra=1)
prp(Ex9_snip2,type=1,extra=1)
par(mfrow=c(1,1))


## 10
Ex9_depth <- max(rpart:::tree.depth(as.numeric(rownames(Ex9_MDL$frame))))

Ex10_MDL <- rpart(Mileage ~., data= car.test.frame,maxdepth = 2)
prp(Ex10_MDL)
