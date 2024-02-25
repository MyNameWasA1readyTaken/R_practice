## 1
data(iris)
set.seed(1)
Ex1_MDL <- kmeans(iris[,c("Sepal.Length","Sepal.Width")],3)

## 2
table(Ex1_MDL$cluster)

## 3
library(viridis)
table(c("versicolor","virginica","setosa")[Ex1_MDL$cluster],iris$Species)
plot(iris$Sepal.Length,iris$Sepal.Width,col=viridis(3)[Ex1_MDL$cluster],pch=as.numeric(iris$Species))
legend("topright",c("predicted as: versicolor","predicted as: virginica","predicted as: setosa","Actual: setosa","Actual: versicolor","Actual: virginica"),pch=c(15,15,15,1,2,3),col=c(viridis(3),"Black","Black","Black"))

## 4
Ex4_MDL <- kmeans(iris[,c("Petal.Length","Petal.Width")],3)
table(c("setosa","virginica","versicolor")[Ex4_MDL$cluster],iris$Species)
plot(iris$Petal.Length,iris$Petal.Width,col=viridis(3)[Ex4_MDL$cluster],pch=as.numeric(iris$Species))
legend("bottomright",c("predicted as: setosa","predicted as: virginica","predicted as: versicolor","Actual: setosa","Actual: versicolor","Actual: virginica"),pch=c(15,15,15,1,2,3),col=c(viridis(3),"Black","Black","Black"))

## 5
Ex5_data <- iris
Ex5_data$Petal.Width <- Ex5_data$Petal.Width * 2
Ex5_data$Petal.Length <- Ex5_data$Petal.Length * 2

Ex5_MDL <- kmeans(Ex5_data[,c("Petal.Length","Petal.Width")],3)
table(c("setosa","virginica","versicolor")[Ex5_MDL$cluster],iris$Species)

## 6
Ex6_data <- scale(Ex5_data[,-5])

Ex6_MDL <- kmeans(Ex6_data[,c("Petal.Length","Petal.Width")],3)
table(c("setosa","virginica","versicolor")[Ex6_MDL$cluster],iris$Species)

## 7
## dataset for further questions require registration with 3rd party website.