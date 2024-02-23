## 1
library(FFTrees)
data("heartdisease")
Ex1_MDL <- FFTrees(diagnosis ~., data = heartdisease)
summary(Ex1_MDL)

## 2
plot(Ex1_MDL)


## 3
Ex3_MDL <- FFTrees(diagnosis ~., data = heartdisease,my.tree="If trestbps >180, predict TRUE.
                                                              If chol >300, predict TRUE.
                                                              If age <35, predict FALSE.
                                                              If thal = {rd,fd}
                                                              otherwise predict FALSE. ")

## 4
plot(Ex3_MDL)
#Observation: Ex3_MDL's confusion matrix suggests that it is less accurate than Ex1_MDL

## 5
plot(Ex1_MDL, what = "cues")

## 6
plot(Ex1_MDL, what = "tree")

## 7
plot(Ex1_MDL, tree="best.train")