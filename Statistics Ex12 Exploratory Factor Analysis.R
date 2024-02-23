## 1
library("psych")
library("GPArotation")

setwd("/home/marcus/Documents/Rwork/R exercises/")
Data <- read.csv("efa.csv")

describe(Data)

## 2
fa.parallel(Data)
# Observation: fa.paralel suggests that there are 5 factors

## 3
vss(Data)
# Observation: vss suggests that either 3 or 2 factors are optimal

## 4
for(i in 1:26){
  print(shapiro.test(Data[,i]))
}
# Observation: the columns are not normally distributed, thus minres would be better in this case

## 5
Ex5_Obs <- fa(Data,nfactors=5)
print(Ex5_Obs$loadings,cutoff = .3)

## 6
plot(Ex5_Obs$loadings)

## 7
fa.diagram(Ex5_Obs)

## 8
Ex8_Obs <- omega(Data,nfactors=5,sl=FALSE)

## 9
Ex9_Obs <- omega(Data,nfactors=5)

## 10
iclust(Data)