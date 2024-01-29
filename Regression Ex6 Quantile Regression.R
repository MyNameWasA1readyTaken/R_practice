## 1
library(quantreg)
data(barro)
summary(barro)

## 2
X <- as.matrix(barro[,-1])
Y <- barro$y.net

## 3
Ex3MDL <- lm(Y ~ X)

## 4
Ex4rqMedian <- rq(Y ~ X)
summary(Ex4rqMedian)

## 5
Ex5rqQ3 <- rq(Y ~ X, tau = .75)
summary(Ex5rqQ3)

Ex5rqQ1 <- rq(Y ~ X, tau = .25)
summary(Ex5rqQ1)

## 6
Ex6 <- rq(Y ~ X, tau = .1 * 1:10)
summary(Ex6)

## 7
Ex7MDL <- rq.fit.lasso(X,Y,lambda=0.5)

## 8
Ex8 <- .05*1:19

##9
Ex9rq <- rq(Y ~ X, tau=Ex8)
plot(summary(Ex9rq))

##
anova(Ex5rqQ3,Ex5rqQ1)