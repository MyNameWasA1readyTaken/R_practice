## 1
library(lars)
data(diabetes)

par(mfrow=c(3,4))
for(name in colnames(diabetes$x)){
  plot(diabetes$y ~ diabetes$x[,name],xlab=name)
  abline(lm(diabetes$y ~ diabetes$x[,name]))
}
par(mfrow=c(1,1))

Ex1MDL <- lm(diabetes$y ~ diabetes$x)

## 2
library(glmnet)
Ex2MDL <- glmnet(diabetes$x,diabetes$y,alpha=0,lambda=10^(5:-5))
plot(Ex2MDL,xvar = "lambda")

## 3
Ex3Data <- cv.glmnet(diabetes$x,diabetes$y,alpha=0)
plot(Ex3Data)
Ex3minLanbda <- Ex3Data$lambda.min

## 4
Ex4MDL <- glmnet(diabetes$x,diabetes$y,alpha=0,lambda=Ex3minLanbda)
Ex4MDL$beta

## 5
Ex5MDL <- glmnet(diabetes$x,diabetes$y,alpha=0,lambda=Ex3Data$lambda.1se)
Ex5MDL$beta

## 6
set.seed(112233)
Ex6Data <- data.frame(cbind(diabetes$x,diabetes$y))
Ex6Data$test <- sample(c(0,1),length(Ex6Data[,1]),replace=TRUE,prob=c(.8,.2))

testTMP <- Ex6Data[Ex6Data$test == 1,]
trainTMP <- Ex6Data[Ex6Data$test != 1,]

test <- data.frame(y = testTMP$V11, x = I(as.matrix(testTMP[,colnames(diabetes$x)])))
train <- data.frame(y = trainTMP$V11, x = I(as.matrix(trainTMP[,colnames(diabetes$x)])))

## 7
Ex7Data <- cv.glmnet(train$x,train$y,alpha=0)
Ex7minLambdaMDL <- glmnet(train$x,train$y,alpha=0,lambda = Ex7Data$lambda.min)
Ex71seLambdaMDL <-glmnet(train$x,train$y,alpha=0,lambda = Ex7Data$lambda.1se)

## 8
Ex8_min_Lambda_prediction <- predict.glmnet(Ex7minLambdaMDL,test$x) 
Ex8_1se_Lambda_prediction <- predict.glmnet(Ex71seLambdaMDL,test$x)

Ex8_se_min_lambda <- sd(test$y - Ex8_min_Lambda_prediction)/sqrt(length(test$y))
Ex8_se_1se_lambda <- sd(test$y - Ex8_1se_Lambda_prediction)/sqrt(length(test$y))

## 9
Ex9MDL <- lm(train$y ~ train$x)

## 10
Ex10_lSq_prediction <- predict(Ex9MDL,test$x)
Ex10_se_lSq <- sd(test$y - Ex10_lSq_prediction)/sqrt(length(test$y))