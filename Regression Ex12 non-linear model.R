## 1
setwd("/home/marcus/Documents/Rwork/R exercises/")
Data <- read.csv("mussel.csv")
Ex1MDL <- nls(SPECIES ~ a * AREA^ b, data = Data, start = list(a=.1,b=1))

## 2
plot(y = fitted(Ex1MDL), x = resid(Ex1MDL))

## 3

Ex3_power_function <- function(x, a, b){
  a * x ^ b
}

Ex3_init_function <- function(mCall, data, LHS, ...){
  xy <- sortedXyData(mCall[["x"]], LHS, data)
  coefs <- coef(lm(log(y) ~ log(x), xy))
  pars <- c( exp(coefs[[1]]), coefs[[2]])
  setNames(pars,mCall[c("a", "b")])
}

Ex3_selfstart_function_power <- selfStart(Ex3_power_function, initial = Ex3_init_function, parameters = c("a","b"))


Ex3_power_MDL <- nls(SPECIES ~ Ex3_selfstart_function_power(AREA,a,b), data = Data)
summary(Ex3_power_MDL)

## 4
Ex4_asymp_MDL <- nls(SPECIES ~ SSasymp(AREA,a,b,c), data = Data)
summary(Ex4_asymp_MDL)

## 5
AIC(Ex3_power_MDL,Ex4_asymp_MDL)
## observation: Due to the lower AIC, the power model seems to be more parsimonious

## 6
plot(y = fitted(Ex4_asymp_MDL), x = resid(Ex4_asymp_MDL))

## 7

library(viridis)
plot(x = Data$AREA, y = Data$SPECIES, xlab = "Area", ylab = "Species")
area_index <- 1:2500 * 10

points(area_index,predict(Ex3_power_MDL,data.frame(AREA = area_index)), type = "l", col =viridis(2)[1], lwd = 3)
points(area_index,predict(Ex4_asymp_MDL,data.frame(AREA = area_index)), type = "l", col =viridis(2)[2], lwd = 3)
legend(x="bottomright",legend = c("power model","asymp model"),fill=viridis(2))

