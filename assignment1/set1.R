rm(list = ls())
setwd("Desktop/courses_term1/econometrics/problem_sets/assignment1/")
library(sandwich)
library(ggplot2)
library(lmtest)
library(zoo)
library(corrplot)
library(car)
library(ggfortify)


cp = read.csv('./data_problemset1/Car_Prices.csv')
cp$gas_die = cp$Gas_1..Diesel_0

# qa:

modela = lm(Car_Price~Kilometers, data = cp)
summary(modela)
Confint(modela)

# qc:
plot(cp$Kilometers, resid(modela))
autoplot(modela)
resettest(modela, power=2, type="regressor")
resettest(modela, power=3, type="regressor")

# qd:
cor_mat <- as.matrix(cor(cp, use = 'na.or.complete')) 
corrplot(cor_mat, type = 'upper', tl.pos = 'lt', tl.cex = 0.6, tl.col = 'black')
modeld1 = lm(Car_Price~HP+Year+Kilometers+ABS+ABD+CD+AW+A_C+MP+gas_die+CC, 
             data = cp)
summary(modeld1)

modeld2 = lm(Car_Price~HP+Year+Kilometers+gas_die, data = cp)
summary(modeld2)

# qe:

H10 = c('Year=1200')
linearHypothesis(modeld2, H10)

H20 = c('CC=0', 'ABS=0')
linearHypothesis(modeld1, H20)

H30 = c('CD=gas_die')
linearHypothesis(modeld1, H30)

H40 = c('20000*Kilometers=4*Year')
linearHypothesis(modeld2, H40)

## White’s test for heteroskedasticity:
bptest(modeld2, ~HP+Year+Kilometers+gas_die, data=cp)
bptest(modeld2, ~fitted(modeld2)+I(fitted(modeld2)^2))

## Breusch-Pagan test for 
bptest(modeld2, ~cp$Year)
bptest(modeld2, ~cp$HP)
bptest(modeld2, ~cp$HP+cp$Year)

# roubust std error
rob.se1 <- vcovHC(modeld2, type='HC0')
coeftest(modeld2, rob.se1)