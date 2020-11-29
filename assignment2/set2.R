rm(list = ls())
setwd("Desktop/courses_term1/econometrics/problem_sets/assignment2/")
library(sandwich)
library(ggplot2)
library(lmtest)
library(zoo)
library(corrplot)
library(car)


gs = read.csv('./gasoline.csv')
gs$g_pop = gs$g/gs$pop
gs$t = gs$year - 1960

#---------exercise 1------------------#
# qa:

modela = lm(g_pop~pg+y+pnc+puc+ppt+pd+pn+ps+t, data=gs)

#qb
Hb0 = c('pnc=puc')
linearHypothesis(modela, Hb0)

# qc
# elasticity of gasoline demand to its own price
elasticity_pg = modela$coefficients['pg'] * mean(gs$pg)/mean(gs$g_pop)


# elasticity of gasoline demand to income
elasticity_y = modela$coefficients['y'] * mean(gs$y)/mean(gs$g_pop)


# elasticity of gasoline demand to changes 
# in the price of public transportation
elasticity_ppt = modela$coefficients['ppt'] * mean(gs$ppt)/mean(gs$g_pop)

# qd
gs_log = log(gs)
gs_log$t = gs$t
modela = lm(g_pop~pg+y+pnc+puc+ppt+pd+pn+ps+t, data=gs_log)


# ---------exercsie 2------------------#
# 2a
pdd = read.csv('./production.csv', sep = ';')
nlsmodel = nls(valueadd ~(alpha*labor^beta*capital^gama), 
     data=pdd, start=list(alpha=1.5, beta=0.5,gama=0.5))


pdd_log  = log(pdd)
model_ols = lm(valueadd~labor+capital, data=pdd_log)
exp(0.5403)

# Test the following hypotheses:

## 1
H10 = c('labor=0.6')
linearHypothesis(model_ols, H10)

## 2
H20 = c('labor+capital=1')
linearHypothesis(model_ols, H20)

## 3
H30 = c('labor=0.6', 'labor+capital=1')
linearHypothesis(model_ols, H30)


## 4 
pdd_log$labor2 = 0.5*pdd_log$labor^2
pdd_log$capital2 = 0.5*pdd_log$capital^2
pdd_log$lc = pdd_log$labor*pdd_log$capital

model_ols4 = lm(valueadd~labor+capital+labor2+capital2+lc, 
                data=pdd_log)


H40 = c('labor2=0', 'capital2=0', 'lc=0')
linearHypothesis(model_ols4, H40)
