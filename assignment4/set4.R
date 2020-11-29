rm(list = ls())
setwd("Desktop/courses_term1/econometrics/problem_sets/assignment4/")
#install.packages('plm')
library(zoo)
library(sandwich)
library(lmtest)
library(plm)
library(car)
library(readstata13)
library(fastDummies)

panel_fin = read.dta13('./panel_fin.dta')
paneldf <- pdata.frame(panel_fin, index = c('entity', 'quarter'))

#panel_fin = read.csv('./panel_fin.csv')
#plm(y ~x1+x2+..., data=paneldf, model = c("within"))
#plm(y ~x1+x2+..., data=paneldf, model = c("random"))
#plm(y ~x1+x2+..., data=paneldf, model = c("between"))
#plmtest(panelmodel, effect = c("individual"), type = c( "bp"))
#phtest(femodel, remodel)

# Qa 1:
pd_roe <- plm(ret ~ l_ret + l_btm + l_roe, data = paneldf, model = c("pooling"))
summary(pd_roe)

# Qa 2:
pd_lep <- plm(ret ~ l_ret + l_btm + l_e_p, data = paneldf, model = c("pooling"))
summary(pd_lep)

# Qb1:
## add dummy variable for every bank:

paneldf_bank_dummy <- dummy_cols(paneldf,select_columns='entity',remove_first_dummy = TRUE)
#paneldf_bank_dummy <- as.data.frame(paneldf_bank_dummy)
## get the formula:
formula_b1 = 'ret ~ l_ret + l_btm + l_roe'
i=1
while (i < 868) {
  i = i + 1
  formula_b1 <- paste(formula_b1, '+entity_', i, sep = "")
  
}
roe_bank_dummy <- plm(formula_b1, data = paneldf_bank_dummy, model = c("pooling"), index = c("entity","quarter"))
## test for the bank dummy
Hb10 = c()
i=1
while (i < 868) {
  i = i + 1
  test_i <- paste('entity_', i, '=0',sep = "")
  Hb10 <- append(Hb10, test_i)
  
}
test_resb1 = linearHypothesis(roe_bank_dummy, Hb10)
test_resb1



# Qb2:
## add dummy variable for every time peroid
paneldf_time_dummy <- dummy_cols(paneldf, select_columns='quarter',
                      remove_first_dummy = TRUE)
## get the formula:
formula_b2 = 'ret ~ l_ret + l_btm + l_roe'
i=1
while (i < 94) {
  i = i + 1
  formula_b2 <- paste(formula_b2, '+quarter_', i, sep = "")
}
roe_time_dummy <- plm(formula_b2, data = paneldf_time_dummy, 
                      model = c("pooling"), index = c("entity","quarter"))
## test for the bank dummy
Hb20 = c()
i=1
while (i < 94) {
  i = i + 1
  test_i <- paste('quarter_', i, '=0',sep = "")
  Hb20 <- append(Hb20, test_i)
}
test_resb2 = linearHypothesis(roe_time_dummy, Hb20)
test_resb2


# Qc:
pd_roe_rand <- plm(ret ~ l_ret + l_btm + l_roe, data = paneldf, model = c("random"))
pd_roe_fixed <- plm(ret ~ l_ret + l_btm + l_roe, data = paneldf, model = c("within"))

plmtest(pd_roe_fixed, effect = c("individual"), type = c( "bp"))
