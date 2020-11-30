rm(list = ls())
setwd("Desktop/courses_term1/econometrics/problem_sets/assignment3/")

library('strucchange')
library('sandwich')
library(AER)

gs = read.csv('../assignment3/gasoline.csv',sep=",")
gasoline = read.csv('./gasoline.csv',sep=",")

sctest(per_g~year+y+pg+pnc+puc, data = gs_log,
       type = "Chow", point = 14)

# q1.a
gs$per_g = gs$g/gs$pop
gs_log = log(gs)
gs_log$year = gs$year
model1 = lm(per_g~year+y+pg+pnc+puc,  subset=(year<=1973), data=gs_log)
model2 = lm(per_g~year+y+pg+pnc+puc,  subset=(year>1973), data=gs_log)
model3 = lm(per_g~year+y+pg+pnc+puc,  data=gs_log)
RSS1 = sum(resid(model1)^2)
RSS2 = sum(resid(model2)^2)
RSS3 = sum(resid(model3)^2)
F_va = ((RSS3-RSS1-RSS2)/6)/((RSS2+RSS1)/(36-12))
p_va = pf(F_va, 6, 24, lower.tail=F)
chow_t = sctest(per_g~year+y+pg+pnc+puc, data = gs_log,
       type = "Chow", point = 14)


# q1.b
gs_log$dum <- ifelse(gs_log$year>=1974, 1, 0)
model4 = lm(per_g~year+y+pg+pnc+puc+dum,  data=gs_log)
model5b = lm(per_g~year+y+pg+pnc+puc+dum,  subset=(year<=1973), data=gs_log)
model6b = lm(per_g~year+y+pg+pnc+puc+dum,  subset=(year>1973), data=gs_log)
RSSb4 = sum(resid(model4)^2)
RSSb5 = sum(resid(model5b)^2)
RSSb6 = sum(resid(model6b)^2)
unresdf = summary(model5b)$df[1]+summary(model6b)$df[1] - summary(model4)$df[1]
resdf = nobs(model4) - summary(model5b)$df[1]-summary(model6b)$df[1]

F_vb = ((RSS4-RSSb5-RSSb6)/unresdf)/((RSSb5+RSSb6)/resdf)
p_vb = pf(F_vb, 5, 26, lower.tail=F)

chow_t = sctest(per_g~year+y+pg+pnc+puc+dum, data =gs_log,
                type = "Chow", point = 14)

# q1.c
gs_log$dum <- ifelse(gs_log$year>=1974, 1, 0)
model5 = lm(per_g~year+y+pg+pnc+puc,  data=gs_log)
model6 = lm(per_g~year+y+pg+pnc+puc+dum+dum*y+dum*pg,  data=gs_log)
RSS5 = sum(resid(model5)^2)
RSS6 = sum(resid(model6)^2)
F_vc = ((RSS5-RSS6)/3)/(RSS6/(36-6))
p_vc = pf(F_vc, 3, 30, lower.tail=F)
summary(model5)
H0 = c('dum=0','y:dum=0','pg:dum=0')
linearHypothesis(model5, H0)


# q2.a

jec = read.csv('./JEC.csv',sep=";")
jec$lnP = log(jec$price)
jec$lnQ = log(jec$quantity)
model2a = lm(lnQ~lnP+ice+seas1+seas2+seas3
             +seas4+seas5+seas6+seas7+seas8+
             seas9+seas10+seas11+seas12,  data=jec)
model2a


# 2.f

model2f = ivreg(lnQ~lnP+ice+seas1+seas2+seas3
             +seas4+seas5+seas6+seas7+seas8+
               seas9+seas10+seas11+seas12|cartel+ice+seas1+seas2+seas3
             +seas4+seas5+seas6+seas7+seas8+
               seas9+seas10+seas11+seas12,  data=jec)
summary(model2f, diagnostics=TRUE)


