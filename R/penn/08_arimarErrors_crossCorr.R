#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/R/penn')
DATA <- '../data'

library(astsa)
library(orcutt)


x <- ts(scan(paste(DATA, 'econpredictor_1.dat', sep='/')))
y <- ts(scan(paste(DATA, 'econmeasure_0.dat', sep='/')))
plot.ts(x, y, xy.lines=F, xy.labels=F)

regmodel <- lm(y ~ x)
summary(regmodel)
acf2(resid(regmodel))

ar1res <- sarima(resid(regmodel), 1, 0, 0, no.constant=T) # AR(1) mod
xl <- ts.intersect(x, lag(x, -1)) # matrix w x and lag1(x)
xnew <- xl[, 1] - 0.6488*xl[, 2] # x var for adj regression
yl <- ts.intersect(y, lag(y, -1))
ynew <- yl[, 1] - 0.6488*yl[, 2]

adjustreg <- lm(ynew ~ xnew)
summary(adjustreg)
acf2(resid(adjustreg))

cochrane.orcutt(regmodel)
summary(cochrane.orcutt(regmodel))



# Regression Model with ARIMA Errors
