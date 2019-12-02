#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/R/penn')
DATA <- '../data'

library(astsa)
library(nlme)
library(orcutt)



# 1. Linear Regression Models with Autoregressive Errors
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

# Simulated
x <- ts(scan(paste(DATA, 'l8.1.x.dat', sep='/')))
y <- ts(scan(paste(DATA, 'l8.1.y.dat', sep='/')))
plot(x, y, xy.lines=F, xy.labels=F, pch=20)

trend <- time(y)
regmodel <- lm(y ~ trend + x)
summary(regmodel)
dtx <- resid(lm(x ~ time(x))) # detrend x
regmod2 <- lm(y ~ trend + dtx)
summary(regmod2)
acf2(resid(regmod2)) # both still signif at lag=1

adjreg <- sarima(y, 0, 0, 1, xreg=cbind(trend, dtx)) # MA(1) adj regression
adjreg
acf2(resid(adjreg$fit))
summary(gls(y ~ dtx + trend, correlation=corARMA(form=~1, p=0, q=1)))
# Should also compare alt hypoths. AR(1), ARMA(1, 1)



# 2 Cross Correlation Functions and Lagged Regressions
ccf(x, y)
ccf.vals <- ccf(x, y)
ccf.vals