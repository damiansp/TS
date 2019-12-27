#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/R/penn')
DATA <- '../../data'

library(astsa)
library(nlme)
library(orcutt)



# 1. Pre-Whitening as an Aid to Interpreting CCF
x <- arima.sim(list(order=c(1, 1, 0), ar=0.7), n=200)
z <- ts.intersect(x, lag(x, -3), lag(x, -4))
head(z)
y <- 15 + 0.8*z[, 2] + 1.5*z[, 3]
ccf(z[, 1], y, na.action=na.omit)
acf(x)
diff1x <- diff(x)
acf(diff1x, na.action=na.omit)
pacf(diff1x, na.action=na.omit)
ar1mod <- arima(x, order=c(1, 1, 0))
ar1mod # ar1 coef appx = to our given 0.7
ar1.coef <- ar1mod$coef
pwx <- ar1mod$residuals
newpwy <- filter(y, filter=c(1, -(1 + ar1.coef), ar1.coef), sides=1)
ccf(pwx, newpwy, na.action=na.omit)



# Intervention Analysis
