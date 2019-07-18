#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/penn')


library(astsa)


DATA <- '../data'


# 1.3 TS Characteristics, ACF Properties, and AR(1) Models
x <- scan(sprintf('%s/quakes.dat', DATA))
x <- ts(x)
plot(x, type='l')
lag1.plot(x, 1)
acf(x, xlim=c(1, 19))
xlag1 <- lag(x, -1)
y <- cbind(x, xlag1)
ar1fit <- lm(y[, 1] ~ y[, 2])
summary(ar1fit)
plot(ar1fit$fit, ar1fit$residuals)
lines(lowess(ar1fit$fit, ar1fit$residuals), col=2)
acf(ar1fit$residuals, xlim=c(1, 19))


mort <- scan(sprintf('%s/cmort.dat', DATA))
plot(mort, type='l')
mort <- ts(mort)
mortdiff <- diff(mort, 1)
plot(mortdiff, type='l')
acf(mortdiff)
mortdifflag1 <- lag(mortdiff, -1)
y <- cbind(mortdiff, mortdifflag1)
mortdiffar1 <- lm(y[, 1] ~ y[, 2])
summary(mortdiffar1)
acf(mortdiffar1$residuals)