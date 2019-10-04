#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/penn')


library(astsa)



# 1. Decomposition Models
beer <- scan('../data/beerprod.dat')
beer <- ts(beer, freq=4)
beer.decomp <- decompose(beer, type='additive')
plot(beer.decomp)
beer.decomp

beer.decomp.mult <- decompose(beer, type='multiplicative')
plot(beer.decomp.mult)

# Lowess Seasonal and Trend Decomposition
beer.decomp.lowess <- stl(beer, 'periodic')
plot(beer.decomp.lowess)



# 2. Smoothing Time Series
trend.pattern <- filter(beer, filter=c(1/8, 1/4, 1/4, 1/4, 1/8), sides=2)
plot(beer)
lines(trend.pattern, col=2)

seasonals <- beer - trend.pattern
plot(seasonals)

plot(beer)
lines(lowess(beer), col=2)

plot(beer)
exp.smooth.fit <- arima(beer, order=c(0, 1, 1))
exp.smooth.fit
preds <- beer - exp.smooth.fit$residuals
lines(preds, col=2)