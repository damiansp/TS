#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/cointegrated')

library(dse)
library(fracdiff)
library(vars)



# 1. Trend- Versus Difference-Stationary Series



# 2. Unit Root Processes
# Code 3.1: Stochastic and Deterministic Trends
n <- 500
e <- rnorm(n)
random.walk <- cumsum(e)
trend <- 1:n
random.walk.with.drift <- 0.5*trend + cumsum(e)
deterministic.trend <- e + 0.5*trend
plot.ts(deterministic.trend, xlab='Time', ylab='')
lines(random.walk.with.drift, col=2)
par(new=T)
plot.ts(random.walk, col=4, axes=F)
axis(4, pretty(range(random.walk)))
abline(h=0, col='grey')
legend('topleft', 
       lty=1, 
       col=c(1, 2, 4), 
       legend=c('Deterministic Trend', 'Random Walk with Drift', 'Random Walk'),
       bg=rgb(1, 1, 1, 0.8))



# 3. Long-Memory Processes
# Code 3.2 ARMA vs. ARFIMA Model
# ARFIMA(0.4, 0.4, 0.0)
N <- 1000
y1 <- fracdiff.sim(n=N, ar=0.4, ma=0.0, d=0.4)
# ARMA (0.4, 0, 0)
y2 <- arima.sim(model=list(ar=0.4), n=N)
op <- par(no.readonly=T)
layout(matrix(1:6, 3, 2, byrow=F))
plot.ts(y1$series, main='TS Plot of Long Memory', ylab='')
acf(y1$series, lag.max=100, main='Long Memory ACF')
spectrum(y1$series, main='Long Memory Spectral Density')
plot.ts(y2, main='TS Plot of Short Memory', ylab='')
acf(y2, lag.max=100, main='Short Memory ACF')
spectrum(y2, main='Short Memory Spectral Density')
par(op)

