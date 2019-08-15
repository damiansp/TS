#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/cointegrated')

library(dse)
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