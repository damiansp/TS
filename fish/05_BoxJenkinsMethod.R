#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/fish')

library(atsalibrary)
library(forecast)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(tseries)
library(urca)
library(datasets)

data(greeklandings)
data(chinook)

landings <- greeklandings
chinook <- chinook.month



# 1. Box-Jenkins Method
#    A) Model Form Selection
#       1) Evaluate Stationarity
#       2) Selection of Differencing Level (d) to Fix Stationarity
#       3) Selection of AR level (p)
#       4) Selection of MA level (q)
#    B) Param Estimation
#    C) Model Checking



# 2. Stationarity

# 2.1 Look at Stationarity in Simulated Data
TT <- 100
y <- rnorm(TT)
op <- par(mfrow=c(2, 1))
plot(y, type='l')
acf(y)
par(op)

dat <- data.frame(t=1:TT, y=y)
p1 <- (ggplot(dat, aes(x=t, y=y)) 
       + geom_line() 
       + ggtitle('White Noise TS') 
       + xlab('') 
       + ylab('value'))
ys <- matrix(rnorm(TT * 10), TT, 10)
ys <- data.frame(ys)
ys$id <- 1:TT

ys2 <- melt(ys, id.var='id')
p2 <- (ggplot(ys2, aes(x=id, y=value, group=variable))
       + geom_line()
       + xlab('')
       + ylab('value')
       + ggtitle('10 White Noise Processes'))
grid.arrange(p1, p2, ncol=1)
# all stationary (mean constnant wrt time); AR(1) also stationary...
theta <- 0.8
n.sim <- 10
ar1 <- arima.sim(TT, model=list(ar=theta))
plot(ar1)

dat <- data.frame(t=1:TT, y=ar1)
p1 <- (ggplot(dat, aes(x=t, y=y))
       + geom_line()
       + ggtitle('AR(1)')
       + xlab('')
       + ylab('value'))
ys <- matrix(0, TT, n.sim)
for (i in 1:n.sim) { ys[, i] <- as.vector(arima.sim(TT, model=list(ar=theta))) }
ys <- data.frame(ys)
ys$id <- 1:TT

ys2 <- melt(ys, id.var='id')
p2 <- (ggplot(ys2, aes(x=id, y=value, group=variable))
       + geom_line()
       + xlab('')
       + ylab('value')
       + ggtitle('Var(AR(1)) is constant wrt time'))
grid.arrange(p1, p2, ncol=1)


# 2.2 Stationary Around a Linear Trend
