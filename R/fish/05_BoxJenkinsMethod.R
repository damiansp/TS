#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/R/fish')

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



# 1. Box-Jenkins Method-------------------------------------------------------
#    A) Model Form Selection
#       1) Evaluate Stationarity
#       2) Selection of Differencing Level (d) to Fix Stationarity
#       3) Selection of AR level (p)
#       4) Selection of MA level (q)
#    B) Param Estimation
#    C) Model Checking



# 2. Stationarity--------------------------------------------------------------


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
intercept <- 0.5
trend <- 0.1
sd <- 0.5
TT <- 20
white.noise <- rnorm(TT, sd=sd)
wn.i <- white.noise + intercept
wn.ti <- white.noise + trend*(1:TT) + intercept

op <- par(mfrow=c(1, 3))
plot(white.noise, type='l')
plot(trend * (1:TT), type='b')
plot(wn.ti, type='l')
par(op)

# As AR(1)
beta1 <- 0.8
ar1 <- arima.sim(TT, model=list(ar=beta1), sd=sd)
ar1i <- ar1 + intercept
ar1ti <- ar1 + trend * (1:TT) + intercept
dat <- data.frame(t=1:TT, ar1=ar1, ar1i=ar1i, ar1ti=ar1ti)
p <- ggplot(dat, aes(x=t, y=ar1)) + geom_line() + ggtitle('AR1')
p2 <- (ggplot(dat, aes(x=t, y=ar1i)) 
       + geom_line() + ggtitle('AR1 (non-zero mean)'))
p3 <- (ggplot(dat, aes(x=t, y=ar1ti)) 
       + geom_line() + ggtitle('AR1 (with linear trend)'))
grid.arrange(p, p2, p3, ncol=3)


# 2.3 Greek Landing Data
anchovy <- subset(landings, Species=='Anchovy')$log.metric.tons
anchovy.ts <- ts(anchovy, start=1964)
plot(anchovy.ts, ylab='log(catch)')



# 3. Dickey-Fuller and Augmented Dickey-Fuller Tests---------------------------


# 3.3 ADF Test Using adf.test()
# Null hypothesis is: series is not stationary

# 3.3.1 Test on White Noise
TT <- 100
wn <- rnorm(TT)
adf.test(wn) # p <= 0.01; cannot reject stationarity (e.g., reject null hyp.)
adf.test(wn, k=0) # note smaller t stat

# 3.3.2 Test on White Noise with Trend
intercept <- 1
wnt <- wn + 1:TT + intercept
plot(wnt, type='l')
# NOTE: test allows for intercept and trend, hence: stationary (about trend 
# line)
adf.test(wnt) 

# 3.3.3 Test on a Random Walk
rw <- cumsum(rnorm(TT))
plot(rw, type='l')
adf.test(rw) # not stationary

# 3.3.4 Test the Anchovy Data
plot(anchovy.ts)
adf.test(anchovy.ts) # Not stationary


# 3.4 ADF Test Using ur.df() (urca)

# 3.4.1 Test on White Noise
test <- urca::ur.df(wn, type='trend', lags=0)
summary(test) # z.lag.1: p << 0.001; cannot reject stationarity)



# 4. KPSS Test-----------------------------------------------------------------
# Null Hypoth: data ARE stationary


# 4.1 Test on Simulated Data
kpss.test(wn)  # p > 0.1 (stationary)
kpss.test(wnt) # p < 0.01 (non-stationary [has known trend])
kpss.test(wnt, null='Trend') # p > 0.1 (stationary [about a trend])


# 4.2 Test the Anchovy Data
kpss.test(anchovy, null='Trend') # p.value < 0.01 (non-stationary)



# 5. Dealing with Non-Stationarity---------------------------------------------
adf.test(diff(rw))  # stationary (p < alpha)
kpss.test(diff(rw)) # stationary (p > alpha)

diff1dat <- diff(anchovy.ts)
adf.test(diff1dat) # p = 0.051: stationary-ish 
# (but includes unneeded trend param)
kpss.test(diff1dat) # p ≥ 0.1: stationary
k <- trunc((length(diff1dat)- 1)^(1/3))
test <- ur.df(diff1dat, type='drift', lags=k)
summary(test)


# 5.1 ndiffs()
ndiffs(anchovy.ts, test='kpss') # 1
ndiffs(anchovy.ts, test='adf')  # 1



# 6. Summary: Stationarity Testing---------------------------------------------



# 7. Estimating ARMA Parameters------------------------------------------------


# 7.1 AR(2) Data
# x[t] = 0.8x[t - 1] + 01x[t - 2] + e[t]
# y[t] = x[t] + m
m <- 1
ar2 <- arima.sim(n=1000, model=list(ar=c(0.8, 0.1))) + m


# 7.2 Fit with Arima()
Arima(ar2, order=c(2, 0, 0), include.constant=T)
# or with arima()
arima(ar2, order=c(2, 0, 0), include.mean=T)


# 7.3 AR(1) Simulated Data
ar1 <- arima.sim(n=100, model=list(ar=c(0.8))) + m
Arima(ar1, order=c(1, 0, 0), include.constant=T)


# 7.4 ARMA(1, 2) Simulated Data
# x[t] = 0.8x[t-1] + e[t] + 0.8e[t - 1] + 0.2e[t - 2]
arma12 <- arima.sim(n=100, model=list(ar=c(0.8), ma=c(0.8, 0.2))) + m
Arima(arma12, order=c(1, 0, 2), include.constant=T)


# 7.5 These Functions Work for Data with Missing Values
ar2miss <- arima.sim(n=100, model=list(ar=c(0.8, 0.1)))
ar2miss[sample(100, 50)] <- NA
plot(ar2miss, type='l')
fit <- Arima(ar2miss, order=c(2, 0, 0))
fit
# NOTE: fitted() does not return expected val at time t.  It is the expected val
# of y[t] given the data up to [t-1]
lines(fitted(fit), col=2)



# 8. Estimating ARMA Orders----------------------------------------------------


# 8.1 Example: Model Selection for AR(2) Data
auto.arima(ar2)
auto.arima(ar2miss)


# 8.2 Fitting to 100 Simulated Data Sets
save.fits <- rep(NA, 100)
for (i in 1:100) {
  a2 <- arima.sim(n=100, model=list(ar=c(0.8, 0.1)))
  fit <- auto.arima(a2, seasonal=F, max.d=0, max.q=0)
  save.fits[i] <- paste0(fit$arma[1], '-', fit$arma[2])
}
table(save.fits)


# 8.3 trace=TRUE
auto.arima(ar2, trace=T)


# 8.4 stepwise=False (i.e., exhaustive up to order 5 by default)
auto.arima(ar2, trace=T, stepwise=F) # may find a better fit


# 8.5 Fit to Anchovy Data
fit <- auto.arima(anchovy.ts)
fit



# 9. Check Residuals-----------------------------------------------------------
res <- resid(fit)
Box.test(res, type='Ljung-Box', lag=12, fitdf=2)
checkresiduals(fit)



# 10. Forecast from a Fitted ARIMA Model---------------------------------------
fr <- forecast(fit, h=10)
plot(fr) # shows 80 and 95% CIs



# 11. Seasonal ARIMA Model-----------------------------------------------------
chinook.ts <- ts(chinook$log.metric.tons, start=c(1990, 1), frequency=12)


# 11.1 Plot Seasonal Data
plot(chinook.ts)


# 11.2 auto.arima() for Seasonal TS
train.dat <- window(chinook.ts, c(1990, 10), c(1998, 12))
test.dat <- window(chinook.ts, c(1999, 1), c(1999, 12))
fit <- auto.arima(train.dat)
fit



# 12. Forecast Using a Seasonal Model
fr <- forecast(fit, h=12)
plot(fr)
points(test.dat, col=2, pch=16)