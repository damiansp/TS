#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/fish')

library(atsalibrary)
library(datasets)
library(devtools)
library(forecast)
library(MARSS)
library(stats)
#devtools::install_github('nwfsc-timeseries/atsalibrary')

data(hourlyphyto)
data(MLCO2)
data(NHTemp)



# 1. TS Plots


# 1.1. ts Objects and plot.ts()
head(MLCO2)
co2 <- ts(MLCO2$ppm, frequency=12, start=c(MLCO2[1, 'year'], MLCO2[1, 'month']))
plot.ts(co2, xlab=expression(paste('CO'[2],' (ppm)')))


# 1.2. Combining and Plotting Multiple ts Objects
temp.ts <- ts(NHTemp$Value, frequency=12, start=c(1880, 1))
dat.I <- ts.intersect(co2, temp.ts)
dim(dat.I)

dat.U <- ts.union(co2, temp.ts)
dim(dat.U)

plot(dat.I, main='')
plot(dat.U, main='')



# 2. Decomposition of Time Series


# 2.1 Estimating Trends
fltr <- c(0.5, rep(1, times=11), 0.5) / 12   # 12 step MA window
co2.trend <- filter(co2, filter=fltr, method='convo', sides=2)
plot.ts(co2.trend, ylab='Trend')


# 2.2 Estimating Seasonal Effects
co2.1T <- co2 - co2.trend
plot.ts(co2.1T, ylab='Seasonal Effect', xlab='Month')

# Avg
ll <- length(co2.1T)
ff <- frequency(co2.1T)
periods <- trunc(ll / ff)
index <- seq(1, ll, by=ff) - 1
mm <- numeric(ff)
for (i in 1:ff) { mm[i] <- mean(co2.1T[index + i], na.rm=T) }
mm <- mm - mean(mm)
plot.ts(mm, ylab='Average Seasonal Effect', xlab='Month')

# Combine trend and seasonal
co2.seas <- ts(rep(mm, periods + 1)[seq(ll)], start=start(co2.1T), frequency=ff)
plot.ts(co2.seas)


# 2.3 Complete the Decomopostion Model
co2.err <- co2 - co2.trend - co2.seas
plot(cbind(co2, co2.trend, co2.seas, co2.err), main='')#, yax.flip=T)



# 2.4 Using decompose() 
co2.decomp <- decompose(co2)
str(co2.decomp)
plot(co2.decomp)



# 3. Differencing to Remove a Trend or Seasonal Effects


# 3.1. Using the diff() Function
co2.D2 <- diff(co2, differences=2)
par(mfrow=c(3, 1))
plot(co2.D2, ylab=expression(paste(nabla^2, 'CO'[2])))
acf(co2.D2)
pacf(co2.D2)
# No trend, but still has obvious seasonal effect
co2.D2D12 <- diff(co2.D2, lag=12)
plot(co2.D2D12)
acf(co2.D2D12)
pacf(co2.D2D12)



# 4. Correlation Within and Among Time Series
par(mfrow=c(1, 1))
acf(co2, lag.max=36)

# "Better" ACF plot
plot.acf <- function(ACF.obj) {
  r <- ACF.obj$acf[-1]
  k <- length(r)
  n <- ACF.obj$n.used
  plot(seq(k), 
       r, 
       type='h', 
       lwd=2, 
       yaxs='i', 
       xaxs='i', 
       ylim=c(floor(min(r)), 1), 
       xlim=c(0, k + 1), 
       xlab='Lag', 
       ylab='Correlation', 
       las=1)
  abline(h=-1 / n + c(-1.96, 196) / sqrt(n), lty=2, col=4)
  abline(h=0)
}

co2.acf <- acf(co2, lag.max=36)
plot.acf(co2.acf) # looks worse to me


# 4.1 Partial Autocorrelation Function (PACF)
pacf(co2, lag.max=36)


# 4.3 Cross-Correlation Function
suns <- ts.intersect(lynx, sunspot.year)[, 'sunspot.year']
lynx <- ts.intersect(lynx, sunspot.year)[, 'lynx']
plot(cbind(suns, lynx))

ccf(suns, lynx)



# 5. White Noise


# 5.1 Simulating White Noise
GWN <- rnorm(100, 5, 0.2)
PWN <- rpois(50, 20)
par(mfrow=c(2, 2))
plot.ts(GWN)
abline(h=5, col=4)
plot.ts(PWN)
abline(h=20, col=4)
acf(GWN, main='Normal', lag.max=20)
acf(PWN, main='Poisson', lag.max=20)



# 6. Random Walks
N <- 300
x <- w <- rnorm(n=N)
for (t in 2:N) { x[t] <- x[t - 1] + w[t] }
par(mfrow=c(2, 1))
plot.ts(x, ylab=expression(x[t]))
acf(x)



# 7. Autoregressive (AR) Models


# 7.1 Simulating an AR(p) Process
# AR(1); Note: |phi| < 1
N <- 100
AR.sm <- list(order=c(1, 0, 0), ar=0.1, sd=0.1)
AR.lg <- list(order=c(1, 0, 0), ar=0.9, sd=0.1)
AR1.sm <- arima.sim(n=N, model=AR.sm)
AR1.lg <- arima.sim(n=N, model=AR.lg)
par(mfrow=c(2, 1))
ylm <- range(c(AR1.sm, AR1.lg))
plot.ts(AR1.sm, 
        ylim=ylm, 
        ylab=expression(italic(x[t])), 
        main=expression(paste(phi, ' = 0.1')))
plot.ts(AR1.lg, 
        ylim=ylm, 
        ylab=expression(italic(x[t])), 
        main=expression(paste(phi, ' = 0.9')))

AR.pos <- list(order=c(1, 0, 0), ar=0.5, sd=0.1)
AR.neg <- list(order=c(1, 0, 0), ar=-0.5, sd=0.1)
AR1.pos <- arima.sim(n=N, model=AR.pos)
AR1.neg <- arima.sim(n=N, model=AR.neg)
ylm <- range(c(AR1.pos, AR1.neg))
plot.ts(AR1.pos, 
        ylim=ylm, 
        ylab=expression(italic(x[t])), 
        main=expression(paste(phi, ' = 0.5')))
plot.ts(AR1.neg, 
        ylim=ylm, 
        ylab=expression(italic(x[t])), 
        main=expression(paste(phi, ' = -0.5')))
# Note: Use caution with arima sim, will complain if  non-stationary model 
# specs.
arima.sim(n=N, model=list(order=c(2, 0, 0), ar=c(0.5, 0.5)))


# 7.2 Correlation Structure of AR(p) Process
ARp <- c(0.7, 0.2, -0.1, -0.3) # AR coefs
AR.mods <- list()
for (p in 1:4) { AR.mods[[p]] <- arima.sim(n=10000, list(ar=ARp[1:p])) }
par(mfrow=c(4, 3))
for (p in 1:4) {
  plot.ts(AR.mods[[p]][1:100], ylab=sprintf('AR(%d)', p))
  acf(AR.mods[[p]], lag.max=12, main='')
  pacf(AR.mods[[p]], lag.max=12, ylab='PACF', main='')
}



# 8. Moving-Average (MA) Models


# 8.1 Simulating an MA(q) Process
MA.sm <- list(order(0, 0, 1), ma=0.2, sd=0.1)
MA.lg <- list(order(0, 0, 1), ma=0.8, sd=0.1)
MA.neg <- list(order(0, 0, 1), ma=-0.5, sd=0.1)
N <- 200
MA1.sm <- arima.sim(n=N, model=MA.sm)
MA1.lg <- arima.sim(n=N, model=MA.lg)
MA1.neg <- arima.sim(n=N, model=MA.neg)
par(mfrow=c(3, 1))
plot.ts(MA1.sm, 
        ylab=expression(itatlic(x[t])), 
        main=expression(paste(theta, ' = 0.2')))
plot.ts(MA1.lg, 
        ylab=expression(itatlic(x[t])), 
        main=expression(paste(theta, ' = 0.8')))
plot.ts(MA1.neg, 
        ylab=expression(itatlic(x[t])), 
        main=expression(paste(theta, ' = -0.5')))        
        
        
# 8.2 Correlation Structure of MA(q) Process
MAq <- c(0.7, 0.2, -0.1, -0.3) # coefs
MA.mods <- list()
for (q in 1:4) {
  # SD assumed to be 1, so not specified (default)
  MA.mods[[q]] <- arima.sim(n=1000, list(ma=MAq[1:q]))
}

par(mfrow=c(4, 3))
for (q in 1:4) {
  plot.ts(MA.mods[[q]][1:100], ylab=paste('MA(', q, ')', sep=''))
  acf(MA.mods[[q]], lag.max=12)
  pacf(MA.mods[[q]], lag.max=12, ylab='PACF')
}



# 9. Autoregressive Moving Average (ARMA) Models


# 9.1 Fitting ARMA(p, q) Models with arima()
# ARMA(2, 2)
arma2.2 <- list(order=c(2, 0, 2), ar=c(-0.7, 0.2), ma=c(0.7, 0.2))
mu <- 5
# sim process (+ mean)
arma.sim <- arima.sim(n=10000, model=arma2.2) + mu
# Est params
arima(x=arma.sim, order=c(2, 0, 2))


# 9.2 Searching Over Model Orders
arma.res <- list()
i <- 1
for (p in 0:3) {
  for (q in 0:3) {
  	arma.res[[i]] <- arima(x=arma.sim, order=c(p, 0, q))
  	i <- i + 1  	
  }
}

arma.aic <- sapply(arma.res, function(x) { x$aic })
arma.res[[which(arma.aic == min(arma.aic))]]

auto.arima(arma.sim, start.p=0, max.p=3, start.q=0, max.q=3)
