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