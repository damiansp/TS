#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/TS/coursera')


# AIC
data <- arima.sim(list(order=c(2, 0, 0), ar=c(0.7, -0.2)), n=2000)
par(mfrow=c(3, 1))
plot(data)
acf(data, main='ACF: 2nd order series', lag.max=100)
pacf(data, main='PACF') # suggests 2nd order process


# Coefs *assuming* 2nd order process
arima(data, order=c(2, 0, 0), include.mean=F)

for (p in 1:5) {
  print(arima(data, order=c(p, 0, 0), include.mean=F))	# AIC minimized at 2
}


