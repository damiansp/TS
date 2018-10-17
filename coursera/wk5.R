#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/TS/coursera')

library(forecast)
data(discoveries)

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


# ARMA properties
plot(discoveries, main='Major Scientific Discoveries by Year')
stripchart(discoveries, method='stack', offset=0.5, at=0.15, pch=19)

par(mfrow=c(2, 1))
acf(discoveries)
pacf(discoveries)
# examine competing models with p, q both on [0, 3]
# ic=.. so select criterion
auto.arima(discoveries, d=0, approximation=F) # 2 0 0; d = 0: no evidence of trend



#        AR(p)                 MA(q)                 ARMA(p, q)
# ACF    Tails off             Cuts off after lag q  Tails off
# PACF   Cuts off after lag p  Tails off             Tails off