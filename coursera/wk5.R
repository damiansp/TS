#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
        detach,
        character.only=T,
        unload=T)
setwd('~/Learning/TS/coursera')

library(astsa)
library(forecast)
#library(TSDL) # not available for 3.5
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



# ARIMA(2, 1, 1) Simulation
phi <- c(0.7, 0.2)
beta <- 0.5
sigma <- 3
m <- 10000

sim <- arima.sim(n=m, list(order=c(2, 1, 1), ar=phi, ma=beta))
par(mfrow=c(3, 1))
plot(sim, ylab='', main='Simulated ARIM(2, 1, 1) Process', col=4, lwd=2)
acf(sim)
pacf(sim)

diff.sim <- diff(sim)
plot(diff.sim, col=4, lwd=2)
acf(diff.sim)
pacf(diff.sim)

sarima(sim, 2, 1, 1, 0, 0, 0)

auto.arima(sim)

(fit1 <- arima(diff.sim, order=c(4, 0, 0)))
(fit2 <- arima(diff.sim, order=c(2, 0, 1)))
(fit3 <- arima(sim, order=c(2, 1, 1)))


# Ljung-Box (Box-Pierce) Q stat:
Box.test(sim, lag=log(length(sim)))


# Applied: Daily Female Births in CA, 1959
births <- read.csv('data/daily-total-female-births-in-cal.csv')
births$Date <- as.Date(births$Date, '%Y-%m-%d')
head(births)
plot(births, type='l')
