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
n.births <- births$Daily.total.female.births.in.California..1959
births$Date <- as.Date(births$Date, '%Y-%m-%d')
head(births)
plot(births, type='l')

# Test for correlation
Box.test(n.births, lag=log(length(n.births))) # correlated
Box.test(rnorm(365), lag=log(365)) # cf.
plot(diff(n.births), type='l')

# Corr in differenced data?
Box.test(diff(n.births), lag=log(length(diff(n.births)))) # correlated

par(mfrow=c(3, 1))
plot(diff(n.births), type='l')
acf(diff(n.births), 50)
pacf(diff(n.births), 50)

# Fit various ARIMA mods
mod1 <- arima(n.births, order=c(0, 1, 1))
(sse1 <- sum(mod1$residuals^2)) # 18148.46
(mod1.box <- Box.test(mod1$residuals, lag=log(length(mod1$residuals)))) # 0.5334

mod2 <- arima(n.births, order=c(0, 1, 2))
(sse2 <- sum(mod2$residuals^2)) # 17914.65
(mod2.box <- Box.test(mod2$residuals, lag=log(length(mod2$residuals)))) # 0.9859

mod3 <- arima(n.births, order=c(7, 1, 1))
(sse3 <- sum(mod3$residuals^2)) # 17584.39
(mod3.box <- Box.test(mod3$residuals, lag=log(length(mod3$residuals)))) # ~1

mod4 <- arima(n.births, order=c(7, 1, 2))
(sse4 <- sum(mod4$residuals^2)) # 17574.06
(mod4.box <- Box.test(mod4$residuals, lag=log(length(mod4$residuals)))) # ~1

df <- data.frame(row.names=c('AIC', 'SSE', 'p'), 
                 c(mod1$aic, sse1, mod1.box$p.value),
                 c(mod2$aic, sse2, mod2.box$p.value),
                 c(mod3$aic, sse3, mod3.box$p.value),
                 c(mod4$aic, sse4, mod4.box$p.value))
colnames(df) <- c('ARIMA(011)', 'ARIMA(012)', 'ARIMA(711)', 'ARIMA(712)')
format(df, scientific=F)

sarima(n.births, 0, 1, 2, 0, 0, 0)

