#=========#=========#=========#=========#=========#=========#=========#=========rm(list = ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/R/basic')

library(nlme)

DATA <- paste0("https://raw.githubusercontent.com/dallascard/",
               "Introductory_Time_Series_with_R_datasets/master/")



# 3 Moving Average Models
	
	
# 3.2 R examples: correlogram and simulation
# Autocorrelation function for MA process at lag k
rho <- function(k, beta) { 
  # order of the MA process
  q <- length(beta) - 1	
  if (k > q) { ACF <- 0 } 
  else {
    s1 <- s2 <- 0
    for (i in 1:(q - k + 1)) { s1 <- s1 + beta[i] * beta[i + k] }
	for (i in 1:(q + 1)) { s2 <- s2 + beta[i]^2 }
    ACF <- s1 / s2
  }
  ACF	
}

# Ex. for an MA(3) with params beta[1] = 0.7, beta[2] = 0.5 and beta[3] = 
# 0.2:
beta <- c(1, 0.7, -0.7, 0.5, -0.5, 0.2, -0.2)
# If betas are all + will have exp decrease, if all - will have dampened
# ocillation, but becomes 0 at lenght(beta)
rho.k <- rep(1, 10)
for (k in 1:10) { rho.k[k] = rho(k, beta) }

plot(0:10, c(1, rho.k), pch=4, type='b', ylab=expression(rho[k]))
abline(h=0, col=2)
	
# Simulate an MA(3) process:
#set.seed(1)
b <- c(0.8, 0.6, 0.4) # 3 coeffs b/c MA(3)
x <- w <- rnorm(1000)
for (t in 4:1000) {
  for (j in 1:3) { x[t] <- x[t] + b[j] * w[t - j] }
}

par(mfrow=c(2, 1))
plot(x, type='l')
acf(x)



# 4 Fitted MA Models


# 4.1 Model fitted to simulated series
x.ma <- arima(x, order=c(0, 0, 3))	# arima order = (AR, I, MA)
# Can force mean to 0 with arima(..., include.mean=F), though generally
# not advisable
x.ma


# 4.2 Exchange rate series: fitted MA model
x <- read.table(paste(DATA, 'pounds_nz.dat', sep=''))
x.ts <- ts(x, st=1991, fr=4)
plot(x.ts)
x.ma = arima(x.ts, order=c(0, 0, 1))
x.ma
acf(x.ma$resid[-1])



# 6 ARMA Models: Empirical Analysis
	
	
# 6.1 Simulation and fitting
#set.seed(1)
x <- arima.sim(n=10000, list(ar=-0.6, ma=0.5))
# x[t] = -0.6*x[t-1] + w[t] + 0.5*w[t-1]
plot(x)
lines(lowess(x), col=2)
coef(arima(x, order=c(1,0,1)))
acf(resid(arima(x, order=c(1, 0, 1))))


# 6.2 Exchange rate series
x.ma <- arima(x.ts, order=c(0, 0, 1))	# MA(1) model
x.ar <- arima(x.ts, order=c(1, 0, 0))	# AR(1) model
x.arma <- arima(x.ts, order=c(1, 0, 1))	# ARMA(1, 1) model
AIC(x.ma)	#278.8
AIC(x.ar)	#246.0
AIC(x.arma)	#243.3
x.arma
acf(resid(x.arma))

	
# 6.3 Electricity production series
CBE <- read.table(paste(DATA, 'cbe.dat', sep = ''), header = T)
matplot(CBE, type='l', lty=1)
Elec.ts <- ts(CBE[, 3], start=1958, freq=12)
Time <- 1:length(Elec.ts)
Imth <- cycle(Elec.ts)
Elec.lm <- lm(log(Elec.ts) ~ Time + I(Time^2) + factor(Imth))

par(mfrow=c(2, 2))
plot(Elec.lm)
par(mfrow=c(1, 1))
acf(resid(Elec.lm))

best.order <- c(0, 0, 0)
best.aic <- Inf
for (i in 0:2) {
  for (j in 0:2) {
    fit.aic <- AIC(arima(resid(Elec.lm), order = c(i, 0, j)))
    if (fit.aic < best.aic) {
      best.order <- c(i, 0, j)
      best.arma <- arima(resid(Elec.lm), order=best.order)
      best.aic <- fit.aic
    }
  }
}

best.order	#(2, 0, 0)
acf(resid(best.arma))

# predict:
new.time <- seq(length(Elec.ts), length=36)
new.data <- data.frame(Time=new.time, Imth=rep(1:12, 3))
predict.lm <- predict(Elec.lm, new.data)
predict.arma <- predict(best.arma, n.ahead=36)
elec.pred <- ts(exp(predict.lm + predict.arma$pred), start=1991, freq=12)
ts.plot(cbind(Elec.ts, elec.pred), col=1:2)

# 6.6.4 Wave tank data
wave.dat <- read.table(paste(DATA, 'wave.dat', sep=''), header=T)
layout(1:3)
plot(as.ts(wave.dat$waveht), ylab='Wave height')
acf(wave.dat$waveht)
pacf(wave.dat$waveht)
wave.arma <- arima(wave.dat$waveht, order=c(4, 0, 4))
acf(wave.arma$res[-(1:4)])
pacf(wave.arma$res[-(1:4)])
hist(wave.arma$res[-(1:4)], xlab='ht/mm', main='')