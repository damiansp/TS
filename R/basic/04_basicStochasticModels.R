#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/R/basic')
options(digits=5)
library(MASS)

DATA <- paste0("https://raw.githubusercontent.com/dallascard/",
               "Introductory_Time_Series_with_R_datasets/master/")



# 2 White Noise-----------------------------------------------------------------


# 2.3 Simulation in R
N <- 100
w <- rnorm(N)
plot(w, type='l')

N <- 1000
x <- seq(-4, 4, length=N)
hist(rnorm(N), prob=T, col=4)
lines(x, dnorm(x), col=5, lw=2)


# 2.4 Second-Order Properties and the Correlogram
acf(rnorm(N))



# 3 Random Walks----------------------------------------------------------------
# 3.7 Simulation
x <- w <- rnorm(1000)
for(t in 2:1000) { x[t] <- x[t-1] + w[t] } 
par(mfrow = c(3, 1))
plot(x, type = 'l')
acf(x)



# 4 Fitted Models and Diagnostic Plots------------------------------------------


# 4.1 Simulated random walk series
acf(diff(x))


# 4.2 Exchange rate series
Z <- read.table(paste(DATA, 'pounds_nz.dat', sep='/'))
Z.ts <- ts(Z, start=1991, frequency=4)
plot(Z.ts)
plot(diff(Z.ts))
acf(diff(Z.ts))

Z.hw <- HoltWinters(Z.ts, alpha=1, gamma=0)
par(mfrow=c(2, 1))
plot(Z.hw)
acf(resid(Z.hw))

Z.hw2 <- HoltWinters(Z.ts)
plot(Z.hw2)
acf(resid(Z.hw2))


# 4.3 Random walk with drift
HP.dat <- read.table(paste(DATA, 'HP.txt', sep=''), header = T)
plot(as.ts(HP.dat$Price))
DP <- diff(HP.dat$Price)
par(mfrow=c(2, 1))
plot(as.ts(DP))
abline(h=0, col='grey')
mean(DP) + c(-2, 2)*sd(DP) / sqrt(length(DP))
abline(h=mean(DP), col=2)
abline(h=mean(DP) + c(-2, 2)*sd(DP) / sqrt(length(DP)), col=4, lty=2)
acf(DP)
	
# Simulate data from model w HP parameters to see how well it applies
par(mfrow=c(1, 1))
n <- dim(HP.dat)[1]
plot(as.ts(HP.dat$Price), ylim=c(0, 80), xlim=c(0, n + 100))
	
mu <- mean(DP)
sigma <- sd(DP)
x <- numeric(n + 100)
iters <- 1000
outM <- matrix(0, (n + 100), iters)
x[1] <- HP.dat[1, ]
	
for (i in 1:iters) {
  for (j in 2:(n + 100)) {
    x[j] <- x[j - 1] + mu + rnorm(1, 0, sigma)
  }
  outM[, i] <- x
  lines(x, col=rgb(0, 0.8, 1, alpha=0.03))
  x[1] <- HP.dat[1, ]
}
	
ci95 <- apply(outM, 1, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
lines(ci95[1,], lty = 1, col=rgb(0, 0.8, 1))
lines(ci95[3,], lty = 1, col=rgb(0, 0.8, 1))
lines(ci95[5,], lty = 1, col=rgb(0, 0.8, 1))
lines(ci95[2,], lty = 2, col=rgb(0, 0.8, 1))
lines(ci95[4,], lty = 2, col=rgb(0, 0.8, 1))
	
# forecast
x <- numeric(100)
outM <- matrix(0, 100, iters)
x[1] <- HP.dat[n, ]
for (i in 1:iters) {
  for (j in 2:100) {
    x[j] <- x[j - 1] + mu + rnorm(1, 0, sigma)
  }
  outM[, i] <- x
  lines(y=x, x=n:(n + 99), col=rgb(1, 0, 0.8, alpha = 0.03))
  x[1] <- HP.dat[n, ]
}
	
ci95 <- apply(outM, 1, quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
lines(x=n:(n + 99), y=ci95[1,], lty=1, col=rgb(1, 0, 0.8))
lines(x=n:(n + 99), y=ci95[3,], lty=1, col=rgb(1, 0, 0.8))
lines(x=n:(n + 99), y=ci95[5,], lty=1, col=rgb(1, 0, 0.8))
lines(x=n:(n + 99), y=ci95[2,], lty=2, col=rgb(1, 0, 0.8))
lines(x=n:(n + 99), y=ci95[4,], lty=2, col=rgb(1, 0, 0.8))



# 5. Autoregressive Models-----------------------------------------------------
	
	
# 5.1 Definition:
# A series {x[t]} is autoregressive of order p: AR(p) ->
# x[t] = alpha[1]x[t - 1] + alpha[2]x[t - 2] + ... + alpha[p]x[t - p] + w[t]
# with {w[t]} as white noise, and alphas as parameters;
# equivlently:
# theta[p](B)x[t] = w[t] = 
#	(1 - alpha[1]B - alpha[2]B^2 - ... - alpha[p]B^p)x[t]
# NOTES: 
#	AR(1) is a special case = the random walk
#	Exponential smoothing is the special case where 
#	alpha[i] = alpha(1 - alpha)^i for i = 1, 2, ..., and p -> Inf
 	
 	
# 5.3 Second-Order Properties of an AR(1) Model
# AR(1): x[t] = alpha*x[t - 1] + w[t]; w[t] ~N(0, sigma^2);
#	mean[x] = 0
#	cov[k] = alpha^k * sigma^2 / (1 - alpha^2)
	
	
# 5.5 Correlogram of an AR(1) process
rho <- function(k, alpha) alpha^k
layout(1:2)
plot(0:10, 
     rho(0:10, 0.7), 
     type='l', 
     xlab='k', 	
     ylab=expression(rho[k]), 
     main=expression(alpha == 0.7) )
plot(0:10, 
     rho(0:10, -0.7), 
     type='l', 
     xlab='k', 
     ylab=expression(rho[k]), 
     main=expression(alpha == -0.7) )

	
# 5.7 Simulation
x <- w <- rnorm(1000)
for(t in 2:1000) { x[t] <- 0.7*x[t - 1] + w[t] }
plot(x, type='l')
plot(x[1:100], type='l')
acf(x)
pacf(x)
	


# 6 Fitted Models--------------------------------------------------------------
	

# 6.1 Model fitted to simulated series
plot(x, type='l')
x.ar <- ar(x, method = 'mle')
x.ar$order	# 1
x.ar$ar	# 0.68 (cf. with 0.7 in the specified model above)
x.ar$ar + c(-2, 2)*c(sqrt(x.ar$asy.var)) # appx 95% CI [0.622, 0.716] 
										 # (includes 0.7)

	# 4.6.2 Exchange rate series: fitted AR model
	Z.ar = ar(Z.ts)
	mean(Z.ts)	# 2.82
	Z.ar$order	# 1 (means Z.ar is AR(1))
	Z.ar$ar	# coefficient for AR term = 0.89
	Z.ar$ar + c(-2, 2) * sqrt(Z.ar$asy.var)	# appx 95%CI = [0.74, 1.04]
	acf(Z.ar$res[-1])
	#The model can now be reconstructed as:
	# z.hat[t] = mean + coef(z[t-1] - mean) or 
	# z.hat[t] = 2.82 + 0.89(z[t-1] - 2.8)
	Z.ar
	
	par(mfrow = c(3,1))
	plot(Z.ts)
	abline(h = mean(Z.ts), col = 'grey')		
	# begins at 2.92; length = 39
	t = numeric(39)
	t[1] = 2.92

	for(i in 2:39){
		t[i] = 2.82 + 0.89 * (t[i-1] - 2.8) + rnorm(1, 0, sd(Z.ts))
	}
	plot(t, type = 'l')
	abline(h = mean(t), col='grey')
	
	for(i in 2:39){
		t[i] = 2.82 + 0.89 * (t[i-1] - 2.8) + rnorm(1, 0, sd(Z.ts))
	}
	plot(t, type = 'l')
	abline(h = mean(t), col='grey')

	# 4.6.3 Global temperature series: fitted AR model
	# Global = scan("http://www.massey.ac.nz/~pscowper/ts/global.dat")
	Global.ts = ts(Global, st = c(1856, 1), end = c(2005, 12), fr = 12)
	Global.ar = ar(aggregate(Global.ts, FUN = mean), method = 'mle')
	mean(aggregate(Global.ts, FUN = mean))	# -0.138
	Global.ar$order	# 4 = AR(4); regressive over prev 4 time steps
	Global.ar$ar 	# coefs: 0.588, 0.126, 0.111, 0.268
	acf(Global.ar$res[-(1:Global.ar$order)], lag = 50)
	plot(Global.ts)
	
	plot(aggregate(Global.ts, FUN = mean), ylim = c(-1.5, 1))
	t = numeric(length(aggregate(Global.ts, FUN = mean)))
	t[1:4] = aggregate(Global.ts, FUN = mean)[1:4]
	mu = mean(aggregate(Global.ts, FUN = mean))
	stdev = sd(aggregate(Global.ts, FUN = mean))	

	iters = 1000

	for (iter in 1:iters) {
		for (i in 5:length(t)) {
			t[i] = mu + 0.588 * (t[i - 1] - mu) + 0.013 * (t[i - 2] - mu) +
						0.111 * (t[i - 3] - mu) + 0.268 * (t[i - 4] - mu) +
						rnorm(1, 0, stdev)
		}
	
		lines(seq(1856, 2005, length = length(t)), t, col = rgb(1, 0, 0, 0.1))
	}
	lines(aggregate(Global.ts, FUN = mean))	