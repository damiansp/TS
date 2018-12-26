#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/coursera')

# SARIMA
n <- 10000
x <- numeric(n)
z <- rnorm(n)
x[1:13] <- 1

for (i in 14:n) {
  x[i] <- z[i] + 0.7*z[i - 1] + 0.6*z[i - 12] + 0.42*z[i - 13]
}

par(mfrow=c(3, 1))
plot.ts(x[12:120], main='First 12 months of SARIMA(0, 0, 1, 0, 0, 1)_12', ylab='')
acf(x)
pacf(x)