#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/R/penn')
DATA <- '../../data'

library(astsa)
library(fGarch)
library(vars)



# 1. ARCH/GARCH
n <- 300
vars <- numeric(n)
vars[1] <- 5
x <- numeric(n)
x[1] <- 0
for (t in 2:n) {
  incr <- sample(c(1, 0), size=1, prob=c(0.02, 0.98))
  if (incr) { vars[t] <- vars[t - 1] + 2 }
  else { vars[t] <- 0.98*vars[t - 1] }
  vars[t] <- abs(vars[t] + rnorm(1, sd=1))
  x[t] <- rnorm(1, sd=sqrt(vars[t - 1]))
}

plot(vars, type='l')
plot(x, type='l')
acf(x)
pacf(x)
acf(x^2)
pacf(x^2)

y <- x - mean(x)
x.g <- garchFit(~garch(1, 1), y, include.mean=F)
summary(x.g)



# 2. VAR(p) Models
x <- cbind(cmort, tempr, part)
plot.ts(x, main='', xlab='')
fitvar1 <- VAR(x, p=1, type='both')
summary(fitvar1)

fitvar2 <- VAR(x, p=2, type='both')
summary(fitvar2)
acf(residuals(fitvar2)[, 1]) # 1 = cmort model
# view all
acf(residuals(fitvar2))