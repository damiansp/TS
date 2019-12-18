#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/R/fish')

library(atsalibrary)
library(datasets)
library(forecast)
library(MARSS)
library(stats)



# 1. Fitting a State-Space Model with MARSS
# MARSS fits multivariate AR models of the form:
# x[t] = Bx[t - 1] + u + w[t] ; (w[t] ~ N(0, Q))
# y[t] = Zx[t] + a + v[t]     ; (v[t] ~ N(0, R))
# x[0] = mu
# y are the data; 
# B, Z, u, a, Q, R are params to be estimated
# x are hidden states

mod.list <- list(B=matrix(1), 
                 U=matrix(0), 
                 Q=matrix('q'), 
                 Z=matrix(1), 
                 A=matrix(0), 
                 R=matrix('r'), 
                 x0=matrix('mu'), 
                 tinitx=0)
# Sim AR(1)
q <- 0.1
r <- 0.1
n <- 100
y <- cumsum(rnorm(n, 0, sqrt(q))) + rnorm(n, 0, sqrt(r))
plot(y, type='l')
fit <- MARSS(y, model=mod.list)

# If you want to fix q = 0.1 -> Q = [0.1]:
mod.list$Q <- matrix(0.1)
fit <- MARSS(y, model=mod.list)