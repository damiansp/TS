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



# 2. Examples Using Nile River Data
dat <- as.vector(Nile)
plot(dat, type='l')


# 2.1 Flat Level Method
# y[t] = µ + v[t]; v~N(0, r)
# As univariate state-space mod:
# x[t] = 1(x[t-1]) + 0 + w[t]; w ~ N(0, 0)
# y[t] = 1(x[t]) + 0 + v[t]; x[0] = µ
nile.mod.0 <- list(B=matrix(1), 
                   U=matrix(0), 
                   Q=matrix(0), 
                   Z=matrix(1), 
                   A=matrix(0), 
                   R=matrix('r'), 
                   x0=matrix('mu'), 
                   tinitx=0)
kem.0 <- MARSS(dat, model=nile.mod.0)
c(coef(kem.0,  type='vector'), LL=kem.0$logLik, AICc=kem.0$AICc)


# 2.2 Linear Trend in Flow
# x[t] = 1(x[t-1]) + u + w[t]; w~N(0, 0)
# y[t] = 1(x[t]) + 0 + v[t];   v~N(0, r); x[0]= µ
nile.mod.1 <- list(B=matrix(1), 
                   U=matrix('u'), 
                   Q=matrix(0), 
                   Z=matrix(1), 
                   A=matrix(0), 
                   R=matrix('r'), 
                   x0=matrix('mu'), 
                   tinitx=0)
kem.1 <- MARSS(dat, model=nile.mod.1)
c(coef(kem.1,  type='vector'), LL=kem.1$logLik, AICc=kem.1$AICc)


# 2.3 Stochastic Level Mod
# x[t] = x[t-1] + w[t]; w~N(0, q)
# y[t] = x[t] + v[t]; v~N(0, r)
# x[0] = µ
nile.mod.2 <- list(B=matrix(1), 
                   U=matrix(0), 
                   Q=matrix('q'), 
                   Z=matrix(1), 
                   A=matrix(0), 
                   R=matrix('r'), 
                   x0=matrix('mu'), 
                   tinitx=0)
kem.2 <- MARSS(dat, model=nile.mod.2)
c(coef(kem.2,  type='vector'), LL=kem.2$logLik, AICc=kem.2$AICc)


# 2.4 Stochastic Model with Drift
# x[t] = x[t-1] + u + w[t]; w~N(0, q)
# y[t] = x[t] + 0 + v[t]; v~N(0, r)
# x[0] = µ
nile.mod.3 <- list(B=matrix(1), 
                   U=matrix('u'), 
                   Q=matrix('q'), 
                   Z=matrix(1), 
                   A=matrix(0), 
                   R=matrix('r'), 
                   x0=matrix('mu'), 
                   tinitx=0)
kem.3 <- MARSS(dat, model=nile.mod.3)
c(coef(kem.3,  type='vector'), LL=kem.3$logLik, AICc=kem.3$AICc)



# 3. The StructTS Function
trees <- window(treering, start=0)
fitts <- StructTS(trees, type='level')
#fitem <- MARSS(as.vector(trees), nile.mod.2)
fitbf <- MARSS(as.vector(trees), method='BFGS')

