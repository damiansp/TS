#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/penn')


library(astsa)



# 1. Decomposition Models
beer <- scan('../data/beerprod.dat')
beer <- ts(beer, freq=4)
beer.decomp <- decompose(beer, type='additive')
plot(beer.decomp)
beer.decomp

beer.decomp.mult <- decompose(beer, type='multiplicative')
plot(beer.decomp.mult)

# Lowess Seasonal and Trend Decomposition
beer.decomp.lowess <- stl(beer, 'periodic')
plot(beer.decomp.lowess)



# 2. Smoothing Time Series