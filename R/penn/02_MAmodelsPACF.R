#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/penn')


library(astsa)


# 2 PACF
ma1pacf <- ARMAacf(ma=c(0.7), lag.max=36, pacf=T)
plot(ma1pacf, type='h', main='Theoretical PACF for MA(1) with theta=0.7')