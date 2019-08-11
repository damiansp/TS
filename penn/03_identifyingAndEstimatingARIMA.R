#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/penn')


library(astsa)


erie <- scan(paste0('https://newonlinecourses.science.psu.edu/stat510/sites/',
                    'stat510/files/data/eriedata.dat'))
erie <- ts(erie)
par(mfrow=c(3, 1))
plot(erie, type='l')
acf(erie)
pacf(erie) # Good AR(1) candidate

sarima(erie, 1, 0, 0) # decent diagnostics, signif AR coef; AIC = 1.469
sarima(erie, 0, 0, 1) # resid autocorrelation; AIC = 1.726
sarima(erie, 1, 0, 1) # diagnosicts look good, bur more params, and AIC = 1.514

# Forecast sneak peek
par(mfrow=c(1, 1))
sarima.for(erie, 4, 1, 0, 0) # e.g. 4 steps ahead for AR(1)

erie.ar1 <- sarima(erie, 1, 0, 0)
acf(erie.ar1$fit$residuals)
pacf(erie.ar1$fit$residuals)
