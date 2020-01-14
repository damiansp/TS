#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/R/frapo')

library(evir)
#library(fBasics)
library(fExtremes)
#library(FRAPO)
library(ismev)

data(siemens)


sie.loss <- -100.0 * siemens
sie.gev <- gev(sie.loss, block='semester')
sie.gev
plot(sie.gev, 
     type='h', 
     col=4, 
     main='Max Biannual Loss for Siemens') # opt 1
     
sie.gev2 <- gev.fit(sie.gev$data)
sie.gev2
gev.diag(sie.gev2)
par(mfrow=c(2, 1))
gev.prof(sie.gev2, m=20, xlow=5, xup=16, conf=0.95)
gev.profxi(sie.gev2, xlow=0, xup=0.7, conf=0.95)
m.loss <- max(sie.gev$data)
m.years <- (1 
            / (1 - pgev(m.loss, 
                        mu=sie.gev2$mle[1], 
                        sigma=sie.gev2$mle[2], 
                        xi=sie.gev2$mle[3])) 
            / 2)

sie.gev3 <- gevFit(sie.gev$data, type='pwm')
sie.gev3