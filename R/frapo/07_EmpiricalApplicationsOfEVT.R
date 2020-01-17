#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/R/frapo')

library(evir)
library(fBasics)
library(fExtremes)
#library(FRAPO)
library(ismev)

data(bmw)
data(DowJones30)
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



# Code 7.2: R-Block Maxima for BMW Losses
bmw.loss <- -1.0 * bmw * 100
years <- format(attr(bmw.loss, 'time'), '%Y')
attr(bmw.loss, 'years') <- years
year.u <- unique(years)
idx <- 1:length(year.u)
r <- 2 # largest no. of losses per year to model
bmw.order <- t(
  sapply(
    idx, 
    function(x) { 
      head(sort(bmw.loss[attr(bmw.loss, 'years') == year.u[x]], decreasing=T),
           r)
    }))
rownames(bmw.order) <- year.u
colnames(bmw.order) <- paste('r', 1:r, sep='')
head(bmw.order)

plot(year.u, 
     bmw.order[, 1], 
     ylim=range(bmw.order), 
     ylab='BMW Losses (%)', 
     xlab='', 
     pch=16)
points(year.u, bmw.order[, 2], col=4, pch=18)

# mle & se are for mu, sigma, and xi respectively
bmw.order.fit <- rlarg.fit(bmw.order)
rlarg.diag(bmw.order.fit)



# Code 7.3 POT GDP for Boeing Losses
dj <- timeSeries(DowJones30[, -1], charvec=as.character(DowJones30[, 1]))
ba.loss <- -1.0 * returns(dj[, 'BA'], percentage=T, trim=T)

# MRL-Plot (Mean Residual Life)
mrlPlot(ba.loss, umin=-10, umax=10)

# GPD
ba.fit <- gpdFit(ba.loss, u=3)
par(mfrow=c(2, 2))
for (i in 1:4) {
  plot(ba.fit, which=i)	
}

gpdRiskMeasures(ba.fit, prob=c(0.95, 0.99, 0.995))
