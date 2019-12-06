#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/R/frapo')

library(fBasics)
library(ghyp)
library(timeSeries)

data(DowJones30)



# 6. Applications of the GHD to Risk Modeling


# 6.1 Fitting Stock Returns to GHD

# Code 6.1: Fitting HPW Returns to the GHD
head(DowJones30)
ticker <- 'MSFT'
MAXIT <- 1000
y <- timeSeries(DowJones30[, ticker], charvec=as.character(DowJones30[, 1]))
yret <- na.omit(diff(log(y)) * 100)

# Fit
ef <- density(yret)
ghd.fit <- fit.ghypuv(yret, symmetric=F, control=list(maxit=MAXIT))
hyp.fit <- fit.hypuv(yret, symmetric=F, control=list(maxit=MAXIT))
nig.fit <- fit.NIGuv(yret, symmetric=F, control=list(maxit=MAXIT))

# Densities
ghd.dens <- dghyp(ef$x, ghd.fit)
hyp.dens <- dghyp(ef$x, hyp.fit)
nig.dens <- dghyp(ef$x, nig.fit)
nor.dens <- dnorm(ef$x, mean=mean(yret), sd=sd(c(yret[, 1])))
col.def <- c('black', 'red', 'blue', 'green', 'cyan')
plot.densities <- function(ylim=c(0, 0.21)) {
  plot(ef, xlab='', ylab=expression(f(x)), ylim=ylim) 
  lines(ef$x, ghd.dens, col='red')
  lines(ef$x, hyp.dens, col='blue')
  lines(ef$x, nig.dens, col='green')
  lines(ef$x, nor.dens, col='cyan')
  legend('topleft', 
         legend=c('Empirical', 'GHP', 'HYP', 'NIG', 'Norm'), 
         col=col.def, 
         lty=1)	
}
plot.densities()
# zoom in on tails
plot.densities(ylim=c(0, 0.01))

# QQ
qqghyp(ghd.fit, line=T, ghyp.col=2, plot.legend=F, gaussian=F, main='', cex=0.8)
qqghyp(hyp.fit, add=T, ghyp.col=4, gaussian=F, line=F, cex=0.8)
qqghyp(nig.fit, add=T, ghyp.col=3, gaussian=F, line=F, cex=0.8)
legend('topleft', 
       legend=c('GHP', 'HYP', 'NIG'), 
       col=c(2, 4, 3), 
       pch=1)

# Diagnostics
AIC <- stepAIC.ghyp(yret, 
                    dist=c('ghyp', 'hyp', 'NIG'), 
                    symmetric=F, 
                    control=list(maxit=MAXIT))
AIC
(LR.ghd.nig <- lik.ratio.test(ghd.fit, nig.fit)) # nig does better than hyp, but
(LR.ghd.hyp <- lik.ratio.test(ghd.fit, hyp.fit)) # ghd does the best of all
# p < 0.05: reject hypothesis that both mods have equal explanatory power






