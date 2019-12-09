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


# Code 6.2 VaR and ES Derived from GHP, HYP and NIG
# probs
p <- seq(0.001, 0.05, 0.001)

# VaR
ghd.VaR <- abs(qghyp(p, ghd.fit))
hyp.VaR <- abs(qghyp(p, hyp.fit))
nig.VaR <- abs(qghyp(p, nig.fit))
nor.VaR <- abs(qnorm(p, mean=mean(yret), sd=sd(c(yret[, 1]))))
emp.VaR <- abs(quantile(x=yret, probs=p))

# VaR Plot
plot(emp.VaR, 
     type='l', 
     xlab='', 
     ylab='Value at Risk', 
     axes=F, 
     ylim=range(c(hyp.VaR, nig.VaR, ghd.VaR, nor.VaR, emp.VaR)))
box()
axis(1, at=seq(along=p), labels=names(emp.VaR), tick=F)
axis(2, at=pretty(range(hyp.VaR, nig.VaR, ghd.VaR, nor.VaR, emp.VaR)))
lines(seq(along=p), ghd.VaR, col=2)
lines(seq(along=p), hyp.VaR, col=4)
lines(seq(along=p), nig.VaR, col=3)
lines(seq(along=p), nor.VaR, col=5)
legend('topright', 
       legend=c('Empirical', 'GHD', 'HYP', 'NIG', 'Normal'),
       col=c(1, 2, 4, 3, 5),
       lty=1)

# ES
ghd.ES <- abs(ESghyp(p, ghd.fit))
hyp.ES <- abs(ESghyp(p, hyp.fit))
nig.ES <- abs(ESghyp(p, nig.fit))
nor.ES <- abs(mean(yret) - sd(c(yret[, 1])) * dnorm(qnorm(1 - p))/p)
obs.p <- ceiling(p * length(yret))
emp.ES <- sapply(obs.p, function(x) abs(mean(sort(c(yret))[1:x])))

# ES Plot
plot(emp.ES,
     type='l',
     xlab='',
     ylab='ES',
     axes=F,
     ylim=range(hyp.ES, nig.ES, ghd.ES, nor.ES, emp.ES))
box()
axis(1, at=1:length(p), labels=names(emp.VaR))
axis(2, at=pretty(range(hyp.ES, nig.ES, ghd.ES, nor.ES, emp.ES)))
lines(1:length(p), ghd.ES, col=2)
lines(1:length(p), hyp.ES, col=4)
lines(1:length(p), nig.ES, col=3)
lines(1:length(p), nor.ES, col=5)
legend('topright', 
       legend=c('Empirical', 'GHD', 'HYP', 'NIG', 'Normal'),
       col=c(1, 2, 4, 3, 5),
       lty=1)
