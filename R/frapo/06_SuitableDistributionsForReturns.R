#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/R/frapo')

library(fBasics)
library(FRAPO)
library(ghyp)
library(lmomco)
library(timeSeries)

data(DowJones30)
data(SP500)


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


# 6.3 Stylized Facts Revisited
# Code 6.3 Shape Triangle for HYP Distribution
rd <- c(1, 5, 10, 20, 40)
y.rets <- na.omit(
  matrix(unlist(lapply(rd, function(x) { diff(log(y), lag=x) })),
         ncol=5))

# Get xi/chi coefs
chi.xi <- function(x) {
  param <- coef(x, type='alpha.delta')
  rho <- param[['beta']] / param[['alpha']]
  zeta <- param[['delta']] + sqrt(param[['alpha']]^2 - param[['beta']]^2)
  xi <- 1 / sqrt(1 + zeta)
  chi <- xi * rho
  res <- c('chi'=chi, 'xi'=xi)
}

# HYP Fitting
hyp.fits <- apply(y.rets, 2, fit.hypuv, symmetric=F)
pts <- matrix(unlist(lapply(hyp.fits, chi.xi)), ncol=2, byrow=T)

# Shape Triangle
col.def <- c('black', 'blue', 'red', 'darkgreen', 'orange')
leg.def <- c(paste(rd, rep('day return', 5)))
plot(pts, 
     ylim=c(-0.2, 1.2), 
     xlim=c(-1.2, 1.2), 
     col=col.def, 
     pch=16, 
     ylab=expression(xi), 
     xlab=expression(chi))
lines(x=c(0, -1), y=c(0, 1))
lines(x=c(0, 1), y=c(0, 1))
lines(x=c(-1, 1), y=c(1, 1))
legend('bottomright', legend=leg.def, col=col.def, pch=16)
text(x=0, y=1.05, label='LaPlace')
text(x=-1, y=1.05, label='Exponential')
text(x=1, y=1.05, label='Exponential')
text(x=0, y=-0.1, label='Normal')
text(x=-0.6, y=0.5, label='Left-skewed Hyperbolic', srt=302)
text(x=0.6, y=0.5, label='Right-skewed Hyperbolic', srt=57)



# 7. Applications of the GLD to Risk Modeling and Data Analysis


# 7.1 VaR for a Single Stock
idx <- SP500[, 'QCOM']
L <- -1 * returnseries(idx, method='discrete', trim=T)

# Compute VaR (Normal & GLD) 99%; Moving Window
WEEKS <- 52
ep <- (2 * WEEKS):length(L)
sp <- 1:length(ep)
level <- 0.99
VaR <- matrix(NA, ncol=2, nrow=length(ep))
for (i in 1:length(sp)) {
  x <- L[sp[i]:ep[i]]
  lmom <- lmom.ub(x)
  fit <- pargld(lmom)
  VaR.Gld <- quagld(level, fit)
  Var.nor <- qnorm(level, mean(x), sd(x))
  VaR[i, ] <- c(VaR.Gld, Var.nor)
  print(paste('Results for', ep[i], ':', VaR.Gld, 'and', Var.nor))
}

# Summarize Results
res <- cbind(L[(2*WEEKS + 1):length(L)], VaR[-nrow(VaR), ])
colnames(res) <- c('Loss', 'VaR.Gld', 'VaR.norm')

# Plot and Backtest
plot(res[, 'Loss'], 
     xlab='Time', 
     ylab='Loss (%)', 
     type='l',
     ylim=c(-15, max(res)))
abline(h=0, col='grey')
lines(res[, 'VaR.Gld'], col=4, lwd=2)
lines(res[, 'VaR.norm'], col=2, lwd=2)
legend('bottomright', 
       legend=c('Losses', 'VaR GLD', 'VaR Normal'),
       col=c(1, 4, 2),
       lty=1,
       lwd=c(1, 2, 2),
       bty='n')