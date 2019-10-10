#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/R/frapo')

library(evir)
library(fBasics)
library(FRAPO)
library(zoo)
data(EuStockMarkets)
data(siemens)



# 1. Stylized Facts about Financial Market Returns


# 1.1 Stylized Facts for Univariate Series
head(siemens)
class(attr(siemens, 'times'))
#sie.dates <- as.character(format(as.POSIXct(attr(siemens, 'times')), '%Y-%m-%d'))
sie.dates <- as.character(attr(siemens, 'times'))
sie.ret <- timeSeries(siemens * 100, charvec=sie.dates)
colnames(sie.ret) <- 'SieRet'
head(sie.ret)

# Code 3.1 Stylized Facts on Returns for Siemens
# Stylized Facts 1
par(mfrow=c(2, 2))
seriesPlot(sie.ret, title=F, main='Daily Returns of Siemens', col=4)
boxPlot(sie.ret, title=F, main='Daily Returns of Siemens', col=4)
acf(sie.ret, main='ACF of Returns', lag.max=20, ylab='', xlab='', col=4, ci.col=2)
pacf(
  sie.ret, main='PACF of Returns', lag.max=20, ylab='', xlab='', col=4, ci.col=2)

# Stylized Facts 2
sie.ret.abs <- abs(sie.ret)
sie.ret.100 <- tail(sort(abs(series(sie.ret))), 100)[1]
idx <- which(series(sie.ret.abs) > sie.ret.100, arr.ind=T)
sie.ret.abs.100 <- timeSeries(rep(0, length(sie.ret)), charvec=time(sie.ret))
sie.ret.abs.100[idx, 1] <- sie.ret.abs[idx]
acf(sie.ret.abs, 
    main='ACF of Absolute Returns', 
    lag.max=20, 
    xlab='', 
    ylab='', 
    col=4, 
    ci.col=2)
pacf(sie.ret.abs, 
     main='PACF of Absolute Returns', 
     lax.max=20, 
     xlab='', 
     ylab='', 
     col=4, 
     ci.col=2)
qqnormPlot(sie.ret, main='QQ-Plot of Returns', title=F, col=4, cex=0.5, pch=19)
plot(sie.ret.abs.100, 
     type='h', 
     main='Volatility Clustering', 
     ylab='', 
     xlab='', 
     col=4)
     
# 3.1.2 Stylized Facts for Multivariate Series
# Code 3.2 Stylized Facts on the European Equity Market
par(mfrow=c(1, 1))
eu.stock.level <- as.zoo(EuStockMarkets)[, c('DAX', 'CAC', 'FTSE')]
eu.stock.ret <- 100 * diff(log(eu.stock.level))
plot(eu.stock.level, xlab='', main='')
plot(eu.stock.ret, xlab='', main='')
layout(matrix(1:6, nrow=3, ncol=2, byrow=T))
ccf(eu.stock.ret[, 1], 
    eu.stock.ret[, 2], 
    ylab='', 
    xlab='', 
    lag.max=20, 
    main='DAX vs CAC Returns')
ccf(abs(eu.stock.ret[, 1]), 
    abs(eu.stock.ret[, 2]), 
    ylab='', 
    xlab='', 
    lag.max=20, 
    main='DAX vs CAC Abs. Returns')
ccf(eu.stock.ret[, 1], 
    eu.stock.ret[, 3], 
    ylab='', 
    xlab='', 
    lag.max=20, 
    main='DAX vs FTSE Returns')
ccf(abs(eu.stock.ret[, 1]), 
    abs(eu.stock.ret[, 3]), 
    ylab='', 
    xlab='', 
    lag.max=20, 
    main='DAX vs FTSE Abs. Returns')
ccf(eu.stock.ret[, 2], 
    eu.stock.ret[, 3], 
    ylab='', 
    xlab='', 
    lag.max=20, 
    main='CAC vs FTSE Returns')
ccf(abs(eu.stock.ret[, 2]), 
    abs(eu.stock.ret[, 3]), 
    ylab='', 
    xlab='', 
    lag.max=20, 
    main='CAC vs FTSE Abs. Returns')

# Rolling Correlations
rollc <- function(x) {
  dim <- ncol(x)
  rcor <- cor(x)[lower.tri(diag(dim), diag=F)]
  rcor
}

rcor <- rollapply(eu.stock.ret, width=250, rollc, align='right', by.column=F)
colnames(rcor) <- c('DAX-CAC', 'DAX-FTSE', 'CAC-FTSE')
plot(rcor, main='', xlab='')