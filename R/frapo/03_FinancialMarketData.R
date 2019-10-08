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
data(siemens)



# 1. Stylized Facts about Financial Market Returns


# 1.1 Stylized Facts for Univariate Series
head(siemens)
attr(siemens, 'times')
class(attr(siemens, 'times'))
#sie.dates <- as.character(format(as.POSIXct(attr(siemens, 'times')), '%Y-%m-%d'))
sie.dates <- as.character(attr(siemens, 'times'))
sie.ret <- timeSeries(siemens * 100, charvec=sie.dates)
colnames(sie.ret) <- 'SieRet'
head(sie.ret)

# Stylized Facts 1
par(mfrow=c(2, 2))
seriesPlot(sie.ret, title=F, main='Daily Returns of Siemens', col=4)