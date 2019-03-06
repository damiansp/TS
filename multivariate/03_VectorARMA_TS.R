#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/multivariate')

library(MTS)



# 1 Vector MA Models


# 1.1 VMA(1) Model
data <- read.table('data/m-dec125910-6111.txt', header=T)
head(data)
PERIODS <- 12
START_YEAR <- 1961
x <- 100 * log(data[, 2:6] + 1)
rtn <- cbind(x$dec5, x$dec9)
tdx <- c(1:nrow(rtn)) / PERIODS + START_YEAR
par(mfcol=c(2, 1))
plot(tdx, rtn[, 1], type='l', xlab='year', ylab='d5')
plot(tdx, rtn[, 2], type='l', xlab='year', ylab='d9')
ccm(rtn)