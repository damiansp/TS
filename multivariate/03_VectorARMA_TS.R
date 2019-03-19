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



# 2 Specifying VMA Order
# VMA order specification
VMAOrder(data$dec1, lag=20)



# 3 Estimation of VMA Models


# 3.1 Conditional Likelihood Estimation
m1 <- VMA(rtn, q=1)
MTSdiag(m1)
r1 <- m1$residuals
mq(r1, adj=4)


# 3.2 Exact Likelihood Estimation