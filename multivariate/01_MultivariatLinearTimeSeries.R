#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/TS/multivariate')

library(MTS)
library(mvtnorm)

# 1. Introduction
# 3 - 4. CCM (Cross-Correlation Matrix)
sig <- diag(2) # I[2]
x <- rmvnorm(300, rep(0, 2), sig)
MTSplot(x)
ccm(x)



# 5. Testing Zero Cross-Correlations
dat <- read.table('data/q-gdpunemp.txt', header=T)
head(dat)

x <- cbind(diff(dat$gdp), diff(dat$rate))
mq(x, lag=10) # series not independent for any lag

# Sim study
sig <- diag(3)
z <- rmvnorm(200, rep(0, 3), sig)
mq(z, 10) # no signif. cross correlation



# 6. Forecasting


