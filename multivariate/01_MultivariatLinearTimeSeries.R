#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/TS/multivariate')

library(MTS)
library(mvtnorm)

# 1. Introduction
# 1.4 CCM (Cross-Correlation Matrix)
sig <- diag(2) # I[2]
x <- rmvnorm(300, rep(0, 2), sig)

??rmvnorm