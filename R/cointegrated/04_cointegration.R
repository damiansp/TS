#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/cointegrated')

#library(dse)
#library(fracdiff)
library(lmtest)



# 1. Spurious Regression
n <- 500
e1 <- rnorm(n)
e2 <- rnorm(n)
trend <- 1:n
y1 <- 0.8*trend + cumsum(e1)
y2 <- 0.6*trend + cumsum(e2)
spurious.reg <- lm(y1 ~ y2)
summary(spurious.reg)
spurious.durbin.watson <- dwtest(spurious.reg)
spurious.durbin.watson # should be suspicious if R^2 > DW (or sig. p)
