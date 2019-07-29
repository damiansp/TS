#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
#detach('package:dplyr')
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/cointegrated')

library(dse)
library(vars)



# 2. Vector Autoregressive Models


# 2.1. Specification, Assumptions, and Estimation
# Simulate a VAR(2) process
# Setting the lag-polynomial A(L)
Apoly <- array(c(1.0, -0.5, 0.3, 0, 0.2, 0.1, 0, -0.2, 0.7, 1, 0.5, -0.3),
               c(3, 2, 2))