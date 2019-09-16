#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/fish')

library(atsalibrary)
library(forecast)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(tseries)
library(urca)
#library(datasets)

data(greeklandings)
data(chinook)

landings <- greeklandings
chinook <- chinook.month



# 1. Box-Jenkins Method
#    A) Model Form Selection
#       1) Evaluate Stationarity
#       2) Selection of Differencing Level (d) to Fix Stationarity
#       3) Selection of AR level (p)
#       4) Selection of MA level (q)
#    B) Param Estimation
#    C) Model Checking