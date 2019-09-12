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
