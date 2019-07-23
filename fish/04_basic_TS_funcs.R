#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/fish')

library(atsalibrary)
library(datasets)
library(devtools)
library(forecast)
library(MARSS)
library(stats)
#devtools::install_github('nwfsc-timeseries/atsalibrary')

data(hourlyphyto)
data(MLCO2)
data(NHTemp)