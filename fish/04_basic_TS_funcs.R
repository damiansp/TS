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



# 1. TS Plots


# 1.1. ts Objects and plot.ts()
head(MLCO2)
co2 <- ts(MLCO2$ppm, frequency=12, start=c(MLCO2[1, 'year'], MLCO2[1, 'month']))
plot.ts(co2, xlab=expression(paste('CO'[2],' (ppm)')))


# 1.2. Combining and Plotting Multiple ts Objects
temp.ts <- ts(NHTemp$Value, frequency=12, start=c(1880, 1))
dat.I <- ts.intersect(co2, temp.ts)
dim(dat.I)

dat.U <- ts.union(co2, temp.ts)
dim(dat.U)

plot(dat.I, main='')
plot(dat.U, main='')