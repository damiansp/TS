#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/multivariate')

library(datasets)
data(lynx)
data(WWWusage)



# 1. Examples of TS
plot.ts(WWWusage)
plot.ts(lynx)