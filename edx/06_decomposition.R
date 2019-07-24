#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
#detach('package:dplyr')
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/edx')

library(TSA)

temp <- read.csv('../data/or_monthly_mean_temp_1895-2019.csv')
temp$Date <- paste(temp$Date, '01', sep='')
temp$Date <- as.Date(temp$Date, format='%Y%m%d')
temp$Month <- rep(1:12, nrow(temp) / 12)[1:nrow(temp)]
head(temp)

plot(temp$Date, temp$Value, type='l')
mod <- lm(Value ~ Date, data=temp)
abline(mod, col=2)
seas <- lm(Value ~ Date + as.factor(Month), data=temp)
summary(seas)

# Alternate Specification
temp <- ts(temp$Value, freq=12, start=c(1895, 1))
month <- season(temp)

mod1 <- lm(temp ~ month)
summary(mod1)

mod2 <- lm(temp ~ month - 1)
summary(mod2)


# Harmonic model for seasonality
har1 <- harmonic(temp, 1)
mod3 <- lm(temp ~ har1)
summary(mod3)

har2 <- harmonic(temp, 2)
mod4 <- lm(temp ~ har2)
summary(mod4) # harXcos(2pi*t) = cos(2pi*t / (X / 2))