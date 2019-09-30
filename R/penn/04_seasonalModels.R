#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/penn')


library(astsa)
DATA <- '../data'


# 2. Identifying Seasonal Models and R Code
flow <- ts(scan(paste(DATA, 'coloradoflow.dat', sep='/')))
plot(flow)

diff12 <- diff(flow, 12)
acf(diff12, lag.max=48)
sarima(flow, 1, 0, 0, 0, 1, 1, 12)
sarima.for(flow, 24, 1, 0, 0, 0, 1, 1, 12)

# without astsa
mod <- arima(flow, order=c(1, 0, 0), seasonal=list(order=c(0, 1, 1), period=12))
mod
predict(mod, n.ahead=24)

flow.m <- matrix(flow, ncol=12, byrow=T)
col.means <- apply(flow.m, 2, mean)
plot(col.means, 
     type='l', 
     main='Monthly Means for Flow', 
     xlab='Month', 
     ylab='Mean Flow')
     
CBE <- read.table(
  "https://raw.githubusercontent.com/dallascard/Introductory_Time_Series_with_R_datasets/master/cbe.dat", 
  header=T)
beer <- ts(CBE[, 2], start=1958, freq=12)
plot.ts(beer)
length(beer)

beer.monthly <- numeric(length(beer) / 4)
t2 <- 1
for (t in seq(1, length(beer), by=3)) {
  beer.monthly[t2] <- sum(beer[t:(t + 2)])
  t2 <- t2 + 1
}
ts(beer.monthly, start=1958, freq=4)
plot.ts(beer.monthly, type='b')
diff4 <- diff(beer.monthly, 4)
diff1and4 <- diff(diff4, 1)
acf(diff1and4)

dtb <- resid(lm(diff4 ~ time(diff4)))
plot.ts(dtb)
acf(dtb)

sarima(dtb, 0, 0, 1, 0, 0, 1, 4)
sarima(dtb, 1, 0, 0, 0, 0, 1, 4)
sarima(dtb, 0, 0, 0, 0, 0, 1, 4)
sarima(dtb, 1, 0, 0, 0, 0, 2, 4) # all bad (underlying dataset diff fr. tutorial)