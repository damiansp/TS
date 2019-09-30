#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/basic')
options(digits=5)
library(MASS)

DATA <- paste0("https://raw.githubusercontent.com/dallascard/",
               "Introductory_Time_Series_with_R_datasets/master/")



# 2 Leading Variables and Associated Variables


# 2.2 Building approvals publication
build <- read.table(paste(DATA, 'ApprovActiv.dat', sep=''), header=T)
App.ts <- ts(build$Approvals, start=c(1996, 1), freq=4)
Act.ts <- ts(build$Activity, start=c(1996, 1), freq=4)
ts.plot(App.ts, Act.ts, col=1:2)

# Cross-Correlation

acf(ts.union(App.ts, Act.ts))

app.ran <- decompose(App.ts)$random
app.ran.ts <- window(app.ran, start=c(1996,3))
act.ran <- decompose(Act.ts)$random
act.ran.ts <- window(act.ran, start=c(1996,3))
plot(app.ran.ts)
lines(act.ran.ts, col=2)
dim(ts.union(app.ran.ts, act.ran.ts))
acf(ts.union(app.ran.ts, act.ran.ts)[-(40:41), ])
ccf(app.ran.ts[-(40:41)], act.ran.ts[-(40:41)])
print(acf(ts.union(app.ran.ts, act.ran.ts)[-(40:41), ]))



# 3 Bass Model


# 3.2 Model Definition
# @param n0	number sold at time = 0
# @param m	total number ultimately sold
# @param p	coef. of innovation (how novel the item is)
# @param q	coef. of imitation (how likely/quickly consumers are to mimic)
# @param periods no. of time steps to plot/calculate
bass <- function(n0, m, p, q, periods) {
  n <- numeric(periods)
  n[1] <- n0
  for (t in 2:periods) {
    n[t] <- n[t - 1] + p*(m - n[t - 1]) + q*n[t - 1]*(m - n[t - 1]) / m
  }
  n
}
	
n0 <- 10 	# no. users at time 0
m <- 1000	# total n who will ultimately buy
p <- 0.9	# coef. of innovation
q <- 0.5 	# coef. of imitiation
plot(bass(n0, m, p, q, 30), type='l')
p <- 0.2
lines(bass(n0, m, p, q, 30), type='l', col=2)
q <- 0.2
lines(bass(n0, m, p, q, 30), type='l', col=3)
p <- 0.001
q <- 0.7
lines(bass(n0, m, p, q, 30), type='l', col=4)
p <- 0.7
q <- 0.001
lines(bass(n0, m, p, q, 30), type='l', col=5)
p <- 0.01
q <- 0.01
lines(bass(n0, m, p, q, 30), type='l', col=6)

p <- 0
q <- 0
plot(bass(n0, m, p, q, 20), type='l', ylim=c(0, 1000))
p <- 1	
lines(bass(n0, m, p, q, 20), type='l', col=2)
q <- 1
lines(bass(n0, m, p, q, 20), type='l', col=3)
p <- 0
lines(bass(n0, m, p, q, 20), type='l', col=4)
	
	
# 3.4 Example	
T79 <- 1:10
Tdelt <- (1:100) / 10
Sales <- c(840, 1470, 2110, 4000, 7590, 10950, 10530, 9470, 7790, 5890)
Cusales <- cumsum(Sales)
Bass.nls <- nls(
  Sales ~ M 
    * (((P + Q)^2 / P) * exp(-(P + Q) * T79)) 
    / (1 + (Q / P) * exp(-(P + Q) * T79))^2, 
  start=list(M=60630, P=0.03, Q=0.38))
summary(Bass.nls)

Bcoef <- coef(Bass.nls)
m <- Bcoef[1]
p <- Bcoef[2]
q <- Bcoef[3]
ngete <- exp(-(p + q) * Tdelt)
Bpdf <- m * (( p + q)^2 / p) * ngete / (1 + (q / p) * ngete)^2
plot(Tdelt, Bpdf, xlab='Years (since 1979)', ylab='Sales per year', type='l')
points(T79, Sales)
Bcdf <- m* (1 - ngete) / (1 + (q / p)*ngete)
plot(Tdelt, Bcdf, col=2, type='l')
lines(Tdelt, Bpdf, xlab='Years (since 1979)', ylab='Cumulative sales')
points(T79, Sales)
points(T79, Cusales, col=2)



# 4 Exponential Smoothing & the Holt-Winters Method


# 4.1 Exponential smoothing
Motor.dat <- read.table(paste(DATA, 'motororg.dat', sep=''), header=T)
head(Motor.dat)
mean(Motor.dat$complaints) #19.4
Comp.ts <- ts(Motor.dat$complaints, start=c(1996, 1), freq=12)
plot.ts(Comp.ts, xlab='Time (months)', ylab='Complaints')

Comp.hw1 <- HoltWinters(Motor.dat$complaints, beta=F, gamma=F)
plot(Comp.hw1)
Comp.hw1		 # a = smoothed mean = 17.7
Comp.hw1$SSE # 2502

# force alpha val
Comp.hw2 <- HoltWinters(Motor.dat$complaints, alpha=0.2, beta=F, gamma=F) 
Comp.hw2		 # a = 18.0
Comp.hw2$SSE # 2526--a substantial increase in error
lines(Comp.hw2$fitted[,'xhat'], col=4)


# 4.2 Holt-Winters Method
wine.dat <- read.table(paste(DATA, 'wine.dat', sep=''), header=T)
sweetw.ts <- ts(wine.dat$sweetw, start=c(1980,1), freq=12)
plot(sweetw.ts, xlab='Time (months)', ylab='Sales (kL)')
plot(decompose(sweetw.ts))
sweetw.hw <- HoltWinters(sweetw.ts, seasonal='mult')
sweetw.hw
sweetw.hw$coef
sweetw.hw$SSE	# 477,693.9
sqrt(sweetw.hw$SSE / length(wine.dat$sweetw))	# 50.542
sd(wine.dat$sweetw)	# 121
plot(sweetw.hw$fitted)
plot(sweetw.hw)


# 4.3 Four-Year Ahead Forecasts for the Air Passenger Data
AP <- AirPassengers
AP.hw <- HoltWinters(AP, seasonal="mult")
plot(AP.hw)
AP.predict <- predict(AP.hw, n.ahead=4*12)
ts.plot(AP, AP.predict, col=1:2)











