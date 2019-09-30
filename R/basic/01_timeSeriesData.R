rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/basic')
options(digits=5)

library(MASS)

data(AirPassengers)



# 4 Plots, Trends, and Seasonal Variation


# 4.1 A flying start: air passenger bookings
#data(AirPassengers)
AP <- AirPassengers
AP
class(AP) # ts
start(AP)
end(AP)
frequency(AP)
summary(AP)
plot(AP, ylab="Passengers (in 1000s)")

layout(1:2)
plot(aggregate(AP))
boxplot(AP ~ cycle(AP))
	

# 4.2 Unemployment: Maine
Maine.month <- read.table( 
  'https://raw.githubusercontent.com/svkerr/R_Files/master/TimeSeries/Maine.dat', 
  header=T)
class(Maine.month)
head(Maine.month)
Maine.month.ts <- ts(Maine.month$unemploy, start=c(1996, 1), freq=12)
Maine.annual.ts <- aggregate(Maine.month.ts) / 12
	
layout(1:2)
plot(Maine.month.ts, ylab="% Unemployed")
plot(Maine.annual.ts, ylab="% Unemployed")
Maine.Feb <- window(Maine.month.ts, start=c(1996,2), freq=T)
Maine.Aug <- window(Maine.month.ts, start=c(1996,8), freq=T)
(Feb.ratio <- mean(Maine.Feb) / mean(Maine.month.ts))
(Aug.ratio <- mean(Maine.Aug) / mean(Maine.month.ts))

US.month <- read.table( 
  "https://raw.githubusercontent.com/dallascard/Introductory_Time_Series_with_R_datasets/master/USunemp.dat", 
  header=T)
US.month.ts <- ts(US.month$USun, start=c(1996, 1), end=c(2006,10), freq=12)
layout(1)
plot(US.month.ts, ylab="% Unemployed", ylim=range(Maine.month.ts))
lines(Maine.month.ts, col=2)
lines(Maine.annual.ts, col=2)

	
# 4.3 Multiple time series: electricity, beer, and chocolate data
CBE <- read.table(
  "https://raw.githubusercontent.com/dallascard/Introductory_Time_Series_with_R_datasets/master/cbe.dat", 
  header=T)
head(CBE)
class(CBE)
Elec.ts <- ts(CBE[,3], start=1958, freq=12)
Beer.ts <- ts(CBE[,2], start=1958, freq=12)
Choc.ts <- ts(CBE[,1], start=1958, freq=12)
plot(cbind(Elec.ts, Beer.ts, Choc.ts))

(AP.elec <- ts.intersect(AP, Elec.ts))
AP <- AP.elec[, 1]
Elec <- AP.elec[, 2]
layout(1:2)
plot(AP, main="", ylab='Air Passengers (1000s)')
plot(Elec, main="", ylab="Electricity Production (MkWhr)")
layout(1)
plot(as.vector(AP), as.vector(Elec), xlab="Air Passengers", ylab="Electricity" )
abline(lm(Elec ~ AP))
cor(AP, Elec)

	
# 4.4 Quarterly exchange rate: GBP to NZ dollar
Z <- read.table(
  "https://raw.githubusercontent.com/dallascard/Introductory_Time_Series_with_R_datasets/master/pounds_nz.dat", 
  header=T)
head(Z)
Z.ts <- ts(Z, st=1991, fr=4)
plot(Z.ts, xlab='Year', ylab='NZ $ / GB Pound')
	
Z.92.96 <- window(Z.ts, start=c(1992, 1), end=c(1996, 1))
Z.96.98 <- window(Z.ts, start=c(1996, 1), end=c(1998, 1))
layout(1:2)
plot(Z.92.96)
plot(Z.96.98)


# 4.5 Global temperature series
Global <- scan( 
  'https://raw.githubusercontent.com/dallascard/Introductory_Time_Series_with_R_datasets/master/global.dat')
Global.ts <- ts(Global, st=c(1856, 1), end=c(2005, 12), fr=12)
Global.annual <- aggregate(Global.ts, FUN=mean)
par(mfrow=c(2,1))
plot(Global.ts) 
plot(Global.annual)

New.series <- window(Global.ts, start=c(1970, 1), end=c(2005, 12))
# returns a numeric equivalent for ea time stamp (e.g. Jan 1970 = 1970.000; Feb 
# 1970 = 1970.083, etc)
New.time <- time(New.series)
par(mfrow=c(1,1))
plot(New.series)
abline(lm(New.series ~ New.time), col=2)



# 5 Decomposition of a Series


# 5.5 Decompostion in R
plot(decompose(Elec.ts))
plot(decompose(Elec.ts, type="mult"))	# cf. error term
Elec.decom <- decompose(Elec.ts, type="mult")
plot(Elec.decom)	# same as plot(decompose(Elec.ts, type='mult))
Trend <- Elec.decom$trend
Seasonal <- Elec.decom$seasonal
par(mfrow=c(2,1))
ts.plot(cbind(Trend, Trend * Seasonal, Elec.ts), 
        col=c(1, 2, 4), 
        main='Multiplicative Model')
legend('topleft', 
       legend=c('Trend','Estimated','Observed'), 
       lty=1, 
       col=c(1, 2, 4), 
       bty='n')
ts.plot(cbind(Trend, Trend + (decompose(Elec.ts)$seasonal), Elec.ts), 
	    col=c(1, 2, 4), 
	    main='Additive Model')
legend('topleft', 
       legend=c('Trend','Estimated','Observed'), 
       lty=1, 
       col=c(1, 2, 4), 
       bty='n')
plot(stl(Elec.ts, 12))



# 7. Exercises


# 1.
# a. Produce a time plot of the beer data
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow=T))
plot(Beer.ts)
# Plot the aggregated annual series...
plot(aggregate(Beer.ts))
# ...and a boxplot that summarize the observed vals for ea season
boxplot(Beer.ts ~cycle(Beer.ts))	# recal that Dec is summer in Austral
	
# b. Decompose the series into the component trend, seasonal effect and
# resids and plot
plot(stl(Beer.ts, 12)) # OR:
plot(decompose(Beer.ts))
# Produce a plot of the trend with a superimposed seasonal effect
beer.decomp <- decompose(Beer.ts)
beer.trend <- beer.decomp$trend
beer.seasonal <- beer.decomp$seasonal
par(mfrow=c(1, 1))
ts.plot(cbind(beer.trend, beer.trend + beer.seasonal, Beer.ts), col=c(1, 2, 4))
	

# 2. Price Indices
items <- c('car', 'petrol (L)', 'sevicing (h)', 'tires', 'clutch')
q0    <- c(0.33,  2000,         40,             3,       2)	# quantity t=0
p0	  <- c(18000, 0.80,         40,             80,      200) # unit price
q4	  <- c(0.5,   1500,         20,             2,       1) # quantity t=4
p4    <- c(20000, 1.60,         60,             120,     360) # unit price

laspeyere.price.index <- function(q0, p0, pt) {
  sum(q0 * pt) / sum(q0 * p0)
}

LI4 <- laspeyere.price.index(q0, p0, p4)	# 1.36


# 3. Paasche Price Index
paasche.price.index <- function(qt, p0, pt) {
  sum(qt * pt) / sum(qt / p0)
}

PI4 <- paasche.price.index(q4, p0, p4)		# 7.57

irving.fisher.price.index <- function(q0, p0, qt, pt) {
  sqrt(laspeyere.price.index(q0, p0, p4) * paasche.price.index(qt, p0, pt))
}

(IFI4 <- irving.fisher.price.index(q0, p0, q4, p4))	# 3.21


# 4. Tyler series expansion
n <- 100
mu <- 4
y <- rnorm(n, mu, sigma)
sigma <- 3

# Tyler expansion for e^x
tyler.exp <- function(y, mu, iters) {
  output <- exp(mu)
  for (i in 1:iters) {
    term <- (y[i] - mu)^i / factorial(i)
  }
  output <- output + exp(mu) * term
  return (output)
}

is <- 1:100
expansion <- numeric(100)

for (i in is) {
  expansion[i] <- tyler.exp(y, mu, i)
}

is <- c(0, is)
expansion <- c(exp(mu), expansion)

plot(expansion ~ is, type='l')
abline(h=exp(mean(y)), col=2)

#save.image(file="~/Desktop/R/Time Series/TimeSeries.RData")