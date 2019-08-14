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

# 1 Purpose



# 2 Expectation and the Ensemble


# 2.1 Expected value
herald <- read.table(
  paste0(DATA, "Herald.dat"), 
  header=T)

x <- herald$CO
y <- herald$Benzoa
n <- length(x)
sum((x - mean(x))*(y - mean(y))) / (n - 1)	# 5.51, same as:
cov(x, y)	# abbreviated form of previous = 5.51
cov(x, y) / (sd(x)*sd(y))	#0.355, same as:
cor(x, y)
	

# 2.5 Autocorrelation
wave <- read.table(paste0(DATA, "wave.dat"), header=T)
plot(ts(wave$waveht))
plot(ts(wave$waveht[1:60]))
acf(wave$waveht)$acf	 
acf(wave$waveht)$acf[2]
# autocovariance is found as:
acf(wave$waveht, type='covariance')$acf



# 3 The Correlogram


# 3.1 General discussion
acf(wave$waveht)
acf(AirPassengers)


# 3.2 Example based on air passenger series
AP <- AirPassengers
	AP.decom <- decompose(AP, "multiplicative")
	plot(ts(AP.decom$random))
	abline(h=1, lty=2)
	acf(AP.decom$random[7:138])	#indexing to remove NAs
	#Check the effectiveness of removing trend and seasonal variation:
	sd(AP[7:138])	#109 for all
	sd(AP[7:138] - AP.decom$trend[7:138])	#41.1 after removing trend
	sd(AP.decom$random[7:138])	#and only 0.033 when trend & seasonal removed

	# 2.3.3 Example based on the Font Reservoir series
	Fontdsdt.dat <- read.table( 
			 "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/Fontdsdt.dat", 
			 header=T )
	attach(Fontdsdt.dat)
	plot(ts(adflow))
	acf(adflow, xlab='lag(months)', main='')
	


# 2.4 Covariance of sums of random variables