rm(list=ls())
setwd('~/Learning/TS/edx')

library(ggplot2)
library(mgcv)

############# DATA EXPLORATION AND PROCESSING ##########################
edvoldata <- read.csv("../data/EGDailyVolume.csv")
head(edvoldata) # Patient volume in a Hospital Emergency Dept.

## Process Dates 
year <- edvoldata$Year
month <- edvoldata$Month
day <- edvoldata$Day
datemat <- cbind(as.character(day), as.character(month), as.character(year))
paste.dates <- function(date) {
  day <- date[1]
  month <- date[2]
  year <- date[3]
  paste(day, month, year, sep="/")
}
dates <- apply(datemat, 1, paste.dates)
dates <- as.Date(dates, format="%d/%m/%Y")
edvoldata <- cbind(dates, edvoldata)
df <- edvoldata
head(df)

(ggplot(edvoldata, aes(dates, Volume)) 
 + geom_line() 
 + xlab("Time") 
 + ylab("Daily ED Volume"))

## ED Volume is count data: Transform
Volume.tr <- sqrt(df$Volume + 3/8)
par(mfrow=c(2, 1))
hist(df$Volume, nclass=20, xlab="ED Volume", main="", col="brown")
hist(Volume.tr, nclass=20, xlab= "Transformed ED Volume", main="", col="blue")
(ggplot(edvoldata, aes(dates, Volume.tr)) 
 + geom_line() 
 + xlab("Time") 
 + ylab("Transformed Daily ED Volume"))


################ TREND AND SEASONALITY ESTIMATION #########################
time.pts = c(1:length(df$Volume))
time.pts = c(time.pts - min(time.pts)) / max(time.pts)
## Trend Estimation: Is there a trend?
## Local Polynomial Trend Estimation
loc.fit = loess(Volume.tr ~ time.pts)
vol.fit.loc = fitted(loc.fit)
## Splines Trend Estimation
gam.fit = gam(Volume.tr ~ s(time.pts))
summary(gam.fit)
vol.fit.gam = fitted(gam.fit)
## Is there a trend? 
(ggplot(edvoldata, aes(dates, Volume.tr)) 
 + geom_line() 
 + xlab("Time") 
 + ylab("Transformed Daily ED Volume"))
par(mfrow=c(1, 1))
plot(df$dates, Volume.tr, type='l')
lines(dates, vol.fit.loc, lwd=2, col=5)
lines(dates, vol.fit.gam, lwd=2, col=2)


## Model Trend + Monthly Seasonality
## Using nonparametric trend and linear regression seasonality 
month = as.factor(format(dates, "%b"))
gam.fit.seastr.1 = gam(Volume.tr ~ s(time.pts) + month)
summary(gam.fit.seastr.1)
vol.fit.gam.seastr.1 = fitted(gam.fit.seastr.1)
plot(df$dates, Volume.tr, type='l')
lines(dates,vol.fit.gam.seastr.1,lwd=2,col=2)

## Add day-of-the-week seasonality
week = as.factor(weekdays(dates))
gam.fit.seastr.2 = gam(Volume.tr ~ s(time.pts) + month + week)
summary(gam.fit.seastr.2)
vol.fit.gam.seastr.2 = fitted(gam.fit.seastr.2)
## Compare the two fits: with & without day-of-the-week seasonality
lines(dates, vol.fit.gam.seastr.2, lwd=2, col=4)

## Does the addition of seasonality of day of the week adds predictive power?
lm.fit.seastr.1 = lm(Volume.tr ~ month)
lm.fit.seastr.2 = lm(Volume.tr ~ month+week)
anova(lm.fit.seastr.1, lm.fit.seastr.2)
vol.fit.lm.seastr.2 = fitted(lm.fit.seastr.2)
## Compare with & without trend
plot(df$dates, Volume.tr, type='l')
lines(dates, vol.fit.lm.seastr.2, col=4)
lines(dates, vol.fit.gam, lwd=2, col=2)



################## STATIONARITY: RESIDUAL PROCESS ####################
## Residual Process: Trend Removal
resid.1 = Volume.tr - vol.fit.gam
## Residual Process: Stationarity Removal
resid.2 = Volume.tr - vol.fit.lm.seastr.2
## Residual Process: Trend & Stationarity Removal
resid.3 = Volume.tr - vol.fit.gam.seastr.2
y.min = min(c(resid.1, resid.2, resid.3))
y.max = max(c(resid.1, resid.2, resid.3))

ggplot(edvoldata, aes(dates, resid.1),ymin=y.min,ymax=y.max) + geom_line() + xlab("Time") + ylab("Residual Process")
plot(df$dates, resid.1, ylim=c(y.min, y.max), type='l')
lines(dates, resid.2, col=rgb(1, 0, 0, 0.6))
lines(dates, resid.3, col=rgb(0, 0, 1, 0.6))
#legend(2012,-3.5,legend=c("Trend","Season","Trend+Season"),lty = 1, col=c("black","blue","brown"))

acf(resid.1, lag.max=12*4, main="")
acf(resid.2, lag.max=12*4, main="", col=2)
acf(resid.3, lag.max=12*4, main="", col=4)