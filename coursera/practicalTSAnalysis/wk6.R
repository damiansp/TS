#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/coursera/practicalTSAnalysis')


library(astsa)
library(forecast)

# SARIMA
n <- 10000
x <- numeric(n)
z <- rnorm(n)
x[1:13] <- 1

for (i in 14:n) {
  x[i] <- z[i] + 0.7*z[i - 1] + 0.6*z[i - 12] + 0.42*z[i - 13]
}

par(mfrow=c(3, 1))
plot.ts(x[12:120], main='First 12 months of SARIMA(0, 0, 1, 0, 0, 1)_12', ylab='')
acf(x)
pacf(x)
Box.test(x)


# SARIMA for Johnson & Johnson Returns
d <- 1
D <- 1
per <- 4

for (p in 1:2) {
  for (q in 1:2) {
  	for (P in 1:2) {
  	  for (Q in 1:2) {
  	  	if (p + q + P + Q + D <= 10) {
  	  	  # jj: Johnson & Johnson quarterly returns (in 'astsa')
  	  	  mod <- arima(x=log(jj), 
  	  	               order=c(p-1, d, q-1), 
  	  	               seasonal=list(order=c(P-1, D, Q-1), 
  	  	               period=per))
  	  	  pval <- Box.test(
  	  	    mod$residuals, lag=log(length(mod$residuals)))$p.value
  	  	  sse <- sum(mod$residuals^2)
  	  	  cat(p-1, d, q-1, P-1, D, Q-1, per, 'AIC: ', mod$aic, ' SSE: ', sse, 
  	  	     ' p: ', pval, '\n')
  	  	}
  	  }
  	}
  }
}

sarima(log(jj), 0,1,1,1,1,0,4)



# SARIMA for milk production (pounds per month)
milk <- c(
  578.3, 609.8, 628.4, 665.6, 713.8, 707.2, 628.4, 588.1, 576.3, 566.5, 561.1,
  571.4, 589.1, 615.3, 641.2, 682.8, 728.5, 726.4, 648, 605.8, 591.5, 576.3, 
  573.2, 587.1, 616.6, 648.6, 675.5, 715.3, 756, 746.7, 665.7, 627.4, 612.8, 
  599.9, 602.7, 622.5, 646.1, 676.1, 696.1, 732.5, 767.8, 767, 689.3, 641.2, 
  624, 609.7, 610.8, 623.5, 664.7, 690.3, 722.6, 766, 796.3, 809.6, 721.7, 
  684.4, 670.6, 654.9, 654.4, 675.5, 700.1, 725.1, 748.2, 795.4, 821.8, 828.9, 
  753.1, 708.9, 690.9, 674.5, 669.6, 685.3, 704, 730.5, 760.9, 807.6, 842.4, 
  838, 768.8, 726.6, 711.2, 693.2, 686.9, 698.1, 720.7, 750.1, 770.8, 816.7, 
  855.2, 857.3, 786.5, 750.1, 735.6, 709.9, 700.1, 720.7, 736.4, 768.5, 792.4, 
  836, 869.9, 871.5, 804.1, 768.8, 750.8, 733.4, 721.4, 737.4, 789.4, 821.8, 
  844.4, 890.8, 924.9, 926.3, 853.2, 818.9, 801.5, 785.5, 774.1, 785.5, 811, 
  838.6, 873.9, 913.1, 943.6, 948.6, 877.8, 839.5, 820.8, 795.3, 777.2, 790.4, 
  806.1, 840.3, 867, 911.1, 939.6, 937.5, 865, 821.8, 795.4, 776.6, 771.1, 
  787.4, 813, 845.7, 872.9, 915.2, 951.4, 960.8, 891.5, 851.3, 826.9, 797.3, 
  784.3, 798.2) 
sarima(milk, 0, 1, 0, 0, 1, 1, 12)

d <- 1
D <- 1
period <- 12
for (p in 0:1) {
  for (q in 0:1) {
    for (P in 0:3) {
      for (Q in 0:4) {
      	if (p + d + q + P + D + Q <= 10) {
      	  mod <- arima(x=milk, 
      	               order=c(p, d, q), 
      	               seasonal=list(order=c(P, D, Q), period=period))
      	  res <- mod$residuals
      	  p.val <- Box.test(res, lag=log(length(res)))$p.value
      	  sse <- sum(res^2)
      	  cat(p, d, q, P, D, Q, period, 'AIC: ', mod$aic, 'SSE: ', sse, 'p: ', 
      	      p.val, '\n')
      	}
      }
    }
  }
}

mod <- arima(
  milk, order=c(0, 1, 0), seasonal=list(order=c(0, 1, 1), period=period))
forecast(mod)
plot(forecast(mod))



#  Souvenir shop sales
#souv <- read.csv('data/souvenirShop.csv')
months <- c(
  '1987-01', '1987-02', '1987-03', '1987-04', '1987-05', '1987-06', '1987-07', 
  '1987-08', '1987-09', '1987-10', '1987-11', '1987-12', '1988-01', '1988-02', 
  '1988-03', '1988-04', '1988-05', '1988-06', '1988-07', '1988-08', '1988-09', 
  '1988-10', '1988-11', '1988-12', '1989-01', '1989-02', '1989-03', '1989-04', 
  '1989-05', '1989-06', '1989-07', '1989-08', '1989-09', '1989-10', '1989-11', 
  '1989-12', '1990-01', '1990-02', '1990-03', '1990-04', '1990-05', '1990-06', 
  '1990-07', '1990-08', '1990-09', '1990-10', '1990-11', '1990-12', '1991-01', 
  '1991-02', '1991-03', '1991-04', '1991-05', '1991-06', '1991-07', '1991-08', 
  '1991-09', '1991-10', '1991-11', '1991-12', '1992-01', '1992-02', '1992-03', 
  '1992-04', '1992-05', '1992-06', '1992-07', '1992-08', '1992-09', '1992-10', 
  '1992-11', '1992-12', '1993-01', '1993-02', '1993-03', '1993-04', '1993-05', 
  '1993-06', '1993-07', '1993-08', '1993-09', '1993-10', '1993-11', '1993-12')
sales <- c(
  1664.81, 2397.53, 2840.71, 3547.29, 3752.96, 3714.74, 4349.61, 3566.34, 
  5021.82, 6423.48, 7600.6, 19756.21, 2499.81, 5198.24, 7225.14, 4806.03, 
  5900.88, 4951.34, 6179.12, 4752.15, 5496.43, 5835.1, 12600.08, 28541.72, 
  4717.02, 5702.63, 9957.58, 5304.78, 6492.43, 6630.8, 7349.62, 8176.62, 
  8573.17, 9690.5, 15151.84, 34061, 5921.1, 5814.58, 12421.25, 6369.77, 7609.12, 
  7224.75, 8121.22, 7979.25, 8093.06, 8476.7, 17914.66, 30114.41, 4826.64, 
  6470.23, 9638.77, 8821.17, 8722.37, 10209.48, 11276.55, 12552.22, 11637.39, 
  13606.89, 21822.11, 45060.69, 7615.03, 9849.69, 14558.4, 11587.33, 9332.56, 
  13082.09, 16732.78, 19888.61, 23933.38, 25391.35, 36024.8, 80721.71, 10243.24, 
  11266.88, 21826.84, 17357.33, 15997.79, 18601.53, 26155.15, 28586.52, 
  30505.41, 30821.33, 46634.38, 104660.7)
souv <- data.frame(Months=months, Sales=sales)
head(souv)
souv <- ts(souv$Sales)

par(mfrow=c(2, 2))
plot(souv, main='Monthly sales fo a souvenir shop', ylab='', col=2, lwd=3)
plot(log(souv), main='Log(Sales)', ylab='', col=4, lwd=3)
plot(diff(diff(log(souv)), 12), 
     main='Log(Sales) without trend or seasonality', 
     col=3, 
     lwd=3)

data <- diff(diff((log(souv)), 12))
acf2(data, 50)

d <- 1
D <- 1
period <- 12
for (p in 1:2) {
  for (q in 1:2) {
    for (P in 1:2) {
      for (Q in 1:4) {
        if (p + d + q + P + D + Q <= 10) {
          model <- arima(
            x=log(souv), 
            order=c((p - 1), d, (q - 1)), 
            seasonal=list(order=c((P - 1), D, (Q-1)), period=period)) 
          pval <- Box.test(
            model$residuals, lag=log(length(model$residuals)))$p.value
          sse <- sum(model$residuals^2)
          cat(p - 1, d, q - 1, P - 1, D, Q - 1, period, 
              'AIC: ', model$aic, ' SSE: ',sse,' p: ', pval,'\n')
        }
      }
    }
  }
}

model <- arima(x=log(souv), 
               order=c(1, 1, 0), 
               seasonal=list(order=c(0, 1, 1), period=period))

par(mfrow=c(1, 1))
plot(forecast(model))
forecast(model)

a <- sarima.for(log(souv), 24, 1, 1, 0, 0, 1, 1, 12)

# Reverse transform
plot.ts(c(souv, exp(a$pred)), 
        main='Monthly sales + Forecast', 
        ylab='', 
        col='blue', 
        lwd=3)
        
        

# Exponential Smoothing
rain.data <- scan('http://robjhyndman.com/tsdldata/hurst/precip1.dat', skip=1)
rain.ts <- ts(rain.data, start=c(1813))
hist(rain.data, main='Annual Rainfall, London (1813-1912)', xlab='in')
qqnorm(rain.data)
qqline(rain.data)

par(mfrow=c(3, 1))
plot(rain.ts)
acf(rain.ts)
pacf(rain.ts)
auto.arima(rain.ts) # 0 0 0




# Forecasting with Smoothing Techniques
rain.df <- scan('http://robjhyndman.com/tsdldata/hurst/precip1.dat', skip=1)
head(rain.df)
rain.ts <- ts(rain.df, start=1813)
par(mfrow=c(1, 2))
hist(rain.df, col=5)
qqnorm(rain.df)
qqline(rain.df)
par(mfrow=c(3, 1))
plot(rain.ts)
acf(rain.ts)
pacf(rain.ts)

# Simple exponential smoothing
generate.forecast <- function(x, alpha) {
  n <- length(x)
  forecast <- numeric(n + 1)
  forecast[1] <- x[1]
  for (i in 1:n) {
    forecast[i + 1] <- alpha*x[i] + (1 - alpha)*forecast[i]
  }
  forecast
}

forecast <- generate.forecast(rain.df, alpha=0.2)
par(mfrow=c(1, 1))
plot(rain.df, type='l')
lines(forecast, col=4)

forecast <- generate.forecast(rain.df, alpha=0.024)
plot(rain.df, type='l')
lines(forecast, col=4)

hw <- HoltWinters(rain.ts, beta=F, gamma=F)
plot(hw)

head(lynx)
plot(lynx)
hw <- HoltWinters(lynx, beta=F, gamma=F)
plot(hw)



# Volume of money study (Holt-Winters)
money.df <- read.csv('../data/volume-of-money-abs-definition-m.csv')
money.ts <- ts(money.df[, 2], start=c(1960, 2), frequency=12)
par(mfrow=c(3, 1))
plot(money.ts)
acf(money.ts)
pacf(money.ts)

money <- money.df[,2]
n <- length(money)
alpha <- 0.7
beta <- 0.5
forecast <- level <- trend <- numeric(n)
level[1] <- money[1]
trend[1] <- money[2] - money[1]
forecast[1:2] <- money[1:2]
for (i in 2:n) {
  level[i] <- alpha*money[i] + (1 - alpha)*(level[i - 1] + trend[i - 1])
  trend[i] <- beta*(level[i] - level[i - 1]) + (1 - beta)*trend[i -1]
  forecast[i + 1] <- level[i] + trend[i]
}

par(mfrow=c(1, 1))
plot(1:n, money, xlim=c(1, n + 5), type='l')
lines(1:n, level, col=4)
lines(1:n, trend, col=2)
lines(1:(n + 1), forecast, col='darkgrey')

forecast[3:n]
m <- HoltWinters(money, alpha=alpha, beta=beta, gamma=F)
m$fitted[,1]
plot(m, main='H-W Fit with hand-picked parameters')
plot(m, log='y', main='H-W Fit with hand-picked parameters')

m <- HoltWinters(money.ts, gamma=F)
plot(m, log='y', main='H-W fit with optimal parameters')



# Air Passengers
head(AirPassengers)
m <- HoltWinters(x=log10(AirPassengers), beta=F, gamma=F)
plot(m) # not so good
m$SSE # 0.3065


AirPass.HW <- HoltWinters(log10(AirPassengers))
plot(AirPass.HW, xlim=c(1949, 1963), ylim=c(2, 2.9))
AirPass.HW$SSE # 0.0383
AirPass.HW
AirPass.fc <- predict(AirPass.HW, n.ahead=12)
x <- seq(1961, 1962, length=13)[1:12]
lines(x, AirPass.fc, col=4)
AirPass.fc <- forecast(AirPass.HW)
AirPass.fc
plot(AirPass.fc)
