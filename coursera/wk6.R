#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/coursera')


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
  	  	  pval <- Box.test(mod$residuals, lag=log(length(mod$residuals)))$p.value
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
  