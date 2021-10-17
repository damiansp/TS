library(zoo)

head(EuStockMarkets)
plot(EuStockMarkets)
class(EuStockMarkets)
frequency(EuStockMarkets)
start(EuStockMarkets)
end(EuStockMarkets)
wind <- window(EuStockMarkets, start=1997, end=1998)
wind
plot(wind)
hist(EuStockMarkets[, 'SMI'], 30)
hist(diff(EuStockMarkets[, 'SMI'], 30))
plot(EuStockMarkets[, 'SMI'], EuStockMarkets[, 'DAX'])
plot(diff(EuStockMarkets[, 'SMI']), diff(EuStockMarkets[, 'DAX']))
plot(lag(diff(EuStockMarkets[, 'SMI']), 1), diff(EuStockMarkets[, 'DAX']))


# Window functions
x <- rnorm(100, sd=10) + 1:100
mn <- function(n) { rep(1/n, n) }

plot(x, type='l')
lines(filter(x, mn(5)), col=2, lw=2)
lines(filter(x, mn(25)), col=3, lw=2)


f1 <- rollapply(zoo(x), 20, function(w) min(w), align='left', partial=TRUE)
f2 <- rollapply(zoo(x), 20, function(w) min(w), align='right', partial=TRUE)

plot(x, type='l')
lines(f1, col=2, lw=2)
lines(f2, col=3, lw=2)


# Expanding windows
plot(x, type='l')
lines(cummax(x), col=2, lw=3)
lines(cumsum(x) / 1:length(x), col=3, lw=3)

plot(x, type='l')
lines(rollapply(zoo(x), seq_along(x), function(w) max(w), partial=T, align='right'),
      col=2,
      lw=3)
lines(rollapply(zoo(x), seq_along(x), function(w) mean(w), partial=T, align='right'),
      col=3,
      lw=3)
      

# ACF
x <- 1:100
y <- sin(x * pi / 3)
plot(y)
lines(y)
acf(y)
cor(y[2:100], y[1:99], use='pairwise.complete.obs')
cor(y[3:100], y[1:98], use='pairwise.complete.obs')

# PACF
plot(y)
lines(y)
pacf(y)

y2 <- sin(x * pi/10)
par(mfrow=c(2, 3))
plot(y)
lines(y)
acf(y)
pacf(y)
plot(y2)
lines(y2)
acf(y2)
pacf(y2)

y <- y + y2
par(mfrow=c(3, 1))
plot(y)
lines(y)
acf(y)
pacf(y)

noise1 <- rnorm(100, sd=0.05)
noise2 <- rnorm(100, sd=0.05)
y1 <- y + noise1
y2 <- y2 + noise2
y <- y1 + y2

par(mfrow=c(3, 3))
plot(y1); lines(y1)
acf(y1)
pacf(y1)
plot(y2); lines(y2)
acf(y2)
pacf(y2)
plot(y); lines(y)
acf(y)
pacf(y)

par(mfrow=c(3, 1))
x <- 1:50
plot(x)
acf(x)
pacf(x)


# Spurious Correlation