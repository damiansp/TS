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