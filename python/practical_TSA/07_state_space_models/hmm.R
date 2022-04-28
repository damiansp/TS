# states for a stock market
bull.mu <- 0.1
bull.sd <- 0.1
neutral.mu <- 0.02
neutral.sd <- 0.08
bear.mu <- -0.03
bear.sd <- 0.2
panic.mu <- -0.1
panic.sd <- 0.3

mus <- c(bull.mu, neutral.mu, bear.mu, panic.mu)
sds <- c(bull.sd, neutral.sd, bear.sd, panic.sd)
N.PERIODS <- 10
SHORTEST.PERIOD <- 20
LONGEST.PERIOD <- 40

days <- sample(SHORTEST.PERIOD:LONGEST.PERIOD, N.PERIODS, replac=T)
returns <- numeric()
true.mean <- numeric()
for (d in days) {
  market.state <- sample(1:(length(mus)), 1, prob=c(0.2, 0.6, 0.18, 0.02))
  returns <- c(returns, rnorm(d, mean=mus[market.state], sd=sds[market.state]))
  true.mean <- c(true.mean, rep(mus[market.state], d))
}

table(true.mean)