library(depmixS4)

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
N.PERIODS <- 30
SHORTEST.PERIOD <- 20
LONGEST.PERIOD <- 40

days <- sample(SHORTEST.PERIOD:LONGEST.PERIOD, N.PERIODS, replace=T)
returns <- numeric()
true.mean <- numeric()
for (d in days) {
  market.state <- sample(1:(length(mus)), 1, prob=c(0.2, 0.4, 0.2, 0.1))
  returns <- c(returns, rnorm(d, mean=mus[market.state], sd=sds[market.state]))
  true.mean <- c(true.mean, rep(mus[market.state], d))
}

table(true.mean)
cum.returns <- cumprod(c(1, 1+ returns))
cum.returns <- cum.returns[-1]

hmm.mod <- depmix(
  returns ~ 1, family=gaussian(), nstates=4, data=data.frame(returns=returns))
mod.fit <- fit(hmm.mod)
post.probs <- posterior(mod.fit)

par(mfrow=c(2, 1))
YMN <- 1.2*min(returns)
YMX <- 1.2*max(returns)
par(mar=c(0, 4, 1, 1))
plot(returns, type='l', ylim=c(YMN, YMX), xaxt='n')
lapply(
  0:length(returns) - 1, 
  function(i) {
  	rect(
  	  i, 
  	  YMN, 
  	  (i + 1),
  	  YMN + 0.2,
  	  col=post.probs$state[i + 1],
  	  border=NA)
  })
par(mar=c(3, 4, 1, 1))
plot(cum.returns, type='l', log='y')  
attr(mod.fit, 'response')