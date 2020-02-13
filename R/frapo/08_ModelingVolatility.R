#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/R/frapo')

library(AER)
library(fGarch)
data(NYSESW)

# 4. Empirical Application of Volatility Models
nyse.loss <- timeSeries(-1.0 * diff(log(NYSESW)) * 100, char.vec=time(NYSESW))

es.garch <- function(y, p=0.99) {
  g.fit <- garchFit(~garch(1, 1), data=y, cond.dist='std', trace=F)
  sigma <- predict(g.fit, n.ahead=1)[3]
  df <- coef(g.fit)['shape']
  ES <- sigma * (dt(qt(p, df), df) / (1 - p)) * ((df * (qt(p, df))^2) / (df - 1))
  ES
}

# Date vecs for backtest
from <- time(nyse.loss)[-c((nrow(nyse.loss) - 999):nrow(nyse.loss))]
to <- time(nyse.loss)[-c(1:1000)]
nyse.es <- fapply(nyse.loss, from=from, to=to, FUN=es.garch)
nyse.es.l1 <- lag(nyse.es, k=1)
res <- na.omit(cbind(nyse.loss, nyse.es.l1))
colnames(res) <- c('nyse.loss', 'es99')

plot(res[, 2], 
     col=2, 
     ylim=range(res), 
     main='NYSE: t-GARCH(1, 1) ES 99%', 
     ylab='Percentages', 
     xlab='')
points(res[, 1], type='p', cex=0.2, pch=19, col=4)
legend('topleft', legend=c('Loss', 'ES'), col=c(4, 2), lty=c(NA, 1), pch=c(19, NA))