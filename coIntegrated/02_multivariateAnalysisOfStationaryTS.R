#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
#detach('package:dplyr')
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/cointegrated')

library(dse)
library(vars)



# 2. Vector Autoregressive Models


# 2.1. Specification, Assumptions, and Estimation
# Simulate a VAR(2) process
# Setting the lag-polynomial A(L)
Apoly <- array(c(1.0, -0.5, 0.3, 0, 0.2, 0.1, 0, -0.2, 0.7, 1, 0.5, -0.3),
               c(3, 2, 2))
B <- diag(2)                          # Init cov to I[2]
Trd <- c(5, 10)                       # setting const term to 5 and 10
var2 <- ARMA(A=Apoly, B=B, TREND=Trd) # generate model
varsim <- simulate(var2, 
                   sampleT=500, 
                   noise=list(w=matrix(rnorm(1000), nrow=500, ncol=2)),
                   rng=list(seed=c(1234))) # 500 observations
vardat <- matrix(varsim$output, nrow=500, ncol=2) # get generated series
colnames(vardat) <- c('y1', 'y2')
plot.ts(vardat, main='', xlab='')

# Determining the appropriate lag-order
infocrit <- VARselect(vardat, lag.max=5, type='const')
infocrit
BEST_P <- 2 # by all criteria
# Estimate model
varsimest <- VAR(vardat, p=BEST_P, type='const', season=NULL, exogen=NULL)
varsimest
# OR:
varsimest2 <- VAR(vardat, type='const', lag.max=5, ic='SC') # use Schwarz crit.
varsimest2
# Check roots
roots(varsimest) # all < 1


# 2.2 Diagnostic Tests
# Code 2.2: Diagnostic Tests of VAR(2) Process
# Testing serial correlation
args(serial.test)
# Portmanteau Test (Null hypothesis: all correlations at all lags = 0)
#?serial.test
(var2c.serial <- serial.test(varsimest, lags.pt=16, type='PT.asymptotic'))
# p = 0.6433: no signif correlations
plot(var2c.serial, names='y1')
plot(var2c.serial, names='y2')

# Testing for heteroskedasticity
args(arch.test)
(var2c.arch <- arch.test(varsimest, lags.multi=5, multivariate.only=T))
# p = 0.088; no signif. heteroskedasticiy

# Test for normality
args(normality.test)
(var2c.norm <- normality.test(varsimest, multivariate.only=T))
# Not significantly non-normal

# misc
class(var2c.serial) # varcheck
class(var2c.arch)   # ""
class(var2c.norm)   # ""
methods(class='varcheck') # plot, print
args(vars:::plot.varcheck)

# Code 2.3: Empirical fluctuation process
reccumsum <- stability(varsimest, type='OLS-CUSUM')
plot(reccumsum)
fluctuation <- stability(varsimest, type='fluctuation')
plot(fluctuation)


# 2.3 Causality Analysis
(var.causal <- causality(varsimest, cause='y2'))
# Granger causality likely (reject H0); inst. caus. unlikely (cannot reject H0)


# 2.4 Forecasting
args(vars:::predict.varest)
preds <- predict(varsimest, n.ahead=25, ci=0.95)
class(preds)
plot(preds, names='y1')
args(fanchart)
fanchart(preds, names='y2')
fanchart(preds, names=c('y1', 'y2'))


# 2.5 Impulse Response Functions
# Code 2.6 IRA of VAR-process
irf.y1 <- irf(varsimest, 
              impulse='y1', 
              response='y2', 
              n.ahead=10, 
              ortho=F, 
              cumualtive=F, 
              boot=T)
irf.y2 <- irf(varsimest,
              impulse='y2', 
              response='y1', 
              n.ahead=10, 
              ortho=F, 
              cumualtive=F, 
              boot=T)
args(vars:::plot.varirf)
plot(irf.y1)
plot(irf.y2)