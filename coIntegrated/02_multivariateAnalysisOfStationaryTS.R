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


# 2.6 Forecast Error Variance Decomposition (FEVD)
# Code 2.7
fevd.var2 <- fevd(varsimest, n.ahead=10)
args(vars:::plot.varfevd)
plot(fevd.var2, addbars=2, col=c(1, 2))



# 3. Structural VAR Models


# 3.2 Estimation
# Code 2.8: SVAR: A-Model
Apoly <- array(c( 1  , -0.5,  0.3,
                  0.8,  0.2,  0.1,
                 -0.7, -0.2,  0.7,
                  1  ,  0.5, -0.3),
                c(3, 2, 2))
B <- diag(2) # Cov matix for A-Model
svarA <- ARMA(A=Apoly, B=B) # VAR(2) mod
# Simulate 500 realizations
svarsim <- simulate(svarA, sampleT=500) # can add rng=list(seed=c(123))
svardat <- matrix(svarsim$output, nrow=500)
colnames(svardat) <- c('y1', 'y2')
varest <- VAR(svardat, p=2, type='none') # Estimate VAR mod
# Set up matrices for A-mod
Amat <- diag(2)
Amat[1, 2] <- Amat[2, 1] <- NA
# Est SVAR A-type mod via MLE
args(SVAR)
svar.A <- SVAR(varest, estmethod='direct', Amat=Amat, hessian=T)
svar.A
svar.A$Ase # SEs of A matrix
svar.A$A / svar.A$Ase # t-stats for off-diaganols


# Code 2.9 SVAR: B-Model
B[2, 1] <- -0.8
svarB <- ARMA(A=Apoly, B=B)
svarsim <- simulate(svarB, sampleT=500)
svardat <- matrix(svarsim$output, nrow=500, ncol=2)
colnames(svardat) <- c('y1', 'y2')
varest <- VAR(svardat, p=2, type='none')
Bmat <- diag(2)
Bmat[2, 1] <- NA
svar.B <- SVAR(varest, estmethod='scoring', Bmat=Bmat, max.iter=200)
svar.B

par(mfrow=c(2, 1))
ts.plot(svardat[, 'y1'])
ts.plot(svardat[, 'y2'])


# 3.3 Impulse Response Function
# Code 2.10: SVAR-IRF
args(vars:::irf.svarest)
irf.svarA <- irf(svar.A, impulse='y1', response='y2', boot=F)
args(vars:::plot.varirf)
plot(irf.svarA)
irf.svarB <- irf(svar.A, impulse='y2', response='y1', boot=F)
plot(irf.svarB)


# 3.4 Forecast Error Variance Decomposition
# Code 2.11 FEVD
args(vars:::fevd.svarest)
fevd.svarB <- fevd(svar.B, n.ahead=5)
plot(fevd.svarB, col=c(2, 4))