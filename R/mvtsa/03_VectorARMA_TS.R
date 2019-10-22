#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/R')

library(MTS)



# 1 Vector MA Models


# 1.1 VMA(1) Model
data <- read.table('data/m-dec125910-6111.txt', header=T)
head(data)
PERIODS <- 12
START_YEAR <- 1961
x <- 100 * log(data[, 2:5] + 1)
head(x)
rtn <- cbind(x$dec5, x$dec9)
tdx <- c(1:nrow(rtn)) / PERIODS + START_YEAR
par(mfcol=c(2, 1))
plot(tdx, rtn[, 1], type='l', xlab='year', ylab='d5')
plot(tdx, rtn[, 2], type='l', xlab='year', ylab='d9')
ccm(rtn)



# 2 Specifying VMA Order
# VMA order specification
head(rtn)
VMAorder(rtn[, 1:2], lag=20)



# 3 Estimation of VMA Models


# 3.1 Conditional Likelihood Estimation
m1 <- VMA(rtn, q=1)
MTSdiag(m1)
r1 <- m1$residuals
mq(r1, adj=4)


# 3.2 Exact Likelihood Estimation
head(rtn)
m2 <- VMAe(rtn, q=1)
MTSdiag(m2)

# Ex. 3.4 Demo of differences between conditional and exact estimations of VMA
#rtn <- cbind(ibm, ko)  # whatever ibm and ko are... 2 dfft stock returns??
ibm <- read.csv('data/IBM.csv')$Adj.Close
ko <- read.csv('data/KO.csv')$Adj.Close
par(mfrow=c(2, 1))
plot(ibm, type='l')
plot(ko, type='l')
n <- 250
ibm <- log(ibm[2:n] / ibm[1:(n-1)])
ko <- log(ko[2:n] / ko[1:(n-1)])
plot(ibm, type='l')
plot(ko, type='l')
rtn <- cbind(ibm, ko)
mq(rtn)
yt <- diffM(rtn)
mm <- ccm(yt)
m2 <- VMAe(rtn, q=1, include.mean=F)
yt <- diffM(rtn)
m1 <- VMA(yt, q=1, include.mean=F)
m2 <- VMAe(yt, q=1, include.mean=F)
t1 <- m1$Theta
t2 <- m2$Theta
eigen(t1)
eigen(t2)



# 5. Estimation


# 5.3 Limiting Properties
# Example 2.3
# k = 3, p = 2, T = 125
dat <- read.table('data/q-gdp-ukcaus.txt', header=T)
head(dat)
gdp <- log(dat[, 3:5])
dim(gdp) # 126 x 3
TT <- 125
z <- gdp[2:(TT + 1), ] - gdp[1:TT, ] # growth rate
z <- 100 * z                         # % growth rate
dim(z) # 125 x 3
Z <- z[3:125, ]
X <- cbind(rep(1, TT - 2), z[2:(TT - 1), ], z[1:(TT - 2), ])
X <- as.matrix(X)
XPX <- t(X) %*% X
XPX.inv <- solve(XPX)
Z <- as.matrix(Z)
XPZ <- t(X) %*% Z
b.hat <- XPX.inv %*% XPZ
b.hat

A <- Z - X %*% b.hat
Sig <- t(A) %*% A / (TT - (3 + 1) * 2 - 1)
Sig
COV <- kronecker(Sig, XPX.inv)
se <- sqrt(diag(COV))
length(b.hat)
length(se)
beta <- as.matrix(as.vector(b.hat), ncol=1)
para <- cbind(beta, se, beta / se)
para

Sig1 <- t(A) %*% A / (TT - 2) # MLE of Sigma_a
Sig1

# Estimation of VAR Models
dat <- read.table('data/q-gdp-ukcaus.txt', header=T)
gdp <- log(dat[, 3: 5])
head(gdp)
TT <- 126
z <- gdp[2:TT, ] - gdp[1:(TT - 1), ] # differenced
z <- 100 * z
head(z)
m1 <- VAR(z, 2) # VAR(2) # i.e., model is:

#        _      _   _                _            _                  _
#        | 0.13 |   | 0.38 0.10 0.05 |            | 0.05  0.11  0.02 |
# z[t] = | 0.13 | + | 0.36 0.35 0.46 | z[t - 1] + |-0.21 -0.17 -0.01 | z[t - 2]
#        | 0.29 |   | 0.50 0.25 0.23 |            |-0.33 -0.13  0.10 |
#        -      -   -                -            -                  -
# + a[t]                     _                _
#                            | 0.28 0.02 0.07 |
# with resid. cov ∑.hat[a] = | 0.02 0.29 0.14 |
#                            | 0.07 0.14 0.34 |
#                            -                -

# Bayesian Estimation
dat <- read.table('data/q-gdp-ukcaus.txt', header=T)
x <- log(dat[, 3:5])
dim(x) # 126, 3
TT <- dim(x)[1]
dx <- x[2:TT, ] - x[1:(TT - 1), ]
dx <- dx * 100
C M <- 0.1 * diag(7) # lambda = 0.01
V0 <- diag(3)      # V[0] = I[3]
mm <- BVAR(dx, p=2, C, V0)  # c.f. prev output... very close


# Ex. 2.5 Comparing Infromation Criteria
dat <- read.table('data/q-gdp-ukcaus.txt', header=T)
gdp <- log(dat[, 3: 5])
head(gdp)
TT <- 126
z <- gdp[2:TT, ] - gdp[1:(TT - 1), ] # differenced
m2 <- VARorder(z)
names(m2)
plot(m2$aic, type='l', ylim=c(-32, -29))
lines(m2$bic, col=2)
lines(m2$hq, col=4)
legend('topleft', lty=1, col=c(1, 2, 4), legend=c('AIC', 'BIC', 'HQ'))


# Ex. 2.6: Checking Multivariate Portmanteau Stats (resid cross corr) on a 
# VAR(2) Model
names(m1)
resi <- m1$residuals # resids of VAR(2)
mq(resi, adj=18) # adj used to adjust degrees of freedom (presumably forcing
                 # 0s at higher lags..?)



# 5. VARMA Models
# Computing autocovariance matrices for AMRA models
phi <- matrix(
  c( 0.816, -1.116, -0.623,  1.074, -0.643,  0.625,  0.592, -0.133), 2, 4)
phi
theta <- matrix(c(0, -0.801, -1.248, 0), 2, 2)
theta
sig <- matrix(c(4, 2, 2, 5), 2, 2)
VARMAcov(Phi=phi, Theta=theta, Sigma=sig, lag=2)



# 6. Implications of VARMA Models