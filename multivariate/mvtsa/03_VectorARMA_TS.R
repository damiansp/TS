#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/multivariate')

library(MTS)



# 1 Vector MA Models


# 1.1 VMA(1) Model
data <- read.table('data/m-dec125910-6111.txt', header=T)
head(data)
PERIODS <- 12
START_YEAR <- 1961
x <- 100 * log(data[, 2:6] + 1)
rtn <- cbind(x$dec5, x$dec9)
tdx <- c(1:nrow(rtn)) / PERIODS + START_YEAR
par(mfcol=c(2, 1))
plot(tdx, rtn[, 1], type='l', xlab='year', ylab='d5')
plot(tdx, rtn[, 2], type='l', xlab='year', ylab='d9')
ccm(rtn)



# 2 Specifying VMA Order
# VMA order specification
VMAOrder(data$dec1, lag=20)



# 3 Estimation of VMA Models


# 3.1 Conditional Likelihood Estimation
m1 <- VMA(rtn, q=1)
MTSdiag(m1)
r1 <- m1$residuals
mq(r1, adj=4)


# 3.2 Exact Likelihood Estimation
m2 <- VMAe(rtn, q=1)
MTSdiag(m2)

#rtn <- cbind(ibm, ko)  # whatever ibm and ko are... 2 dfft stock returns??
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

#        / 0.13 \   / 0.38 0.10 0.05 \            / 0.05  0.11  0.02 \
# z[t] = | 0.13 | + | 0.36 0.35 0.46 | z[t - 1] + |-0.21 -0.17 -0.01 | z[t - 2]
#        \ 0.29 /   \ 0.50 0.25 0.23 /            \-0.33 -0.13  0.10 /
#
# + a[t] 
#                            / 0.28 0.02 0.07 \
# with resid. cov ∑.hat[a] = | 0.02 0.29 0.14 |
#                            \ 0.07 0.14 0.34 /

# Bayesian Estimation
dat <- read.table('data/q-gdp-ukcaus.txt', header=T)
x <- log(dat[, 3:5])
dim(x) # 126, 3
TT <- dim(x)[1]
dx <- x[2:TT, ] - x[1:(TT - 1), ]
dx <- dx * 100
C = 0.1 * diag(7) # lambda = 0.01
V0 = diag(3)      # V[0] = I[3]
mm <- BVAR(dx, p=2, C, V0)  # c.f. prev output... very close

