#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/TS/multivariate')

library(MTS)



# 2. VAR(1) Models


# 2.5 Moment Equations
(phi1 <- matrix(c(0.2, -0.6, 0.3, 1.1), 2, 2))
(sig <- matrix(c(1, 0.8, 0.8, 2), 2, 2)) # sig[a] (cov matrix of a)
(m1 <- eigen(phi1))                      # |eigenvals| all < 1 so stationary
I4 <- diag(4)
(pp <- kronecker(phi1, phi1))            # Kronecker product
(c1 <- c(sig))                           # vectorize
dd <- I4 - pp
dd.inv <- solve(dd)
(gam0 <- dd.inv %*% matrix(c1, 4, 1))    # Solve for gamma0


(g0 <- matrix(gam0, 2, 2))               # Restack
(g1 <- phi1 %*% g0)                      # Find others recursively
(g2 <- phi1 %*% g1)

(D <- diag(sqrt(diag(g0))))              # compute cross correlation matrices
Di <- solve(D)

(rho1 <- Di %*% g0 %*% Di)               # lag-1 CCM
(rho2 <- Di %*% g1 %*% Di)               # lag-2
(rho3 <- Di %*% g2 %*% Di)               # ...



# 5 Estimation
dat <- read.table('data/q-gdp-ukcaus.txt', header=T)
head(dat)
gdp <- log(dat[, 3:5])
dim(gdp)
z <- gdp[2:126, ] - gdp[1:125, ]
head(z)
z <- z*100
dim(z)
Z <- z[3:125, ]
Z <- as.matrix(Z)
X <- cbind(rep(1, 123), z[2:124,], z[1:123,])
X <- as.matrix(X)
XPX <- t(X) %*% X
XPX.inv <- solve(XPX)
XPZ <- t(X) %*% Z
b.hat <- XPX.inv %*% XPZ
b.hat

A <- Z - X %*% b.hat
Sig <- t(A) %*% A / (125 - (3 + 1)*2 - 1)
Sig

Cov <- kronecker(Sig, XPX.inv)
se <- sqrt(diag(Cov))
para <- cbind(Cov, se, Cov / se)

Sig1 <- t(A) %*% A / (125 - 2)
Sig1


# Demo 2
dat <- read.table('data/q-gdp-ukcaus.txt', header=T)
head(dat)
gdp <- log(dat[, 3:5])
z <- gdp[2:126, ] - gdp[1:125, ]
z <- z * 100
m1 <- VAR(z, p=2) # VAR(2)


# Demo: Bayesian estimation
x <- log(dat[, 3:5])
n <- nrow(x)
dx <- x[2:n, ] - x[1:(n - 1), ]
dx <- dx * 100
lambda <- 0.1
C <- lambda * diag(7) 
V0 <- diag(3)
mm <- BVAR(dx, p=2, C, V0)



# 6 Order Selection
z1 <- z / 100 # back to original values
m2 <- VARorder(z1)
order <- 0:13
plot(order, m2$aic, type='l', ylim=c(-32, -29))
lines(order, m2$bic, col=2)
lines(order, m2$hq, col=4)
legend('topleft', lty=1, col=c(1, 2, 4), legend=c('AIC', 'BIC', 'HQ'), bty='n')



# 7 Model Checking


# 7.2 Multivariate Portmanteau Statistics
names(m1)
resi <- m1$residuals # resid ofr VAR(2)
mq(resi, adj=18) # adj: adjust degrees of freedom: p = 2, k = 3 (# of ts)


# 7.3 Model Simplification

# 7.3.1 Testing Zero Parmeters for Model Reduction
m3 <- VARchi(z, p=2)             # default thres=1.645 (p <= 0.1)
m3 <- VARchi(z, p=2, thres=1.96) # (p <= 0.05)

# Infomation-criterion based stepwise reduction
m1 <- VAR(z, 2)
m2 <- refVAR(m1, thres=1.96)
# Reduced model is
#        T 0.16 T   T 0.47 0.21    0 T          T     0 0 0 T
# z[t] = |    0 | + | 0.33 0.27 0.50 | z[t-1] + | -0.20 0 0 | z[t-2] + a[t]
#        L 0.28 |   L 0.47 0.23 0.23 |          L -0.30 0 0 |
#
# with Res Cross COV Matrix:
# T 0.29 0.02 0.07 T
# | 0.02 0.31 0.15 |
# L 0.07 0.15 0.36 |
# 
# where
# UK = z[1, t] = 0.16 + 0.47z[1, t-1] + 0.21z[2, t-1]                
#                                + a[1, t]
# CA = z[2, t] =        0.33z[1, t-1] + 0.27z[2, t-1] + 0.50z[3, t-1] 
#                - 0.20z[1, t-2] + a[2, t]
# US = z[3, t] = 0.28 + 0.47z[1, t-1] + 0.23z[2, t-1] + 0.23z[3, t-1]
#                - 0.30z[1, t-2] + a[3, t]
#
# and Cross COR Matrix (lag0):
# T 1.00 0.06 0.22 T
# | 0.06 1.00 0.44 |
# L 0.22 0.44 1.00 |
resid(m2)


# Check Model Outputs
MTSdiag(m2, adj=12) 
# Resid CC ...basically good, pvals all non-signif; one questionable p val for 
# lag 4 in Ljung-Box (MQ) stats




################################################################################

# 9 Forecasting
VARpred(m1, 8) # 8 steps ahead
colMeans(z)
sqrt(apply(z, 2, var)) # sample ses



# 10 Impulse Response Functions
Phi <- m2$Phi # simplified VAR(2) mod
Sig <- m2$Sigma
VARirf(Phi, Sig) # orthog innovations
VARirf(Phi, Sig, orth=F) # orig innovations



# 11 Forecast Error Variance Decompostion
m1 <- VAR(z, 2)
m2 <- refVAR(m1)
names(m2)
Phi <- m2$Phi
Sig <- m2$Sigma
Theta <- NULL
FEVdec(Phi, Theta, Sig, lag=5)
