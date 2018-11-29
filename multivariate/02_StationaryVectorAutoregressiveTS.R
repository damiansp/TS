#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
setwd('~/Learning/TS/multivariate')

library(MTS)



# 2. VAR(1) Models
# 2.5 Moment Equations
(phi1 <- matrix(c(0.2, -0.6, 0.3, 1.1), 2, 2))
(sig <- matrix(c(1, 0.8, 0.8, 2), 2, 2))
(m1 <- eigen(phi1))
I4 <- diag(4)
(pp <- kronecker(phi1, phi1))
(c1 <- c(sig))
dd <- I4 - pp
dd.inv <- solve(dd)
(gam0 <- dd.inv %*% matrix(c1, 4, 1))

g0 <- matrix(gam0, 2, 2)
(g1 <- phi1 %*% g0)
(g2 <- phi1 %*% g1)

(D <- diag(sqrt(diag(g0)))) # cross correlation
Di <- solve(D)

Di %*% g0 %*% Di
Di %*% g1 %*% Di
Di %*% g2 %*% Di



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