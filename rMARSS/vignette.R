library(MARSS)


# 1.4.1 Getting a ts obj into the right form
# Univariate
z <- ts(rnorm(10), frequency=4, start=c(1976, 4))
dat <- data.frame(Yr=floor(time(z) + .Machine$double.eps), Qtr=cycle(z), Temp=z)
dat <- t(dat)

# Multivariate
z <- ts(matrix(rnorm(300), 100, 3), 
        start=c(1976, 11), 
        frequency=12, 
        names=c('Temp1', 'Temp2', 'Sal'))
dat <- data.frame(Yr=floor(time(z) + .Machine$double.eps), Month=cycle(z), z)



# 1.5 Important Notes About Algorithms
