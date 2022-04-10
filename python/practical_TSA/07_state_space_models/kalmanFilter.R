ts.len <- 100
acc <- rep(0.5, ts.len)
x <- rep(0, ts.len)
vel <- rep(0, ts.len)

for (t in 2:ts.len) {
  x[t] <- 2*vel[t - 1] + x[t - 1] + 1/2*acc[t - 1]^2
  x[t] <- x[t] + rnorm(1, sd=20)  # stochastic comp.
  vel[t] <- vel[t - 1] + 2*acc[t - 1]
}

par(mfrow=c(3, 1))
plot(x, main='Position', type='l')
plot(vel, main='Velocity', type='l')
plot(acc, main='Acceleration', type='l'

z <- x + rnorm(ts.len, sd=300)  # noisy measurement
par(mfrow=c(1, 1))
plot(x, ylim=range(c(x, z)), type='l')
lines(z, col=2)