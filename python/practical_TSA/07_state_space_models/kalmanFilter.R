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
plot(acc, main='Acceleration', type='l')

z <- x + rnorm(ts.len, sd=300)  # noisy measurement
par(mfrow=c(1, 1))
plot(x, ylim=range(c(x, z)), type='l')
lines(z, col=2)


kalman.motion <- function(z, Q, R, A, H) {
  dim.state <- dim(Q)[1]
  x.hat.minus <- array(rep(0, ts.len * dim.state), c(ts.len, dim.state))
  x.hat <- array(rep(0, ts.len * dim.state), c(ts.len, dim.state))
  P.minus <- array(rep(0, ts.len * dim.state^2), c(ts.len, dim.state, dim.state))
  P <- array(rep(0, ts.len * dim.state^2), c(ts.len, dim.state, dim.state))
  K <- array(rep(0, ts.len * dim.state), c(ts.len, dim.state))  # Kalman gain
  # init guess - 0 for all
  x.hat[1, ] <- rep(0, dim.state)
  P[1, ,] <- diag(dim.state)
  for (k in 2:ts.len) {
    # time update
    x.hat.minus [k, ] <- A %*% matrix(x.hat[k - 1,])
    P.minus[k, , ]<- A %*% P[k - 1, , ] %*% t(A) + Q
    K[k,] <- P.minus[k, ,] %*% H %*% solve(t(H) %*% P.minus[k, ,] %*% H + R)
    x.hat[k,] <- x.hat.minus[k,] + K[k,] %*% (z[k] - t(H) %*% x.hat.minus[k,])
    P[k, ,] <- (diag(dim.state) - K[k,] %*% t(H)) %*% P.minus[k, ,]
  }
  list(x.hat=x.hat, x.hat.minus=x.hat.minus)  # forecast and smooth
}


# Noise params
R <- 10^2  # meas var
Q <- 10    # tunable process var

# Dynamic params
A <- matrix(1)  # x[t] = Ax[t-1]; how prior x affects later x
H <- matrix(1)  # y[t] = Hx[t]; translating state

x.hat <- kalman.motion(z, diag(1) * Q, R, A, H)
lines(x.hat$x.hat, col=4)
lines(x.hat$x.hat.minus, col=3)

legend(
  'topleft', 
  lty=1, 
  col=c(1, 2, 4, 3), 
  legend=c('Actual', 'Measured', 'Kalman Filter', 'Smooth'))