par(mfrow = c(1,4))


# Normal  -----------------------------------------------------------------


n = 1000;
x = seq(0,1,length.out = n)
plot(x,(dnorm(x, mean = 0.5, sd = 0.05) - 1), type='l', ylab = expression(f[i](t/n)), xlab = expression(t/n), main = "n=1000")

n = 2000;
x = seq(0,1,length.out = 2000)
plot(x,(dnorm(x, mean = 0.5, sd = 0.05)-1), type='l',ylab = expression(f[i](t/n)), xlab = expression(t/n), main = "n=2000")


n = 5000;
x = seq(0,1,length.out = n)
plot(x,(dnorm(x, mean = 0.5, sd = 0.05)-1), type='l',ylab = expression(f[i](t/n)), xlab = expression(t/n), main ="n=5000")

n = 10000;
x = seq(0,1,length.out = n)
plot(x,(dnorm(x, mean = 0.5, sd = 0.05)-1), type='l',ylab = expression(f[i](t/n)), xlab = expression(t/n), main = "n=10000")



# Gamma -------------------------------------------------------------------
par(mfrow = c(1,4))
n = 1000;
x = seq(0,1,length.out = n)
plot(x, (dgamma(x, shape = 2, scale = 0.05)-1), type='l', ylab = expression(f[i](t/n)), xlab = expression(t/n), main = "n = 1000")

n = 2000;
x = seq(0,1,length.out = n)
plot(x, (dgamma(x, shape = 2, scale = 0.05)-1), type='l', ylab = expression(f[i](t/n)), xlab = expression(t/n), main = "n = 2000")

n = 5000;
x = seq(0,1,length.out = n)
plot(x, (dgamma(x, shape = 2, scale = 0.05)-1), type='l', ylab = expression(f[i](t/n)), xlab = expression(t/n), main = "n = 5000")

n = 10000;
x = seq(0,1,length.out = n)
plot(x, (dgamma(x, shape = 2, scale = 0.05)-1), type='l', ylab = expression(f[i](t/n)), xlab = expression(t/n), main = "n = 10000")


# checks ------------------------------------------------------------------

# Define the normal density function
normal_density <- function(x) {
  dnorm(x, mean = 0.5, sd = 0.05)-1
}

# Numerically integrate the normal density over the interval (0,1)
result <- integrate(normal_density, 0, 1)


print(result$value)  

gamma_density <- function(x) {
  dgamma(x, shape = 2 , scale = 0.05)-1
}
result <- integrate(gamma_density, 0, 1)

print(result$value)


# plot --------------------------------------------------------------------

par(mfrow = c(1,1))
plot(x, (dgamma(x, shape = 2, scale = 0.05)-1), type='l', ylim=c(-1,9),
     ylab = expression(f[i](t/n)), xlab = expression(t/n))
lines(x, (dnorm(x, mean = 0.5, sd = 0.05)-1), col="red")
legend("topright", c("gamma", "normal"), col=c("black", "red"), lty= c(1,1))
abline(h = 0, col="blue", lty=2)
