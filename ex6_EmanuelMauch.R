################################################################################
# Exercise 6
################################################################################

# a)
library(coda)

set.seed(44566)

M <- 1000

X <- rgamma(n = M, shape = 1.6, rate = 0.4)

X <- as.mcmc(X)

# Traceplot
traceplot(X)

# Histogram overlayed true density
seq <- seq(0, 20, by = 0.01)

hist(X, probability = TRUE, ylim = c(0, 0.25), breaks = 100)
lines(seq, dgamma(seq, shape = 1.6, rate = 0.4))
abline(v = mean(X), col = "magenta")
abline(v = median(X), col = "blue")

# Sample mean and sample median
mean(X)
median(X)
################################################################################

# b)
library(invgamma)

Y <- 1/X
Y <- as.mcmc(Y)

# Traceplot
traceplot(Y)

# Histogram overlayed true density
hist(Y, probability = TRUE, xlim = c(0, 10), ylim = c(0, 2.5), breaks = 100)
lines(seq, dinvgamma(seq, shape = 1.6, rate = 0.4))
abline(v = mean(Y), col = "magenta")
abline(v = median(Y), col = "blue")

# Sample mean and sample median
mean(Y)
median(Y)
################################################################################

# c)
Z <- sqrt(1/X)

# Density of the square-root inverse gamma distribution
sqinvgamma <- function(z, alpha, beta){
  beta^alpha/gamma(alpha) * 2/z^(1 + 2*alpha) * exp(-beta*1/z^2)
}


# Traceplot
traceplot(Z)

# Histogram overlayed true density
hist(Z, probability = TRUE, xlim = c(0, 6), ylim = c(0, 2.5), breaks = 100)
for (i in seq){
  points(i, sqinvgamma(z = i, alpha = 1.6, beta = 0.4))
}
abline(v = mean(Z), col = "magenta")
abline(v = median(Z), col = "blue")

# Sample mean and sample median
mean(Z)
median(Z)

# The Median of Y is 1/Median(X), and the median of Z is sqrt(1/Median(X))
# Since the median divides the probability mass of the density into to
# equally sized areas, the transformations
# can be applied to the median as well. For the mean, this does not work.


