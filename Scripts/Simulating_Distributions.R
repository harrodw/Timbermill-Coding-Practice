# Sample size
n <- 100000

# Poisson
lambda <- 0.1

hist(rpois(n, lambda))


# Beta
alpha <- 1
beta <- 1.1

hist(rbeta(n, alpha, beta))
