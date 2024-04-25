n <- 1e5
rsample <- rnorm(n, mean=0, sd=5)

generalized_moments_estimator <- (1/n) * sqrt(pi/2) * sum(abs(rsample)) 