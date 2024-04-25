mu <- 0.7

prob <- c(mu^2, 2*mu*(1-mu), 0, (1-mu)^2)

rsample <- sample.int(4, size=1e5, replace=TRUE, prob=prob)
moment_estimator <- -sqrt(mean(rsample)) + 2