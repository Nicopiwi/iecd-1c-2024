rsample <- rpois(10000, 5)

first_moment_estimator <- mean(rsample)
second_moment_estimator <- (-1 + sqrt(1+4*mean(rsample)^2))/2
