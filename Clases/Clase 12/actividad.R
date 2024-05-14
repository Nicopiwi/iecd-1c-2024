set.seed(2023)
n <- 500 
sexp <- rexp(n)
sample_mean <- mean(sexp)
sample_median <- median(sexp)
N <- 1000
samples <- list()

for (i in 1:N){
  new_sample <- sample(sexp, n, replace=TRUE)
  samples <- append(samples, list(new_sample))
}

medians <- c()
means <- c()

for (bootstrap_sample in samples){
  medians <- c(medians, median(bootstrap_sample))
  means <- c(means, mean(bootstrap_sample))
}

hist(medians, main="Histograma de las medianas bootstrap")
hist(means, main="Histograma de las medias bootstrap")

kde_medians <- density(medians)
kde_means <- density(means)

alpha <- 0.05

z <- qnorm(1-alpha/2) 
ic_bootstrap_median <- c(sample_median - z*sd(medians), sample_median + z*sd(medians))
ic_bootstrap_mean <- c(sample_mean - z*sd(means), sample_mean + z*sd(means))