p <- 0.7
n <- 38415

bs <- rbinom(n, 1, 0.5)
rs <- c()

for (b in bs){
  if (b == 1){
    r <- runif(1) < p
    rs <- c(rs, r)
  }
  if (b == 0){
    r <- runif(1) < 0.4
    rs <- c(rs, r)
  }
}

intervalo <- c(
  2*mean(rs)-0.4-2*sqrt(mean(rs)*(1-mean(rs)))/sqrt(n),
  2*mean(rs)-0.4+2*sqrt(mean(rs)*(1-mean(rs)))/sqrt(n)
)