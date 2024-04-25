n <- 1e3
N <- 1e3
thita <- 0.8
estim <- exp(-thita)
asympt_1 <- c()
asympt_2 <- c()

for (i in 1:N){
  rsample <- rexp(n, rate=thita)
  T1 <- exp(-1/mean(rsample))
  T2 <- mean(rsample >= 1)
  
  asympt_1 <- c(asympt_1, sqrt(n)*(T1-estim))
  asympt_2 <- c(asympt_2, sqrt(n)*(T2-estim))
}

# Se demuestra que asintóticamente el error se comporta como una normal, con las siguientes varianzas
cat("Varianza asintótica T1:", exp(-2*thita)*thita^2, ". Varianza conseguida:", var(asympt_1))
cat("Varianza asintótica T2:", exp(-thita)-exp(-2*thita), ". Varianza conseguida:", var(asympt_2))