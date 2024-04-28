Y <- c(1.52, 1.65, 1.72, 1.65, 1.72, 1.83, 1.61, 1.75, 1.72, 1.68, 1.51, 1.65, 1.58,
       1.65, 1.61, 1.70, 1.60, 1.73, 1.61, 1.52, 1.81, 1.72, 1.50, 1.82, 1.65)

alpha <- 0.05
mean_y <- mean(Y)
std_y <- sd(Y)
t <- qt(1-alpha/2, df=24)

#Intervalo de confianza para mu de 0.95
l_mu <- c(mean_y - std_y*t/5, mean_y + std_y*t/5)

#Intervalo de confianza para exp(-mu) de 0.95
l_exp_neg_mu <- c(exp(-mean_y - std_y*t/5), exp(-mean_y + std_y*t/5))