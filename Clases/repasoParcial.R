# Acordarse de poner LINES, POINTS, etc, luego de un plot

set.seed(2023)

# Obtener estadístico de una distribución a partir de p-valor (cuantil 1-p).

qnorm(1-0.025, mean = 0, sd = 1)

# Función de distribución

pnorm(1)

# Muestra aleatoria

rnorm(10)


# Ejercicio 3 Repaso

# Inciso a)


#' Función que genera una observación para T1 y para T2
#' 
#' @param n Tamaño de muestra con distribución exp(lambda).
#' @param lambda Parámetro de la distribución.
#' @returns El primer componente corresponde a la observación de T1 y la segunda
#' a la observación de T2.
calculate_estimators <- function(n, lambda){
  sample1 <- rexp(n, rate=lambda)
  T1 <- mean(sample1)^2
  T2 <- 1/(n-1) * sum((sample1 - mean(sample1))^2)
  
  c(T1, T2)
}

n <- 20
lambda <- 1/2
calculate_estimators(n, lambda)

# Inciso b)

set.seed(2023)
Nreps <- 1000
T1_obs <- c()
T2_obs <- c()

for (i in 1:Nreps){
  obs <- calculate_estimators(n, lambda)
  T1_obs <- c(T1_obs, obs[1])
  T2_obs <- c(T2_obs, obs[2])
}

# Inciso c)

par(mfrow = c(1, 2))
ymin <- min(c(T1_obs, T2_obs))
ymax <- max(c(T1_obs, T2_obs))
boxplot(T1_obs, main="T1", ylim = c(ymin, ymax))
boxplot(T2_obs, main="T2", ylim = c(ymin, ymax))

# En ambos casos se ven colas gordas hacia valores más altos. En T2, sin embargo,
# se ve más cantidad de outliers.

# Inciso d)

# Sino, es con el comando density()

par(mfrow = c(1, 1))

kde_gauss <- function(datos, h, t){
  n <- length(datos)
  u <- (t - datos)/h
  return((1/(n*h)) * (1/sqrt(2*pi)) * sum(exp(-u^2 / 2)))
}

# Creo función para calcular ventana de Silverman

calculate_h_sil <- function(datos){
  n <- length(datos)
  datos_iqr <- IQR(datos)
  s<-sd(datos)
  1.06*min(s, datos_iqr/1.34)*n^(-1/5)
} 

x_graf <- seq(from=ymin, to=ymax, length.out=1000)
y_graf_t1 <- c()
y_graf_t2 <- c()
h_sil_t1 <- calculate_h_sil(T1_obs)
h_sil_t2 <- calculate_h_sil(T2_obs)

for (x in x_graf){
  y_graf_t1 <- c(y_graf_t1, kde_gauss(T1_obs, h_sil_t1, x))
}

plot(
  x_graf, 
  y_graf_t1, 
  main="Densidad estimada de T1", type="l", xlab="X", ylab="Densidad"
)


for (x in x_graf){
  y_graf_t2 <- c(y_graf_t2, kde_gauss(T2_obs, h_sil_t2, x))
}

plot(
  x_graf, 
  y_graf_t2, 
  main="Densidad estimada de T2", type="l", xlab="X", ylab="Densidad"
)


# Inciso e)

ecm_estimate <- function(datos, real){
  (1/length(datos))*sum((datos-real)^2)
}

estim_ecm_t1 <- ecm_estimate(T1_obs, (1/lambda^2))
estim_ecm_t2 <- ecm_estimate(T2_obs, (1/lambda^2))

# Empíricamente, T1 ha obtenido un error cuadrático medio menor. Esto puede tener sentido
# ya que T1 se construye con más información acerca de la muestra exponencial, que T2.

sample_datos<-scan(file = "./datos.txt")
