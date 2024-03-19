# Ejercicio 1

cicle <- function(n){
  t = 0
  for (i in 1:n){
    t = t + i
  }
  t
}

# 1.a

t <- cicle(1000)
print(t)
# 1.b

n_count <- 1
while (cicle(n_count) <= 1e5){
  n_count <- n_count + 1
}
print(n_count)

# 1.c

positive_elements_sum <- function(v){
  sum <- 0
  for (elem in v){
    sum <- sum + (elem > 0) * elem
  }
  sum
}

print(positive_elements_sum(seq(from=-10, to=5)))

# Ejercicio 2

f <- function(p){
  return(p*(1-p))
}
secuencia <- seq(from=0, to=1, by=0.05)
plot(secuencia, f(secuencia))

# Ejercicio 3

secuencia <- seq(from=-10, to=10, length.out=10000)
plot(secuencia, sin(secuencia), type="l")
lines(secuencia, cos(secuencia), col="green")
lines(secuencia, cos(secuencia^2), col="blue")

# Ejercicio 4

autos <- read.table("autos.txt", header=TRUE)

terceraFila <- autos[3,]

segundaColumna <- autos[,2]

cheapest_idx <- which.min(autos$precio)
cheapest_quality <- autos$calidad[cheapest_idx]

sum_first_four <- sum(autos$precio[1:4])

sum_columns <- apply(autos, 2, sum)

plot(autos$precio, autos$calidad, xlab="Precio", ylab="Calidad")

sorted_autos <- autos[order(autos$precio)]