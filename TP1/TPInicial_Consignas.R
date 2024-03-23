# Actividad 1



data_prep <- function() {
    datos_encuesta <- read.table("./ENNyS_menorA2.txt", header = TRUE)

    datos_encuesta$Sexo <- as.factor(datos_encuesta$Sexo)
    datos_encuesta$Tipo_embarazo <- as.factor(datos_encuesta$Tipo_embarazo)
    datos_encuesta
}

datos <- data_prep()

ejercicio1 <- function() {
    hist(datos$Perim_encef, probability = TRUE)

    kernels <- c("gaussian", "epanechnikov", "rectangular")
    colors <- c("red", "blue", "green")

    for (i in seq_along(kernels)) {
        kde_perim_encef <- density(
            datos$Perim_encef,
            kernel = kernels[i]
        )
        lines(kde_perim_encef, col = colors[i], lw = "2")
    }
}

ejercicio1()

# Se puede observar que la densidad estimada por cada uno de los núcleos proporcionados ajustan
# de forma muy similar y que se asimilan al histograma.

print(sum(datos$Edad > 2))

# Es manual o hay que hacerlo más sofisticado
# Podemos usar esto ? integrate areas under curves
# library(sfsmisc)
# total <- integrate.xy(d$x, d$a) + integrate.xy(d$x, d$b)
ejercicio2 <- function() {
    # Consultar como hacerlo con el histograma.
    # ¿Debemos modificar el histograma default?
    histogram_perim_encef <- hist(
        datos$Perim_encef,
        probability = TRUE
    )
    lower_bound <- 42
    upper_bound <- 48

    kde_perim_encef_epa <- density(
        datos$Perim_encef,
        kernel = "epanechnikov"
    )

    indices_in_range <- (kde_perim_encef_epa$x >= lower_bound &
        kde_perim_encef_epa$x <= upper_bound)
    dx <- diff(kde_perim_encef_epa$x[1:2])

    density_in_range <- kde_perim_encef_epa$y[indices_in_range]
    prob_between <- sum(density_in_range) * dx
    print(prob_between)
}

ejercicio2()

# Consultar que concluir en qué pasa
ejercicio3 <- function() {
    kernels <- c("gaussian", "epanechnikov", "rectangular")
    colors <- c("red", "blue", "green")
    dx <- NULL
    for (i in seq_along(kernels)) {
        kde_perim_encef <- density(
            datos$Perim_encef,
            kernel = kernels[i]
        )
        dx <- diff(kde_perim_encef$x[1:2])
        cat("Ventana para núcleo", kernels[i], ":", dx, "\n")
    }

    for (i in seq_along(kernels)) {
        kde_perim_encef <- density(
            datos$Perim_encef,
            kernel = kernels[i],
            width = dx / 2
        )

        if (i == 1){
            plot(
                kde_perim_encef,
                col = colors[i],
                main = sprintf("Densidad estimada con ventana %f", dx / 2)
            )
        }
        else{
            lines(
                kde_perim_encef,
                col = colors[i],
                lw = "2"
            )
        }
    }

    for (i in seq_along(kernels)) {
        kde_perim_encef <- density(
            datos$Perim_encef,
            kernel = kernels[i],
            width = dx * 2
        )

        if (i == 1){
            plot(
                kde_perim_encef,
                col = colors[i],
                main = sprintf("Densidad estimada con ventana %f", dx * 2)
            )
        }
        else{
            lines(
                kde_perim_encef,
                col = colors[i],
                lw = "2"
            )
        }
    }
}

ejercicio3()
