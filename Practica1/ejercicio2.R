data_titanic <- read.csv("./data/datos_titanic.csv")

get_proportion_woman_from_survived <- function() {
  # Por default, el índice de un DataFrame es en el eje de columnas
  survived <- data_titanic[data_titanic$Survived == 1, ]

  return(sum(survived$Sex == "female") / nrow(survived))
}

create_contingency_table_pclass_survived <- function() {
  contingency_table <- table(
    data_titanic$Survived,
    data_titanic$Pclass
  )

  return(contingency_table)
}

print(sprintf(
  "Proporción de mujeres dentro de sobrevivientes: %f",
  get_proportion_woman_from_survived()
))

contingency_table <- create_contingency_table_pclass_survived()

estimated_probs <- apply(
  contingency_table, 2,
  function(pclass) pclass[1] / sum(pclass)
)

for (pclass in colnames(contingency_table)) {
  print(sprintf(
    "Probabilidad estimada de sobrevivir para clase %s es %f",
    pclass,
    estimated_probs[pclass]
  ))
}

barplot(
  contingency_table,
  legend = c("No", "Sí"),
  main = "Grafico de contingencia",
  xlab = "Clase de pasajero",
  ylab = "Sobrevivió",
)
