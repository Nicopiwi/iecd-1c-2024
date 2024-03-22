#Ejercicio 1

data_survey <- read.table('./ENNyS_menorA2.txt', header=TRUE)

names(data_survey)

data_survey$Sexo <- as.factor(data_survey$Sexo)
data_survey$Tipo_embarazo <- as.factor(data_survey$Tipo_embarazo)

sexo_rel_freqs <- table(data_survey$Sexo) / sum(table(data_survey$Sexo))
print(sexo_rel_freqs)

embarazo_rel_freqs <- table(data_survey$Tipo_embarazo) / sum(
  table(data_survey$Tipo_embarazo)
)

print(embarazo_rel_freqs)

barplot(sexo_rel_freqs, width=0.02, col="red")
barplot(embarazo_rel_freqs, width=0.02, col="blue")

pie(sexo_rel_freqs)
pie(embarazo_rel_freqs)

# Ejercicio 2

contingency_sexo_embarazo <- table(data_survey$Sexo,
                                   data_survey$Tipo_embarazo)
barplot(contingency_sexo_embarazo,
        width=0.02,
        col = c("blue", "red"),
        beside=TRUE,
        legend = rownames(contingency_sexo_embarazo),
        args.legend = list(x = "topleft")
)


# Ejercicio 3
