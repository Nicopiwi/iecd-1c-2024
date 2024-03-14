data_patients <- read.csv("./data/Debernardi.csv")

data_patients$diagnosis <- as.character(data_patients$diagnosis)

get_relative_freqs_by_diagnosis <- function(diagnosis) {
  unique_groups <- unique(diagnosis)
  freqs <- numeric(length(unique_groups))
  for (i in 1:length(unique_groups)) {
    freqs[i] <- mean(diagnosis == unique_groups[i])
  }
  data.frame(diagnosis = unique_groups, relative_freq = freqs)
}

relative_freqs_by_diagnosis <- get_relative_freqs_by_diagnosis(
  data_patients$diagnosis
)

barplot(
  relative_freqs_by_diagnosis$relative_freq, 
  names.arg = relative_freqs_by_diagnosis$diagnosis, 
  xlab = "Diagnóstico", 
  ylab = "FR", 
  main = "Frecuencias relativas por diagnóstico",
  ylim=c(0, 1)
)
