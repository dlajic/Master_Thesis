library(readxl)
library(openxlsx)

tonlineMig <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/tonline/tonlineMig.xlsx"))
tonlineAll<- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/tonline/tonlineAll.xlsx"))

tonlineFinal <- rbind(tonlineMig,tonlineAll)

#build sample with same amount of 0 and 1s in migration
num_zeros <- sum(tonlineFinal$migration == 0)
num_ones <- sum(tonlineFinal$migration == 1)

# Anzahl der Zeilen berechnen, die wir aus jeder Gruppe ausw?hlen m?ssen
num_samples <- min(num_zeros, num_ones)

# Zuf?llige Zeilen ausw?hlen
zero_rows <- sample(which(tonlineFinal$migration == 0), size = num_samples, replace = FALSE)
one_rows <- sample(which(tonlineFinal$migration == 1), size = num_samples, replace = FALSE)

# Die ausgew?hlten Zeilen in einen neuen Datensatz einf?gen
tonlineFinal <- tonlineFinal[c(zero_rows, one_rows), ]


# Delete all rows which contains more than 32000 chars in $Text -> Ticker
count_chars <- function(x) {
  if (is.character(x)) {
    nchar(x)
  } else {
    0
  }
}

# Wenden Sie die Funktion auf die Textspalte des Dataframes an und prüfen Sie, ob die Anzahl der Zeichen in einer Zelle 32.000 überschreitet
long_cells <- sapply(tonlineFinal$Text, count_chars) > 32000

# Entfernen Sie die Zeilen, bei denen die Textspalte mehr als 32.000 Zeichen enthält
tonlineFinal <- tonlineFinal[!long_cells, ]

write.xlsx(tonlineFinal, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/tonline/tonlineFinal.xlsx", rowNames = FALSE)

