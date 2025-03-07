library(readxl)
library(openxlsx)

spiegelMig <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/spiegel/spiegelMig.xlsx"))
spiegelAll_noMig <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/spiegel/spiegelAll_noMig.xlsx"))

spiegelFinal_Mig <- rbind(spiegelMig,spiegelAll_noMig)

#build sample with same amount of 0 and 1s in migration
num_zeros <- sum(spiegelFinal_Mig$migration == 0)
num_ones <- sum(spiegelFinal_Mig$migration == 1)

# Anzahl der Zeilen berechnen, die wir aus jeder Gruppe ausw?hlen m?ssen
num_samples <- min(num_zeros, num_ones)

# Zuf?llige Zeilen ausw?hlen
zero_rows <- sample(which(spiegelFinal_Mig$migration == 0), size = num_samples, replace = FALSE)
one_rows <- sample(which(spiegelFinal_Mig$migration == 1), size = num_samples, replace = FALSE)

# Die ausgew?hlten Zeilen in einen neuen Datensatz einf?gen
spiegelFinal_Mig <- spiegelFinal_Mig[c(zero_rows, one_rows), ]

write.xlsx(spiegelFinal_Mig, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/spiegel/spiegelFinal_Mig.xlsx", rowNames = FALSE)




#Final Sample for All vs. Opinion
spiegelOpi <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/spiegel/spiegelOpi.xlsx"))
spiegelAll_noOpi <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/spiegel/spiegelAll_noOpi.xlsx"))

spiegelFinal_Opi <- rbind(spiegelOpi,spiegelAll_noOpi)

#build sample with same amount of 0 and 1s in Opinion
num_zeros <- sum(spiegelFinal_Opi$Opinion == 0)
num_ones <- sum(spiegelFinal_Opi$Opinion == 1)

# Anzahl der Zeilen berechnen, die wir aus jeder Gruppe ausw?hlen m?ssen
num_samples <- min(num_zeros, num_ones)

# Zuf?llige Zeilen ausw?hlen
zero_rows <- sample(which(spiegelFinal_Opi$Opinion == 0), size = num_samples, replace = FALSE)
one_rows <- sample(which(spiegelFinal_Opi$Opinion == 1), size = num_samples, replace = FALSE)

# Die ausgew?hlten Zeilen in einen neuen Datensatz einf?gen
spiegelFinal_Opi <- spiegelFinal_Opi[c(zero_rows, one_rows), ]

write.xlsx(spiegelFinal_Opi, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/spiegel/spiegelFinal_Opi.xlsx", rowNames = FALSE)
