library(xlsx)
library(dplyr)
library(readxl)
library(openxlsx)

#### prep DW articles (Migration/Opinion)
ntvAll <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/ntv/ntvAll.xlsx"))
ntvMig <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/ntv/ntvMig.xlsx"))

# construct flags for fine-tuning BERT
ntvAll$migration <- 0
ntvMig$migration <- 1

# take sample of augsburger_allgemeineCli to match length of augsburger_allgemeineAll
ntvAll <- sample_n(ntvAll, 665)

# bind datasets together
ntv_final_mig <- rbind(ntvMig, ntvAll)

write.xlsx(ntv_final_mig, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/ntv/ntv_final_mig.xlsx", rowNames = FALSE)
