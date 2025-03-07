library(xlsx)
library(dplyr)
library(readxl)
library(openxlsx)

#### prep DW articles (Climate/Opinion)
TagesspiegelAll_mig  <- read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/Tagesspiegel/TagesspiegelAll.xlsx")
TagesspiegelOpi  <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/Tagesspiegel/TagesspiegelOpi.xlsx"))
TagesspiegelOpi2 <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/Tagesspiegel/TagesspiegelOpi2.xlsx"))
TagesspiegelMig  <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/Tagesspiegel/TagesspiegelMig3.xlsx"))


# combine Opi & Opi2
TagesspiegelOpi_combined <- rbind(TagesspiegelOpi, TagesspiegelOpi2)

# make sure that TagespiegelOpi_combined and TagespiegelAll have the same length
TagesspiegelOpi_combined <- TagesspiegelOpi_combined %>%
  slice(-1)

#take sample
TagesspiegelAll_mig <- sample_n(TagesspiegelAll_mig, 7141)


# construct flags for fine-tuning BERT
TagesspiegelAll_opi$opinion       <- 0
TagesspiegelOpi_combined$opinion  <- 1

TagesspiegelAll_mig$migration   <- 0
TagesspiegelMig$migration       <- 1

# bind datasets together
tagesspiegel_final_opi <- rbind(TagesspiegelOpi_combined, TagesspiegelAll_opi)
tagesspiegel_final_mig <- rbind(TagesspiegelMig, TagesspiegelAll_mig)

openxlsx::write.xlsx(tagesspiegel_final_opi, "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/Tagesspiegel/tagesspiegel_final_opi.xlsx", rowNames = F)
openxlsx::write.xlsx(tagesspiegel_final_mig, "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/Tagesspiegel/tagesspiegel_final_mig.xlsx", rowNames = F)
