library(xlsx)
library(dplyr)
library(readxl)
library(openxlsx)


#### prep rnd articles (Migration/Opinion)
rndMig      <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/rnd/rndMig.xlsx", col_types = c("date", "text", "text", "text", "text")))
rndOpi      <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/rnd/rndOpi.xlsx", col_types = c("date", "text", "text", "text", "text")))

rndHealth   <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/rnd/rndHealth.xlsx", col_types = c("date", "text", "text", "text", "text")))
rndSport    <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/rnd/rndSport.xlsx", col_types = c("date", "text", "text", "text", "text")))
rndEcon     <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/rnd/rndEcon.xlsx", col_types = c("text", "text", "text", "text", "text")))
rndPol      <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/rnd/rndPol.xlsx", col_types = c("date", "text", "text", "text", "text")))
rndTec      <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/rnd/rndTec.xlsx", col_types = c("date", "text", "text", "text", "text")))



#rndMig$flag <- "Climate"
#rndOpi$flag <- "Opinion"
#
#rndEcon$flag <- "Economy"
#rndPol$flag <- "Politics"
#rndSport$flag <- "Sports"
#rndHealth$flag <- "Health"
#rndTec$flag <- "Technic"

rndEcon$Date <- as.Date(rndEcon$Date)

# combine all datasets
rndAll <- rbind(rndEcon, rndHealth, rndPol, rndSport, rndTec)

# filter on all rows in the first data frame that do not have a matching team in the second data frame
rndAll_Mig <- anti_join(rndAll, rndMig, by = "link")
rndAll_Opi <- anti_join(rndAll, rndOpi, by = "link")

# get sample out of rndAll matching the obs. of rndMig
rndAll_Mig_sample <- sample_n(rndAll_Mig, size = nrow(rndMig))
rndAll_Opi_sample <- sample_n(rndAll_Opi, size = nrow(rndOpi))

# construct flags for fine-tuning BERT
rndAll_Mig_sample$migration <- 0
rndMig$migration <- 1

rndAll_Opi_sample$opinion <- 0
rndOpi$opinion <- 1

rndAll_Opi_sample$Date <- as.Date(rndAll_Opi_sample$Date)
rndAll_Mig_sample$Date <- as.Date(rndAll_Mig_sample$Date)

# construct final dataset
rnd_final_mig <- rbind(rndMig, rndAll_Mig_sample)
rnd_final_opi <- rbind(rndOpi, rndAll_Opi_sample)

openxlsx::write.xlsx(rnd_final_mig, "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/rnd/rnd_final_mig.xlsx", rowNames = F)
openxlsx::write.xlsx(rnd_final_opi, "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/rnd/rnd_final_opi.xlsx", rowNames = F)


