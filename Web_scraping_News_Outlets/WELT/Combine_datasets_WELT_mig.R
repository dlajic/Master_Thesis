library(xlsx)
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)
library(stringr)

# remove rows with emtpy cells
WELTMig <- read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/WELT/WELTMig.xlsx")
WELTOpi <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/WELT/WELTOpi.xlsx"))
WELTAll <- na.omit(read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/WELT/WELTAll.xlsx"))


# filter on all rows in the first data frame that do not have a matching team in the second data frame
WELTAll_opi <- anti_join(WELTAll, WELTOpi, by = "link")
WELTAll_mig <- anti_join(WELTAll, WELTMig, by = "link")


# filter out articles longer than 30000 characters (typ: longread)
WELTAll_opi$cnt_characters <- str_count(WELTAll_opi$Text)
WELTAll_mig$cnt_characters <- str_count(WELTAll_mig$Text)
WELTOpi$cnt_characters <- str_count(WELTOpi$Text)
WELTMig$cnt_characters <- str_count(WELTMig$Text)

WELTAll_opi <- WELTAll_opi[WELTAll_opi$cnt_characters <= 30000, ]
WELTAll_mig <- WELTAll_mig[WELTAll_mig$cnt_characters <= 30000, ]
WELTOpi     <- WELTOpi[WELTOpi$cnt_characters <= 30000, ]
WELTmig     <- WELTMig[WELTMig$cnt_characters <= 30000, ]

WELTAll_opi <- WELTAll_opi[,-6]
WELTAll_mig <- WELTAll_mig[,-6]
WELTOpi     <- WELTOpi[,-6]
WELTMig     <- WELTMig[,-6]

# Note: in WELTAll/WELTAll_mig are no opinion pieces 

# get sample out of WELTAll_mig matching the obs. of WELTmig and get sample out of WELTOpi matching the obs. of WELTAll_opi
#WELTAll_mig_sample <- sample_n(WELTAll_mig, size = 1649)
WELTOpi_sample <- sample_n(WELTOpi, size = 5422)


# construct flags for fine-tuning BERT
WELTAll_mig$migration <- 0
WELTMig$migration <- 1

WELTAll_opi$opinion <- 0
WELTOpi_sample$opinion <- 1 


WELT_final_opi <- rbind(WELTOpi_sample, WELTAll_opi)
WELT_final_mig <- rbind(WELTMig, WELTAll_mig)

openxlsx::write.xlsx(WELT_final_opi, "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/WELT/WELT_final_opi.xlsx", rowNames = F)
openxlsx::write.xlsx(WELT_final_mig, "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/WELT/WELT_final_mig.xlsx", rowNames = F)

