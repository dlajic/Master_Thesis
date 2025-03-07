library(xlsx)
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)
library(stringr)


#### prep tagesschau articles (opinion/descriptive)
tagesschauAll <- read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/Scraping Tagesschau/tagesschauAll.xlsx")
tagesschauOpi <- read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/Scraping Tagesschau/tagesschauOpi.xlsx")


# filter on all rows in the first data frame that do not have a matching team in the second data frame
tagesschauOpi <- anti_join(tagesschauOpi, tagesschauAll, by = "link")


# construct flags for fine-tuning BERT
tagesschauOpi$opinion <- 1
tagesschauAll$opinion <- 0


tagesschau_final_opi <- rbind(tagesschauOpi, tagesschauAll)

openxlsx::write.xlsx(tagesschau_final_opi, "C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/tagesschau_final_opi.xlsx", rowNames = F)

