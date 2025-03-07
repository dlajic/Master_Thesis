library(xlsx)
library(dplyr)
library(readxl)
library(openxlsx)

#### prep DW articles (Climate/Opinion)
szAll_cli <- na.omit(read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/Scraping SZ/szAll_cli.xlsx"))
szAll_opi <- na.omit(read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/Scraping SZ/szAll_opi.xlsx"))
szCli     <- na.omit(read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/Scraping SZ/szCli.xlsx"))
szOpi     <- na.omit(read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/Scraping SZ/szOpi.xlsx"))

# construct flags for fine-tuning BERT
szAll_cli$environment <- 0
szCli$environment     <- 1

szAll_opi$opinion     <- 0
szOpi$opinion         <- 1

# delete word_count column (not needed anymore)
szAll_cli$word_count <- NULL
szAll_opi$word_count <- NULL

# bind datasets together
sz_final_opi <- rbind(szOpi, szAll_opi)
sz_final_cli <- rbind(szCli, szAll_cli)

openxlsx::write.xlsx(sz_final_opi, "C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/sz_final_opi.xlsx", rowNames = F)
openxlsx::write.xlsx(sz_final_cli, "C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/sz_final_cli.xlsx", rowNames = F)
