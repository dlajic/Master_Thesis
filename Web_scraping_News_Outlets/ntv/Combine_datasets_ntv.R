library(xlsx)
library(dplyr)
library(readxl)
library(openxlsx)

#### prep DW articles (Climate/Opinion)
ntvAll <- na.omit(read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/Scraping ntv/ntvAll.xlsx"))
ntvCli <- na.omit(read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/Scraping ntv/ntvCli.xlsx"))

ntvAll <- ntvAll %>%
  filter(!grepl("(?i)Klimaschutz.*|(?i)Klimakaschütz.*|(?i)Klimakatastrophe|(?i)Klimawandel|(?i)Klimaerwärmung|(?i)Erderwärmung|(?i)globale Erwärmung|(?i)Klimadebatte|(?i)Klimapolitik|(?i)Klimakonferenz|(?i)Weltklimarat.*|(?i)Treibhauseffekt|(?i)klimaschädlich.*|(?i)klimafreundlich.*|(?i)Klimagipfel|(?i)Klimaziel|(?i)Klimaaktivist.*|(?i)Klimaclub|(?i)Klimakollaps|(?i)Klimarat|(?i)Klimakongress|(?i)Klimastiftung|(?i)Klimakrise|(?i)Klimanotstand|(?i)Klimakatastrophe", Title1)) %>%
  filter(!grepl("(?i)Klimaschutz.*|(?i)Klimakaschütz.*|(?i)Klimakatastrophe|(?i)Klimawandel|(?i)Klimaerwärmung|(?i)Erderwärmung|(?i)globale Erwärmung|(?i)Klimadebatte|(?i)Klimapolitik|(?i)Klimakonferenz|(?i)Weltklimarat.*|(?i)Treibhauseffekt|(?i)klimaschädlich.*|(?i)klimafreundlich.*|(?i)Klimagipfel|(?i)Klimaziel|(?i)Klimaaktivist.*|(?i)Klimaclub|(?i)Klimakollaps|(?i)Klimarat|(?i)Klimakongress|(?i)Klimastiftung|(?i)Klimakrise|(?i)Klimanotstand|(?i)Klimakatastrophe", Title2)) %>%
  filter(!grepl("(?i)Klimaschutz.*|(?i)Klimakaschütz.*|(?i)Klimakatastrophe|(?i)Klimawandel|(?i)Klimaerwärmung|(?i)Erderwärmung|(?i)globale Erwärmung|(?i)Klimadebatte|(?i)Klimapolitik|(?i)Klimakonferenz|(?i)Weltklimarat.*|(?i)Treibhauseffekt|(?i)klimaschädlich.*|(?i)klimafreundlich.*|(?i)Klimagipfel|(?i)Klimaziel|(?i)Klimaaktivist.*|(?i)Klimaclub|(?i)Klimakollaps|(?i)Klimarat|(?i)Klimakongress|(?i)Klimastiftung|(?i)Klimakrise|(?i)Klimanotstand|(?i)Klimakatastrophe", Text)) %>%
  filter(!grepl("(?i)Klimaschutz.*|(?i)Klimakaschütz.*|(?i)Klimakatastrophe|(?i)Klimawandel|(?i)Klimaerwärmung|(?i)Erderwärmung|(?i)globale Erwärmung|(?i)Klimadebatte|(?i)Klimapolitik|(?i)Klimakonferenz|(?i)Weltklimarat.*|(?i)Treibhauseffekt|(?i)klimaschädlich.*|(?i)klimafreundlich.*|(?i)Klimagipfel|(?i)Klimaziel|(?i)Klimaaktivist.*|(?i)Klimaclub|(?i)Klimakollaps|(?i)Klimarat|(?i)Klimakongress|(?i)Klimastiftung|(?i)Klimakrise|(?i)Klimanotstand|(?i)Klimakatastrophe", link)) %>%
  filter(!grepl("(?i)Klimaschutz.*|(?i)Klimakaschütz.*|(?i)Klimakatastrophe|(?i)Klimawandel|(?i)Klimaerwärmung|(?i)Erderwärmung|(?i)globale Erwärmung|(?i)Klimadebatte|(?i)Klimapolitik|(?i)Klimakonferenz|(?i)Weltklimarat.*|(?i)Treibhauseffekt|(?i)klimaschädlich.*|(?i)klimafreundlich.*|(?i)Klimagipfel|(?i)Klimaziel|(?i)Klimaaktivist.*|(?i)Klimaclub|(?i)Klimakollaps|(?i)Klimarat|(?i)Klimakongress|(?i)Klimastiftung|(?i)Klimakrise|(?i)Klimanotstand|(?i)Klimakatastrophe", Lead))

# construct flags for fine-tuning BERT
ntvAll$environment <- 0
ntvCli$environment <- 1

# take sample of augsburger_allgemeineCli to match length of augsburger_allgemeineAll
ntvAll <- sample_n(ntvAll, 447)

# bind datasets together
ntv_final_cli <- rbind(ntvCli, ntvAll)

write.xlsx(ntv_final_cli, file= "C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/ntv_final_cli.xlsx", rowNames = FALSE)
