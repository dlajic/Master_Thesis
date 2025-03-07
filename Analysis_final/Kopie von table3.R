library(readxl)
library(openxlsx)
library(readr)
library(dplyr)
library(htmlTable)
library(htmltools)

# participation table from online panel access vendor
Feldarbeit_Doku <- read_excel("./data/14309 Uni Mannheim Feldarbeit Doku.xlsx")

# get tracking data containing also respondents who participated only in one wave
df.track <- readRDS("./data/track_survey_all3_waves_new_5_june_only_classified_mig.rds")


# get the final analysis sample which contains only people having participated at least in two waves
df.analysis <- read.xlsx("data/df_analysis.xlsx")

# get tracking data that entails only individuals having read at least one classified article in the time span used in the thesis
df.clean <- read_csv("data/df.clean.csv")

# get common new_ids 
common_new_ids <- intersect(df.analysis$new_id, df.clean$new_id)

# filter df.analysis, to get only new_ids that appear in both datasets so that only individuals that have participated in two waves and have read at least one classified article are included
df.analysis <- df.analysis %>%
  filter(new_id %in% common_new_ids)

# construct new variables
Feldarbeit_Doku$SuT <- 0
Feldarbeit_Doku$Analysis_Sample <- 0

# write S&T data to df 
Feldarbeit_Doku$SuT[1] <- df.track %>% filter(wave == 1) %>% distinct(new_id) %>% count()
Feldarbeit_Doku$SuT[3] <- df.track %>% filter(wave == 2) %>% distinct(new_id) %>% count()
Feldarbeit_Doku$SuT[4] <- df.track %>% filter(wave == 3) %>% distinct(new_id) %>% count()

# write analysis sample N to df
Feldarbeit_Doku$Analysis_Sample[1] <- df.analysis %>% filter(wave == 1) %>% distinct(new_id) %>% count() %>% as.numeric()
Feldarbeit_Doku$Analysis_Sample[3] <- df.analysis %>% filter(wave == 2) %>% distinct(new_id) %>% count() %>% as.numeric()
Feldarbeit_Doku$Analysis_Sample[4] <- df.analysis %>% filter(wave == 3) %>% distinct(new_id) %>% count() %>% as.numeric()

# calculate participation rates
Feldarbeit_Doku$S_Participation_Rate <- round(Feldarbeit_Doku$`Beendet (client)`/Feldarbeit_Doku$`Eingeladen (Total)`, digits = 2)*100
Feldarbeit_Doku$SuT_Participation_Rate <- round(as.numeric(Feldarbeit_Doku$SuT)/Feldarbeit_Doku$`Eingeladen (Total)`, digits = 2)*100

# construct df for table
table <- Feldarbeit_Doku %>% 
  select(Welle, Feldzeit, `Eingeladen (Total)`, `Beendet (client)`, SuT, Analysis_Sample, S_Participation_Rate, SuT_Participation_Rate) %>%
  rename(Wave = Welle,
         `Data collection` = Feldzeit,
         Invited = `Eingeladen (Total)`,
         `Completed Survey` = `Beendet (client)`,
         `S&T data available` = SuT,
         `Analysis sample` = Analysis_Sample,
         `S participation rate` = S_Participation_Rate,
         `S&T particpation rate` = SuT_Participation_Rate) %>%
  filter(Wave == "W1" | Wave == "W2" | Wave == "W3") %>%
  mutate(Wave = c("One", "Two", "Three"))

# construct html table
table3 <- htmlTable(table, rnames = F, total = F)

# export
save_html(table3, file = "./Tables/table3.html")
