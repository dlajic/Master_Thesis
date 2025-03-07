library(openxlsx)
library(dplyr)
library(readr)
library(tidyr)
library(janitor)
library(htmlTable)
library(htmltools)


## Load Data -----

## Load analytic data (constructed in "0_2_survey_data_prep")
# contains only people having participated at least in two waves but also participants that read only unclassified articles
df.analysis <- read.xlsx("data/df_analysis.xlsx")


# get tracking data that entails only individuals having read at least one classified article in the time span used in the thesis
df.clean <- read_csv("data/df.clean.csv")


# get common new_ids 
common_new_ids <- intersect(df.analysis$new_id, df.clean$new_id)

# filter df.analysis, to get only new_ids that appear in both datasets so that only individuals that have participated in two waves and have read at least one classified article are included
df.analysis <- df.analysis %>%
  filter(new_id %in% common_new_ids)



# gender

gender <- df.analysis  %>%
  group_by(wave, female) %>% 
  count() %>%
  pivot_wider(names_from = wave, values_from = n) %>%
  rename(Wave1 = `1`, Wave2 = `2`, Wave3 = `3`) %>%
  mutate(female = case_when(female == 0 ~ "Male",
                          female == 1 ~ "Female",
                          .default = "Other")) %>%
  ungroup() %>%
  rename(Var = female) %>%
  mutate(across(starts_with("Wave"), ~round(./sum(.), digits = 3), .names = "perc_{.col}")) %>%
  adorn_totals()

  
# age
age <- df.analysis %>% 
  group_by(wave, age_cat) %>% 
  count() %>%
  pivot_wider(names_from = wave, values_from = n) %>%
  rename(Wave1 = `1`, Wave2 = `2`, Wave3 = `3`) %>%
  ungroup() %>%
  rename(Var = age_cat) %>%
  mutate(across(starts_with("Wave"), ~round(./sum(.), digits = 3), .names = "perc_{.col}")) %>%
  adorn_totals()

# education
edu1 <- df.analysis %>% 
  group_by(wave, ed_secondary_cat) %>% 
  count() %>%
  pivot_wider(names_from = wave, values_from = n)%>%
  rename(Wave1 = `1`, Wave2 = `2`, Wave3 = `3`) %>%
  ungroup() %>%
  slice(2,3,1,4) %>% # reorder
  rename(Var = ed_secondary_cat) %>%
  mutate(across(starts_with("Wave"), ~round(./sum(., na.rm=T), digits = 3), .names = "perc_{.col}")) %>%
  adorn_totals()

# voc training

edu2 <- df.analysis %>% 
  group_by(wave, voc_training_cat) %>% 
  count() %>%
  pivot_wider(names_from = wave, values_from = n) %>%
  rename(Wave1 = `1`, Wave2 = `2`, Wave3 = `3`) %>%
  rename(Var = voc_training_cat) %>% 
  ungroup() %>%
  slice(3,1,2,4) %>% # reorder
  mutate(across(starts_with("Wave"), ~round(./sum(., na.rm=T), digits = 3), .names = "perc_{.col}")) %>%
  adorn_totals()

# in-party

inparty <- df.analysis %>% 
  group_by(wave, which.inparty) %>% 
  count() %>%
  pivot_wider(names_from = wave, values_from = n) %>%
  rename(Wave1 = `1`, Wave2 = `2`, Wave3 = `3`) %>%
  rename(Var = which.inparty) %>%
  ungroup() %>%
  slice(2,8,5,4,3,1,7, 6) %>% # reorder
  mutate(across(starts_with("Wave"), ~round(./sum(., na.rm=T), digits = 3), .names = "perc_{.col}")) %>%
  adorn_totals()




table5 <- rbind(gender, age, edu1, edu2, inparty)

table5 <- table5 %>%  
  mutate(Wave1 = paste0(Wave1, " (", scales::percent(perc_Wave1,accuracy = 0.01), ")"),
         Wave2 = paste0(Wave2, " (", scales::percent(perc_Wave2,accuracy = 0.01), ")"),
         Wave3 = paste0(Wave3, " (", scales::percent(perc_Wave3,accuracy = 0.01), ")")) %>%
  select(Var, Wave1, Wave2, Wave3)
  

# construct html table
table5 <- htmlTable(table5, rnames = F, total = F)

# export
save_html(table5, file = "./Tables/table5.html")


