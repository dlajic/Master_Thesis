## -------------------------

## Load Packages -----
library(tidyverse)
library(lubridate)
library(kableExtra)
library(dplyr)
library(openxlsx)
library(readr)
library(janitor)

## Load Data -----

setwd("D:/MASTERARBEIT/Analysis")

alt_tracking <- readRDS("./data/FAKE_tracking_combined.rds")

mainstream_tracking <- readRDS("./data/NEWS_tracking_combined.rds")

all_scraped <- readRDS("./data/scraped_newspapers_all.rds")

classified_all <- readxl::read_excel("./data/all_news_clean.xlsx") %>% 
  select(link) # unique URLs, duplicate titles (match on URL)
  
classified_mig <- readxl::read_excel("./data/news_categorised_final_all_Mig_NER_BERTopic.xlsx") %>% 
  select(-c(text, tags, lead)) # unique titles (match on title)

## Survey-tracking linkage 
all_dat_avail <- readRDS("./data/track_survey_all3_waves_new_5_june_mig.rds")
is_in_model <- read.csv("./data/df_analysis_mig.csv") %>% 
  distinct(new_id) %>% 
  mutate(is_in_model = T,
         new_id = as.integer(new_id))
all_dat_avail <- left_join(all_dat_avail, is_in_model, by = "new_id")
all_dat_avail <- all_dat_avail %>% 
  mutate(is_in_model = replace_na(is_in_model, F))


## Data Preparation ----

## Tidy tracking data for alt and main news

alt_tracking <- alt_tracking %>% 
  select(new_id, domain, url, used_at) %>% 
  mutate(is_alt_news=1)

mainstream_tracking <- mainstream_tracking %>% 
  select(new_id, domain, url, used_at) %>% 
  mutate(is_alt_news=0)


all_tracking <- bind_rows(mainstream_tracking, alt_tracking)

## Tidy scraped articles

all_scraped <- all_scraped %>% 
  mutate(url = str_remove(link,"https://"),
         scraped = T) %>% 
  select(-c(text, lead, tags)) 

# Merge original scraped data to tracking data

df <- left_join(all_tracking, all_scraped, by = c("url", "is_alt_news"))

# Know which articles were used in classification model

df <- left_join(df, classified_all %>% mutate(classified=T))

# Add migration classification

df <- left_join(df, classified_mig %>% select(-word_count) %>% 
                  drop_na(title), by = "title", suffix = c("", ".y")) %>% 
  select(-c(ends_with(".y"))) %>% # drop duplicate columns
  rename(topic_number = Topic,
         topic_count = Count,
         topic_keywords = Representation,
         topic_label = Name,
         topic_super_category = "Super Category")

# Tidy URL flags

df <- df %>% 
  mutate(scraped = replace_na(scraped, F),
         classified = replace_na(classified, F),
         migration = case_when(
           is.na(migration) & classified ~ 0, # classified but not migration  
           TRUE ~ migration
         ))


# Exclude more unnecessar columns

df <- df %>% 
  select(-c(language, source))



## Get N of scraped articles before filtering

df %>% 
  group_by(is_alt_news) %>% 
  summarise(n = n_distinct(title))


## Exclude data from individuals not in survey 

df <- df %>% 
  filter(new_id %in% all_dat_avail$new_id)

## Exclude post-w3 data

# Get days of participation

temp <- all_dat_avail[all_dat_avail$is_in_model==T,]

temp <- temp %>% 
  group_by(new_id) %>% 
  mutate(last_survey_day = max(as_date(na.omit(surv.part.date))),
         participation_days = list(surv.part.date)) %>% 
  select(new_id, last_survey_day, participation_days) %>% 
  distinct(new_id,.keep_all = T) 

temp <- temp[temp$new_id %in% df$new_id,]  # this excludes ppl who didn't consume news but still logged

df <- left_join(df, temp)

# Format date
df$used_day <- lubridate::as_date(df$used_at)


# Filter before last day of participation
df <- df %>% 
  filter(used_day < last_survey_day) # ppl not in model excluded here because they lack last_survey_day (see above)


# Analysis -----

#!!!
# get again duration 

all_track_with_all_news_flag <- readRDS("./data/all_track_with_all_news_flag.rds")

#  left join duration to df
df <- left_join(df, all_track_with_all_news_flag, by = c("domain", "new_id", "used_at", "url"))
#!!!


#Save this df and filter in 03_survey_analysis analysis_df, just for the ids that occur here (only ids included that have classified articles from whom we know what content was inlcuded in the article)
df_2 <- df %>%
  filter(classified == T)

write_csv(df_2, "df.clean.csv")



#* Visited outlets ####

# some titles have more than one domain
# classified
main.outlets.visits1 <- df %>% 
  filter(classified == T & is_alt_news == 0) %>% # filter
  group_by(domain) %>%
  summarise(n_articles_classified = n_distinct(title, na.rm=T),
            n_visits = n(),
            Hours = sum(duration)/3600) %>% # only visits to classified articles
  arrange(desc(n_articles_classified))


# since I filter for classified and some titles appear with both booleans (True & False)
# I filter all double entries out within the summarise and n_distinct command

filter1 <- df %>%
  filter(is_alt_news == 0) %>%
  group_by(domain, title) %>%
  filter(any(classified == TRUE) && any(classified == FALSE)) %>%
  distinct(title, classified) %>%
  distinct(title) %>%
  filter(title != "NA") # remove NA

# unclassified
main.outlets.visits2 <- df %>% 
  filter(classified == F & is_alt_news ==0) %>% 
  group_by(domain) %>%
  summarise(n_articles_unclassified = n_distinct(title[!title %in% filter1$title], na.rm=T),
            n_visits_unclassifed = n(),
            Hours_unclassified = sum(duration)/3600) %>% 
  arrange(desc(n_visits_unclassifed))

add_totals <- function(x) x %>%
  bind_rows(summarise(., across(where(is.numeric), ~ ifelse(all(is.na(.)), NA, sum(., na.rm = T))),
                      across(where(is.character), ~ "Total")))


# HTML table

table8 <- main.outlets.visits1 %>% 
  full_join(main.outlets.visits2, main.outlets.visits1,  by = "domain") %>%
  add_totals %>%  
  kbl(format = "html", longtable = T, booktabs = T,
      format.args = list(big.mark = ","), digits = 2,
      col.names = c("Domain", "N articles classified", "N visits to classified articles", "Total duration of classified articles consumption in hours",
                    "N articles unclassified", "N visits to unclassified articles", "Total duration of unclassified articles consumption in hours"),
      caption = "List of mainstream outlets by domain \\label{tab:list.main.outlets}") %>% 
  kable_styling(position = "center")

save_kable(table8, file = "./Tables/table8.html")


# classified
alt.outlets.visits1 <- df %>% 
  filter(is_alt_news==1 & classified == T) %>% 
  group_by(domain) %>% 
  summarise(n_articles_classified = n_distinct(title, na.rm=T),
            n_visits = n(),
            Hours = sum(duration)/3600) %>% 
  arrange(desc(n_articles_classified))


# since I filter for classified and some titles appear with both booleans (True & False)
# I filter all double entries out within the summarise and n_distinct command
filter2 <- df %>%
  filter(is_alt_news == 1) %>%
  group_by(domain, title) %>%
  filter(any(classified == TRUE) && any(classified == FALSE)) %>%
  distinct(title, classified) %>%
  distinct(title) %>%
  filter(title != "NA")


# unclassified
alt.outlets.visits2 <- df %>% 
  filter(is_alt_news==1 & classified == F) %>% 
  group_by(domain) %>% 
  summarise(n_articles_unclassified = n_distinct(title[!title %in% filter2$title], na.rm=T),
            n_visits_unclassifed = n(),
            Hours_unclassified = sum(duration)/3600) %>% 
  arrange(desc(n_articles_unclassified))


table9 <- alt.outlets.visits1 %>%
  full_join(alt.outlets.visits2, alt.outlets.visits1,  by = "domain") %>%
  add_totals %>%
  kbl(format = "html", longtable = T, booktabs = T,
      format.args = list(big.mark = ","), digits = 2,
      col.names = c("Domain", "N articles classified", "N visits to classified articles", "Total duration of classified articles consumption in hours",
                    "N articles unclassified", "N visits to unclassified articles", "Total duration of unclassified articles consumption in hours"),
      caption = "List of alternative outlets by domain \\label{tab:list.alt.outlets}") %>% 
  kable_styling(position = "center")

save_kable(table9, file = "./Tables/table9.html")


## table 1

# Count unique articles
# Note: In Joaos code n_classified was counted across the whole datasets which returns a smaller number than this code because there are the same titles that appear in multiple domains
# same applies for n_scraped
count <- df %>% 
  group_by(is_alt_news, domain) %>% 
  summarise(n_urls = n_distinct(url, na.rm=T),
            n_scraped = n_distinct(title, na.rm=T),
            n_classified = n_distinct(title[classified == T], na.rm=T),
            n_mig = n_distinct(title[migration==1], na.rm=T),
            n_mig_opinion = n_distinct(title[migration==1 & opinion==1], na.rm=T),
            n_mig_descriptive = n_distinct(title[migration==1 & opinion==0], na.rm=T)) %>% 
  summarise(n_urls = sum(n_urls),
            n_scraped = sum(n_scraped),
            n_classified = sum(n_classified),
            n_mig = sum(n_mig),
            n_mig_opinion = sum(n_mig_opinion),
            n_mig_descriptive = sum(n_mig_descriptive)) %>%
  pivot_longer(cols = starts_with("n_"),
               names_to = "stat",
               names_prefix = "n_",
               values_to = "n") %>% 
  pivot_wider(names_from = "is_alt_news",
              values_from = "n") %>% 
  rename(mainstream_news_count = `0`,
         alt_news_count = `1`)


# count visits to the previously unique counted urls 
visits <- df %>% 
  group_by(is_alt_news) %>% 
  summarise(visits_urls = sum(length(url)),
            visits_scraped = sum(scraped),
            visits_classified = sum(classified),
            visits_mig = sum(migration, na.rm=T),
            visits_mig_opinion = sum(opinion, na.rm=T),
            visits_mig_descriptive = visits_mig - visits_mig_opinion) %>% 
  pivot_longer(cols = starts_with("visits_"),
               names_to = "stat",
               names_prefix = "visits_",
               values_to = "n") %>% 
  pivot_wider(names_from = "is_alt_news",
              values_from = "n") %>% 
  rename(mainstream_news_visits = `0`,
         alt_news_visits = `1`)


# get duration of visiting news websites
duration <- df %>% 
  group_by(is_alt_news) %>% 
  summarise(dur_urls = sum(duration)/3600,
            dur_scraped = sum(duration[scraped])/3600,
            dur_classified = sum(duration[classified])/3600,
            dur_mig = sum(duration[migration == 1], na.rm=T)/3600,
            dur_mig_opinion = sum(duration[migration == 1 & opinion == 1] , na.rm=T)/3600,
            dur_mig_descriptive = dur_mig - dur_mig_opinion) %>% 
  pivot_longer(cols = starts_with("dur_"),
               names_to = "stat",
               names_prefix = "dur_",
               values_to = "dur") %>% 
  pivot_wider(names_from = "is_alt_news",
              values_from = "dur") %>% 
  rename(mainstream_news_hours = `0`,
         alt_news_hours = `1`)

# Users

#tracking.wide <- df %>% 
#  group_by(new_id, is_alt_news) %>% 
#  summarise(n_urls = n(),
#            n_scraped = sum(scraped),
#            n_classified = sum(classified),
#            n_mig = sum(migration, na.rm=T),
#            n_mig_opinion = sum(opinion, na.rm=T),
#            n_mig_descriptive = n_mig - n_mig_opinion) %>% 
#  pivot_wider(names_from = "is_alt_news",
#              values_from = starts_with("n_"))

users <- df %>% 
  group_by(is_alt_news) %>% 
  summarise(users_urls = n_distinct(new_id),
            users_scraped = n_distinct(new_id[scraped]),
            users_classified = n_distinct(new_id[classified]),
            users_mig = n_distinct(new_id[migration==1], na.rm=T),
            users_mig_opinion = n_distinct(new_id[opinion==1], na.rm=T),
            users_mig_descriptive = n_distinct(new_id[opinion==0], na.rm=T)) %>% 
  pivot_longer(cols = starts_with("users_"),
               names_to = "stat",
               names_prefix = "users_",
               values_to = "n") %>% 
  pivot_wider(names_from = "is_alt_news",
              values_from = "n") %>% 
  rename(mainstream_news_users = `0`,
         alt_news_users = `1`)

temp.list <- list(count, visits, duration, users)

count_visits_users <- temp.list %>% 
  reduce(full_join, by = "stat") %>% 
  select(stat,starts_with("main"), everything()) %>% 
  mutate(stat = factor(stat, levels = c("urls", "scraped","classified" ,"mig","mig_opinion", "mig_descriptive"),
                       labels = c("All URLs", "Scraped URLs", "Classified URLs","Migration-related", "Opinion articles", "Descriptive articles")))


table1 <- kbl(count_visits_users %>% arrange(stat), format = "html", format.args = list(big.mark = ","),
    booktabs = T,col.names = c("", "Unique count", "Visits", "Hours", "Users" ,"Unique count", "Visits", "Hours", "Users"), digits = 2) %>% 
  add_header_above(header = c(" " = 1,"Mainstream News" = 4, "Alternative News" = 4)) %>% 
  kable_styling(position = "center") %>% 
  add_indent(c(5, 6),all_cols = F)


save_kable(table1, file = "./Tables/table1.html")

## Topics

#N of articles per topic

topic_articles <- df %>% 
  filter(migration==1) %>%
  group_by(domain) %>%
  distinct(title,.keep_all = T) %>% 
  group_by(is_alt_news) %>% 
  count(topic_label) %>% 
  rename(n_articles = n)

#N of visits per topic
topic_visits <- df %>% 
  group_by(is_alt_news) %>% 
  drop_na(topic_label) %>% # exclude unclassified articles
  count(topic_label) %>% 
  rename(n_visits = n)

# duration of visits per topic
topic_duration <- df %>% 
  group_by(is_alt_news, topic_label) %>% 
  drop_na(topic_label) %>% # exclude unclassified articles
  summarise(n_hours = round(sum(duration)/3600, digits = 2))


merged1 <- merge(topic_articles, topic_visits)
topic_articles_visits <- merge(merged1, topic_duration)

topic_articles_visits <- topic_articles_visits %>% 
  pivot_wider(names_from = is_alt_news,
              values_from = starts_with("n")) %>% 
  select(topic_label, ends_with("0"), everything())


# Create subtables for table 11
WithdrawalAfghanistan <- topic_articles_visits %>% 
  slice(c(1)) %>%
  adorn_totals()

MigrationEUBorderBelarus  <- topic_articles_visits %>% 
  slice(c(2)) %>%
  adorn_totals()

EUAgreementTurkey <- topic_articles_visits %>% 
  slice(c(22)) %>%
  adorn_totals()

MigrationInternational <- topic_articles_visits %>% 
  slice(c(19,4,9,12)) %>%
  adorn_totals()

MediterraneanSeaRescue <- topic_articles_visits %>% 
  slice(c(20)) %>%
  adorn_totals()

Politics <- topic_articles_visits %>% 
  slice(c(13,18,14)) %>%
  adorn_totals()

DomesticPolicyIssues <- topic_articles_visits %>% 
  slice(c(10,11))%>%
  adorn_totals()

Crime <- topic_articles_visits %>% 
  slice(c(16,21,8,15)) %>%
  adorn_totals()

RightWingPressure <- topic_articles_visits %>% 
  slice(c(17, 3)) %>%
  adorn_totals()

Other <- topic_articles_visits %>% 
  slice(c(5, 6, 7)) %>%
  adorn_totals()


topic_articles_visits_total <-  rbind(WithdrawalAfghanistan, MigrationEUBorderBelarus, EUAgreementTurkey, MigrationInternational, MediterraneanSeaRescue, Politics,
    DomesticPolicyIssues, Crime, RightWingPressure, Other)

  #arrange(desc(n_articles_0)) %>%
table11 <- topic_articles_visits_total %>%
  add_row(topic_articles_visits_total %>%  filter(topic_label != "Total") %>% add_totals() %>% slice(n())) %>%
  kbl(format = "html", format.args = list(big.mark = ","), longtable = T,
      booktabs = T, col.names = c("Topic number and keywords", "N articles", "N visits", "Hours", "N articles", "N visits", "Hours")) %>% 
      add_header_above(header = c(" " = 1,"Mainstream News" = 3, "Alternative News" = 3)) %>%
      kable_styling(position = "center")

save_kable(table11, file = "./Tables/table11.html")


# Super categories

supercategories_visits <- df %>% 
  drop_na(topic_label) %>% 
  group_by(is_alt_news) %>% 
  count(topic_super_category) %>% 
  rename(n_visits = n) %>% 
  mutate(share_visits = n_visits/sum(n_visits))


supercategories_count <- df %>% 
  drop_na(topic_label) %>% 
  group_by(is_alt_news, domain, topic_super_category) %>% 
  distinct(title, .keep_all = T) %>% 
  count(topic_super_category) %>% 
  ungroup(domain) %>%
  summarise(n = sum(n)) %>%
  rename(n_count = n) %>% 
  mutate(share_count = n_count/sum(n_count))


supercategories_duration <- df %>% 
  drop_na(topic_label) %>% 
  group_by(is_alt_news, topic_super_category) %>% 
  summarise(n_dur = round(sum(duration)/3600, digits = 2)) %>% 
  mutate(share_dur = n_dur/sum(n_dur))


merged2 <- merge(supercategories_count, supercategories_visits)
supercategories <- merge(merged2, supercategories_duration)
supercategories <- supercategories %>% 
  pivot_wider(names_from = is_alt_news,
              values_from = starts_with(c("n_", "share_")))
supercategories[is.na(supercategories)] <- 0

supercategories <- supercategories %>% 
  bind_rows(
    summarise(supercategories, topic_super_category = "Total", across(c(starts_with("n_"), starts_with("share")),sum))) %>%
  mutate(count_main = paste0(n_count_0, " (", scales::percent(share_count_0,accuracy = 0.01), ")"),
         visits_main = paste0(n_visits_0, " (", scales::percent(share_visits_0,accuracy = 0.01), ")"),
         count_alt = paste0(n_count_1, " (", scales::percent(share_count_1,accuracy = 0.01), ")"),
         visits_alt = paste0(n_visits_1, " (", scales::percent(share_visits_1,accuracy = 0.01), ")"),
         dur_main = paste0(n_dur_0, " (", scales::percent(share_dur_0,accuracy = 0.01), ")"),
         dur_alt = paste0(n_dur_1, " (", scales::percent(share_dur_1,accuracy = 0.01), ")"))

  

  

# Format cat names

supercategories <- supercategories %>% 
  mutate(topic_super_category = case_when(
    TRUE ~ topic_super_category
  ))

## Table for main text

table2 <- supercategories %>%
  arrange(desc(n_count_0)) %>% 
  select(topic_super_category, count_main, visits_main, dur_main, count_alt, visits_alt, dur_alt) %>%
  kbl(format = "html", format.args = list(big.mark = ","),
    booktabs = T,col.names = c("Manual category", "N articles (%)", "N visits (%)", "Duration in hours (%)","N articles (%)", "N visits (%)", "Duration in hours (%)"), digits = 2) %>% 
  add_header_above(header = c(" " = 1,"Mainstream News" = 3, "Alternative News" = 3)) %>% 
  kable_styling(position = "center")

save_kable(table2, file = "./Tables/table2.html")

## List of topics for each super category

supercat_topics <- df %>% 
  drop_na(topic_label) %>% 
  select(topic_super_category, topic_label) %>% 
  distinct(topic_label,.keep_all = T) %>% 
  group_by(topic_super_category) %>% 
  #summarise(list_of_topics = str_c(topic_label, collapse = "; "))  %>% 
  mutate(topic_super_category = case_when(
    TRUE ~ topic_super_category
  ))

table11_2 <- kbl(supercat_topics,
    booktabs = T, format = "html", longtable = T,
    caption = "List of topics included in each manual category.  \\label{tab:cat.to.super.cat}",
    col.names = c("Manual category", "Original topics")) %>% 
  kable_styling(position = "center") %>%
  collapse_rows(1, latex_hline = "major") %>% 
  row_spec(0,bold=TRUE)
  #row_spec(row = 1:nrow(supercat_topics), hline_after = T)

save_kable(table11_2, file = "./Tables/table11_2.html")

## Explore topics

# Top 10 by n of visits in AN in category "Politics"

top10_alt <- df %>% 
  filter(is_alt_news==1 & topic_super_category=="Politics" & migration==1) %>% 
  drop_na(title) %>% 
  group_by(title) %>% 
  mutate(n_visits = n()) %>%
  ungroup %>% 
  distinct(title, .keep_all = T) %>% 
  slice_max(order_by = n_visits, n = 10)
  

top10_main <- df %>% 
  filter(is_alt_news==0 & topic_super_category=="Politics") %>% 
  drop_na(title) %>% 
  group_by(title) %>% 
  mutate(n_visits = n()) %>%
  ungroup %>% 
  distinct(title, .keep_all = T) %>% 
  slice_max(order_by = n_visits, n = 10)

#* Over-time ----

## AN Visits
an_visits.plot.data <- df %>% 
  filter(is_alt_news==1) %>% 
  group_by(used_day) %>% 
  mutate(users.per.day = n_distinct(new_id)) %>% 
  mutate(n_visits_all = n(),
         n_visits_mig = sum(migration, na.rm=T),
         n_visits_other = sum(migration==0, na.rm=T),
         rel_daily_visits_all = n_visits_all/users.per.day,
         rel_daily_visits_mig = n_visits_mig/users.per.day,
         rel_daily_visits_other = n_visits_other/users.per.day) %>% 
  distinct(used_day, .keep_all = T) %>% 
  select(is_alt_news, used_day, users.per.day,starts_with(c("n_", "rel_"))) %>% 
  pivot_longer(cols = starts_with("rel_"),
               names_to = "type",
               names_prefix =  "rel_daily_visits_",
               values_to = "rel_daily_visits") %>% 
  mutate(type = factor(type, levels = c("all", "mig", "other"),
                       labels = c("All URLs", "Classified URLs: Migration-related", "Classified URLs: Other topics")),
         outlet = "Alternative News")

## AN duration
an_duration.plot.data <- df %>% 
  filter(is_alt_news==1) %>% 
  group_by(used_day) %>% 
  mutate(users.per.day = n_distinct(new_id)) %>% 
  mutate(n_dur_all = sum(duration)/60,
         n_dur_mig = sum(duration[migration == 1], na.rm=T)/60,
         n_dur_other = sum(duration[migration==0], na.rm=T)/60,
         rel_daily_dur_all = n_dur_all/users.per.day,
         rel_daily_dur_mig = n_dur_mig/users.per.day,
         rel_daily_dur_other = n_dur_other/users.per.day) %>% 
  distinct(used_day, .keep_all = T) %>% 
  select(is_alt_news, used_day, users.per.day,starts_with(c("n_", "rel_"))) %>% 
  pivot_longer(cols = starts_with("rel_"),
               names_to = "type",
               names_prefix =  "rel_daily_dur_",
               values_to = "rel_daily_dur") %>% 
  mutate(type = factor(type, levels = c("all", "mig", "other"),
                       labels = c("All URLs", "Classified URLs: Migration-related", "Classified URLs: Other topics")),
         outlet = "Alternative News")

## MS Visits

ms_visits.plot.data <- df %>% 
  group_by(used_day) %>% 
  mutate(users.per.day = n_distinct(new_id)) %>% 
  mutate(n_visits_all = n(),
         n_visits_mig = sum(migration, na.rm=T),
         n_visits_other = sum(migration==0, na.rm=T),
         rel_daily_visits_all = n_visits_all/users.per.day,
         rel_daily_visits_mig = n_visits_mig/users.per.day,
         rel_daily_visits_other = n_visits_other/users.per.day) %>% 
  distinct(used_day, .keep_all = T) %>% 
  select(is_alt_news, used_day, users.per.day,starts_with(c("n_", "rel_"))) %>% 
  pivot_longer(cols = starts_with("rel_"),
               names_to = "type",
               names_prefix =  "rel_daily_visits_",
               values_to = "rel_daily_visits") %>% 
  mutate(type = factor(type, levels = c("all", "mig", "other"),
                       labels = c("All URLs", "Classified URLs: Migration-related", "Classified URLs: Other topics")),
         outlet = "Mainstream News")

## MS duration

ms_duration.plot.data <- df %>% 
  group_by(used_day) %>% 
  mutate(users.per.day = n_distinct(new_id)) %>% 
  mutate(n_dur_all = sum(duration)/60,
         n_dur_mig = sum(duration[migration == 1], na.rm=T)/60,
         n_dur_other = sum(duration[migration==0], na.rm=T)/60,
         rel_daily_dur_all = n_dur_all/users.per.day,
         rel_daily_dur_mig = n_dur_mig/users.per.day,
         rel_daily_dur_other = n_dur_other/users.per.day) %>%
  distinct(used_day, .keep_all = T) %>% 
  select(is_alt_news, used_day, users.per.day,starts_with(c("n_", "rel_"))) %>% 
  pivot_longer(cols = starts_with("rel_"),
               names_to = "type",
               names_prefix =  "rel_daily_dur_",
               values_to = "rel_daily_dur") %>% 
  mutate(type = factor(type, levels = c("all", "mig", "other"),
                       labels = c("All URLs", "Classified URLs: Migration-related", "Classified URLs: Other topics")),
         outlet = "Mainstream News")

plot.data_visits <- bind_rows(an_visits.plot.data, ms_visits.plot.data)
plot.data_duration <- bind_rows(an_duration.plot.data, ms_duration.plot.data)

## All news visits

plot.data_visits %>% 
  #ungroup %>% 
  filter(used_day < as_date("2021-10-01")) %>% # exclude one weird AN day (2021-07-24)
  ggplot(aes(x = used_day, y = rel_daily_visits, colour = type, fill = type)) +
  geom_point(shape=20, size=3) +
  geom_smooth(method="loess", span = 0.5) +
  geom_vline(xintercept=as.Date("2021-09-26"), linetype = "dotted", size = 1) +
  #scale_colour_brewer(type = "qual", palette = "Set1", name = "",direction = "1") +
  scale_colour_manual(values = c("#E41A1C", "#4DAF4A", "#377EB8"),
                      name = "") +
  theme_bw(base_size=(16)) +
  scale_y_continuous(breaks = c(0,5,10,15,20, 25, 30)) + 
  scale_x_date(date_labels = "%b %d",
               date_breaks = "15 days",
               limits = c(min(plot.data_visits$used_day), max(plot.data_visits$used_day))) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "top",
        legend.text = element_text(size=14),
        axis.title.x = element_text(margin = margin(15,0,0,0)),
        axis.title.y = element_text(margin = margin(0,15,0,0)),
        axis.text.x = element_text(color="black", 
                                   size=12),
        axis.text.y = element_text(color="black", 
                                   size=12)) +
  guides(color = guide_legend(override.aes = list(fill = NA)),
         fill = "none") + # no SE shade in legend
  labs(x = "Date",
       y = "Visits/Users") +
  #ylim(c(0,27)) +
  facet_wrap(~outlet)

ggsave("./Figures/fig7_visits_users.over.time.png", width=12, height = 8)


## All news duration

plot.data_duration %>% 
  #ungroup %>% 
  filter(used_day < as_date("2021-10-01")) %>% # exclude one weird AN day (2021-07-24)
  ggplot(aes(x = used_day, y = rel_daily_dur, colour = type, fill = type)) +
  geom_point(shape=20, size=3) +
  geom_smooth(method="loess", span = 0.5) +
  geom_vline(xintercept=as.Date("2021-09-26"), linetype = "dotted", size = 1) +
  #scale_colour_brewer(type = "qual", palette = "Set1", name = "",direction = "1") +
  scale_colour_manual(values = c("#E41A1C", "#4DAF4A", "#377EB8"),
                      name = "") +
  theme_bw(base_size=(16)) +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45)) + 
  scale_x_date(date_labels = "%b %d",
               date_breaks = "15 days",
               limits = c(min(plot.data_duration$used_day), max(plot.data_duration$used_day))) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "top",
        legend.text = element_text(size=14),
        axis.title.x = element_text(margin = margin(15,0,0,0)),
        axis.title.y = element_text(margin = margin(0,15,0,0)),
        axis.text.x = element_text(color="black", 
                                   size=12),
        axis.text.y = element_text(color="black", 
                                   size=12)) +
  guides(color = guide_legend(override.aes = list(fill = NA)),
         fill = "none") + # no SE shade in legend
  labs(x = "Date",
       y = "Duration in minutes/Users") +
  #ylim(c(0,27)) +
  facet_wrap(~outlet)

ggsave("./Figures/fig8_duration_users.over.time.png", width=12, height = 8)

## Migration visits only

plot.data_visits %>% 
  #ungroup %>% 
  filter(used_day < as_date("2021-10-01"), # exclude one weird AN day (2021-07-24)
         type == "Classified URLs: Migration-related") %>% 
  ggplot(aes(x = used_day, y = rel_daily_visits, colour = type, fill = type)) +
  geom_point(shape=20, size=3) +
  geom_smooth(method="loess", span = 0.5) +
  geom_vline(xintercept=as.Date("2021-09-26"), linetype = "dotted", size = 1) +
  #scale_colour_brewer(type = "qual", palette = "Set1", name = "",direction = "1") +
  scale_colour_manual(values = c("#4DAF4A"),
                      name = "") +
  scale_fill_manual(values = c("#4DAF4A"),
                    name = "") +
  theme_bw(base_size=(16)) +
  scale_y_continuous(limits=c(-.5, 5),breaks = c(0,1,2,3,4,5)) + 
  scale_x_date(date_labels = "%b %d",
               date_breaks = "15 days",
               limits = c(min(plot.data_visits$used_day), max(plot.data_visits$used_day))) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "top",
        legend.text = element_text(size=14),
        axis.title.x = element_text(margin = margin(15,0,0,0)),
        axis.title.y = element_text(margin = margin(0,15,0,0)),
        axis.text.x = element_text(color="black", 
                                   size=12),
        axis.text.y = element_text(color="black", 
                                   size=12)) +
  guides(color = guide_legend(override.aes = list(fill = NA)),
         fill = "none") + # no SE shade in legend
  labs(x = "Date",
       y = "All Visits per day/All Users per day") +
  #ylim(c(0,27)) +
  facet_wrap(~outlet)

ggsave("./Figures/fig9_visits_users.migration.over.time.png", width=12, height = 8)


## Migration duration only

plot.data_duration %>% 
  #ungroup %>% 
  filter(used_day < as_date("2021-10-01"), # exclude one weird AN day (2021-07-24)
         type == "Classified URLs: Migration-related") %>% 
  ggplot(aes(x = used_day, y = rel_daily_dur, colour = type, fill = type)) +
  geom_point(shape=20, size=3) +
  geom_smooth(method="loess", span = 0.5) +
  geom_vline(xintercept=as.Date("2021-09-26"), linetype = "dotted", size = 1) +
  #scale_colour_brewer(type = "qual", palette = "Set1", name = "",direction = "1") +
  scale_colour_manual(values = c("#4DAF4A"),
                      name = "") +
  scale_fill_manual(values = c("#4DAF4A"),
                    name = "") +
  theme_bw(base_size=(16)) +
  scale_y_continuous(limits=c(-.5, 9), breaks = c(0,1,2,3,4,5,6,7,8,9)) + 
  scale_x_date(date_labels = "%b %d",
               date_breaks = "15 days",
               limits = c(min(plot.data_duration$used_day), max(plot.data_duration$used_day))) +
  #scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "top",
        legend.text = element_text(size=14),
        axis.title.x = element_text(margin = margin(15,0,0,0)),
        axis.title.y = element_text(margin = margin(0,15,0,0)),
        axis.text.x = element_text(color="black", 
                                   size=12),
        axis.text.y = element_text(color="black", 
                                   size=12)) +
  guides(color = guide_legend(override.aes = list(fill = NA)),
         fill = "none") + # no SE shade in legend
  labs(x = "Date",
       y = "All Visits per day/All Users per day") +
  #ylim(c(0,27)) +
  facet_wrap(~outlet)

ggsave("./Figures/fig10_visits_duration.migration.over.time.png", width=12, height = 8)



