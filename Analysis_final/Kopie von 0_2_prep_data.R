library(tidyverse) 
library(lubridate) 
library(openxlsx)

##NOTES:

## Missing tracking data (in df.all.expanded) for:
# 15   72  507  656  680 1245 1309 1524 2222 2223 2224

setwd("D:/MASTERARBEIT/Analysis")

# Subset to individuals for whom we also have survey data
all_dat_avail <- readRDS("data/survey_and_tracking.rds")

comb.all.data <- readRDS("data/all_track_with_all_news_flag.rds")
comb.all.data <- comb.all.data[comb.all.data$new_id %in% all_dat_avail$new_id,]

## Merge in content data

#EPSA: News mentioning parties and politicians in title/lead
#political_news <- read_csv("data/work/news_outlets/political_news.csv")
#political_news$url <- str_remove(political_news$link, "https://") # check match title or elements of url


#IJPR: migrational news and opinion pieces
mig_opinion_news <- read.xlsx("data/news_categorised_final_all_Mig_NER_BERTopic.xlsx")
mig_opinion_news$url <- str_remove(mig_opinion_news$link, "https://")

#df.topic <- read_csv("data/work/news_outlets/scraped/all_news_with_topic.csv") # OLD
#comb.all.data <- left_join(comb.all.data, df.topic, by = "url")
#rm(df.topic)

comb.all.data <- left_join(comb.all.data, mig_opinion_news, by = "url")

rm(political_news, mig_opinion_news)

####


comb.all.data$day.collected <- as.character(comb.all.data$used_at)
comb.all.data$day.collected <- str_sub(comb.all.data$day.collected, 1, 10)
comb.all.data$day.collected <- str_replace_all(comb.all.data$day.collected, "[^[:alnum:]]", "")


all.days <- unique(comb.all.data$day.collected)
all.days.df <- as.data.frame(all.days)
rm(all.days)
all.days.df <- all.days.df %>%
  rename(day.collected = all.days)
#saveRDS(all.days.df, "./data/work/all_days_for_expansion.rds")

### all content
df.all <- comb.all.data %>%
  group_by(new_id, day.collected) %>%
  summarise(n.logs.all = n(),
            dur.logs.all = sum(duration, na.rm = TRUE))

df.all <- merge(all.days.df, df.all, by = "day.collected", all.x = TRUE)








#### filter for only ####

#just use ids which are also in df.clean -> only people who read classified articles
df.clean <- read_csv("data/df.clean.csv")


ergebnis <- comb.all.data %>%
  inner_join(df.clean, by = c("new_id", "url", "used_at", "duration"))


ergebnis <- ergebnis %>%
  filter(classified == T)




#all.id.days <-  expand(df.all, new_id, day.collected)
#df.all.expanded <- merge(all.id.days, df.all, by = c("new_id", "day.collected"), all.x = TRUE)
#df.all.expanded[is.na(df.all.expanded)] <- 0
#saveRDS(df.all.expanded, "./data/work/all_expanded.rds")

df.all.expanded <- readRDS("data/all_expanded.rds")

### alt news (political news atm only for mainstream - left out of this)
df.alt_news <- ergebnis %>%
  filter(is_alt_news.y == 1) %>%
  group_by(new_id, day.collected) %>%
  summarise(n.logs.alt_news = n(),
            dur.logs.alt_news = sum(duration, na.rm = TRUE),
            n.logs.alt_mig = sum(migration.y, na.rm=T),
            dur.logs.alt_mig = sum(duration[migration.y==1], na.rm=T),
            n.logs.alt_mig_opinion = sum(opinion.y, na.rm=T),
            dur.logs.alt_mig_opinion = sum(duration[opinion.y==1], na.rm=T),
            n.outlets.alt_news = n_distinct(domain.y, na.rm=T),
            n.outlets.alt_mig = n_distinct(domain.y[migration.y==1], na.rm=T))

df.alt_news <- merge(all.days.df, df.alt_news, by = "day.collected", all.x = TRUE)

all.id.days <-  tidyr::expand(df.alt_news, new_id, day.collected)

df.alt_news.expanded <- merge(all.id.days, df.alt_news, by = c("new_id", "day.collected"), all.x = TRUE)
df.alt_news.expanded[is.na(df.alt_news.expanded)] <- 0
df.alt_news.expanded$has.alt_news <- TRUE
#saveRDS(df.alt_news.expanded, "./data/alt_news_expanded.rds")



### main news (had to use data.table and remove comb.all.data otherwise crash)
library(data.table)

df.main_news <- ergebnis %>% 
  filter(is_alt_news.y == 0)

df.main_news <- as.data.table(df.main_news) # filter first
rm(comb.all.data)

df.main_news <- df.main_news[, .(
  n.logs.main_news = .N,
  dur.logs.main_news = sum(duration, na.rm = TRUE),
  n.logs.main_mig = sum(migration.y, na.rm = TRUE),
  dur.logs.main_mig = sum(duration[migration.y == 1], na.rm = TRUE),
  n.logs.main_mig_opinion = sum(opinion.y, na.rm = TRUE),
  dur.logs.main_mig_opinion.y = sum(duration[opinion.y == 1], na.rm = TRUE),
  n.outlets.main_news = uniqueN(domain.y, na.rm = TRUE),
  n.outlets.main_mig = uniqueN(domain.y[migration.y == 1], na.rm = TRUE),
  n.outlets.main_mig_opinion = uniqueN(domain.y[opinion.y == 1], na.rm = TRUE)
)     , by = c("new_id", "day.collected")]


df.main_news <- merge(all.days.df, df.main_news, by = "day.collected", all.x = TRUE)

all.id.days <-  tidyr::expand(df.main_news, new_id, day.collected)

df.main_news.expanded <- merge(all.id.days, df.main_news, by = c("new_id", "day.collected"), all.x = TRUE)
df.main_news.expanded[is.na(df.main_news.expanded)] <- 0
df.main_news.expanded$has.main_news <- TRUE
#saveRDS(df.main_news.expanded, "./data/work/main_news_expanded.rds")
df.all.2 <- merge(df.all.expanded, df.main_news.expanded, by=c("new_id", "day.collected"), all.x = TRUE)
df.all.2 <- merge(df.all.2, df.alt_news.expanded, by=c("new_id", "day.collected"), all.x = TRUE)

sel <- grepl("has.",names(df.all.2))
df.all.2[sel] <- lapply(df.all.2[sel], function(x) replace(x,x %in% NA, FALSE) )

rm(df.main_news, df.main_news.expanded,
   df.alt_news, df.alt_news.expanded,
   all.days.df, all.id.days,
   df.all, df.all.expanded, sel)


## calculate share of news content per day.
# NA means user did not have that content at all
# NaN means user did not use device on that day (bc 0/0==NaN)
df.all.2 <- df.all.2 %>%
  mutate(rel.n.main_news      = n.logs.main_news      / n.logs.all,
         rel.n.alt_news   = n.logs.alt_news   / n.logs.all,
         rel.dur.main_news    = dur.logs.main_news    / dur.logs.all,
         rel.dur.alt_news = dur.logs.alt_news / dur.logs.all,
         rel.n.main_mig = n.logs.main_mig / n.logs.all,
         rel.n.main_mig_opinion = n.logs.main_mig_opinion / n.logs.all,
         rel.n.alt_mig = n.logs.alt_mig / n.logs.all,
         rel.n.alt_mig_opinion = n.logs.alt_mig_opinion / n.logs.all) # didn't do party cause won't use it

#saveRDS(df.all.2, "./data/work/usage_per_day.rds")

## next steps: aggregate to data before surveys (e.g., week before survey etc)
#df.all.2 <- readRDS("./data/work/usage_per_day.rds")
table(df.all.2$day.collected)
df.all.2 <- merge(df.all.2, all_dat_avail, by="new_id")

df.all.2$not.yet.observed <- ifelse(as.Date(df.all.2$day.collected, format = "%Y%m%d")<df.all.2$first.day, T, F)
df.all.2$no.longer.observed <- ifelse(as.Date(df.all.2$day.collected, format = "%Y%m%d")>df.all.2$last.day, T, F)

## merge in survey participation dates
#survey_allwaves <- readRDS("./data/clean/survey/survey_allwaves_final_April5.rds")
survey_allwaves <- readRDS("data/PINCET_v1-0-0.rds")


## remove survey duplicates IDs
dupe_ids <- survey_allwaves$new_id[duplicated(survey_allwaves$new_id) | duplicated(survey_allwaves$new_id, fromLast=TRUE)]

`%nin%` = Negate(`%in%`)
survey_allwaves <- survey_allwaves[survey_allwaves$new_id %nin% dupe_ids,]

survey_allwaves$w1_datetime <- as.character(survey_allwaves$w1_datetime)
survey_allwaves$w1_datetime <- str_sub(survey_allwaves$w1_datetime, 1, 10)
survey_allwaves$w1_datetime <- as.Date(survey_allwaves$w1_datetime, format = "%m/%d/%Y")

survey_allwaves$w1b_datetime <- as.character(survey_allwaves$w1b_datetime)
survey_allwaves$w1b_datetime <- str_sub(survey_allwaves$w1b_datetime, 1, 10)
survey_allwaves$w1b_datetime <- as.Date(survey_allwaves$w1b_datetime, format = "%m/%d/%Y")

survey_allwaves$w2_datetime <- as.character(survey_allwaves$w2_datetime)
survey_allwaves$w2_datetime <- str_sub(survey_allwaves$w2_datetime, 1, 10)
survey_allwaves$w2_datetime <- as.Date(survey_allwaves$w2_datetime, format = "%m/%d/%Y")

survey_allwaves$w3_datetime <- as.character(survey_allwaves$w3_datetime)
survey_allwaves$w3_datetime <- str_sub(survey_allwaves$w3_datetime, 1, 10)
survey_allwaves$w3_datetime <- as.Date(survey_allwaves$w3_datetime, format = "%Y-%m-%d")

survey_allwaves <- survey_allwaves %>% 
  select(new_id, w1_datetime, w1b_datetime, w2_datetime, w3_datetime)

df.all.2 <- merge(df.all.2, survey_allwaves, by="new_id", all.x = T)

## Flag all tracking days before each survey wave


df.all.2$pre.wave.1 <- ifelse(
  as.Date(df.all.2$day.collected, format = "%Y%m%d") < df.all.2$w1_datetime,T, F)


df.all.2$pre.wave.1[is.na(df.all.2$pre.wave.1)] <- F


#df.all.2$pre.wave.2 <- ifelse(as.Date(df.all.2$day.collected, format = "%Y%m%d") <= df.all.2$w2_datetime
#                              &
#                              as.Date(df.all.2$day.collected, format = "%Y%m%d") >= df.all.2$w1_datetime,
#                              T,
#                              ifelse(is.na(df.all.2$w1_datetime) & 
#                                       as.Date(df.all.2$day.collected, format = "%Y%m%d") <= df.all.2$w2_datetime,
#                                     T,F)) # ppl who didn't participate in w1 get an NA here


df.all.2 <- df.all.2 %>% 
  mutate(pre.wave.2 = case_when(
    as.Date(day.collected, format = "%Y%m%d") < w2_datetime & as.Date(day.collected, format = "%Y%m%d") >= w1_datetime ~ TRUE,
    is.na(w1_datetime) & as.Date(day.collected, format = "%Y%m%d") < w2_datetime ~ TRUE,
    TRUE ~ FALSE
  ))

#the above deals with people who would get an NA (because w1_datetime is NA)


#df.all.2$pre.wave.3 <- ifelse(
#                              as.Date(df.all.2$day.collected, format = "%Y%m%d") <= df.all.2$w3_datetime
#                              &
#                              as.Date(df.all.2$day.collected, format = "%Y%m%d") >= df.all.2$w2_datetime
#                              ,
#                              T, F)


## Wave 3 is pretty easy now

df.all.2$pre.wave.3 <- ifelse(as.Date(df.all.2$day.collected, format = "%Y%m%d") < df.all.2$w3_datetime, T, F)
df.all.2$pre.wave.3 <- ifelse(df.all.2$pre.wave.1 | df.all.2$pre.wave.2, F, df.all.2$pre.wave.3) # no overlap






### Seven days before survey

df.all.2$w1_pre_logs_7days <- ifelse(as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w1_datetime) - 7))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w1_datetime) - 6))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w1_datetime) - 5))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w1_datetime) - 4))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w1_datetime) - 3))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w1_datetime) - 2))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w1_datetime) - 1)), 
                                     T, F)


df.all.2$w2_pre_logs_7days <- ifelse(as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w2_datetime) - 7))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w2_datetime) - 6))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w2_datetime) - 5))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w2_datetime) - 4))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w2_datetime) - 3))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w2_datetime) - 2))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w2_datetime) - 1)), 
                                     T, F)


df.all.2$w3_pre_logs_7days <- ifelse(as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w3_datetime) - 7))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w3_datetime) - 6))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w3_datetime) - 5))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w3_datetime) - 4))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w3_datetime) - 3))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w3_datetime) - 2))
                                     |as.Date(df.all.2$day.collected, format = "%Y%m%d") == ((as.Date(df.all.2$w3_datetime) - 1)), 
                                     T, F)


df.pre.wave.1 <- df.all.2 %>% 
  #filter(w1_pre_logs==T) %>% 
  filter(pre.wave.1) %>% 
  mutate(across(matches("\\.logs\\.|\\.outlets\\."),
                .fns = ~replace_na(.x, 0))) %>% 
  group_by(new_id) %>%
  mutate(across(matches("\\.logs\\.|\\.outlets\\."),
                .fns = ~sum(.x),
                .names = "cum_{col}"),
         w1_has.logs = ifelse(cum_n.logs.all!=0,T,F),
         days.active = length(n.logs.all[n.logs.all>0])) %>%
  distinct(new_id, .keep_all = T) %>% 
  select(new_id, starts_with("has."), days.tracked,
         first.day, last.day, first.to.last.day, avg.tracked,
         starts_with("participation"), starts_with("w"), starts_with("pre.wave"),
         starts_with("cum_"), days.active) %>% 
  mutate(wave = "1") %>% 
  rename(surv.participated = participation_w1,
         surv.part.date = w1_datetime,
         has.logs = w1_has.logs) %>% 
  select(-c(pre.wave.1, starts_with("participation"), w2_datetime,
            w1b_datetime, w3_datetime, pre.wave.2, pre.wave.3))




df.pre.wave.2 <- df.all.2 %>% 
  filter(pre.wave.2) %>% 
  mutate(across(matches("\\.logs\\.|\\.outlets\\."),
                .fns = ~replace_na(.x, 0))) %>% 
  group_by(new_id) %>%
  mutate(across(matches("\\.logs\\.|\\.outlets\\."),
                .fns = ~sum(.x),
                .names = "cum_{col}"),
         w2_has.logs = ifelse(cum_n.logs.all!=0,T,F),
         days.active = length(n.logs.all[n.logs.all>0])) %>%
  distinct(new_id, .keep_all = T) %>% 
  select(new_id, starts_with("has."), days.tracked,
         first.day, last.day, first.to.last.day, avg.tracked,
         starts_with("participation"), starts_with("w"), starts_with("pre.wave"),
         starts_with("cum_"), days.active) %>% 
  mutate(wave = "2") %>% 
  rename(surv.participated = participation_w2,
         surv.part.date = w2_datetime,
         has.logs = w2_has.logs) %>% 
  select(-c(pre.wave.1, starts_with("participation"),
            w1b_datetime, w3_datetime, pre.wave.2, pre.wave.3, w1_datetime))



df.pre.wave.3 <- df.all.2 %>% 
  filter(pre.wave.3) %>% 
  mutate(across(matches("\\.logs\\.|\\.outlets\\."),
                .fns = ~replace_na(.x, 0))) %>% 
  group_by(new_id) %>%
  mutate(across(matches("\\.logs\\.|\\.outlets\\."),
                .fns = ~sum(.x),
                .names = "cum_{col}"),
         w3_has.logs = ifelse(cum_n.logs.all!=0,T,F),
         days.active = length(n.logs.all[n.logs.all>0])) %>% 
  select(new_id, starts_with("has."), days.tracked,
         first.day, last.day, first.to.last.day, avg.tracked,
         starts_with("participation"), starts_with("w"), starts_with("pre.wave"),
         starts_with("cum_"), days.active) %>% 
  mutate(wave = "3") %>% 
  distinct(new_id, .keep_all = T) %>% 
  rename(surv.participated = participation_w3,
         surv.part.date = w3_datetime,
         has.logs = w3_has.logs) %>% 
  select(-c(pre.wave.1, starts_with("participation"), w2_datetime,
            w1b_datetime, pre.wave.2, pre.wave.3, w1_datetime))





df.all.waves.long <- rbind(df.pre.wave.1, df.pre.wave.2, df.pre.wave.3)






## 7 days


df.pre.wave.1_7days <- df.all.2 %>% 
  filter(w1_pre_logs_7days==T) %>% 
  mutate(across(matches("\\.logs\\.|\\.outlets\\."),
                .fns = ~replace_na(.x, 0))) %>% 
  group_by(new_id) %>%
  mutate(across(matches("\\.logs\\.|\\.outlets\\."),
                .fns = ~sum(.x),
                .names = "cum_{col}"),
         w1_has.logs = ifelse(cum_n.logs.all!=0,T,F),
         days.active = length(n.logs.all[n.logs.all>0])) %>%
  distinct(new_id, .keep_all = T) %>% 
  select(new_id, starts_with("has."), days.tracked,
         first.day, last.day, first.to.last.day, avg.tracked,
         starts_with("participation"), starts_with("w"), starts_with("pre.wave"),
         starts_with("cum_"), days.active) %>% 
  mutate(wave = "1") %>% 
  rename(surv.participated = participation_w1,
         surv.part.date = w1_datetime,
         has.logs = w1_has.logs) %>% 
  select(-c(w1_pre_logs_7days, starts_with("participation"), w2_datetime,
            w1b_datetime, w3_datetime, w2_pre_logs_7days, w3_pre_logs_7days)) %>% 
  rename_at(vars(starts_with("cum_"), days.active),function(x) paste0(x,"_7days"))


df.pre.wave.2_7days <- df.all.2 %>% 
  filter(w2_pre_logs_7days==T) %>% 
  mutate(across(matches("\\.logs\\.|\\.outlets\\."),
                .fns = ~replace_na(.x, 0))) %>% 
  group_by(new_id) %>%
  mutate(across(matches("\\.logs\\.|\\.outlets\\."),
                .fns = ~sum(.x),
                .names = "cum_{col}"),
         w2_has.logs = ifelse(cum_n.logs.all!=0,T,F),
         days.active = length(n.logs.all[n.logs.all>0])) %>%
  distinct(new_id, .keep_all = T) %>% 
  select(new_id, starts_with("has."), days.tracked,
         first.day, last.day, first.to.last.day, avg.tracked,
         starts_with("participation"), starts_with("w"), starts_with("pre.wave"),
         starts_with("cum_"), days.active) %>% 
  mutate(wave = "2") %>% 
  rename(surv.participated = participation_w2,
         surv.part.date = w2_datetime,
         has.logs = w2_has.logs) %>% 
  select(-c(w2_pre_logs_7days, starts_with("participation"), w1_datetime,
            w1b_datetime, w3_datetime, w1_pre_logs_7days, w3_pre_logs_7days) ) %>% 
  rename_at(vars(starts_with("cum_"), days.active),function(x) paste0(x,"_7days"))


df.pre.wave.3_7days <- df.all.2 %>% 
  filter(w3_pre_logs_7days==T) %>% 
  mutate(across(matches("\\.logs\\.|\\.outlets\\."),
                .fns = ~replace_na(.x, 0))) %>% 
  group_by(new_id) %>%
  mutate(across(matches("\\.logs\\.|\\.outlets\\."),
                .fns = ~sum(.x),
                .names = "cum_{col}"),
         w3_has.logs = ifelse(cum_n.logs.all!=0,T,F),
         days.active = length(n.logs.all[n.logs.all>0])) %>%
  distinct(new_id, .keep_all = T) %>% 
  select(new_id, starts_with("has."), days.tracked,
         first.day, last.day, first.to.last.day, avg.tracked,
         starts_with("participation"), starts_with("w"), starts_with("pre.wave"),
         starts_with("cum_"), days.active) %>% 
  mutate(wave = "3") %>% 
  rename(surv.participated = participation_w3,
         surv.part.date = w3_datetime,
         has.logs = w3_has.logs) %>% 
  select(-c(w3_pre_logs_7days, starts_with("participation"), w1_datetime,
            w1b_datetime, w2_datetime, w1_pre_logs_7days, w2_pre_logs_7days))  %>% 
  rename_at(vars(starts_with("cum_"), days.active),function(x) paste0(x,"_7days"))













#df.pre.wave.1_7days <- df.all.2 %>% 
#  filter(w1_pre_logs_7days==T) %>% 
#  replace_na(list(n.logs.alt_news = 0,
#                  n.logs.main_news = 0,
#                  dur.logs.alt_news = 0,
#                  dur.logs.main_news = 0,
#                  n.logs.alt_climate = 0,
#                  n.logs.alt_climate.nofloods = 0,
#                  n.logs.main_climate = 0,
#                  n.logs.main_climate.nofloods = 0,
#                  n.logs.alt_migration = 0,
#                  n.logs.main_migration = 0,
#                  n.logs.alt_gender = 0,
#                  n.logs.main_gender = 0,
#                  dur.logs.alt_climate = 0,
#                  dur.logs.alt_climate.nofloods = 0,
#                  dur.logs.main_climate = 0,
#                  dur.logs.main_climate.nofloods = 0,
#                  dur.logs.alt_migration = 0,
#                  dur.logs.main_migration = 0,
#                  dur.logs.alt_gender = 0,
#                  dur.logs.main_gender = 0))  %>% 
#  group_by(new_id) %>%
#  mutate(cum_alt_news = sum(n.logs.alt_news),
#         cum_main_news = sum(n.logs.main_news),
#         cum_n.logs = sum(n.logs.all),
#         cum_alt_climate = sum(n.logs.alt_climate),
#         cum_alt_climate.nofloods = sum(n.logs.alt_climate.nofloods),
#         cum_main_climate = sum(n.logs.main_climate),
#         cum_main_climate.nofloods = sum(n.logs.main_climate.nofloods),
#         cum_alt_migration = sum(n.logs.alt_migration),
#         cum_main_migration = sum(n.logs.main_migration),
#         cum_dur_alt_news = sum(dur.logs.alt_news),
#         cum_dur_main_news = sum(dur.logs.main_news),
#         cum_dur_n.logs = sum(dur.logs.all),
#         cum_dur_alt_climate = sum(dur.logs.alt_climate),
#         cum_dur_alt_climate.nofloods = sum(dur.logs.alt_climate.nofloods),
#         cum_dur_main_climate = sum(dur.logs.main_climate),
#         cum_dur_main_climate.nofloods = sum(dur.logs.main_climate.nofloods),
#         cum_dur_alt_migration = sum(dur.logs.alt_migration),
#         cum_dur_main_migration = sum(dur.logs.main_migration),
#         cum_alt_gender = sum(n.logs.alt_gender),
#         cum_main_gender = sum(n.logs.main_gender),
#         cum_dur_alt_gender = sum(dur.logs.alt_gender),
#         cum_dur_main_gender = sum(dur.logs.main_gender),
#         w1_has.logs = ifelse(cum_n.logs!=0,T,F)) %>% 
#  select(new_id, starts_with("has."), days.tracked,
#         first.day, last.day, first.to.last.day, avg.tracked,
#         starts_with("participation"), starts_with("w"), 
#         starts_with("cum_")) %>% 
#  mutate(wave = "1") %>% 
#  distinct(new_id, .keep_all = T) %>% 
#  rename(surv.participated = participation_w1,
#         surv.part.date = w1_datetime,
#         has.logs = w1_has.logs) %>% 
#  select(-c(w1_pre_logs_7days, starts_with("participation"), w2_datetime,
#            w1b_datetime, w3_datetime, w2_pre_logs_7days, w3_pre_logs_7days)) %>% 
#  select(new_id, wave, cum_alt_news:cum_dur_main_gender) %>% 
#  rename_at(vars(cum_alt_news:cum_dur_main_gender),function(x) paste0(x,"_7days"))
#
#
#df.pre.wave.2_7days <- df.all.2 %>% 
#  filter(w2_pre_logs_7days==T) %>% 
#  replace_na(list(n.logs.alt_news = 0,
#                  n.logs.main_news = 0,
#                  dur.logs.alt_news = 0,
#                  dur.logs.main_news = 0,
#                  n.logs.alt_climate = 0,
#                  n.logs.alt_climate.nofloods = 0,
#                  n.logs.main_climate = 0,
#                  n.logs.main_climate.nofloods = 0,
#                  n.logs.alt_migration = 0,
#                  n.logs.main_migration = 0,
#                  n.logs.alt_gender = 0,
#                  n.logs.main_gender = 0,
#                  dur.logs.alt_climate = 0,
#                  dur.logs.alt_climate.nofloods = 0,
#                  dur.logs.main_climate = 0,
#                  dur.logs.main_climate.nofloods = 0,
#                  dur.logs.alt_migration = 0,
#                  dur.logs.main_migration = 0,
#                  dur.logs.alt_gender = 0,
#                  dur.logs.main_gender = 0))  %>% 
#  group_by(new_id) %>%
#  mutate(cum_alt_news = sum(n.logs.alt_news),
#         cum_main_news = sum(n.logs.main_news),
#         cum_n.logs = sum(n.logs.all),
#         cum_alt_climate = sum(n.logs.alt_climate),
#         cum_alt_climate.nofloods = sum(n.logs.alt_climate.nofloods),
#         cum_main_climate = sum(n.logs.main_climate),
#         cum_main_climate.nofloods = sum(n.logs.main_climate.nofloods),
#         cum_alt_migration = sum(n.logs.alt_migration),
#         cum_main_migration = sum(n.logs.main_migration),
#         cum_dur_alt_news = sum(dur.logs.alt_news),
#         cum_dur_main_news = sum(dur.logs.main_news),
#         cum_dur_n.logs = sum(dur.logs.all),
#         cum_dur_alt_climate = sum(dur.logs.alt_climate),
#         cum_dur_alt_climate.nofloods = sum(dur.logs.alt_climate.nofloods),
#         cum_dur_main_climate = sum(dur.logs.main_climate),
#         cum_dur_main_climate.nofloods = sum(dur.logs.main_climate.nofloods),
#         cum_dur_alt_migration = sum(dur.logs.alt_migration),
#         cum_dur_main_migration = sum(dur.logs.main_migration),
#         cum_alt_gender = sum(n.logs.alt_gender),
#         cum_main_gender = sum(n.logs.main_gender),
#         cum_dur_alt_gender = sum(dur.logs.alt_gender),
#         cum_dur_main_gender = sum(dur.logs.main_gender),
#         w2_has.logs = ifelse(cum_n.logs!=0,T,F)) %>% 
#  select(new_id, starts_with("has."), days.tracked,
#         first.day, last.day, first.to.last.day, avg.tracked,
#         starts_with("participation"), starts_with("w"),
#         starts_with("cum_")) %>% 
#  mutate(wave = "2") %>% 
#  distinct(new_id, .keep_all = T) %>% 
#  rename(surv.participated = participation_w2,
#         surv.part.date = w2_datetime,
#         has.logs = w2_has.logs) %>% 
#  select(-c(w2_pre_logs_7days, starts_with("participation"), w1_datetime,
#            w1b_datetime, w3_datetime, w1_pre_logs_7days, w3_pre_logs_7days) ) %>% 
#  select(new_id, wave, cum_alt_news:cum_dur_main_gender) %>% 
#  rename_at(vars(cum_alt_news:cum_dur_main_gender),function(x) paste0(x,"_7days"))
#
#
#df.pre.wave.3_7days <- df.all.2 %>% 
#  filter(w3_pre_logs_7days==T) %>% 
#  replace_na(list(n.logs.alt_news = 0,
#                  n.logs.main_news = 0,
#                  dur.logs.alt_news = 0,
#                  dur.logs.main_news = 0,
#                  n.logs.alt_climate = 0,
#                  n.logs.alt_climate.nofloods = 0,
#                  n.logs.main_climate = 0,
#                  n.logs.main_climate.nofloods = 0,
#                  n.logs.alt_migration = 0,
#                  n.logs.main_migration = 0,
#                  n.logs.alt_gender = 0,
#                  n.logs.main_gender = 0,
#                  dur.logs.alt_climate = 0,
#                  dur.logs.alt_climate.nofloods = 0,
#                  dur.logs.main_climate = 0,
#                  dur.logs.main_climate.nofloods = 0,
#                  dur.logs.alt_migration = 0,
#                  dur.logs.main_migration = 0,
#                  dur.logs.alt_gender = 0,
#                  dur.logs.main_gender = 0))  %>% 
#  group_by(new_id) %>%
#  mutate(cum_alt_news = sum(n.logs.alt_news),
#         cum_main_news = sum(n.logs.main_news),
#         cum_n.logs = sum(n.logs.all),
#         cum_alt_climate = sum(n.logs.alt_climate),
#         cum_alt_climate.nofloods = sum(n.logs.alt_climate.nofloods),
#         cum_main_climate = sum(n.logs.main_climate),
#         cum_main_climate.nofloods = sum(n.logs.main_climate.nofloods),
#         cum_alt_migration = sum(n.logs.alt_migration),
#         cum_main_migration = sum(n.logs.main_migration),
#         cum_dur_alt_news = sum(dur.logs.alt_news),
#         cum_dur_main_news = sum(dur.logs.main_news),
#         cum_dur_n.logs = sum(dur.logs.all),
#         cum_dur_alt_climate = sum(dur.logs.alt_climate),
#         cum_dur_alt_climate.nofloods = sum(dur.logs.alt_climate.nofloods),
#         cum_dur_main_climate = sum(dur.logs.main_climate),
#         cum_dur_main_climate.nofloods = sum(dur.logs.main_climate.nofloods),
#         cum_dur_alt_migration = sum(dur.logs.alt_migration),
#         cum_dur_main_migration = sum(dur.logs.main_migration),
#         cum_alt_gender = sum(n.logs.alt_gender),
#         cum_main_gender = sum(n.logs.main_gender),
#         cum_dur_alt_gender = sum(dur.logs.alt_gender),
#         cum_dur_main_gender = sum(dur.logs.main_gender),
#         w3_has.logs = ifelse(cum_n.logs!=0,T,F)) %>% 
#  select(new_id, starts_with("has."), days.tracked,
#         first.day, last.day, first.to.last.day, avg.tracked,
#         starts_with("participation"), starts_with("w"),
#         starts_with("cum_")) %>% 
#  mutate(wave = "3") %>% 
#  distinct(new_id, .keep_all = T) %>% 
#  rename(surv.participated = participation_w3,
#         surv.part.date = w3_datetime,
#         has.logs = w3_has.logs) %>% 
#  select(-c(w3_pre_logs_7days, starts_with("participation"), w1_datetime,
#            w1b_datetime, w2_datetime, w1_pre_logs_7days, w2_pre_logs_7days))  %>% 
#  select(new_id, wave, cum_alt_news:cum_dur_main_gender) %>% 
#  rename_at(vars(cum_alt_news:cum_dur_main_gender),function(x) paste0(x,"_7days"))
#


df.all.waves.long_7days <- rbind(df.pre.wave.1_7days, df.pre.wave.2_7days, df.pre.wave.3_7days)



#df.all.waves.long <- merge(df.all.waves.long, df.all.waves.long_14days %>% select(matches("14days"), new_id, wave), by = c("new_id", "wave"), all.x = T)
df.all.waves.long <- merge(df.all.waves.long, df.all.waves.long_7days %>% select(matches("7days"), new_id, wave), by = c("new_id", "wave"), all.x = T)


# df.all.waves <- df.all.waves %>% 
#   group_by(new_id) %>% 
#   fill(w1_has.main_news, w2_has.main_news, w3_has.main_news,
#        w1_has.alt_news, w2_has.alt_news, w3_has.alt_news,
#        w1_has.logs, w2_has.logs, w3_has.logs, .direction = "downup") %>% 
#   fill(w1_has.main_news, w2_has.main_news, w3_has.main_news,
#        w1_has.alt_news, w2_has.alt_news, w3_has.alt_news,
#        w1_has.logs, w2_has.logs, w3_has.logs, .direction = "updown") %>% 
#   mutate(has.alt_news = ifelse(w1_has.alt_news==T |w2_has.alt_news==T |w3_has.alt_news==T, T, F),
#          has.main_news = ifelse(w1_has.main_news==T |w2_has.main_news==T |w3_has.main_news==T, T, F)
#   )
# 
# df.all.waves$has.alt_news <- ifelse(is.na(df.all.waves$has.alt_news) & !is.na(df.all.waves$w1_has.alt_news),
#                                     df.all.waves$w1_has.alt_news, df.all.waves$has.alt_news)
# 
# df.all.waves$has.alt_news <- ifelse(is.na(df.all.waves$has.alt_news) & !is.na(df.all.waves$w2_has.alt_news),
#                                     df.all.waves$w2_has.alt_news, df.all.waves$has.alt_news)
# 
# df.all.waves$has.alt_news <- ifelse(is.na(df.all.waves$has.alt_news) & !is.na(df.all.waves$w3_has.alt_news),
#                                     df.all.waves$w3_has.alt_news, df.all.waves$has.alt_news)
# 
# 
# 
# df.all.waves$has.main_news <- ifelse(is.na(df.all.waves$has.main_news) & !is.na(df.all.waves$w1_has.main_news),
#                                     df.all.waves$w1_has.main_news, df.all.waves$has.main_news)
# 
# df.all.waves$has.main_news <- ifelse(is.na(df.all.waves$has.main_news) & !is.na(df.all.waves$w2_has.main_news),
#                                     df.all.waves$w2_has.main_news, df.all.waves$has.main_news)
# 
# df.all.waves$has.main_news <- ifelse(is.na(df.all.waves$has.main_news) & !is.na(df.all.waves$w3_has.main_news),
#                                     df.all.waves$w3_has.main_news, df.all.waves$has.main_news)
# 
# df.all.waves$has.logs.in.wave <- ifelse(df.all.waves$wave==1 & df.all.waves$w1_has.logs==TRUE,
#                                         T, NA)
# df.all.waves$has.logs.in.wave <- ifelse(df.all.waves$wave==2 & df.all.waves$w2_has.logs==TRUE,
#                                         T, df.all.waves$has.logs.in.wave)
# df.all.waves$has.logs.in.wave <- ifelse(df.all.waves$wave==3 & df.all.waves$w3_has.logs==TRUE,
#                                         T, df.all.waves$has.logs.in.wave)
# df.all.waves <- df.all.waves %>% 
#   replace_na(list(has.logs.in.wave = F))
# 
# table(df.all.waves$has.logs.in.wave)                                          
# 
# table(df.all.waves$wave, df.all.waves$w1_has.logs)
# table(df.all.waves$wave, df.all.waves$w2_has.logs)
# table(df.all.waves$wave, df.all.waves$w3_has.logs)
# 
# 
# table(df.all.waves$cum_alt_news)
# 
# table(df.all.waves$wave, df.all.waves$has.alt_news)
# table(df.all.waves$wave, df.all.waves$has.main_news)
# 
# df.all.waves <- df.all.waves %>% 
#   rename(has.any.alt_news = has.alt_news,
#          has.any.main_news = has.main_news
#   ) %>% 
#   select(-w1b_datetime)



saveRDS(df.all.waves.long, "track_survey_all3_waves_new_5_june_only_classified_mig.rds") # safety check

#saveRDS(df.all.waves.long, "data/work/track_survey_all3_waves_whole_period.rds")



plot(df.all.waves.long$wave, df.all.waves.long$cum_alt_news)
df.all.waves.long %>%
  filter(cum_alt_news<50 & cum_alt_news>0) %>% 
  ggplot(aes(x = wave, y = cum_alt_news)) +
  geom_point(position = position_jitter(w = .2), alpha = .1) +
  geom_line(stat = 'identity', aes(group = new_id), alpha = .04) + 
  geom_line(stat = 'smooth', method = 'lm', size = 1) + 
  theme_classic()

df.all.waves.long %>%
  filter(cum_main_news<250 & cum_main_news>0) %>% 
  ggplot(aes(x = wave, y = cum_main_news)) +
  geom_point(position = position_jitter(w = .2), alpha = .1) +
  geom_line(stat = 'identity', aes(group = new_id), alpha = .04) + 
  geom_line(stat = 'smooth', method = 'lm', size = 1) + 
  theme_classic()


### add in topical analysis such that we get exposure to topics per wave



compare <- readRDS("./data/track_survey_all3_waves_new_5_june_mig.rds")


