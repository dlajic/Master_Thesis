## -------------------------

## Load Packages -----
library(tidyverse) # for data wrangling
library(lmerTest) # for p-values from lmer
library(modelsummary) # for reg tables
library(readr)
library(dplyr)
library(tidyr)

## Load Data -----

## Tracking data for each individual 
df.track <- readRDS("./data/track_survey_all3_waves_new_5_june_only_classified_mig.rds")


## Survey data 
df.survey <- read_csv("./data/PINCET_v1-0-0_long.csv")

## Restrict to people for whom we have data in tracking dataset
df <- df.survey[df.survey$new_id %in% df.track$new_id,]

  
## Merge long survey data with tracking data

df <- merge(df, df.track, by = c("new_id", "wave"), all.x = T)


## Remove ancillary datasets

rm(df.survey, df.track)

## Exclude waves 1b and 4

df <- df %>% 
  filter(!wave %in% c("1b", "4"))




## UDF ----

## Function to convert disagree/agree labels to integers

agree.int <- function(x) {
  case_when(
    x=="Stimme überhaupt nicht zu" ~ 1,
    x=="Stimme eher nicht zu" ~ 2,
    x=="Teils/Teils" ~ 3,
    x=="Teils/ Teils" ~ 3,
    x=="Stimme eher zu" ~ 4,
    x=="Stimme voll und ganz zu" ~ 5
  )
}

## Data Preparation -------


#* Meta-data ####

df <- df %>% 
  mutate(wave = factor(wave, levels = c("1", "2", "3"),ordered = T),
         new_id = as.character(new_id)) 

#* Controls ####

#** Socio-demographics ####
# Get first non-na value 

## Education 
#lose 24 people with "Other"

df <- df %>% 
  group_by(new_id) %>% 
  mutate(ed_secondary_cat = first(na.omit(edu)),
         ed_secondary_cat = case_when(
           edu %in% c("(Noch) kein Schulabschluss", "Abschluss einer Förderschule (Sonderschule, Hilfsschule)", 
                      "Volks- oder Hauptschulabschluss bzw. Polytechnische Oberschule der ehem. DDR mit Abschluss der 8. oder 9. Klasse") ~ "Low",
           edu == "Mittlere Reife, Realschulabschluss, Fachoberschulreife oder mittlerer Schulabschluss bzw. Polytechnische Oberschule der ehem. DDR mit Abschluss der 10. Klasse" 
           ~ "Medium",
           edu == "Allgemeine oder fachgebundene Hochschulreife, Abitur" ~ "High"
         ),
         ed_secondary_cat = factor(ed_secondary_cat, levels = c("Low", "Medium", "High"))) %>% 
  ungroup()



df <- df %>% 
  group_by(new_id) %>% 
  mutate(voc_training_cat = case_when(
    edu4 == "quoted" ~ "None",
    edu3 == "quoted" ~ "In Training",
    edu5 == "quoted" ~ "Apprenticeship",
    edu6 == "quoted" ~ "Apprenticeship",
    edu7 == "quoted" ~ "Apprenticeship",
    edu10 == "quoted" ~ "Apprenticeship",
    edu8 == "quoted" ~ "College",
    edu9 == "quoted" ~ "College"
    
  ),
  voc_training_cat = factor(voc_training_cat, levels = c("None", "In Training", "Apprenticeship", "College"))) %>% 
  mutate(voc_training_cat = first(na.omit(voc_training_cat))) %>% 
  ungroup()


## Sex 
# Lose 5 'divers'

df <- df %>% 
  group_by(new_id) %>% 
  mutate(female = first(na.omit(gender))) %>% 
  mutate(female = case_when(
    female == "Männlich" ~ 0,
    female == "Weiblich" ~ 1
  )) %>% 
  ungroup()


## Age 
# Lose 34 people younger than 18

df <- df %>% 
  group_by(new_id) %>% 
  mutate(age = as.numeric(first(na.omit(birth_year))),
         age = 2021 - age) %>% 
  ungroup()

df$age[df$age>100 | df$age < 18] <- NA

df <- df %>% 
  mutate(age_cat = case_when(
    age <25 ~ "18-24",
    age >24 & age<35 ~ "25-34",
    age > 34 & age<55 ~ "35-54",
    age>54 ~ "55+"
  ),
  age_cat = factor(age_cat, levels = c("55+", "18-24", "25-34", "35-54")))


#** Political Interest #####

df$pol.int_cat <- df$polint
df$pol.int_cat[df$pol.int_cat=="Überhaupt nicht interessiert"] <- "Not interested at all"
df$pol.int_cat[df$pol.int_cat=="Wenig interessiert"] <- "Little interest"
df$pol.int_cat[df$pol.int_cat=="Teilweise interessiert"] <- "Somewhat interested"
df$pol.int_cat[df$pol.int_cat=="Ziemlich interessiert"] <- "Fairly interested"
df$pol.int_cat[df$pol.int_cat=="Sehr interessiert"] <- "Very interested"

df$pol.int_cat <- factor(df$pol.int_cat, 
                         levels = c("Not interested at all", "Little interest", "Somewhat interested", "Fairly interested", "Very interested"))
df$pol.int <- df$pol.int_cat %>% as.integer() # continuous measure


#** In-party ####
#Note that some people change between waves

df$which.inparty <- df$pid1_par
df$which.inparty[df$which.inparty %in% c(".")] <- "None"
df$which.inparty[df$which.inparty %in% c("Andere Partei, und zwar", "Piratenpartei")] <- "Other"
df$which.inparty[df$which.inparty %in% c("CDU", "CSU")] <- "CDU/CSU"
df$which.inparty[df$which.inparty=="Bündnis 90/ Die Grünen"] <- "Greens"
df$which.inparty <- factor(df$which.inparty, levels = c("None","Die Linke", "Greens",
                                                        "SPD", "FDP", "CDU/CSU", "AfD","Other"))


#** Left-right ####

df <- df %>% 
  mutate(left_right = case_when(
    str_detect(leftright, "Rechts") ~ 10,
    str_detect(leftright, "Links") ~ 0,
    TRUE ~ parse_number(leftright)
    
  ))

#** Populist att ####

df[,c("pop_party", "pop_elite", "pop_politician")] <- df[,c("op_party", "op_elite", "op_politician")]

df <- df %>% 
  mutate(across(starts_with("pop_"),
                .fns = ~agree.int(.x)))


df$pop_att <- apply(df %>% select(starts_with("pop_")), 1, mean, na.rm=T)

#** Issue Positions ####



## Climate

df <- df %>% 
  mutate(dv.issue.climate = parse_number(clim, na = "."),
         dv.issue.climate = abs(dv.issue.climate - 6)) # higher values, higher support for tackling climate change

## Climate salience

df <- df %>% 
  mutate(dv.issue.climate.salience = case_when(
    env == "sehr wichtig" ~ 4,
    env == "ziemlich wichtig" ~ 3,
    env == "nicht sehr wichtig" ~ 2,
    env == "überhaupt nicht wichtig" ~ 1
  ))

### Migration variable

df <- df %>% 
  mutate(dv.issue.migration = parse_number(assim, na = "."),
         dv.issue.migration = abs(dv.issue.migration - 6)) # higher values, higher anti immigration attitude


##* Green ####

df <- df %>% 
  mutate(dv.green_sympathy = parse_number(gruene, na = "."))


##* AfD ####
df <- df %>% 
  mutate(dv.afd_sympathy = parse_number(afd, na = "."))


  
#* IVs ####

#** Media consumption ####

# convert duration from seconds to minutes
df <- df %>% 
  mutate(across(starts_with("cum_dur.logs"),
                .fns = function(x) x/60,
                .names = "{col}_minutes")) 


# Decompose env

df$cum_n.logs.alt_env_descriptive <- df$cum_n.logs.alt_env - df$cum_n.logs.alt_env_opinion
df$cum_n.logs.main_env_descriptive <- df$cum_n.logs.main_env - df$cum_n.logs.main_env_opinion

df$cum_dur.logs.alt_env_descriptive_minutes <- df$cum_dur.logs.alt_env_minutes - df$cum_dur.logs.alt_env_opinion_minutes
df$cum_dur.logs.main_env_descriptive_minutes <- df$cum_dur.logs.main_env_minutes - df$cum_dur.logs.main_env_opinion.y_minutes


#Decompose migration assimilation

df$cum_n.logs.alt_mig_descriptive <- df$cum_n.logs.alt_mig - df$cum_n.logs.alt_mig_opinion
df$cum_n.logs.main_mig_descriptive <- df$cum_n.logs.main_mig - df$cum_n.logs.main_mig_opinion

df$cum_dur.logs.alt_mig_descriptive_minutes <- df$cum_dur.logs.alt_mig_minutes - df$cum_dur.logs.alt_mig_opinion_minutes
df$cum_dur.logs.main_mig_descriptive_minutes <- df$cum_dur.logs.main_mig_minutes - df$cum_dur.logs.main_mig_opinion.y_minutes

# Logged count

df <- df %>% 
  mutate(across(starts_with("cum_n.logs"),
                .fns = function(x) log(x+1),
                .names = "log_{col}"))

# Logged duration

df <- df %>% 
  mutate(across(starts_with("cum_dur.logs"),
                .fns = function(x) log(x+1),
                .names = "log_{col}"))

# Dummies

df <- df %>% 
  mutate(across(starts_with("cum_n.logs"),
                .fns = ~ case_when(. > 0 ~ 1,
                                   TRUE ~ 0),
                .names = "dummy_{col}"))


df <- df %>% 
  mutate(across(starts_with("cum_dur.logs"),
                .fns = ~ case_when(. > 0 ~ 1,
                                   TRUE ~ 0),
                .names = "dummy_{col}"))


# Relative (both count and n of outlets)
#
#
#df <- df %>% 
#  mutate(across(matches("^cum_n.*(?<!days)$",perl = T), # starts with cum_n and does not finish with 'days'
#                .fns = function(x) (x/days.active),
#                .names = "rel_{col}"))
#
## Logged relative count (to be used in main models)
#
#
#df <- df %>% 
#  mutate(across(starts_with("rel_cum_n.logs"),
#                .fns = function(x) log(x+1),
#                .names = "log_{col}"))

### Last 7 days
#
#
## Logged relative count
#
#df <- df %>% 
#  mutate(across(matches("^cum_n.*(?<=7days)$",perl = T), 
#                .fns = function(x) log((x/days.active_7days)+1),
#                .names = "log_rel_{col}"))


# Analytic Data -----

## Exclude unnecessary tracking data

df <- df %>% 
  select(-matches("(_climate)|(_gender)|(_migration)|(n.outlets)"))

## Vars to be selected

dvs <- df %>% select(starts_with("dv")) %>% names()
ivs <- df %>% select(matches("cum_n.|cum.dur",perl = T)) %>% names()
ivs_7days <- df %>% select(matches("cum_n.*(?<=7days)$",perl = T)) %>% names()
controls <- c("pop_att", "left_right", "pol.int", "which.inparty", "female", "age_cat", "ed_secondary_cat", "voc_training_cat", "wave")
meta <- c("new_id", "participation", "days.active", "has.logs")


## Drop NAs

df.analysis <- df %>% 
  select(dvs, ivs, controls, meta)  %>% 
  drop_na(-ivs_7days)

## Drop units with only 1 wave (cannot be in the model)

df.analysis <- df.analysis %>% 
  filter(has.logs) %>% 
  group_by(new_id) %>% 
  mutate(n_waves_in_model = n(),
         waves_in_model = str_c(as.character(wave[participation=="yes"]), collapse = " and ")) %>% 
  filter(n_waves_in_model>1) %>% 
  ungroup()

## Standardisation 

df.analysis <- df.analysis %>% 
  mutate(across(.cols = c(starts_with(c("dv.", "log_")), pol.int, left_right, pop_att),
                .fns = ~as.numeric(scale(.)),
                .names = "{col}_z"))


## Demean IVs

df.analysis <- cbind(df.analysis,
                     parameters::demean(df.analysis,
                                        select = c("log_cum_n.logs.alt_news",
                                                   "log_cum_n.logs.main_news",
                                                   "log_cum_n.logs.all",
                                                   "log_cum_n.logs.alt_mig",
                                                   "log_cum_n.logs.main_mig",
                                                   "log_cum_n.logs.alt_mig_opinion",
                                                   "log_cum_n.logs.main_mig_opinion",
                                                   "log_cum_n.logs.alt_mig_descriptive",
                                                   "log_cum_n.logs.main_mig_descriptive",
                                                   "log_cum_dur.logs.alt_news_minutes",
                                                   "log_cum_dur.logs.main_news_minutes",
                                                   "log_cum_dur.logs.all_minutes",
                                                   "log_cum_dur.logs.alt_mig_minutes",
                                                   "log_cum_dur.logs.main_mig_minutes",
                                                   "log_cum_dur.logs.alt_mig_opinion_minutes",
                                                   "log_cum_dur.logs.main_mig_opinion.y_minutes",
                                                   "log_cum_dur.logs.alt_mig_descriptive_minutes",
                                                   "log_cum_dur.logs.main_mig_descriptive_minutes",
                                                   "cum_n.logs.alt_news",
                                                   "cum_n.logs.main_news",
                                                   "cum_n.logs.all",
                                                   "cum_n.logs.alt_mig",
                                                   "cum_n.logs.main_mig",
                                                   "cum_n.logs.alt_mig_opinion",
                                                   "cum_n.logs.main_mig_opinion",
                                                   "cum_n.logs.alt_mig_descriptive",
                                                   "cum_n.logs.main_mig_descriptive",
                                                   "cum_dur.logs.alt_news_minutes",
                                                   "cum_dur.logs.main_news_minutes",
                                                   "cum_dur.logs.all_minutes",
                                                   "cum_dur.logs.alt_mig_minutes",
                                                   "cum_dur.logs.main_mig_minutes",
                                                   "cum_dur.logs.alt_mig_opinion_minutes",
                                                   "cum_dur.logs.main_mig_opinion.y_minutes",
                                                   "cum_dur.logs.alt_mig_descriptive_minutes",
                                                   "cum_dur.logs.main_mig_descriptive_minutes",
                                                   "dummy_cum_dur.logs.alt_news_minutes",
                                                   "dummy_cum_dur.logs.main_news_minutes",
                                                   "dummy_cum_dur.logs.alt_mig_minutes",
                                                   "dummy_cum_dur.logs.main_mig_minutes",
                                                   "dummy_cum_dur.logs.alt_mig_opinion_minutes",
                                                   "dummy_cum_dur.logs.main_mig_opinion.y_minutes",
                                                   "dummy_cum_dur.logs.alt_mig_descriptive_minutes",
                                                   "dummy_cum_dur.logs.main_mig_descriptive_minutes",
                                                   "dummy_cum_n.logs.alt_news",
                                                   "dummy_cum_n.logs.main_news",
                                                   "dummy_cum_n.logs.alt_mig",
                                                   "dummy_cum_n.logs.main_mig",
                                                   "dummy_cum_n.logs.alt_mig_opinion",
                                                   "dummy_cum_n.logs.main_mig_opinion",
                                                   "dummy_cum_n.logs.alt_mig_descriptive",
                                                   "dummy_cum_n.logs.main_mig_descriptive",
                                                   "pol.int_z", "left_right_z", "pop_att_z"),
                                        group = "new_id"))

## Is in model

#write_csv(df.analysis %>% distinct(new_id) %>% mutate(is_in_model = T),
#         paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/is_in_model.csv"))


#* Export it ----
library(openxlsx)

write.xlsx(df.analysis,"df_analysis_mig_final.xlsx")











