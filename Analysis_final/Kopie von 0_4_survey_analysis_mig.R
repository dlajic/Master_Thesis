## -------------------------

## Load Packages -----
library(tidyverse)
library(broom.mixed)
library(lmerTest) # for p-values from lmer
library(modelsummary) # for reg tables
library(openxlsx)
library(readr)
library(kableExtra)


## Load Data -----

## Load analytic data (constructed in "0_2_survey_data_prep")
# contains only people having participated at least in two waves but also participants that read only unclassified articles
df.analysis <- read.xlsx("data/df_analysis_mig_final.xlsx")


# get tracking data that entails only individuals having read at least one classified article in the time span used in the thesis
df.clean <- read_csv("data/df.clean.csv")


# get common new_ids 
common_new_ids <- intersect(df.analysis$new_id, df.clean$new_id)

# filter df.analysis, to get only new_ids that appear in both datasets so that only individuals that have participated in two waves and have read at least one classified article are included
df.analysis <- df.analysis %>%
  filter(new_id %in% common_new_ids)



# get people who change between main and alt news
te <- df.analysis %>% group_by(new_id, wave) %>%
  mutate(alt_general = if_else(cum_n.logs.alt_news >0, T, F),
         main_general = if_else(cum_n.logs.main_news >0, T, F),
         alt_mig = if_else(cum_n.logs.alt_mig >0, T, F))

te2 <- te %>% 
  group_by(new_id) %>% 
  summarise(alt_general = sum(alt_general),
            main_general = sum(main_general),
            alt_mig = sum(alt_mig))

# alternate from wave to to wave alternative consumption
te2 %>% 
  filter(alt_general > 0 & alt_general < 3) %>%
  distinct(new_id) %>%
  count()


# alternate from wave to to wave alternative mig consumption
te2 %>% 
  filter(alt_mig > 0 & alt_mig < 3) %>%
  distinct(new_id) %>%
  count()

# alternate from wave to to wave mainstream consumption
te2 %>% 
  filter(main_general > 0 & main_general < 3) %>%
  distinct(new_id) %>%
  count()

# only mainstream
te2 %>% 
  filter(main_general > 0 & alt_general == 0) %>%
  distinct(new_id) %>%
  count()

# only alternative
te2 %>% 
  filter(alt_general > 0 & main_general == 0) %>%
  distinct(new_id) %>%
  count()

## Models ------------

#* Formulas ####

## General articles


wb.general.model <- function(dv, data) {
  
  formula <- as.formula(paste(dv, "~  dummy_cum_n.logs.alt_news + dummy_cum_n.logs.alt_news_between + # dummy alt news general
                                    cum_n.logs.alt_news_within + cum_n.logs.alt_news_between + # count alt news general
                                    dummy_cum_n.logs.alt_mig + dummy_cum_n.logs.alt_mig_between + # dummy mig alt news
                                    cum_n.logs.alt_mig_within + cum_n.logs.alt_mig_between + # count mig alt 
                                    dummy_cum_n.logs.main_news +  dummy_cum_n.logs.main_news_between +  # dummy main news general
                                    cum_n.logs.main_news_within + cum_n.logs.main_news_between + # count main news general
                                    dummy_cum_n.logs.main_mig + dummy_cum_n.logs.main_mig_between + # dummy main mig 
                                    cum_n.logs.main_mig_within + cum_n.logs.main_mig_between + # count main mig
                                    pop_att_z_within + pop_att_z_between + # populist attitudes
                                    pol.int_z_within + pol.int_z_between + left_right_z_within + left_right_z_between + # pol int and l-r
                                    which.inparty + female + age_cat  + ed_secondary_cat + voc_training_cat +  as.character(wave) + 
                                    (1|new_id)"))
  
  model <- lmer(formula, data = data)
  
}
glm

## Opinion articles


wb.opinion.model <- function(dv, data) {
  
  formula <- as.formula(paste(dv, "~ dummy_cum_n.logs.alt_news + dummy_cum_n.logs.alt_news_between + # dummy alt news general
                                      cum_n.logs.alt_news_within + cum_n.logs.alt_news_between + # count alt news general
                                      dummy_cum_n.logs.alt_mig_descriptive + dummy_cum_n.logs.alt_mig_descriptive_between + # dummy mig alt news descriptive
                                      cum_n.logs.alt_mig_descriptive_within + cum_n.logs.alt_mig_descriptive_between + # count mig alt descriptive
                                      dummy_cum_n.logs.alt_mig_opinion + dummy_cum_n.logs.alt_mig_opinion_between + # dummy mig alt opinion
                                      cum_n.logs.alt_mig_opinion_within + cum_n.logs.alt_mig_opinion_between  + # count mig alt opinion
                                      dummy_cum_n.logs.main_news +  dummy_cum_n.logs.main_news_between +  # dummy main news general
                                      cum_n.logs.main_news_within + cum_n.logs.main_news_between + # count main news general
                                      dummy_cum_n.logs.main_mig_descriptive + dummy_cum_n.logs.main_mig_descriptive_between + # dummy main mig - descriptive
                                      cum_n.logs.main_mig_descriptive_within + cum_n.logs.main_mig_descriptive_between + # count main mig - descriptive
                                      dummy_cum_n.logs.main_mig_opinion + dummy_cum_n.logs.main_mig_opinion_between + # dummy main mig - opinion 
                                      cum_n.logs.main_mig_opinion_within + cum_n.logs.main_mig_opinion_between  +  # count main mig - opinion
                                      pop_att_z_within + pop_att_z_between + # populist attitudes
                                      pol.int_z_within + pol.int_z_between + left_right_z_within + left_right_z_between + # pol int and l-r
                                      which.inparty + female + age_cat  + ed_secondary_cat + voc_training_cat +  as.character(wave) + 
                                      (1|new_id)"))
  
  model <- lmer(formula, data = data)
  
}
lmer

## List of dvs
dv_z <- c("dv.issue.migration_z", "dv.afd_sympathy_z")


#* Apply models ####
general.models <- lapply(dv_z, wb.general.model, data = df.analysis)
general.models <- setNames(general.models, c("Migration", "AfD sympathy"))


opinion.models <- lapply(dv_z, FUN = wb.opinion.model, data = df.analysis)
opinion.models <- setNames(opinion.models, c("Migration", "AfD sympathy"))




#* Tidy and plot it ####


#** General ####

## Create tidy dataframe from model results

tidy.general.models <- map(general.models, ~broom.mixed::tidy(.,conf.int=T))
tidy.general.models <- bind_rows(tidy.general.models, .id = "name")

## Filter for relevant vars 

tidy.general.models <- tidy.general.models %>% 
  filter(str_detect(term, "(news|mig)")) %>% 
  mutate(type = case_when(
    str_detect(term, "dummy_cum_n") ~ "Dummy (any articles consumed)",
    str_detect(term, "cum_n") ~ "Cumulative count of articles consumed",
    str_detect(term, "cum_dur") ~ "Cumulative duration (minutes) of articles consumed"
  ),
  effect = case_when(
    str_detect(term, "between") ~ "Between",
    TRUE ~ "Within"
  ),
  term = case_when(
    str_detect(term, "alt_mig") ~ "Alt. News -\nMigration",
    str_detect(term, "alt_news") ~ "Alt. News -\nAll articles",
    str_detect(term, "main_mig") ~ "Main. News -\nMigration",
    str_detect(term, "main_news") ~ "Main. News -\nAll articles",
    TRUE ~ term
  ))


tidy.general.models$term <- factor(tidy.general.models$term)
tidy.general.models$term <- fct_rev(tidy.general.models$term)
tidy.general.models$name <- factor(tidy.general.models$name, levels=c("Migration", "AfD sympathy"))
tidy.general.models$type <- factor(tidy.general.models$type, levels=c("Dummy (any articles consumed)", "Cumulative count of articles consumed"))


## Plot it

ggplot(data = tidy.general.models, 
       aes(x = estimate, y = term, colour = fct_rev(type), linetype = effect, 
           shape = ifelse(p.value<0.05, "p < 0.05", "p > 0.05"))) +
  geom_point(size = 5, position = ggstance::position_dodgev(height=.8), stroke = 2) +
  geom_errorbar(aes(y = term, xmin = conf.low, xmax=conf.high), lwd=2, width=.2,
                position = ggstance::position_dodgev(height=.8)) +
  geom_vline(xintercept = 0, lty=2, lwd = 1) +
  scale_colour_brewer(type = "seq", palette = "Set1",direction = -1, name = "IV Type:") +
  scale_shape_manual(values=c(16,1), name = "") +
  scale_linetype_manual(values = c("solid", "dotted")) +
  theme_bw(base_size=18) +
  theme(legend.position = "top",
        axis.text.y = element_text(size=20),
        #plot.title   = element_text(size=20, hjust = 0.5),
        #plot.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.text.x  = element_text(size=20, color="black"),
        strip.text.x = element_text(size=20,
                                    face="bold")) +
  guides(colour = guide_legend(override.aes = list(),reverse = T, order = 1, nrow=2),
         shape = guide_legend(order=3),
         linetype = guide_legend(reverse = T, order = 2,nrow = 2)) +
  labs(x = "Beta Coefficient", linetype = "Effect", y = "") +
  facet_wrap(~name)

## Save
ggsave("./Figures/fig2_main.models.png", width = 23, height=18, units = "in", dpi = 400)

#** Opinion ####

tidy.opinion.models <- map(opinion.models, ~broom.mixed::tidy(.,conf.int=T))

tidy.opinion.models <- bind_rows(tidy.opinion.models, .id = "name")

# Filter for relevant vars

tidy.opinion.models <- tidy.opinion.models %>% 
  filter(str_detect(term, "(opinion|descriptive)")) %>% 
  mutate(type = case_when(
    str_detect(term, "dummy_cum_n") ~ "Dummy (any articles consumed)",
    str_detect(term, "cum_n") ~ "Cumulative count of articles consumed",
    str_detect(term, "cum_dur") ~ "Cumulative duration (minutes) of articles consumed"
  ),
  effect = case_when(
    str_detect(term, "between") ~ "Between",
    TRUE ~ "Within"
  ),
  term = case_when(
    str_detect(term, "alt_mig_opinion") ~ "Alt. News -\nMigration\nOpinion",
    str_detect(term, "alt_mig_descriptive") ~ "Alt. News -\nMigration\nDescriptive",
    str_detect(term, "main_mig_opinion") ~ "Main. News -\nMigration\nOpinion",
    str_detect(term, "main_mig_descriptive") ~ "Main. News -\nMigration\nDescriptive",
    TRUE ~ term
  ))

tidy.opinion.models$term <- factor(tidy.opinion.models$term,
                                   levels = rev(c("Alt. News -\nMigration\nOpinion", "Alt. News -\nMigration\nDescriptive",
                                                  "Main. News -\nMigration\nOpinion", "Main. News -\nMigration\nDescriptive")))
tidy.opinion.models$name <- factor(tidy.opinion.models$name, levels=c("Migration", "AfD sympathy"))
tidy.opinion.models$type <- factor(tidy.opinion.models$type, levels=c("Dummy (any articles consumed)", "Cumulative count of articles consumed"))


ggplot(data = tidy.opinion.models, 
       aes(x = estimate, y = term, colour = fct_rev(type), linetype = effect, 
           shape = ifelse(p.value<0.05, "p < 0.05", "p > 0.05"))) +
  geom_point(size = 5, position = ggstance::position_dodgev(height=.8), stroke = 2) +
  geom_errorbar(aes(y = term, xmin = conf.low, xmax=conf.high), lwd=2, width=.2,
                position = ggstance::position_dodgev(height=.8)) +
  geom_vline(xintercept = 0, lty=2, lwd = 1) +
  scale_colour_brewer(type = "seq", palette = "Set1",direction = -1, name = "IV Type:") +
  scale_shape_manual(values=c(16,1), name = "") +
  scale_linetype_manual(values = c("solid", "dotted")) +
  theme_bw(base_size=18) +
  theme(legend.position = "top",
        axis.text.y = element_text(size=20),
        #plot.title   = element_text(size=20, hjust = 0.5),
        #plot.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.text.x  = element_text(size=20, color="black"),
        strip.text.x = element_text(size=20,
                                    face="bold")) +
  guides(colour = guide_legend(override.aes = list(),reverse = T, order = 1, nrow=2),
         shape = guide_legend(order=3),
         linetype = guide_legend(reverse = T, order = 2,nrow = 2)) +
  labs(x = "Beta Coefficient", linetype = "Effect", y = "") +
  facet_wrap(~name)

## Save
ggsave("./Figures/fig14_opinion.model.png", width = 23, height=18, units = "in", dpi = 400)


#* Regression tables ####

## Labels

covariates.general <- c("Intercept", 
                        "Alt. News Dummy", "Alt. News Dummy (Between)",
                        "Alt. News Count (Within)", "Alt. News Count (Between)",
                        "Alt. News Mig. Dummy", "Alt. News Mig. Dummy (Between)",
                        "Alt. News Mig. Count (Within)", "Alt. News Mig. Count (Between)",
                        "Main News Dummy", "Main News Dummy (Between)",
                        "Main News Count (Within)", "Main News Count (Between)",
                        "Main News Mig. Dummy", "Main News Mig. Dummy (Between)",
                        "Main News Mig. Count (Within)", "Main News Mig. Count (Between)",
                        "Populist Att. (Within)", "Populist Att. (Between)",
                        "Pol. Interest (Within)", "Pol. Interest (Between)",
                        "Left-Right (Within)", "Left-Right (Between)",
                        "In-Party: Die Linke", "In-Party: Greens", "In-Party: SPD",
                        "In-Party: FDP", "In-Party:CDU/CSU", "In-Party: AfD", "In-Party: Other", 
                        "Female", "Age: 18-24","Age: 25-34", "Age: 35-54",
                        "Sec. Education: Medium", "Sec. Education: High",
                        "Voc. Training: In Training", "Voc. Training: Apprenticeship", "Voc. Training: College", 
                        "Wave: 2", "Wave: 3")


# Map labels to variable names
names(covariates.general) <- coef(summary(general.models[[1]])) %>% rownames 




covariates.opinion <- c("Intercept", 
                        "Alt. News Dummy", "Alt. News Dummy (Between)",
                        "Alt. News Count (Within)", "Alt. News Count (Between)",
                        "Alt. News Mig. Descriptive Dummy", "Alt. News Mig. Descriptive Dummy (Between)",
                        "Alt. News Mig. Descriptive Count (Within)", "Alt. News Mig. Descriptive Count (Between)",
                        "Alt. News Mig. Opinion \nDummy", "Alt. Mig. Opinion Dummy (Between)",
                        "Alt. News Mig. Opinion Count (Within)", "Alt. News Mig. Opinion Count (Between)",
                        "Main News Dummy", "Main News Dummy (Between)",
                        "Main News Count (Within)", "Main News Count (Between)",
                        "Main News Mig. Descriptive Dummy", "Main News Mig. Descriptive Dummy (Between)",
                        "Main News Mig. Descriptive Count (Within)", "Main News Mig. Descriptive Count (Between)",
                        "Main News Mig. Opinion Dummy", "Main News Mig. Opinion Dummy (Between)",
                        "Main News Mig. Opinion Count (Within)", "Main News Mig. Opinion Count (Between)",
                        "Populist Att. (Within)", "Populist Att. (Between)",
                        "Pol. Interest (Within)", "Pol. Interest (Between)",
                        "Left-Right (Within)", "Left-Right (Between)",
                        "In-Party: Die Linke", "In-Party: Greens", "In-Party: SPD",
                        "In-Party: FDP", "In-Party:CDU/CSU", "In-Party: AfD", "In-Party: Other", 
                        "Female", "Age: 18-24","Age: 25-34", "Age: 35-54",
                        "Sec. Education: Medium", "Sec. Education: High",
                        "Voc. Training: In Training", "Voc. Training: Apprenticeship", "Voc. Training: College", 
                        "Wave: 2", "Wave: 3")


names(covariates.opinion) <- coef(summary(opinion.models[[1]])) %>% rownames 




## General models

table12_reg <- modelsummary(general.models,output = "kableExtra", longtable =TRUE,
                           coef_map = covariates.general, booktabs=T, stars = T,
                           fmt_decimal(digits = 5),
                           title = "Results of random effects within-between models.") 

save_kable(table12_reg, file = "./Tables/table12_reg.html")

## Opinion models

table13_reg <- modelsummary(opinion.models,output = "kableExtra", longtable =TRUE,
                           coef_map = covariates.opinion, booktabs=T, stars = T,
                           fmt_decimal(digits = 5),
                           title = "Results of random effects within-between models. Opinion and descriptive news.") 

save_kable(table13_reg, file = "./Tables/table13_reg.html")


## Robustness checks --------

#* Log ####

#** General
wb.rob.log <- function(dv, data) {
  
  formula <- as.formula(paste(dv, "~  dummy_cum_n.logs.alt_news + dummy_cum_n.logs.alt_news_between + # dummy alt news general
                                    log_cum_n.logs.alt_news_within + log_cum_n.logs.alt_news_between + # count alt news general
                                    dummy_cum_n.logs.alt_mig + dummy_cum_n.logs.alt_mig_between + # dummy mig alt news
                                    log_cum_n.logs.alt_mig_within + log_cum_n.logs.alt_mig_between + # count mig alt 
                                    dummy_cum_n.logs.main_news +  dummy_cum_n.logs.main_news_between +  # dummy main news general
                                    log_cum_n.logs.main_news_within + log_cum_n.logs.main_news_between + # count main news general
                                    dummy_cum_n.logs.main_mig + dummy_cum_n.logs.main_mig_between + # dummy main mig 
                                    log_cum_n.logs.main_mig_within + log_cum_n.logs.main_mig_between + # count main mig
                                    pop_att_z_within + pop_att_z_between + # populist attitudes
                                    pol.int_z_within + pol.int_z_between + left_right_z_within + left_right_z_between + # pol int and l-r
                                    which.inparty + female + age_cat  + ed_secondary_cat + voc_training_cat +  as.character(wave) + 
                                    (1|new_id)"))
  model <- lmer(formula, data = data)
  
}


#Apply models ##
rob.log.models <- lapply(dv_z, wb.rob.log, data = df.analysis)
rob.log.models <- setNames(rob.log.models, c("Migration", "AfD sympathy"))

tidy.rob.log.models <- map(rob.log.models, ~broom.mixed::tidy(.,conf.int=T))
tidy.rob.log.models <- bind_rows(tidy.rob.log.models, .id = "name")


tidy.rob.log.models <- tidy.rob.log.models %>% 
  filter(str_detect(term, "(news|mig)")) %>% 
  mutate(type = case_when(
    str_detect(term, "dummy_cum_n") ~ "Dummy (any articles consumed)",
    str_detect(term, "cum_n") ~ "Cumulative count of articles consumed",
    str_detect(term, "cum_dur") ~ "Cumulative duration (minutes) of articles consumed"
  ),
  effect = case_when(
    str_detect(term, "between") ~ "Between",
    TRUE ~ "Within"
  ),
  term = case_when(
    str_detect(term, "alt_mig") ~ "Alt. News -\nMigration",
    str_detect(term, "alt_news") ~ "Alt. News -\nAll articles",
    str_detect(term, "main_mig") ~ "Main. News -\nMigration",
    str_detect(term, "main_news") ~ "Main. News -\nAll articles",
    TRUE ~ term
  ))

tidy.rob.log.models$term <- factor(tidy.rob.log.models$term)
tidy.rob.log.models$term <- fct_rev(tidy.rob.log.models$term)
tidy.rob.log.models$name <- factor(tidy.rob.log.models$name, levels=c("Migration", "AfD sympathy"))
tidy.rob.log.models$type <- factor(tidy.rob.log.models$type, levels=c("Dummy (any articles consumed)", "Cumulative count of articles consumed"))

## Plot it

ggplot(data = tidy.rob.log.models, 
       aes(x = estimate, y = term, colour = fct_rev(type), linetype = effect, 
           shape = ifelse(p.value<0.05, "p < 0.05", "p > 0.05"))) +
   geom_point(size = 5, position = ggstance::position_dodgev(height=.8), stroke = 2) +
  geom_errorbar(aes(y = term, xmin = conf.low, xmax=conf.high), lwd=2, width=.2,
                position = ggstance::position_dodgev(height=.8)) +
  geom_vline(xintercept = 0, lty=2, lwd = 1) +
  scale_colour_brewer(type = "seq", palette = "Set1",direction = -1, name = "IV Type:") +
  scale_shape_manual(values=c(16,1), name = "") +
  scale_linetype_manual(values = c("solid", "dotted")) +
  theme_bw(base_size=18) +
  theme(legend.position = "top",
        axis.text.y = element_text(size=20),
        #plot.title   = element_text(size=20, hjust = 0.5),
        #plot.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.text.x  = element_text(size=20, color="black"),
        strip.text.x = element_text(size=20,
                                    face="bold")) +
  guides(colour = guide_legend(override.aes = list(),reverse = T, order = 1, nrow=2),
         shape = guide_legend(order=3),
         linetype = guide_legend(reverse = T, order = 2,nrow = 2)) +
  labs(x = "Beta Coefficient", linetype = "Effect", y = "") +
  facet_wrap(~name)

## Save
ggsave("./Figures/fig15_main.models.png", width = 23, height=18, units = "in", dpi = 400)


#** Opinion

wb.rob.log.opinion <- function(dv, data) {
  
  formula <- as.formula(paste(dv, "~ dummy_cum_n.logs.alt_news + dummy_cum_n.logs.alt_news_between + # dummy alt news general
                                      log_cum_n.logs.alt_news_within + log_cum_n.logs.alt_news_between + # count alt news general
                                      dummy_cum_n.logs.alt_mig_descriptive + dummy_cum_n.logs.alt_mig_descriptive_between + # dummy mig alt news descriptive
                                      log_cum_n.logs.alt_mig_descriptive_within + log_cum_n.logs.alt_mig_descriptive_between + # count mig alt descriptive
                                      dummy_cum_n.logs.alt_mig_opinion + dummy_cum_n.logs.alt_mig_opinion_between + # dummy mig alt opinion
                                      log_cum_n.logs.alt_mig_opinion_within + log_cum_n.logs.alt_mig_opinion_between  + # count mig alt opinion
                                      dummy_cum_n.logs.main_news +  dummy_cum_n.logs.main_news_between +  # dummy main news general
                                      log_cum_n.logs.main_news_within + log_cum_n.logs.main_news_between + # count main news general
                                      dummy_cum_n.logs.main_mig_descriptive + dummy_cum_n.logs.main_mig_descriptive_between + # dummy main mig - descriptive
                                      log_cum_n.logs.main_mig_descriptive_within + log_cum_n.logs.main_mig_descriptive_between + # count main mig - descriptive
                                      dummy_cum_n.logs.main_mig_opinion + dummy_cum_n.logs.main_mig_opinion_between + # dummy main mig - opinion 
                                      log_cum_n.logs.main_mig_opinion_within + log_cum_n.logs.main_mig_opinion_between  +  # count main mig - opinion
                                      pop_att_z_within + pop_att_z_between + # populist attitudes
                                      pol.int_z_within + pol.int_z_between + left_right_z_within + left_right_z_between + # pol int and l-r
                                      which.inparty + female + age_cat  + ed_secondary_cat + voc_training_cat +  as.character(wave) + 
                                      (1|new_id)"))
  model <- lmer(formula, data = data)
  
}

rob.log.opinion <- lapply(dv_z, FUN = wb.rob.log.opinion, data = df.analysis)
rob.log.opinion <- setNames(rob.log.opinion, c("Migration", "AfD sympathy"))


tidy.rob.log.opinion <- map(rob.log.opinion, ~broom.mixed::tidy(.,conf.int=T))

tidy.rob.log.opinion <- bind_rows(tidy.rob.log.opinion, .id = "name")

# Filter for relevant vars

tidy.rob.log.opinion <- tidy.rob.log.opinion %>% 
  filter(str_detect(term, "(opinion|descriptive)")) %>% 
  mutate(type = case_when(
    str_detect(term, "dummy_cum_n") ~ "Dummy (any articles consumed)",
    str_detect(term, "cum_n") ~ "Cumulative count of articles consumed",
    str_detect(term, "cum_dur") ~ "Cumulative duration (minutes) of articles consumed"
  ),
  effect = case_when(
    str_detect(term, "between") ~ "Between",
    TRUE ~ "Within"
  ),
  term = case_when(
    str_detect(term, "alt_mig_opinion") ~ "Alt. News -\nMigration\nOpinion",
    str_detect(term, "alt_mig_descriptive") ~ "Alt. News -\nMigration\nDescriptive",
    str_detect(term, "main_mig_opinion") ~ "Main. News -\nMigration\nOpinion",
    str_detect(term, "main_mig_descriptive") ~ "Main. News -\nMigration\nDescriptive",
    TRUE ~ term
  ))

tidy.rob.log.opinion$term <- factor(tidy.rob.log.opinion$term,
                                   levels = rev(c("Alt. News -\nMigration\nOpinion", "Alt. News -\nMigration\nDescriptive",
                                                  "Main. News -\nMigration\nOpinion", "Main. News -\nMigration\nDescriptive")))
tidy.rob.log.opinion$name <- factor(tidy.rob.log.opinion$name, levels=c("Migration", "AfD sympathy"))
tidy.rob.log.opinion$type <- factor(tidy.rob.log.opinion$type, levels=c("Dummy (any articles consumed)", "Cumulative count of articles consumed"))


ggplot(data = tidy.rob.log.opinion, 
       aes(x = estimate, y = term, colour = fct_rev(type), linetype = effect, 
           shape = ifelse(p.value<0.05, "p < 0.05", "p > 0.05"))) +
  geom_point(size = 5, position = ggstance::position_dodgev(height=.8), stroke = 2) +
  geom_errorbar(aes(y = term, xmin = conf.low, xmax=conf.high), lwd=2, width=.2,
                position = ggstance::position_dodgev(height=.8)) +
  geom_vline(xintercept = 0, lty=2, lwd = 1) +
  scale_colour_brewer(type = "seq", palette = "Set1",direction = -1, name = "IV Type:") +
  scale_shape_manual(values=c(16,1), name = "") +
  scale_linetype_manual(values = c("solid", "dotted")) +
  theme_bw(base_size=18) +
  theme(legend.position = "top",
        axis.text.y = element_text(size=20),
        #plot.title   = element_text(size=20, hjust = 0.5),
        #plot.title = element_text(size=20),
        legend.text = element_text(size=20),
        axis.text.x  = element_text(size=20, color="black"),
        strip.text.x = element_text(size=20,
                                    face="bold")) +
  guides(colour = guide_legend(override.aes = list(),reverse = T, order = 1, nrow=2),
         shape = guide_legend(order=3),
         linetype = guide_legend(reverse = T, order = 2,nrow = 2)) +
  labs(x = "Beta Coefficient", linetype = "Effect", y = "") +
  facet_wrap(~name)

## Save
ggsave("./Figures/fig16_opinion.models.png", width = 23, height=18, units = "in", dpi = 400)
