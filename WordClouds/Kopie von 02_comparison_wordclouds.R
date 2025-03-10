library(pacman)

p_load(stringr,
       dplyr,
       tidyr,
       purrr,
       readxl,
       wordcloud,
       quanteda,
       quanteda.textstats,
       quanteda.textplots,
       quanteda.textmodels)


#### package comparison.cloud ####
# works by calculating for every word its relative frequency and then the size of each word is mapped to its maximum deviation

#### only for entities that appear in both sets (entity in conv AND alt) ####
# conv. vs. alt.
# PER
matched_per_comp_alt_conv_both_contained  <- data.frame(inner_join(per_words_conv, per_words_alt, by = c("words" = "words")))
colnames(matched_per_comp_alt_conv_both_contained) <-c("words", "count_conv", "count_alt")
rownames(matched_per_comp_alt_conv_both_contained) <- matched_per_comp_alt_conv_both_contained$words
matched_per_comp_alt_conv_both_contained$words <- NULL

# LOC
matched_loc_comp_alt_conv_both_contained  <- data.frame(inner_join(loc_words_conv, loc_words_alt, by = c("words" = "words")))
colnames(matched_loc_comp_alt_conv_both_contained) <-c("words", "count_conv", "count_alt")
rownames(matched_loc_comp_alt_conv_both_contained) <- matched_loc_comp_alt_conv_both_contained$words
matched_loc_comp_alt_conv_both_contained$words <- NULL

# ORG
matched_org_comp_alt_conv_both_contained  <- data.frame(inner_join(org_words_conv, org_words_alt, by = c("words" = "words")))
colnames(matched_org_comp_alt_conv_both_contained) <-c("words", "count_conv", "count_alt")
rownames(matched_org_comp_alt_conv_both_contained) <- matched_org_comp_alt_conv_both_contained$words
matched_org_comp_alt_conv_both_contained$words <- NULL

# MISC
matched_misc_comp_alt_conv_both_contained  <- data.frame(inner_join(misc_words_conv, misc_words_alt, by = c("words" = "words")))
colnames(matched_misc_comp_alt_conv_both_contained) <-c("words", "count_conv", "count_alt")
rownames(matched_misc_comp_alt_conv_both_contained) <- matched_misc_comp_alt_conv_both_contained$words
matched_misc_comp_alt_conv_both_contained$words <- NULL

# construct word clouds conv. vs. alt.
comparison.cloud(matched_per_comp_alt_conv_both_contained, max.words = 35, random.order= F, title.size = 1.5)
comparison.cloud(matched_loc_comp_alt_conv_both_contained, max.words = 30, random.order= F, title.size = 1.5)
comparison.cloud(matched_org_comp_alt_conv_both_contained, max.words = 30, random.order= F, title.size = 1.5)
comparison.cloud(matched_misc_comp_alt_conv_both_contained, max.words = 30, random.order= F, title.size = 1.5)




# opi. vs. normal
# PER
matched_per_comp_opi_desc_both_contained  <- data.frame(inner_join(per_words_desc, per_words_opi, by = c("words" = "words")))
colnames(matched_per_comp_opi_desc_both_contained) <-c("words", "count_desc", "count_opi")
rownames(matched_per_comp_opi_desc_both_contained) <- matched_per_comp_opi_desc_both_contained$words
matched_per_comp_opi_desc_both_contained$words <- NULL

# LOC
matched_loc_comp_opi_desc_both_contained  <- data.frame(inner_join(loc_words_desc, loc_words_opi, by = c("words" = "words")))
colnames(matched_loc_comp_opi_desc_both_contained) <-c("words", "count_desc", "count_opi")
rownames(matched_loc_comp_opi_desc_both_contained) <- matched_loc_comp_opi_desc_both_contained$words
matched_loc_comp_opi_desc_both_contained$words <- NULL

# ORG
matched_org_comp_opi_desc_both_contained  <- data.frame(inner_join(org_words_desc, org_words_opi, by = c("words" = "words")))
colnames(matched_org_comp_opi_desc_both_contained) <-c("words", "count_desc", "count_opi")
rownames(matched_org_comp_opi_desc_both_contained) <- matched_org_comp_opi_desc_both_contained$words
matched_org_comp_opi_desc_both_contained$words <- NULL

# MISC
matched_misc_comp_opi_desc_both_contained  <- data.frame(inner_join(misc_words_desc, misc_words_opi, by = c("words" = "words")))
colnames(matched_misc_comp_opi_desc_both_contained) <-c("words", "count_desc", "count_opi")
rownames(matched_misc_comp_opi_desc_both_contained) <- matched_misc_comp_opi_desc_both_contained$words
matched_misc_comp_opi_desc_both_contained$words <- NULL

# construct word clouds desc. vs. opi.
comparison.cloud(matched_per_comp_opi_desc_both_contained, max.words = 40, random.order= F,  title.size = 1)
comparison.cloud(matched_loc_comp_opi_desc_both_contained, max.words = 40, random.order= F,  title.size = 1)
comparison.cloud(matched_org_comp_opi_desc_both_contained, max.words = 40, random.order= F,  title.size = 1)
comparison.cloud(matched_misc_comp_opi_desc_both_contained, max.words = 40, random.order= F, title.size = 1)




#### all entities (in conv OR alt) ####
# conv. vs. alt.
# PER
matched_per_comp_alt_conv_all_contained  <- data.frame(full_join(per_words_conv, per_words_alt, by = c("words" = "words")))
colnames(matched_per_comp_alt_conv_all_contained) <-c("words", "count_conv", "count_alt")
rownames(matched_per_comp_alt_conv_all_contained) <- matched_per_comp_alt_conv_all_contained$words
matched_per_comp_alt_conv_all_contained$words <- NULL
matched_per_comp_alt_conv_all_contained[is.na(matched_per_comp_alt_conv_all_contained)] <- 0

# LOC
matched_loc_comp_alt_conv_all_contained  <- data.frame(full_join(loc_words_conv, loc_words_alt, by = c("words" = "words")))
colnames(matched_loc_comp_alt_conv_all_contained) <-c("words", "count_conv", "count_alt")
rownames(matched_loc_comp_alt_conv_all_contained) <- matched_loc_comp_alt_conv_all_contained$words
matched_loc_comp_alt_conv_all_contained$words <- NULL
matched_loc_comp_alt_conv_all_contained[is.na(matched_loc_comp_alt_conv_all_contained)] <- 0

# ORG
matched_org_comp_alt_conv_all_contained  <- data.frame(full_join(org_words_conv, org_words_alt, by = c("words" = "words")))
colnames(matched_org_comp_alt_conv_all_contained) <-c("words", "count_conv", "count_alt")
rownames(matched_org_comp_alt_conv_all_contained) <- matched_org_comp_alt_conv_all_contained$words
matched_org_comp_alt_conv_all_contained$words <- NULL
matched_org_comp_alt_conv_all_contained[is.na(matched_org_comp_alt_conv_all_contained)] <- 0

# MISC
matched_misc_comp_alt_conv_all_contained  <- data.frame(full_join(misc_words_conv, misc_words_alt, by = c("words" = "words")))
colnames(matched_misc_comp_alt_conv_all_contained) <-c("words", "count_conv", "count_alt")
rownames(matched_misc_comp_alt_conv_all_contained) <- matched_misc_comp_alt_conv_all_contained$words
matched_misc_comp_alt_conv_all_contained$words <- NULL
matched_misc_comp_alt_conv_all_contained[is.na(matched_misc_comp_alt_conv_all_contained)] <- 0

# construct word clouds conv. vs. alt.
comparison.cloud(matched_per_comp_alt_conv_all_contained, max.words = 40, random.order= F, title.size = 1.5)
comparison.cloud(matched_loc_comp_alt_conv_all_contained, max.words = 40, random.order= F, title.size = 1.5)
comparison.cloud(matched_org_comp_alt_conv_all_contained, max.words = 40, random.order= F, title.size = 1.5)
comparison.cloud(matched_misc_comp_alt_conv_all_contained, max.words = 40, random.order= F, title.size = 1.5)




# opi. vs. normal
# PER
matched_per_comp_opi_desc_all_contained  <- data.frame(full_join(per_words_desc, per_words_opi, by = c("words" = "words")))
colnames(matched_per_comp_opi_desc_all_contained) <-c("words", "count_desc", "count_opi")
rownames(matched_per_comp_opi_desc_all_contained) <- matched_per_comp_opi_desc_all_contained$words
matched_per_comp_opi_desc_all_contained$words <- NULL
matched_per_comp_opi_desc_all_contained[is.na(matched_per_comp_opi_desc_all_contained)] <- 0

# LOC
matched_loc_comp_opi_desc_all_contained  <- data.frame(full_join(loc_words_desc, loc_words_opi, by = c("words" = "words")))
colnames(matched_loc_comp_opi_desc_all_contained) <-c("words", "count_desc", "count_opi")
rownames(matched_loc_comp_opi_desc_all_contained) <- matched_loc_comp_opi_desc_all_contained$words
matched_loc_comp_opi_desc_all_contained$words <- NULL
matched_per_comp_opi_desc_all_contained[is.na(matched_per_comp_opi_desc_all_contained)] <- 0

# ORG
matched_org_comp_opi_desc_all_contained  <- data.frame(full_join(org_words_desc, org_words_opi, by = c("words" = "words")))
colnames(matched_org_comp_opi_desc_all_contained) <-c("words", "count_desc", "count_opi")
rownames(matched_org_comp_opi_desc_all_contained) <- matched_org_comp_opi_desc_all_contained$words
matched_org_comp_opi_desc_all_contained$words <- NULL
matched_per_comp_opi_desc_all_contained[is.na(matched_per_comp_opi_desc_all_contained)] <- 0

# MISC
matched_misc_comp_opi_desc_all_contained  <- data.frame(full_join(misc_words_desc, misc_words_opi, by = c("words" = "words")))
colnames(matched_misc_comp_opi_desc_all_contained) <-c("words", "count_desc", "count_opi")
rownames(matched_misc_comp_opi_desc_all_contained) <- matched_misc_comp_opi_desc_all_contained$words
matched_misc_comp_opi_desc_all_contained$words <- NULL
matched_per_comp_opi_desc_all_contained[is.na(matched_per_comp_opi_desc_all_contained)] <- 0

# construct word clouds desc. vs. opi.
comparison.cloud(matched_per_comp_opi_desc_all_contained, max.words = 40, random.order= F, title.size = 1.5)
comparison.cloud(matched_loc_comp_opi_desc_all_contained, max.words = 40, random.order= F, title.size = 1.5)
comparison.cloud(matched_org_comp_opi_desc_all_contained, max.words = 40, random.order= F, title.size = 1.5)
comparison.cloud(matched_misc_comp_opi_desc_all_contained, max.words = 40, random.order= F, title.size = 1.5)




#### package quanteda:textplots -> function: textplot_wordcloud ####
# works like the previous package but has some additional commands 

#### only for entities that appear in both sets (entity in conv AND alt) ####
# conv. vs. alt.
# PER
list_to_df_alt_PER <- list_to_df_alt %>% filter(entity == "PER") %>% mutate(set = "Alternative news")
list_to_df_conv_PER <- list_to_df_conv %>% filter(entity == "PER") %>% mutate(set = "Conventional news")

# LOC
list_to_df_alt_LOC <- list_to_df_alt %>% filter(entity == "LOC") %>% mutate(set = "Alternative news")
list_to_df_conv_LOC <- list_to_df_conv %>% filter(entity == "LOC") %>% mutate(set = "Conventional news")

# ORG
list_to_df_alt_ORG <- list_to_df_alt %>% filter(entity == "ORG") %>% mutate(set = "Alternative news")
list_to_df_conv_ORG <- list_to_df_conv %>% filter(entity == "ORG") %>% mutate(set = "Conventional news")

# MISC
list_to_df_alt_MISC <- list_to_df_alt %>% filter(entity == "MISC") %>% mutate(set = "Alternative news")
list_to_df_conv_MISC <- list_to_df_conv %>% filter(entity == "MISC") %>% mutate(set = "Conventional news")

merged_alt_conv_PER <- bind_rows(list_to_df_alt_PER, list_to_df_conv_PER)
merged_alt_conv_LOC <- bind_rows(list_to_df_alt_LOC, list_to_df_conv_LOC)
merged_alt_conv_ORG <- bind_rows(list_to_df_alt_ORG, list_to_df_conv_ORG)
merged_alt_conv_MISC <- bind_rows(list_to_df_alt_MISC, list_to_df_conv_MISC)

merged2_alt_conv_PER <- merged_alt_conv_PER %>% group_by(words) %>% filter(all(c("Alternative news", "Conventional news") %in% set)) %>% ungroup()
merged2_alt_conv_LOC <- merged_alt_conv_LOC %>% group_by(words) %>% filter(all(c("Alternative news", "Conventional news") %in% set)) %>% ungroup()
merged2_alt_conv_ORG <- merged_alt_conv_ORG %>% group_by(words) %>% filter(all(c("Alternative news", "Conventional news") %in% set)) %>% ungroup()
merged2_alt_conv_MISC <- merged_alt_conv_MISC %>% group_by(words) %>% filter(all(c("Alternative news", "Conventional news") %in% set)) %>% ungroup()

# construct a corpus-object from the datasets
corpus_alt_conv_PER <- corpus(merged2_alt_conv_PER$words, docvars = merged2_alt_conv_PER$set)
corpus_alt_conv_LOC <- corpus(merged2_alt_conv_LOC$words, docvars = merged2_alt_conv_LOC$set)
corpus_alt_conv_ORG <- corpus(merged2_alt_conv_ORG$words, docvars = merged2_alt_conv_ORG$set)
corpus_alt_conv_MISC <- corpus(merged2_alt_conv_MISC$words, docvars = merged2_alt_conv_MISC$set)

# construct a Document-Term-Matrix (dfm) from the corpus
dfm_alt_conv_PER <- tokens(corpus_alt_conv_PER, what = "sentence", include_docvars = TRUE) %>%
  dfm(tolower = FALSE) %>%
  dfm_group(groups = docvars)

dfm_alt_conv_LOC <- tokens(corpus_alt_conv_LOC, what = "sentence", include_docvars = TRUE) %>%
  dfm(tolower = FALSE) %>%
  dfm_group(groups = docvars)

dfm_alt_conv_ORG <- tokens(corpus_alt_conv_ORG, what = "sentence", include_docvars = TRUE) %>%
  dfm(tolower = FALSE) %>%
  dfm_group(groups = docvars)

dfm_alt_conv_MISC <- tokens(corpus_alt_conv_MISC, what = "sentence", include_docvars = TRUE) %>%
  dfm(tolower = FALSE) %>%
  dfm_group(groups = docvars)


textplot_wordcloud(dfm_alt_conv_PER, 
                   max_words = 30,
                   #min_count = 50, # !!! filters data and calculates rel. frequency based on the filtered data
                   comparison = TRUE,  
                   color = c('red', 'blue'), 
                   min_size = 1,
                   max_size = 4, 
                   random_order = F)

textplot_wordcloud(dfm_alt_conv_LOC, 
                   max_words = 30,
                   #min_count = 50, # !!! filters data and calculates rel. frequency based on the filtered data
                   comparison = TRUE,  
                   color = c('red', 'blue'), 
                   min_size = 1,
                   max_size = 4, 
                   random_order = F)

textplot_wordcloud(dfm_alt_conv_ORG, 
                   max_words = 30,
                   #min_count = 50, # !!! filters data and calculates rel. frequency based on the filtered data
                   comparison = TRUE,  
                   color = c('red', 'blue'), 
                   min_size = 1,
                   max_size = 4, 
                   random_order = F)

textplot_wordcloud(dfm_alt_conv_MISC, 
                   max_words = 30,
                   #min_count = 50, # !!! filters data and calculates rel. frequency based on the filtered data
                   comparison = TRUE,  
                   color = c('red', 'blue'), 
                   min_size = 1,
                   max_size = 4, 
                   random_order = F)



# opi. vs. desc.
# PER
list_to_df_opi_PER <- list_to_df_opi %>% filter(entity == "PER") %>% mutate(set = "Opinion news")
list_to_df_desc_PER <- list_to_df_desc %>% filter(entity == "PER") %>% mutate(set = "Descriptive news")

# LOC
list_to_df_opi_LOC <- list_to_df_opi %>% filter(entity == "LOC") %>% mutate(set = "Opinion news")
list_to_df_desc_LOC <- list_to_df_desc %>% filter(entity == "LOC") %>% mutate(set = "Descriptive news")

# ORG
list_to_df_opi_ORG <- list_to_df_opi %>% filter(entity == "ORG") %>% mutate(set = "Opinion news")
list_to_df_desc_ORG <- list_to_df_desc %>% filter(entity == "ORG") %>% mutate(set = "Descriptive news")

# MISC
list_to_df_opi_MISC <- list_to_df_opi %>% filter(entity == "MISC") %>% mutate(set = "Opinion news")
list_to_df_desc_MISC <- list_to_df_desc %>% filter(entity == "MISC") %>% mutate(set = "Descriptive news")

merged_opi_desc_PER <- bind_rows(list_to_df_opi_PER, list_to_df_desc_PER)
merged_opi_desc_LOC <- bind_rows(list_to_df_opi_LOC, list_to_df_desc_LOC)
merged_opi_desc_ORG <- bind_rows(list_to_df_opi_ORG, list_to_df_desc_ORG)
merged_opi_desc_MISC <- bind_rows(list_to_df_opi_MISC, list_to_df_desc_MISC)

merged2_opi_desc_PER <- merged_opi_desc_PER %>% group_by(words) %>% filter(all(c("Opinion news", "Descriptive news") %in% set)) %>% ungroup()
merged2_opi_desc_LOC <- merged_opi_desc_LOC %>% group_by(words) %>% filter(all(c("Opinion news", "Descriptive news") %in% set)) %>% ungroup()
merged2_opi_desc_ORG <- merged_opi_desc_ORG %>% group_by(words) %>% filter(all(c("Opinion news", "Descriptive news") %in% set)) %>% ungroup()
merged2_opi_desc_MISC <- merged_opi_desc_MISC %>% group_by(words) %>% filter(all(c("Opinion news", "Descriptive news") %in% set)) %>% ungroup()

# construct a corpus-object from the datasets
corpus_opi_desc_PER <- corpus(merged2_opi_desc_PER$words, docvars = merged2_opi_desc_PER$set)
corpus_opi_desc_LOC <- corpus(merged2_opi_desc_LOC$words, docvars = merged2_opi_desc_LOC$set)
corpus_opi_desc_ORG <- corpus(merged2_opi_desc_ORG$words, docvars = merged2_opi_desc_ORG$set)
corpus_opi_desc_MISC <- corpus(merged2_opi_desc_MISC$words, docvars = merged2_opi_desc_MISC$set)

# construct a Document-Term-Matrix (dfm) from the corpus
dfm_opi_desc_PER <- tokens(corpus_opi_desc_PER, what = "sentence", include_docvars = TRUE) %>%
  dfm(tolower = FALSE) %>%
  dfm_group(groups = docvars)

dfm_opi_desc_LOC <- tokens(corpus_opi_desc_LOC, what = "sentence", include_docvars = TRUE) %>%
  dfm(tolower = FALSE) %>%
  dfm_group(groups = docvars)

dfm_opi_desc_ORG <- tokens(corpus_opi_desc_ORG, what = "sentence", include_docvars = TRUE) %>%
  dfm(tolower = FALSE) %>%
  dfm_group(groups = docvars)

dfm_opi_desc_MISC <- tokens(corpus_opi_desc_MISC, what = "sentence", include_docvars = TRUE) %>%
  dfm(tolower = FALSE) %>%
  dfm_group(groups = docvars)


textplot_wordcloud(dfm_opi_desc_PER[c(2, 1), ], # change order, since dfm object uses alphabetical order 
                   max_words = 30,
                   #min_count = 50, # !!! filters data and calculates rel. frequency based on the filtered data
                   comparison = TRUE,  
                   color = c('red', 'blue'), 
                   min_size = 1,
                   max_size = 4, 
                   random_order = F)

textplot_wordcloud(dfm_opi_desc_LOC[c(2, 1), ], # change order, since dfm object uses alphabetical order  
                   max_words = 30,
                   #min_count = 50, # !!! filters data and calculates rel. frequency based on the filtered data
                   comparison = TRUE,  
                   color = c('red', 'blue'), 
                   min_size = 1,
                   max_size = 4,
                   random_order = F)

textplot_wordcloud(dfm_opi_desc_ORG[c(2, 1), ], # change order, since dfm object uses alphabetical order  
                   max_words = 30,
                   #min_count = 50, # !!! filters data and calculates rel. frequency based on the filtered data
                   comparison = TRUE,  
                   color = c('red', 'blue'), 
                   min_size = 1,
                   max_size = 4, 
                   random_order = F)

textplot_wordcloud(dfm_opi_desc_MISC[c(2, 1), ], # change order, since dfm object uses alphabetical order  
                   max_words = 30,
                   #min_count = 50, # !!! filters data and calculates rel. frequency based on the filtered data
                   comparison = TRUE,  
                   color = c('red', 'blue'), 
                   min_size = 1,
                   max_size = 4, 
                   random_order = F)




#### all entities (in conv OR alt) ####

# conv. vs. alt.

merged2_alt_conv_PER_all <- merged_alt_conv_PER 
merged2_alt_conv_LOC_all <- merged_alt_conv_LOC 
merged2_alt_conv_ORG_all <- merged_alt_conv_ORG 
merged2_alt_conv_MISC_all <- merged_alt_conv_MISC

# construct a corpus-object from the datasets
corpus_alt_conv_PER_all <- corpus(merged2_alt_conv_PER_all$words, docvars = merged2_alt_conv_PER_all$set)
corpus_alt_conv_LOC_all <- corpus(merged2_alt_conv_LOC_all$words, docvars = merged2_alt_conv_LOC_all$set)
corpus_alt_conv_ORG_all <- corpus(merged2_alt_conv_ORG_all$words, docvars = merged2_alt_conv_ORG_all$set)
corpus_alt_conv_MISC_all <- corpus(merged2_alt_conv_MISC_all$words, docvars = merged2_alt_conv_MISC_all$set)

# construct a Document-Term-Matrix (dfm) from the corpus
dfm_alt_conv_PER_all <- tokens(corpus_alt_conv_PER_all, what = "sentence", include_docvars = TRUE) %>%
  dfm(tolower = FALSE) %>%
  dfm_group(groups = docvars)

dfm_alt_conv_LOC_all <- tokens(corpus_alt_conv_LOC_all, what = "sentence", include_docvars = TRUE) %>%
  dfm(tolower = FALSE) %>%
  dfm_group(groups = docvars)

dfm_alt_conv_ORG_all <- tokens(corpus_alt_conv_ORG_all, what = "sentence", include_docvars = TRUE) %>%
  dfm(tolower = FALSE) %>%
  dfm_group(groups = docvars)

dfm_alt_conv_MISC_all <- tokens(corpus_alt_conv_MISC_all, what = "sentence", include_docvars = TRUE) %>%
  dfm(tolower = FALSE) %>%
  dfm_group(groups = docvars)


textplot_wordcloud(dfm_alt_conv_PER_all, 
                   max_words = 30,
                   #min_count = 50, # !!! filters data and calculates rel. frequency based on the filtered data
                   comparison = TRUE,  
                   color = c('red', 'blue'), 
                   min_size = 1,
                   max_size = 4, 
                   random_order = F)

textplot_wordcloud(dfm_alt_conv_LOC_all, 
                   max_words = 40,
                   #min_count = 50, # !!! filters data and calculates rel. frequency based on the filtered data
                   comparison = TRUE,  
                   color = c('red', 'blue'), 
                   min_size = 1,
                   max_size = 4, 
                   random_order = F)

textplot_wordcloud(dfm_alt_conv_ORG_all, 
                   max_words = 40,
                   #min_count = 50, # !!! filters data and calculates rel. frequency based on the filtered data
                   comparison = TRUE,  
                   color = c('red', 'blue'), 
                   min_size = 1,
                   max_size = 4, 
                   random_order = F)

textplot_wordcloud(dfm_alt_conv_MISC_all, 
                   max_words = 40,
                   #min_count = 50, # !!! filters data and calculates rel. frequency based on the filtered data
                   comparison = TRUE,  
                   color = c('red', 'blue'), 
                   min_size = 1,
                   max_size = 4, 
                   random_order = F)



# opi. vs. desc.
merged2_opi_desc_PER_all <- merged_opi_desc_PER 
merged2_opi_desc_LOC_all <- merged_opi_desc_LOC
merged2_opi_desc_ORG_all <- merged_opi_desc_ORG 
merged2_opi_desc_MISC_all <- merged_opi_desc_MISC 

# construct a corpus-object from the datasets
corpus_opi_desc_PER_all <- corpus(merged2_opi_desc_PER_all$words, docvars = merged2_opi_desc_PER_all$set)
corpus_opi_desc_LOC_all <- corpus(merged2_opi_desc_LOC_all$words, docvars = merged2_opi_desc_LOC_all$set)
corpus_opi_desc_ORG_all <- corpus(merged2_opi_desc_ORG_all$words, docvars = merged2_opi_desc_ORG_all$set)
corpus_opi_desc_MISC_all <- corpus(merged2_opi_desc_MISC_all$words, docvars = merged2_opi_desc_MISC_all$set)

# construct a Document-Term-Matrix (dfm) from the corpus
dfm_opi_desc_PER_all <- tokens(corpus_opi_desc_PER_all, what = "sentence", include_docvars = TRUE) %>%
  dfm(tolower = FALSE) %>%
  dfm_group(groups = docvars)

dfm_opi_desc_LOC_all <- tokens(corpus_opi_desc_LOC_all, what = "sentence", include_docvars = TRUE) %>%
  dfm(tolower = FALSE) %>%
  dfm_group(groups = docvars)

dfm_opi_desc_ORG_all <- tokens(corpus_opi_desc_ORG_all, what = "sentence", include_docvars = TRUE) %>%
  dfm(tolower = FALSE) %>%
  dfm_group(groups = docvars)

dfm_opi_desc_MISC_all <- tokens(corpus_opi_desc_MISC_all, what = "sentence", include_docvars = TRUE) %>%
  dfm(tolower = FALSE) %>%
  dfm_group(groups = docvars)


textplot_wordcloud(dfm_opi_desc_PER_all[c(2, 1), ], # change order, since dfm object uses alphabetical order , 
                   max_words = 40,
                   #min_count = 50, # !!! filters data and calculates rel. frequency based on the filtered data
                   comparison = TRUE,  
                   color = c('red', 'blue'), 
                   min_size = 1,
                   max_size = 4, 
                   random_order = F)

textplot_wordcloud(dfm_opi_desc_LOC_all[c(2, 1), ], # change order, since dfm object uses alphabetical order , 
                   max_words = 40,
                   #min_count = 50, # !!! filters data and calculates rel. frequency based on the filtered data
                   comparison = TRUE,  
                   color = c('red', 'blue'), 
                   min_size = 1,
                   max_size = 4, 
                   random_order = F)

textplot_wordcloud(dfm_opi_desc_ORG_all[c(2, 1), ], # change order, since dfm object uses alphabetical order , 
                   max_words = 40,
                   #min_count = 50, # !!! filters data and calculates rel. frequency based on the filtered data
                   comparison = TRUE,  
                   color = c('red', 'blue'), 
                   min_size = 1,
                   max_size = 4, 
                   random_order = F)

textplot_wordcloud(dfm_opi_desc_MISC_all[c(2, 1), ], # change order, since dfm object uses alphabetical order , 
                   max_words = 40,
                   #min_count = 50, # !!! filters data and calculates rel. frequency based on the filtered data
                   comparison = TRUE,  
                   color = c('red', 'blue'), 
                   min_size = 1,
                   max_size = 4, 
                   random_order = F)




matched_per_comp_alt_conv_all_contained$row_names <- row.names(matched_per_comp_alt_conv_all_contained)
matched_org_comp_alt_conv_all_contained$row_names <- row.names(matched_org_comp_alt_conv_all_contained)  
matched_misc_alt_conv_all_contained$row_names <- row.names(matched_misc_comp_alt_conv_all_contained)  


