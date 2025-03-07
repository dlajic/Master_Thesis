library(readr)
library(openxlsx)
library(dplyr)
library(kableExtra)


df.analysis <- read.xlsx("./data/df_analysis.xlsx")


table4 <- df.analysis %>% 
  group_by(new_id) %>% 
  summarise(days.active= sum(days.active)) %>% 
  summarise(Wave = "All",
            Median = median(days.active),
            Mean = mean(days.active),
            SD = sd(days.active),
            Min = min(days.active),
            Max = max(days.active)) %>% 
  bind_rows(.,df.analysis %>% 
              #filter(consumed_alt_overall==1) %>% 
              group_by(Wave =as.character(wave)) %>% 
              summarise(Median = median(days.active),
                        Mean = mean(days.active),
                        SD = sd(days.active),
                        Min = min(days.active),
                        Max = max(days.active))) %>% 
  kbl(format = "html",
      booktabs = T,label = "data_tracked.days",
      caption = "Descriptive Statistics on Active Days (Overall and Per Wave)",
      digits = 2) %>%
  kable_styling(position = "center")

save_kable(table4, file = "./Tables/table4.html")