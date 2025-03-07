library(rvest)
library(dplyr)
library(jsonlite)
library(xlsx)
library(tidyverse) 
library(xml2)  
library(data.table)

day_start <- "2017/01/01"
day_end <- "2023/05/08"

day_seq <- seq(as.Date(day_start), as.Date(day_end), by = "day")
links <- data.frame("links" = NA)
mem <- data.frame("links" = NA)

for (i in as.character(day_seq)){
  
  html <- read_html(paste0("https://www.tagesspiegel.de/meinung/archiv/", format(as.Date(i), '%Y/%m/%d')))

  # filter out podcasts and premium articles
  mem <- html %>% 
    html_nodes(xpath = "//*[@id='main-content']/div/section/article[not(contains(., 'Fünf Minuten Berlin') or contains(., ' Tagesspiegel Plus ') or contains(., 'Checkpoint-Podcast') or contains(., 'eine-runde-berlin') or contains(., 'WM-Podcast') or contains(., 'podcast'))]") %>%
    html_element('a') %>%
    html_attr("href") %>%
    as_tibble() %>%
    rename(links = value) %>%
    filter(!grepl("/lesermeinung/", links))
  
  links <- rbind(links, mem)
  
  print(i)
}


# delete empty row
links <- links %>% slice(-1)

# delete duplicate entries (website contains for some dates all articles of a month)
links <- links %>%
  distinct()

# complete the url
for (t in 1:nrow(links)){
  
  links$links[t] <- paste0("https://www.tagesspiegel.de", links$links[t])
  
} 



get_text_tagesspiegel <- function(html) {
  
  html %>%
    html_nodes("#story-elements > p") %>%
    html_text2() %>%
    str_c(collapse = " ")
  
}

get_title_tagesspiegel <- function(html) {
  
  html %>%
    html_nodes(".Up") %>%
    html_text2() 
}

get_lead_tagesspiegel <- function(html) {
  
  html %>%
    html_nodes(".Us") %>%
    html_text2() 
}

get_date_tagesspiegel <- function(html) {
  html %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
}



# create empty dataframes where results are written to
TagesspiegelOpi <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA)
mem <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA)


for(i in links$links){
  
  html <- tryCatch(read_html(i),
                   error = function(e) {NA})
  # get headline of article
  mem$Title <- ifelse(is.na(html), NA_character_, get_title_tagesspiegel(html)) 
  
  # get publishing date
  mem$Date <- ifelse(is.na(html), NA_character_, get_date_tagesspiegel(html))
  
  mem$Lead <- ifelse(is.na(html), NA_character_, get_lead_tagesspiegel(html))
  
  mem$Text <- ifelse(is.na(html), NA_character_, get_text_tagesspiegel(html))
  
  TagesspiegelOpi <- rbind(TagesspiegelOpi, mem)
  
  print(i)
  #Sys.sleep(rnorm(1, 1, 0.2)^2)
}

# remove first row that is empty
TagesspiegelOpi <- TagesspiegelOpi  %>% slice(-1)

# assign links to scraped articles
TagesspiegelOpi$link <- links$links

# remove empty rows
TagesspiegelOpi <- na.omit(TagesspiegelOpi)

# filter out remaining podcasts articles
TagesspiegelOpi <- TagesspiegelOpi %>%
  filter(!grepl("Podcast|Fünf Minuten:|Fünf Minute Berlin:|5 Minuten Berlin", Title)) %>%
  filter(!grepl("Podcast|Ringbahnpodcast", Lead)) %>%
  distinct(Title, .keep_all = T)
  

# clean date column
TagesspiegelOpi$Date <- as.Date(TagesspiegelOpi$Date)


write.xlsx(TagesspiegelOpi, file= "C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/Scraping Tagesspiegel/TagesspiegelOpi.xlsx", rowNames = FALSE)



