library(rvest)
library(dplyr)
library(jsonlite)
library(xlsx)
library(tidyverse) 
library(xml2)  
library(data.table)


page <- seq(1, 100, 1)
links_all <- data.frame("links" = NA)
links_all_opi <- data.frame("links" = NA)
mem1 <- data.frame("links" = NA)
mem2 <- data.frame("links" = NA)

for (i in page){
  
  #html <- read_html(paste0("https://www.sueddeutsche.de/news/page/", i, "?search=&sort=date&all%5B%5D=dep&typ%5B%5D=article&sys%5B%5D=sz&sys%5B%5D=dpa&catsz%5B%5D=szOverviewPageThemes&catdpa%5B%5D=dpaImportant&time=2021-07-01T00%3A00%2F2021-08-31T23%3A59&startDate=01.07.2021&endDate=15.08.2021"))
  html <- read_html(paste0("https://www.sueddeutsche.de/news/page/", i, "?search=&sort=date&all%5B%5D=dep&typ%5B%5D=article&sys%5B%5D=sz&sys%5B%5D=dpa&catsz%5B%5D=szOverviewPageThemes&catdpa%5B%5D=dpaImportant&time=2022-07-01T00%3A00%2F2022-08-31T23%3A59&startDate=01.07.2022&endDate=30.08.2022"))
  
  
  # filter out podcasts and premium articles
  all_plus_articles <- html %>% 
    html_nodes("p.entrylist__detail.detailed-information") %>%
    html_element("img") %>%
    html_attr("alt") %>%
    as_tibble() %>%
    rename(SZplus = value)
    
  all_articles <- html %>%
    html_nodes("p.entrylist__detail.detailed-information") %>%
    html_attr("data-clickurl") %>%
    as_tibble() %>%
    rename(links = value)
  
  all_plus_articles_opi <- html %>% 
    html_nodes("p.entrylist__detail.detailed-information:contains(Meinung)") %>%
    html_element("img") %>%
    html_attr("alt") %>%
    as_tibble() %>%
    rename(SZplus = value)
  
  all_articles_without_opi <- html %>%
    html_nodes("p.entrylist__detail.detailed-information:contains(Meinung)") %>%
    html_attr("data-clickurl") %>%
    as_tibble() %>%
    rename(links = value)
  
  mem1 <- cbind(all_articles, all_plus_articles)
  mem2 <- cbind(all_articles_without_opi, all_plus_articles_opi)
  
  mem1 <- mem1 %>%
    filter(!(grepl("SZplus", SZplus))) %>%
    select(links)
  
  mem2 <- mem2 %>%
    filter(!(grepl("SZplus", SZplus))) %>%
    select(links)
  
  links_all <- rbind(links_all, mem1)
  links_all_opi <- rbind(links_all_opi, mem2)
  
  print(i)
}


# delete empty row
links_all <- links_all %>% slice(-1)
links_all_opi <- links_all_opi %>% slice(-1)

# delete duplicate entries (website contains for some dates all articles of a month)
links_all <- links_all %>%
  distinct()


links1 <- links_all
links2 <- links_all

links_all <- rbind(links1, links2)



get_text_sz <- function(html) {
  
  html %>%
    html_nodes(".css-13wylk3") %>%
    html_text2() %>%
    str_c(collapse = " ")
  
}

get_title1_sz <- function(html) {
  
  html %>%
    html_nodes(".css-1bhnxuf") %>%
    html_text2() 
}

get_title2_sz <- function(html) {
  
  html %>%
    html_nodes(".css-1tm5due") %>%
    html_text2() 
}

get_lead_sz <- function(html) {
  
  html %>%
    html_nodes(".css-1485smx") %>%
    html_text2() 
}

get_date_sz <- function(html) {
  html %>% 
    html_nodes(".css-1r5gb7q") %>% 
    html_attr("datetime")
}



# create empty dataframes where results are written to
szAll <- data.frame("Date" = NA, "Title1" = NA, "Title2" = NA, "Text" = NA, "Lead" = NA)
mem <- data.frame("Date" = NA, "Title1" = NA, "Title2" = NA, "Text" = NA, "Lead" = NA)


for(i in links_all$links){
  
  html <- tryCatch(read_html(i),
                   error = function(e) {NA})
  # get headline of article
  mem$Title1 <- ifelse(is.na(html), NA_character_, get_title1_sz(html))
  
  mem$Title2 <- ifelse(is.na(html), NA_character_, get_title2_sz(html)) 
  
  # get publishing date
  mem$Date <- ifelse(is.na(html), NA_character_, get_date_sz(html))
  
  mem$Lead <- ifelse(is.na(html), NA_character_, get_lead_sz(html))
  
  mem$Text <- ifelse(is.na(html), NA_character_, get_text_sz(html))
  
  szAll <- rbind(szAll, mem)
  
  print(i)
  #Sys.sleep(rnorm(1, 1, 0.1)^2)
}


# remove first row that is empty
szAll <- szAll  %>% slice(-1)

# assign links to scraped articles
szAll$link <- links_all$links

# filter out news blogs
szAll <- szAll %>%
  filter(!grepl("Liveblog|Newsblog|SZ am Morgen|SZ am Abend", Title1)) %>%
  filter(!grepl("Liveblog|Newsblog|SZ am Morgen|SZ am Abend", Title2))

# count words in articles and delete articles containing less or equal than 100 words
szAll <- szAll %>%
  mutate(word_count = str_count(Text, "\\w+"))

szAll <- szAll %>%
  filter(word_count >= 50)

#djdjdjdjdjdjjdjd
szAll1 <- szAll

# split dataset into articles with opinion articles and no climate articles and without opinion articles
szAll_opi <- szAll %>%
  filter(!(.$link %in% links_all_opi$links)) %>%
  filter(!grepl("(?i)Kolumne|\\bkolumne\\b", Title1)) %>%
  filter(!grepl("(?i)Kolumne|\\bkolumne\\b|Kommentar|Glosse", Title2)) %>%
  filter(!grepl("rezension|kolumne|kommentar|filmessay|glosse|/meinung/", link)) %>%
  distinct(Title1, .keep_all = T) %>%
  distinct(Text, .keep_all = T) %>%
  na.omit()
  
szMig <- read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/SZ/szMig.xlsx")

# filter climate articles out of data 
szAll_mig <- szAll %>%
  filter(!(.$link %in% szMig$link)) %>%
  distinct(Title1, .keep_all = T) %>%
  distinct(Text, .keep_all = T) %>%
  na.omit() 


# clean date column
szAll_opi$Date <- as.Date(szAll__opi$Date)
szAll_mig$Date <- as.Date(szAll_mig$Date)

# take sample 
szAll_opi <- sample_n(szAll_opi, 2161)
szAll_mig  <- sample_n(szAll_mig, 3996)

openxlsx::write.xlsx(szAll_opi, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/SZ/szAll_opi.xlsx", rowNames = FALSE)
openxlsx::write.xlsx(szAll_mig, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/SZ/szAll_mig.xlsx", rowNames = FALSE)



