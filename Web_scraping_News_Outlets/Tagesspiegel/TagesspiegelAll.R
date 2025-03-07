library(rvest)
library(dplyr)
library(jsonlite)
library(xlsx)
library(tidyverse) 
library(xml2)  
library(data.table)
library(openxlsx)
library(readxl)


day_start <- "2022/01/11"
day_end <- "2022/12/11"

day_seq <- seq(as.Date(day_start), as.Date(day_end), by = "day")
links <- data.frame("links" = NA)
mem <- data.frame("links" = NA)

for (i in as.character(day_seq)){

  html1 <- read_html(paste0("https://www.tagesspiegel.de/politik/archiv/", format(as.Date(i), '%Y/%m/%d')))
  html2 <- read_html(paste0("https://www.tagesspiegel.de/internationales/archiv/", format(as.Date(i), '%Y/%m/%d')))
  html3 <- read_html(paste0("https://www.tagesspiegel.de/gesellschaft/archiv/", format(as.Date(i), '%Y/%m/%d')))
  html4 <- read_html(paste0("https://www.tagesspiegel.de/wirtschaft/archiv/", format(as.Date(i), '%Y/%m/%d')))
  # html5 <- read_html(paste0("https://www.tagesspiegel.de/kultur/archiv/", format(as.Date(i), '%Y/%m/%d'))) -> contains opinions!!!!
  html6 <- read_html(paste0("https://www.tagesspiegel.de/wissen/archiv/", format(as.Date(i), '%Y/%m/%d')))
  html7 <- read_html(paste0("https://www.tagesspiegel.de/gesundheit/archiv/", format(as.Date(i), '%Y/%m/%d')))
  html8 <- read_html(paste0("https://www.tagesspiegel.de/sport/archiv/", format(as.Date(i), '%Y/%m/%d')))
  
  # filter out podcasts and premium articles
  mem1 <- html1 %>% 
    html_nodes(xpath = "//*[@id='main-content']/section/div/article[not(contains(., 'Fünf Minuten Berlin') or contains(., ' Tagesspiegel Plus ') or contains(., 'Checkpoint-Podcast') or contains(., 'eine-runde-berlin') or contains(., 'WM-Podcast') or contains(., 'podcast') or contains(., ' Eine Kolumne von ') or contains(., ' Ein Kommentar von ') or contains(., ' Ein Essay von '))]") %>%
    html_element('a') %>%
    html_attr("href") %>%
    as_tibble() %>%
    rename(links = value) %>%
    filter(!grepl("/lesermeinung/|/meinung/", links))
  
  
  mem2 <- html2 %>% 
    html_nodes(xpath = "//*[@id='main-content']/section/div/article[not(contains(., 'Fünf Minuten Berlin') or contains(., ' Tagesspiegel Plus ') or contains(., 'Checkpoint-Podcast') or contains(., 'eine-runde-berlin') or contains(., 'WM-Podcast') or contains(., 'podcast') or contains(., ' Eine Kolumne von ') or contains(., ' Ein Kommentar von ') or contains(., ' Ein Essay von '))]") %>%
    html_element('a') %>%
    html_attr("href") %>%
    as_tibble() %>%
    rename(links = value) %>%
    filter(!grepl("/lesermeinung/|/meinung/", links))
  
  mem3 <- html3 %>% 
    html_nodes(xpath = "//*[@id='main-content']/section/div/article[not(contains(., 'Fünf Minuten Berlin') or contains(., ' Tagesspiegel Plus ') or contains(., 'Checkpoint-Podcast') or contains(., 'eine-runde-berlin') or contains(., 'WM-Podcast') or contains(., 'podcast') or contains(., ' Eine Kolumne von ') or contains(., ' Ein Kommentar von ') or contains(., ' Ein Essay von '))]") %>%
    html_element('a') %>%
    html_attr("href") %>%
    as_tibble() %>%
    rename(links = value) %>%
    filter(!grepl("/lesermeinung/|/meinung/", links))
  
  mem4 <- html4 %>% 
    html_nodes(xpath = "//*[@id='main-content']/section/div/article[not(contains(., 'Fünf Minuten Berlin') or contains(., ' Tagesspiegel Plus ') or contains(., 'Checkpoint-Podcast') or contains(., 'eine-runde-berlin') or contains(., 'WM-Podcast') or contains(., 'podcast') or contains(., ' Eine Kolumne von ') or contains(., ' Ein Kommentar von ') or contains(., ' Ein Essay von '))]") %>%
    html_element('a') %>%
    html_attr("href") %>%
    as_tibble() %>%
    rename(links = value) %>%
    filter(!grepl("/lesermeinung/|/meinung/", links))
  
  #mem5 <- html5 %>% 
  #  html_nodes(xpath = "//*[@id='main-content']/div/section/article[not(contains(., 'Fünf Minuten Berlin') or contains(., ' Tagesspiegel Plus ') or contains(., 'Checkpoint-Podcast') or contains(., 'eine-runde-berlin') or contains(., 'WM-Podcast') or contains(., 'podcast') or contains(., ' Eine Kolumne von ') or contains(., ' Ein Kommentar von ') or contains(., ' Ein Essay von '))]") %>%
  #  html_element('a') %>%
  #  html_attr("href") %>%
  #  as_tibble() %>%
  #  rename(links = value) %>%
  #  filter(!grepl("/lesermeinung/|/meinung/", links))
  
  mem6 <- html6 %>% 
    html_nodes(xpath = "//*[@id='main-content']/section/div/article[not(contains(., 'Fünf Minuten Berlin') or contains(., ' Tagesspiegel Plus ') or contains(., 'Checkpoint-Podcast') or contains(., 'eine-runde-berlin') or contains(., 'WM-Podcast') or contains(., 'podcast') or contains(., ' Eine Kolumne von ') or contains(., ' Ein Kommentar von ') or contains(., ' Ein Essay von '))]") %>%
    html_element('a') %>%
    html_attr("href") %>%
    as_tibble() %>%
    rename(links = value) %>%
    filter(!grepl("/lesermeinung/|/meinung/", links))
  
  mem7 <- html7 %>% 
    html_nodes(xpath = "//*[@id='main-content']/section/div/article[not(contains(., 'Fünf Minuten Berlin') or contains(., ' Tagesspiegel Plus ') or contains(., 'Checkpoint-Podcast') or contains(., 'eine-runde-berlin') or contains(., 'WM-Podcast') or contains(., 'podcast') or contains(., ' Eine Kolumne von ') or contains(., ' Ein Kommentar von ') or contains(., ' Ein Essay von '))]") %>%
    html_element('a') %>%
    html_attr("href") %>%
    as_tibble() %>%
    rename(links = value) %>%
    filter(!grepl("/lesermeinung/|/meinung/", links))
  
  mem8 <- html8 %>% 
    html_nodes(xpath = "//*[@id='main-content']/section/div/article[not(contains(., 'Fünf Minuten Berlin') or contains(., ' Tagesspiegel Plus ') or contains(., 'Checkpoint-Podcast') or contains(., 'eine-runde-berlin') or contains(., 'WM-Podcast') or contains(., 'podcast') or contains(., ' Eine Kolumne von ') or contains(., ' Ein Kommentar von ') or contains(., ' Ein Essay von '))]") %>%
    html_element('a') %>%
    html_attr("href") %>%
    as_tibble() %>%
    rename(links = value) %>%
    filter(!grepl("/lesermeinung/|/meinung/", links))
  
  Sys.sleep(rnorm(1, 1, 0.1))
  
  links <- rbind(links, mem1, mem2, mem3, mem4, mem6, mem7, mem8)
  
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
    html_nodes("h1") %>%
    html_text2() 
}

get_lead_tagesspiegel <- function(html) {
  
  html %>%
    html_nodes("p.By") %>%
    html_text2() 
}

get_date_tagesspiegel <- function(html) {
  html %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
}


# create empty dataframes where results are written to
TagesspiegelAll <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA)
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
  
  TagesspiegelAll <- rbind(TagesspiegelAll, mem)
  
  print(i)
  #Sys.sleep(rnorm(1, 1, 0.1)^2)
}

# remove first row that is empty
TagesspiegelAll <- TagesspiegelAll  %>% slice(-1)

# assign links to scraped articles
TagesspiegelAll$link <- links$links

# remove empty rows
TagesspiegelAll <- TagesspiegelAll %>%
  filter(!is.na(Text))

# filter out remaining podcasts articles
TagesspiegelAll <- TagesspiegelAll %>%
  filter(!grepl("Tagesspiegel Plus|Podcast|Fünf Minuten:|Fünf Minute Berlin:|5 Minuten Berlin|Kolumne|Hotelkolumne|Sommerserie|Fragen des Tages:|Gastbeitrag|Schnelle Küche:|Warum ich E-Scooter fahre:|Berliner Intensivpfleger an der Corona-Front:", Title)) %>%
  filter(!grepl("Podcast|Ringbahnpodcast|Kolumne|Kommentar|Zeitschriftenkolumne|Essay|Eine Meinung|Leitartikel|Ein Gastbeitrag|Eine Kritik|Eine Rezension|Eine Analyse|eine Analyse|Eine Glosse|Unser Autor|Unsere Autorin|Ein Blick auf", Lead)) %>%
  filter(!grepl("Eine Kolumne von", Text)) %>%
  filter(!grepl("/literatur/|/kultur/", link))

# delete double entries
TagesspiegelAll <- TagesspiegelAll %>%
  distinct(Text, .keep_all = T)

# clean date column
TagesspiegelAll$Date <- as.Date(TagesspiegelAll$Date)

# Delete all rows which contains more than 32000 chars in $Text -> Ticker
count_chars <- function(x) {
  if (is.character(x)) {
    nchar(x)
  } else {
    0
  }
}

# Wenden Sie die Funktion auf die Textspalte des Dataframes an und prüfen Sie, ob die Anzahl der Zeichen in einer Zelle 32.000 überschreitet
long_cells <- sapply(TagesspiegelAll$Text, count_chars) > 32000

# Entfernen Sie die Zeilen, bei denen die Textspalte mehr als 32.000 Zeichen enthält
TagesspiegelAll <- TagesspiegelAll[!long_cells, ]


openxlsx::write.xlsx(TagesspiegelAll, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/Tagesspiegel/TagesspiegelAll.xlsx", rowNames = FALSE)
