library(rvest)
library(dplyr)
library(jsonlite)
library(xlsx)
library(tidyverse) 
library(xml2)  
library(data.table)


page <- seq(1, 190, 1)
links <- data.frame("links" = NA)
mem <- data.frame("links" = NA)

for (i in page){
  
  html <- read_html(paste0("https://www.tagesspiegel.de/politik/themen/fluchtlinge/", i))
  
  # filter out podcasts and premium articles
  mem <- html %>% 
    html_nodes(".AKf5:not(:contains('Plus')):not(:contains('Gradmesser')):not(:contains('Podcast')):not(:contains('Potsdam Heute')):not(:contains('Live-Stream'))") %>%
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
TagesspiegelMig <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA)
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
  
  TagesspiegelMig <- rbind(TagesspiegelMig, mem)
  
  print(i)
  #Sys.sleep(rnorm(1, 1, 0.1)^2)
}

# remove first row that is empty
TagesspiegelMig <- TagesspiegelMig  %>% slice(-1)

# assign links to scraped articles
TagesspiegelMig$link <- links$links

# filter out remaining podcast articles
TagesspiegelMig <- TagesspiegelMig %>%
  filter(!grepl("Podcast|Fünf Minuten:|Fünf Minute Berlin:|5 Minuten Berlin", Title)) %>%
  filter(!grepl("Podcast|Ringbahnpodcast", Lead)) %>%
  filter(!grepl("/liveblog/", link))


TagesspiegelMig <- TagesspiegelMig %>%
  filter(!is.na(Text))


# make sure that only articles are included that contain one of the specified words (\\bklima\\b -> include word that embedd "klima" / (?i) -> no matter if lower- or uppercase)
TagesspiegelMig3 <- TagesspiegelMig %>%
  filter(grepl("(?i)migration|(?i)Flüchtling|(?i)flüchtling|(?i)Migranten|(?i)migranten|(?i)Migration|(?i)Immigration|(?i)immigration", Text))

# clean date column
TagesspiegelMig$Date <- as.Date(TagesspiegelMig$Date)
TagesspiegelMig3$Date <- as.Date(TagesspiegelMig3$Date)


# Delete all rows which contains more than 32000 chars in $Text -> Ticker
count_chars <- function(x) {
  if (is.character(x)) {
    nchar(x)
  } else {
    0
  }
}

# Wenden Sie die Funktion auf die Textspalte des Dataframes an und prüfen Sie, ob die Anzahl der Zeichen in einer Zelle 32.000 überschreitet
long_cells <- sapply(TagesspiegelMig3$Text, count_chars) > 32000

# Entfernen Sie die Zeilen, bei denen die Textspalte mehr als 32.000 Zeichen enthält
TagesspiegelMig3 <- TagesspiegelMig3[!long_cells, ]

write.xlsx(TagesspiegelMig3, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/Tagesspiegel/TagesspiegelMig.xlsx", rowNames = FALSE)
write.xlsx(TagesspiegelMig3, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/Tagesspiegel/TagesspiegelMig3.xlsx", rowNames = FALSE)


