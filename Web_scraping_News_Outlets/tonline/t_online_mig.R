library(rvest)
library(tidyverse)
library(stringr)
library(xlsx)
library(readxl)
library(utils)
library(openxlsx)


links_tonlineMig <- data.frame("Link" = NA)

t <- seq(1,31,1)
j <- 0


for(i in t){
  
  url <- paste0("https://www.t-online.de/themen/migration/page_", i, "/")
  
  page <- read_html(url)
  
  links <- page %>% html_nodes("div.css-1b3bf3e:nth-of-type(n+2) a")
  
  hrefs <- links %>% html_attr("href")
  
  links2 <- data.frame("Link" = paste0("https://www.t-online.de", hrefs))
  
  links_tonlineMig <- rbind(links_tonlineMig, links2)
  
  j <- j+1
  print(j)
  
}

links_tonlineMig <- links_tonlineMig %>% slice(-1)

write.xlsx(links_tonlineMig,file = "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/tonline/links_tonline.xlsx")
#links_tonlineMig2 <- read.csv2("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/tonline/tonlinelinks_Mig.csv")
#links_tonlineMig <- links_tonlineMig2$link

#Use Links to scrape articles
#create functions for scraping
get_date_tonline <- function(html) {
  html %>%
    html_nodes("span.css-1qrq0jm:nth-of-type(1)") %>%
    html_text2()
}

get_text_tonline <- function(html) {
  html %>% 
    html_nodes(".css-wzramg") %>% 
    html_text() %>% 
    str_c(collapse = " ")
}


get_title_tonline <- function(html) {
  html %>%
    html_nodes("h1") %>%
    html_text2() 
}


get_lead_tonline <- function(html) {
  html %>% 
    html_elements("p.css-170a2tk") %>% 
    html_text %>% 
    str_c(collapse = " ")
  
}


get_tags_tonline <- function(html) {
  html %>% 
    html_elements("p.css-170a2tk") %>% 
    html_text(trim=T) %>% 
    str_replace_all(" ", ",")
}


# create empty dataframes where results are written to
tonlineMig <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA, "Links"=NA)
mem <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA, "Links"=NA)

# write links to object for loop
urls <- links_tonlineMig


j <-0

for(i in urls$Link){
  
  html <- tryCatch(read_html(i),
                   error = function(e) {NA})
  # get headline of article
  mem$Title <- ifelse(is.na(html), NA_character_, get_title_tonline(html)) 
  
  # get publishing date
  mem$Date <- ifelse(is.na(html), NA_character_, get_date_tonline(html))
  
  mem$Text <- ifelse(is.na(html), NA_character_, get_text_tonline(html))
  
  mem$Lead <- ifelse(is.na(html), NA_character_, get_lead_tonline(html))
  
  mem$Links <- i
  
  tonlineMig <- rbind(tonlineMig, mem)
  
  #Sys.sleep(rnorm(1, 0, 0.5)^2)
  
  print(j)
  j= j+1
  
}

# remove first row that is empty
tonlineMig <- tonlineMig  %>% slice(-1)

# assign links to scraped articles
tonlineMig$link <- links_tonlineMig$links

# clean date column
tonlineMig$Date <- gsub("Aktualisiert am ", "", tonlineMig$Date)
tonlineMig$Date <- as.Date(tonlineMig$Date, "%d.%m.%Y")

#new column migration
tonlineMig$migration <- 1

write.xlsx(tonlineMig, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/tonline/tonlineMig.xlsx", rowNames = FALSE)


