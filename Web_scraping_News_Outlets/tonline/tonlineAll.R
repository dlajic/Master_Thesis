library(rvest)
library(tidyverse)
library(stringr)
library(xlsx)
library(readxl)
library(utils)
library(openxlsx)


links_tonlineAll <- data.frame("Link" = NA)

t <- seq(1,180,1)
j <- 0


for(i in t){
  
  url <- paste0("https://suche.t-online.de/news?q=a&context=themen-tab&tax=t-online&page=", i)
  
  page <- read_html(url)
  
  links <- page %>% html_nodes("a.tMMReshl2")
  
  hrefs <- links %>% html_attr("href")
  
  links2 <- data.frame("Link" =  hrefs)
  
  links_tonlineAll <- rbind(links_tonlineAll, links2)
  
  j <- j+1
  print(j)
  
  #Sys.sleep(rnorm(1, 1, 0.5)^2)
  
}

links_tonlineAll <- links_tonlineAll %>% slice(-1)

write.xlsx(links_tonlineAll,file = "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/tonline/links_tonline_All.xlsx")
#links_tonlineAll2 <- read.csv2("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/tonline/tonlinelinks_All.csv")
#links_tonlineAll <- links_tonlineAll2$link

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
tonlineAll <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA, "Links"=NA)
mem <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA, "Links"=NA)

# write links to object for loop
urls <- links_tonlineAll


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
  
  tonlineAll <- rbind(tonlineAll, mem)
  
  #Sys.sleep(rnorm(1, 0, 0.5)^2)
  
  print(j)
  j= j+1
  
}

# remove first row that is empty
tonlineAll <- tonlineAll  %>% slice(-1)


# clean date column
tonlineAll$Date <- gsub("Aktualisiert am ", "", tonlineAll$Date)
tonlineAll$Date <- as.Date(tonlineAll$Date, "%d.%m.%Y")



#For counterset migration, delete all articles which are in migration
tonlineMig <- read.xlsx("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/tonline/tonlineMig.xlsx")
tonlineAll <- anti_join(tonlineAll, tonlineMig , by = c("Links"))

#new column migration
tonlineAll$migration <- 0


# Delete all rows which contains more than 32000 chars in $Text -> Ticker
count_chars <- function(x) {
  if (is.character(x)) {
    nchar(x)
  } else {
    0
  }
}

# Wenden Sie die Funktion auf die Textspalte des Dataframes an und prüfen Sie, ob die Anzahl der Zeichen in einer Zelle 32.000 überschreitet
long_cells <- sapply(tonlineAll$Text, count_chars) > 32000

# Entfernen Sie die Zeilen, bei denen die Textspalte mehr als 32.000 Zeichen enthält
tonlineAll_filtered <- tonlineAll[!long_cells, ]


write.xlsx(tonlineAll_filtered, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/tonline/tonlineAll.xlsx", rowNames = FALSE)


