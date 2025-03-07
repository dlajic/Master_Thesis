library(rvest)
library(dplyr)
library(jsonlite)
library(xlsx)
library(tidyverse) 
library(xml2)  
library(data.table)
library(openxlsx)


day_seq <- seq(1, 49, 1)
links <- data.frame()
mem <- data.frame("links" = NA)
mem2 <- data.frame("Quelle/Typ" = NA)
mem3 <- data.frame("Ressort" = NA)
mem3 <- data.frame("Typ" = NA)
final <- data.frame("links" = NA, "Quelle/Typ" = NA, "Ressort" = NA, "Typ" = NA)

for (i in as.character(day_seq)){

  html <- read_html(paste0("https://taz.de/!s=&IrTagId=5007824/?search_page=", i))
  
  mem <- html %>% 
    html_nodes("a.objlink") %>%
    html_attr("href") %>%
    data.frame("links" = .)
  
  mem2 <- html %>% 
    html_nodes("div.extension") %>%
    html_node("p:nth-child(3)") %>%
    html_text()
  
  mem3 <- html %>% 
    html_nodes("div.extension") %>%
    html_node("p:nth-child(4)") %>%
    html_text()
  
  mem4 <- html %>% 
    html_nodes("div.extension") %>%
    html_node("p:nth-child(5)") %>%
    html_text()
  
  mem <- data.frame("links" = mem)
  mem2 <- data.frame("Quelle/Typ" = mem2)
  mem3 <- data.frame("Ressort" = mem3)
  mem4 <- data.frame("Typ" = mem4)
  
  links <- cbind(mem, mem2, mem3, mem4)
  
  final <- rbind(final, links)
  
}


# delete empty row
final <- final %>% slice(-1)


final <- final %>%
  filter(!grepl("Typ: Podcast", final$Quelle.Typ)) 

final <- final %>%
  filter(!grepl("Typ: Podcast", final$Typ))


# complete the url
for (t in 1:nrow(final)){
  
  final$links[t] <- paste0("https://www.taz.de", final$links[t])
  
} 

get_text_taz <- function(html) {
  html %>%
    html_nodes("[class='article even'], [class='article odd'], [class='article first odd Initial'], [class='article first even'], [class='article last odd'],  [class='article last even']") %>%
    html_text2() %>%
    str_c(collapse = " ")
}

get_title1_taz <- function(html) {
  
  html %>% 
    html_elements("h1 > span:nth-child(3)") %>%
    html_text() 
}

get_title2_taz <- function(html) {
  
  html %>% 
    html_elements("h1 > span:nth-child(1)") %>%
    html_text() 
}

get_date_taz <- function(html) {
  html %>% 
    html_nodes(".date") %>% 
    html_attr("content")
}

get_date2_taz <- function(html) {
  html %>% 
    html_nodes("div.dateLocWrapper") %>% 
    html_text2()
}

get_lead_taz <- function(html) {
  
  html %>%
    html_nodes(".intro") %>%
    html_text2() 
}


# create empty dataframes where results are written to
tazMig <- data.frame("Date" = NA, "Date2" = NA, "Title" = NA, "Title2" = NA, "Text" = NA, "Lead" = NA)
mem <- data.frame("Date" = NA, "Date2" = NA, "Title" = NA, "Title2" = NA, "Text" = NA, "Lead" = NA)

# write links to object for loop
urls <- final$links

for(i in urls){
  
  html <- tryCatch(read_html(i),
                   error = function(e) {NA})
  # get headline of article
  mem$Title <- ifelse(is.na(html), NA_character_, get_title1_taz(html)) 
  
  mem$Title2 <- ifelse(is.na(html), NA_character_, get_title2_taz(html)) 
  
  # get publishing date
  mem$Date <- ifelse(is.na(html), NA_character_, get_date_taz(html))
  
  mem$Date2 <- ifelse(is.na(html), NA_character_, get_date2_taz(html))
  
  
  mem$Text <- ifelse(is.na(html), NA_character_, get_text_taz(html))
  
  mem$Lead <- ifelse(is.na(html), NA_character_, get_lead_taz(html))
  
  
  tazMig <- rbind(tazMig, mem)
  
  print(i)
  Sys.sleep(rnorm(1, 0, 0.5)^2)
}

# remove first row that is empty
tazMig <- tazMig  %>% slice(-1)

# assign links to scraped articles
tazMig$link <- final$links

# clean date column
tazMig$Date <- as.Date(tazMig$Date)


write.xlsx(tazMig, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/taz/tazMig.xlsx", rowNames = FALSE)


