library(rvest)
library(dplyr)
library(jsonlite)
library(xlsx)
library(tidyverse) 
library(xml2)  
library(data.table)


pages <- seq(0, 68, 1)
links <- data.frame()

for (i in pages){
  
  html <- read_html(paste0("https://www.n-tv.de/thema/migranten/archiv-", i))
  
  mem <- html %>% 
    html_nodes("section > article > div > a") %>%
    html_attr("href") %>%
    as_tibble() %>%
    rename(links = value)
  
  links <- rbind(links, mem)
  
  print(i)
}

# delete duplicate entries (website contains for some dates all articles of a month)
links <- links %>%
  distinct()


get_text_ntv <- function(html) {
  
  html %>%
    html_nodes(".article__text > p") %>%
    html_text2() %>%
    str_c(collapse = " ")
  
}


get_lead_ntv <- function(html) {
  
  html %>%
    html_nodes("p strong") %>%
    html_text2() 
}

get_title1_ntv <- function(html) {
  
  html %>%
    html_nodes(".article__headline") %>%
    html_text2() 
}

get_title2_ntv <- function(html) {
  
  html %>%
    html_nodes(".article__kicker") %>%
    html_text2() 
}

get_date_ntv <- function(html) {
  html %>% 
    html_nodes(".article__date") %>% 
    html_text2()
}



# create empty dataframes where results are written to
ntvMig <- data.frame("Date" = NA, "Title1" = NA, "Title2" = NA, "Text" = NA, "Lead" = NA)
mem <- data.frame("Date" = NA, "Title1" = NA, "Title2" = NA, "Text" = NA, "Lead" = NA)

# write links to a list for loop
urls <- links[,1]

for(i in urls$links){
  
  html <- tryCatch(read_html(i),
                   error = function(e) {NA})
  # get headline of article
  mem$Title1 <- ifelse(is.na(html), NA_character_, get_title1_ntv(html)) 
  
  mem$Title2 <- ifelse(is.na(html), NA_character_, get_title2_ntv(html)) 
  
  # get publishing date
  mem$Date <- ifelse(is.na(html), NA_character_, get_date_ntv(html))
  
  mem$Text <- ifelse(is.na(html), NA_character_, get_text_ntv(html))
  
  mem$Lead <- ifelse(is.na(html), NA_character_, get_lead_ntv(html))
  
  ntvMig <- rbind(ntvMig, mem)
  
  print(i)
  #Sys.sleep(rnorm(1, 1, 0.2)^2)
}


# remove first row that is empty
ntvMig <- ntvMig  %>% slice(-1)

# assign links to scraped articles
ntvMig$link <- links$links

# clean date column
ntvMig$Date <- as.Date(ntvMig$Date, "%d.%m.%Y")



write.xlsx(ntvMig, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/ntv/ntvMig.xlsx", row.names = FALSE)

