library(rvest)
library(dplyr)
library(jsonlite)
library(xlsx)
library(tidyverse) 
library(xml2)  
library(data.table)

day_start <- "2018-01-01"
day_end <- "2023-04-23"

day_seq <- seq(as.Date(day_start), as.Date(day_end), by = "day")
links <- data.frame("links" = NA)
mem <- data.frame("links" = NA)

for (i in as.character(day_seq)){
  
  html <- read_html(paste0("https://www.tagesschau.de/archiv/?datum=", i))
  
  mem <- html %>% 
    html_nodes(".teaser-xs__link") %>%
    html_attr("href") %>%
    as_tibble() %>%
    rename(links = value) %>%
    filter(!grepl("/multimedia/|/euarchiv|/kommentar|kommentar-", links))
  
  
  links <- rbind(links, mem)
  
  print(i)
  
  
  pag <- html %>%
    html_node(".paginierung__liste") %>%
    html_elements("a") %>%
    html_attr("href") %>%
    as_tibble() %>%
    distinct() %>%
    last()
  

  if (length(pag$value) > 0){
    
    pag1 <- seq(2, str_sub(pag$value, -1, -1), 1)
    
    
    for (k in pag1){
      
      
      html <- read_html(paste0("https://www.tagesschau.de/archiv/?datum=", i, "&pageIndex=", k))
      
      html %>% 
        html_nodes(".teaser-xs__link") %>%
        html_attr("href") %>%
        as_tibble() %>%
        rename(links = value) %>%
        filter(!grepl("/multimedia/|/euarchiv|/kommentar|kommentar-", links))
      
      
      links <- rbind(links, mem)
      
      print(k)
      
    }
  }
}


# delete empty row
links <- links %>% slice(-1)

# delete duplicate entries (website contains for some dates all articles of a month)
links <- links %>%
  distinct()

# noticed that with the used css selector also articles are scraped with the word "kommentar" in the title 
# filter out articles that do not contain "kommentar" in link
links <- links %>%
  filter(!grepl("/kommentar/|kommentar-|newsticker|/eilmeldung/", links))


## sample
links <- dplyr::sample_n(links, 400)


get_text_tagesschau <- function(html) {
  
  html %>%
    html_nodes(".twelve+ .textabsatz") %>%
    html_text2() %>%
    str_c(collapse = " ")
  
}

get_lead_tagesschau <- function(html) {
  
  html %>%
    html_nodes(".textabsatz strong") %>%
    html_text2() 
}

get_title1_tagesschau <- function(html) {
  
  html %>%
    html_nodes(".seitenkopf__topline") %>%
    html_text2() 
}

get_title2_tagesschau <- function(html) {
  
  html %>%
    html_nodes(".seitenkopf__headline--text") %>%
    html_text2() 
}

get_date_tagesschau <- function(html) {
  html %>% 
    html_nodes(".metatextline") %>% 
    html_text2()
}



# create empty dataframes where results are written to
tagesschauAll <- data.frame("Date" = NA, "Title1" = NA, "Title2" = NA, "Text" = NA, "Lead" = NA)
mem <- data.frame("Date" = NA, "Title1" = NA, "Title2" = NA, "Text" = NA, "Lead" = NA)

# write links to object for loop
urls <- links[,1]

for(i in urls){
  
  html <- tryCatch(read_html(i),
                   error = function(e) {NA})
  # get headline of article
  mem$Title1 <- ifelse(is.na(html), NA_character_, get_title1_tagesschau(html)) 
  
  mem$Title2 <- ifelse(is.na(html), NA_character_, get_title2_tagesschau(html)) 
  
  # get publishing date
  mem$Date <- ifelse(is.na(html), NA_character_, get_date_tagesschau(html))
  
  mem$Text <- ifelse(is.na(html), NA_character_, get_text_tagesschau(html))
  
  mem$Lead <- ifelse(is.na(html), NA_character_, get_lead_tagesschau(html))
  
  tagesschauAll <- rbind(tagesschauAll, mem)
  
  print(i)
  Sys.sleep(rnorm(1, 1, 0.2)^2)
}


# remove first row that is empty
tagesschauAll <- tagesschauAll  %>% slice(-1)

# assign links to scraped articles
tagesschauAll$link <- links$links

# remove article with empty text
tagesschauAll <- tagesschauAll %>%
  filter(!grepl("https://www.tagesschau.de/ausland/polarstern-arktis-101.html", link))


tagesschauAll <- dplyr::sample_n(tagesschauAll, 361)


# clean date column
tagesschauAll$Date <- gsub("Stand: | Uhr", "", tagesschauAll$Date)
tagesschauAll$Date <- as.Date(tagesschauAll$Date, "%d.%m.%Y")



write.xlsx(tagesschauAll, file= "C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/Scraping Tagesschau/tagesschauAll.xlsx", row.names = FALSE)

