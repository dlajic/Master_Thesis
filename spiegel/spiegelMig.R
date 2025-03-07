library(rvest)
library(tidyverse)
library(stringr)
library(xlsx)
library(readxl)
library(utils)
library(writexl)
library(openxlsx)

links_spiegelMig <- data.frame("Link" = NA)

t <- seq(0,70,1)
j <- 0


for(i in t){
  
  url <- paste0("https://www.spiegel.de/thema/migration/p", i, "/")
  
  page <- read_html(url)
  
  links <- page %>% html_nodes("h2 a")
  
  filtered_links <- links[!sapply(links, function(x) {
    svg_nodes <- html_nodes(x, xpath = ".//svg")
    length(svg_nodes) > 0
  })]
  
  hrefs <- filtered_links %>% html_attr("href")
  
  links2 <- data.frame("Link" = hrefs)
  
  links_spiegelMig <- rbind(links_spiegelMig, links2)
  
  Sys.sleep(rnorm(1, 0, 0.2)^2)
  
  j <- j+1
  print(j)
  
}

links_spiegelMig <- links_spiegelMig  %>% slice(-1)

write.xlsx(links_spiegelMig,file = "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/spiegel/links_spiegelMig.xlsx")

#links_spiegelMig <- read.xlsx("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/spiegel/links_spiegelMig.xlsx")

#Use Links to scrape articles
#create functions for scraping
get_date_spiegel <- function(html) {
  html %>%
    html_nodes(".timeformat") %>%
    html_text2()
}

get_text_spiegel <- function(html) {
  html %>% 
    html_nodes(".word-wrap p") %>% 
    html_text() %>% 
    str_c(collapse = " ")
}
#.word-wrap p
#.RichText > p


get_title_spiegel <- function(html) {
  html %>%
    html_nodes("h1") %>%
    html_text2() 
}

get_lead_spiegel <- function(html) {
  html %>% 
    html_elements("div[class='RichText RichText--sans leading-loose lg:text-xl md:text-xl sm:text-l lg:mb-32 md:mb-32 sm:mb-24']") %>% 
    html_text %>% 
    str_c(collapse = " ") %>%
    str_squish()
  
}


get_tags_spiegel <- function(html) {
  html %>% 
    html_elements(".js-a-hd__tgs.a-hd__tgs.f-p--500") %>% 
    html_text(trim=T) %>% 
    str_replace_all(" ", ",")
}


# create empty dataframes where results are written to
spiegelMig <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA, "Links"=NA)
mem <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA, "Links"=NA)

# write links to object for loop
urls <- links_spiegelMig

j <-0

for(i in urls$Link){
  
  html <- tryCatch(read_html(i),
                   error = function(e) {NA})
  # get headline of article
  mem$Title <- ifelse(is.na(html), NA_character_, get_title_spiegel(html)) 
  
  # get publishing date
  mem$Date <- ifelse(is.na(html), NA_character_, get_date_spiegel(html))
  
  mem$Text <- ifelse(is.na(html), NA_character_, get_text_spiegel(html))
  
  mem$Lead <- ifelse(is.na(html), NA_character_, get_lead_spiegel(html))
  
  mem$Links <- i
  
  spiegelMig <- rbind(spiegelMig, mem)
  
  Sys.sleep(rnorm(1, 0, 0.5)^2)
  
  print(j)
  j= j+1
}

# remove first row that is empty
spiegelMig <- spiegelMig  %>% slice(-1)

# clean date column
spiegelMig$Date <- gsub(",.*", "", spiegelMig$Date)
spiegelMig$Date <- as.Date(spiegelMig$Date, "%d.%m.%Y")

#new column migration
spiegelMig$migration <- 1

# Delete all rows which contains more than 32000 chars in $Text -> Ticker
count_chars <- function(x) {
  if (is.character(x)) {
    nchar(x)
  } else {
    0
  }
}

# Wenden Sie die Funktion auf die Textspalte des Dataframes an und prüfen Sie, ob die Anzahl der Zeichen in einer Zelle 32.000 überschreitet
long_cells <- sapply(spiegelMig$Text, count_chars) > 32000

# Entfernen Sie die Zeilen, bei denen die Textspalte mehr als 32.000 Zeichen enthält
spiegelMig_filtered <- spiegelMig[!long_cells, ]



write.xlsx(spiegelMig_filtered, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/spiegel/spiegelMig.xlsx", rowNames = FALSE)
saveRDS(spiegelMig,"C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/spiegel/spiegelMig.RData")


