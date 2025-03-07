library(rvest)
library(tidyverse)
library(stringr)
library(xlsx)
library(readxl)
library(utils)
library(openxlsx)


links_spiegelAll <- data.frame("Link" = NA, "Opinion" = NA)

j <- 0

day_start <- "2021-01-01"
day_end <- "2023-04-09"
day_seq <- seq(as.Date(day_start), as.Date(day_end), by = "day")
day_seq <- format(as.Date(day_seq), "%d.%m.%Y")

for(i in day_seq){
  
  url <- paste0("https://www.spiegel.de/nachrichtenarchiv/artikel-", i, ".html")
  
  page <- read_html(url)
  
  links <- page %>% html_nodes("h2 a")
  
  filtered_links <- links[!sapply(links, function(x) {
    svg_nodes <- html_nodes(x, xpath = ".//svg")
    length(svg_nodes) > 0
  })]
  
  hrefs <- filtered_links %>% html_attr("href")
  
  opinion <- filtered_links %>% html_node("picture , .loaded") %>% html_text() %>% data.frame()
  opinion <- ifelse(opinion$.=="",1,0)
  
  links2 <- data.frame("Link" = hrefs, "Opinion" = opinion)
  
  links_spiegelAll <- rbind(links_spiegelAll, links2)
  
  Sys.sleep(rnorm(1, 0, 0.2)^2)
  
  j <- j+1
  print(j)
  
}

links_spiegelAll <- links_spiegelAll  %>% slice(-1)

write.xlsx(links_spiegelAll,file = "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/spiegel/links_spiegelAll_Opi.xlsx")
#links_spiegelAll <- read.xlsx("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/spiegel/links_spiegelAll_Opi.xlsx")

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


### just for opinion

# create empty dataframes where results are written to
spiegelOpi <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA, "Links"=NA)
mem <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA, "Links"=NA)

# write links to object for loop
urls <- subset(links_spiegelAll, Opinion == 1)

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
  
  spiegelOpi <- rbind(spiegelOpi, mem)
  
  Sys.sleep(rnorm(1, 0, 0.2)^2)
  
  print(j)
  j= j+1
}

# remove first row that is empty
spiegelOpi <- spiegelOpi  %>% slice(-1)

# clean date column
spiegelOpi$Date <- gsub(",.*", "", spiegelOpi$Date)
spiegelOpi$Date <- as.Date(spiegelOpi$Date, "%d.%m.%Y")

#new column environment
spiegelOpi$Opinion <- 1

# Delete all rows which contains more than 32000 chars in $Text -> Ticker
count_chars <- function(x) {
  if (is.character(x)) {
    nchar(x)
  } else {
    0
  }
}


# Wenden Sie die Funktion auf die Textspalte des Dataframes an und prüfen Sie, ob die Anzahl der Zeichen in einer Zelle 32.000 überschreitet
long_cells <- sapply(spiegelOpi$Text, count_chars) > 32000

# Entfernen Sie die Zeilen, bei denen die Textspalte mehr als 32.000 Zeichen enthält
spiegelOpi_filtered <- spiegelOpi[!long_cells, ]

write.xlsx(spiegelOpi_filtered, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/spiegel/spiegelOpi.xlsx", rowNames = FALSE)



# create empty dataframes where results are written to
spiegelAll <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA, "Links"=NA)
mem <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA, "Links"=NA)

# write links to object for loop
urls <- links_spiegelAll


j <-0

for(i in urls$Link[1:20000]){
  
  html <- tryCatch(read_html(i),
                   error = function(e) {NA})
  # get headline of article
  mem$Title <- ifelse(is.na(html), NA_character_, get_title_spiegel(html)) 
  
  # get publishing date
  mem$Date <- ifelse(is.na(html), NA_character_, get_date_spiegel(html))
  
  mem$Text <- ifelse(is.na(html), NA_character_, get_text_spiegel(html))
  
  mem$Lead <- ifelse(is.na(html), NA_character_, get_lead_spiegel(html))
  
  mem$Links <- i
  
  spiegelAll <- rbind(spiegelAll, mem)
  
  Sys.sleep(rnorm(1, 0, 0.2)^2)
  
  print(j)
  j= j+1
}

# remove first row that is empty
spiegelAll <- spiegelAll  %>% slice(-1)

# clean date column
spiegelAll$Date <- gsub(",.*", "", spiegelAll$Date)
spiegelAll$Date <- as.Date(spiegelAll$Date, "%d.%m.%Y")

# Delete all rows which contains more than 32000 chars in $Text -> Ticker
count_chars <- function(x) {
  if (is.character(x)) {
    nchar(x)
  } else {
    0
  }
}

# Wenden Sie die Funktion auf die Textspalte des Dataframes an und prüfen Sie, ob die Anzahl der Zeichen in einer Zelle 32.000 überschreitet
long_cells <- sapply(spiegelAll$Text, count_chars) > 32000

# Entfernen Sie die Zeilen, bei denen die Textspalte mehr als 32.000 Zeichen enthält
spiegelAll <- spiegelAll[!long_cells, ]


#For counterset for opinion pieces
spiegelOpi <- read.xlsx("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/spiegel/spiegelOpi.xlsx")
spiegelAll_noOpi <- anti_join(spiegelAll, spiegelOpi , by = c("Links"))
spiegelAll_noOpi$Opinion <- 0

write.xlsx(spiegelAll_noOpi, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/spiegel/spiegelAll_noOpi.xlsx", rowNames = FALSE)


#For counterset Environment, delete all articles which are in environment
spiegelEnv <- read.xlsx("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/spiegel/spiegelEnv.xlsx")
spiegelAll_noEnv <- anti_join(spiegelAll, spiegelEnv , by = c("Links"))

#new column environment
spiegelAll_noEnv$environment <- 0

write.xlsx(spiegelAll_noEnv, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/spiegel/spiegelAll_noEnv.xlsx", rowNames = FALSE)



#For counterset Migration, delete all articles which are in migration
#spiegelAll <- read.xlsx("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/spiegel/spiegelAll.xlsx")
spiegelMig<- read.xlsx("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/spiegel/spiegelMig.xlsx")
spiegelAll_noMig <- anti_join(spiegelAll, spiegelMig , by = c("Links"))

#new column environment
spiegelAll_noMig$migration <- 0

write.xlsx(spiegelAll_noMig, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/spiegel/spiegelAll_noMig.xlsx", rowNames = FALSE)

