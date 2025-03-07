library(rvest)
library(dplyr)
library(jsonlite)
library(xlsx)
library(tidyverse) 
library(xml2)  
library(data.table)


page <- seq(1, 200, 1)

prep <- data.frame()
links <- data.frame("links" = NA, "premium" = NA)


for (i in page){
  
  html <- read_html(paste0("https://www.welt.de/themen/migration/", i, "/"))
  
  mem <- html %>% 
    html_nodes(".o-teaser__link--is-headline") %>%
    html_attr("href") %>%
    data.frame("links" = .)
  
  mem2 <- html %>% 
    html_nodes(".c-grid__item") %>%
    html_node("svg use") %>%
    html_attr("href") %>%
    data.frame("premium" = .)

  
  mem <- data.frame("links" = mem)
  mem2 <- data.frame("premium" = mem2)
  
  prep <- cbind(mem, mem2)
  
  links <- rbind(links, prep)
  
  print(i)
}




# delete empty row
links <- links %>% slice(-1)

# filter out premium articles
links <- links %>%
  filter(!grepl("premium", .$premium) & !grepl("video", .$links) & !grepl("/podcasts/", .$links))



# complete the url
for (t in 1:nrow(links)){
  
  links$links[t] <- paste0("https://www.welt.de", links$links[t])
  
} 


get_text_welt <- function(html) {
  
  html %>%
    html_nodes(".__margin-bottom--is-0 p") %>%
    html_text2() %>%
    str_c(collapse = " ")
}

get_title_welt <- function(html) {
  
  html %>% 
    html_nodes(".rf-o-headline") %>%
    html_text() 
}

get_date_welt <- function(html) {
  html %>% 
    html_nodes(".c-publish-date") %>% 
    html_attr("datetime")
}

get_lead_welt <- function(html) {
  
  html %>%
    html_nodes(".c-summary__intro") %>%
    html_text2() 
}


# create empty dataframes where results are written to
weltMig <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA)
mem <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA)


# write links to object for loop
urls <- links$links

for(i in urls){
  
  html <- tryCatch(read_html(i),
                   error = function(e) {NA})
  # get headline of article
  mem$Title <- ifelse(is.na(html), NA_character_, get_title_welt(html)) 
  
  # get publishing date
  mem$Date <- ifelse(is.na(html), NA_character_, get_date_welt(html))
  
  mem$Text <- ifelse(is.na(html), NA_character_, get_text_welt(html))
  
  mem$Lead <- ifelse(is.na(html), NA_character_, get_lead_welt(html))
  
  
  weltMig <- rbind(weltMig, mem)
  
  print(i)
}

# remove first row that is empty
weltMig <- weltMig  %>% slice(-1)

# assign links to scraped articles
weltMig$link <- links$links

# clean date column
weltMig$Date <- as.Date(weltMig$Date)

# filter old articles out
weltMig <- weltMig %>%
  filter(weltMig$Date >= "2016-01-01")

# check for empty cells in text column and filter out corresponding rows
weltMig <- weltMig %>%
  filter(!is.na(Text)) %>%
  data.frame()

write.xlsx(weltMig, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/WELT/weltMig.xlsx", rowNames = FALSE)


