library(rvest)
library(dplyr)
library(jsonlite)
library(xlsx)
library(tidyverse) 
library(xml2)  
library(data.table)
library(DescTools)

day_start <- as.Date("2019-01-01", "%Y-%m-%d")
day_end <- as.Date("2022-03-31", "%Y-%m-%d")


day_seq <- seq(day_start, day_end, by = "day")
links <- data.frame("links" = NA, "premium" = NA, "opinion" = NA)
mem <- data.frame("links" = NA)
mem2 <- data.frame("premium" = NA)
mem3 <- data.frame("opinion" = NA)


for (i in as.character(day_seq)){
  
  html <- read_html(paste0("https://www.welt.de/schlagzeilen/nachrichten-vom-", format(as.Date(i), "%d-%m-%Y"), ".html"))
  
  mem <- html %>%
    html_nodes("#tab-panel-texts") %>%
    html_nodes("h4 a") %>%
    html_attr("href")
  
  mem2 <- html %>%
    html_nodes("#tab-panel-texts") %>%
    html_nodes(".c-teaser__header") %>%
    html_node("svg") %>%
    html_text2()

  mem3 <- html %>%
    html_nodes("#tab-panel-texts") %>%
    html_nodes(".c-teaser__header") %>%
    html_node(":contains('Meinung'), :contains('Satire')") %>%
    html_text()
  
  #  mem2 <- html %>%
  #    html_nodes(".c-teaser--archive") %>%
  #    html_node("a") %>%
  #    html_attr("href")
  #   
  #  mem3 <- html %>%
  #    html_nodes(".c-teaser--archive") %>%
  #    html_node("title") %>%
  #    html_text()
  #  
  #  mem4 <- html %>%
  #    html_nodes(".c-teaser--archive") %>%
  #    html_node(".c-teaser__header") %>%
  #    html_text()
  
  mem <- data.frame("links" = mem)
  mem2 <- data.frame("premium" = mem2)
  mem3 <- data.frame("opinion" = mem3)
  
  prep <- cbind(mem, mem2, mem3)
  
  links <- rbind(links, prep)
  
  print(i)
  
}


# delete empty row
links <- links %>% slice(-1)


# filter for descriptive articles and filter out double entries
links_descr <- links %>% 
  filter(!grepl("Weltplus", .$premium) & !grepl("/podcasts/", .$links) & !grepl("Artikeltyp:Meinung|Artikeltyp:Satire", .$opinion)) %>%
  distinct(links)

# filter for opinion articles and filter out double entries
links_opi <- links %>%
  filter(!grepl("Weltplus", .$premium) & grepl("Artikeltyp:Meinung", .$opinion) & !grepl("Artikeltyp:Satire", .$opinion)) %>%
  distinct(links)

# sample out of descr articles 
links_descr1 <- links_descr %>% sample_n(size = 5686)


# complete the url
for (t in 1:nrow(links_descr)){
  
  links_descr$links[t] <- paste0("https://www.welt.de", links_descr$links[t])
  
} 

# complete the url
for (t in 1:nrow(links_opi)){
  
  links_opi$links[t] <- paste0("https://www.welt.de", links_opi$links[t])
  
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
WELTOpi <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA)
mem <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA)

# write links to object for loop
urls <- links_opi[,1]

for(i in urls){
  
  html <- tryCatch(read_html(i),
                   error = function(e) {NA})
  # get headline of article
  mem$Title <- ifelse(is.na(html), NA_character_, get_title_welt(html)) 
  
  # get publishing date
  mem$Date <- ifelse(is.na(html), NA_character_, get_date_welt(html))
  
  mem$Text <- ifelse(is.na(html), NA_character_, get_text_welt(html))
  
  mem$Lead <- ifelse(is.na(html), NA_character_, get_lead_welt(html))
  
  WELTOpi <- rbind(WELTOpi, mem)
  
  print(i)
}

# remove first row that is empty
WELTOpi <- WELTOpi  %>% slice(-1)

# assign links to scraped articles
WELTOpi$link <- links_opi$links

# clean date column
WELTOpi$Date <- as.Date(WELTOpi$Date)


write.xlsx(WELTOpi, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/WELT/WELTOpi.xlsx", row.names = FALSE)



# create empty dataframes where results are written to
WELTAll <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA)
mem <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA)

# write links to object for loop
urls <- links_descr1[,1]

for(i in urls){
  
  html <- tryCatch(read_html(i),
                   error = function(e) {NA})
  # get headline of article
  mem$Title <- ifelse(is.na(html), NA_character_, get_title_welt(html)) 
  
  # get publishing date
  mem$Date <- ifelse(is.na(html), NA_character_, get_date_welt(html))
  
  mem$Text <- ifelse(is.na(html), NA_character_, get_text_welt(html))
  
  mem$Lead <- ifelse(is.na(html), NA_character_, get_lead_welt(html))
  
  WELTAll <- rbind(WELTAll, mem)
  
  print(i)
  Sys.sleep(rnorm(1, 1, 0.2)^2)
}


# remove first row that is empty
WELTAll <- WELTAll  %>% slice(-1)

# assign links to scraped articles
WELTAll$link <- links_descr1$links

# clean date column
WELTAll$Date <- as.Date(WELTAll$Date)

# delete one article that was not marked as opinion piece
WELTAll <- WELTAll %>%
  filter(link != "https://www.welt.de/debatte/kommentare/article233418437/Unionskandidat-Armin-Laschet-fordert-zinslose-Darlehen-fuer-Solardaecher.html")


openxlsx::write.xlsx(WELTAll, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/WELT/WELTAll.xlsx", rowNames = FALSE)
