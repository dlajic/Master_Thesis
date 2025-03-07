library(rvest)
library(dplyr)
library(jsonlite)
library(xlsx)
library(tidyverse) 
library(xml2)  
library(data.table)


day_seq <- seq(1, 49, 1)
links <- data.frame()
mem <- data.frame("links" = NA)
mem2 <- data.frame("Quelle/Typ" = NA)
mem3 <- data.frame("Ressort" = NA)
mem3 <- data.frame("Typ" = NA)
final <- data.frame("links" = NA, "Quelle/Typ" = NA, "Ressort" = NA, "Typ" = NA)

pages <- c("https://taz.de/!s=kommentar/?search_page=", "https://taz.de/!s=kolumne/?search_page=", "https://taz.de/!s=gastkommentar/?search_page=")

for (k in pages){

  for (i in as.character(day_seq)){
    
    html <- read_html(paste0(k, i))
    
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
    
    print(i)
  }
}

# delete empty row
final <- final %>% slice(-1)


final <- final %>%
  filter(!grepl("&SuchRahmen=Print/.*", final$links))

final_t <- final %>%
  filter(grepl("Typ: Kolumne|Kommentar|Gastkommentar", final$Quelle.Typ) | grepl("Typ: Kolumne|Kommentar|Gastkommentar", final$Typ))




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

get_lead_taz <- function(html) {
  
  html %>%
    html_nodes(".intro") %>%
    html_text2() 
}


# create empty dataframes where results are written to
tazOpi <- data.frame("Date" = NA, "Title" = NA, "Title2" = NA, "Text" = NA, "Lead" = NA)
mem <- data.frame("Date" = NA, "Title" = NA, "Title2" = NA, "Text" = NA, "Lead" = NA)

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
  
  mem$Text <- ifelse(is.na(html), NA_character_, get_text_taz(html))
  
  mem$Lead <- ifelse(is.na(html), NA_character_, get_lead_taz(html))
  
  
  tazOpi <- rbind(tazOpi, mem)
  
  print(i)
  Sys.sleep(rnorm(1, 1, 0.5)^2)
}

# remove first row that is empty
tazOpi <- tazOpi  %>% slice(-1)

# assign links to scraped articles
tazOpi$link <- final$links

# clean date column
tazOpi$Date <- as.Date(tazOpi$Date)


write.xlsx(tazOpi, file= "C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/Scraping taz/tazOpi.xlsx", row.names = FALSE)


