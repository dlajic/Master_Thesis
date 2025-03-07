library(rvest)
library(dplyr)
library(jsonlite)
library(xlsx)
library(tidyverse) 
library(xml2)  
library(data.table)

output1 <- data.frame("website_url" = NA)
output2 <- data.frame("content_code" = NA)

t <- seq(0,9900,100)

# guide to find a hidden API:
# visit rnd.de topic site (like economy) and open the browsers developer tools and navigate to network
# then click the load more button on the page and watch which new entries appear in the network list (look for the typ "fetch")
# copy the link which is queried by the webpage to load new content and insert it in a browser tab and look if the data is what you want (use chrome extension like jsonviewer to make sense of json output)
# keep attention to the format of the link encoded/decoded -> copy link from previous step (from browser tab) into R to get it encoded (needed by rvest)
for (i in t){

  test <- jsonlite::fromJSON(paste0("https://www.rnd.de/pf/api/v3/content/fetch/Themenfeeds?query={%22excludePaidContent%22:%22yes%22,%22offset%22:%22", i, "%22,%22size%22:100,%22sortfield%22:%22display_date%22,%22tags%22:%22kommentar%22}&d=274&_website=rndde"),
                             simplifyVector = TRUE)
  
  link <- as.data.frame(test[["content_elements"]][["websites"]][["rndde"]])
  content_type <- as.data.frame(test[["content_elements"]][["content_restrictions"]])
  
  output1 <- rbind(output1, link)
  output2 <- rbind(output2, content_type)
  
  Sys.sleep(rnorm(1, 1, 0.5)^2)
  
}


# delete empty row
final <- output1 %>% slice(-1)

# complete the url
for (t in 1:nrow(final)){
  
  final$website_url[t] <- paste0("https://www.rnd.de", final$website_url[t])
  
} 



#create functions for scraping
get_date_rnd <- function(html) {
  html %>% 
    html_node("[data-testid=timestamp]") %>% 
    html_attr("datetime")
}


get_text_rnd <- function(html) {
  html %>% 
    html_nodes(".Textstyled__Text-sc-14jlruk-0") %>% 
    html_text() %>% 
    str_c(collapse = " ")
}

get_title_rnd <- function(html) {
  html %>%
    html_nodes("h1") %>%
    html_text2() 
}

get_lead_rnd <- function(html) {
  html %>% 
    html_elements(".dLKpPi") %>% 
    html_text %>% 
    str_c(collapse = " ")
  
}

get_tags_rnd <- function(html) {
  html %>% 
    html_elements(".js-a-hd__tgs.a-hd__tgs.f-p--500") %>% 
    html_text(trim=T) %>% 
    str_replace_all(" ", ",")
}


# create empty dataframes where results are written to
rndOpi <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA)
mem <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA)

# write links to object for loop
urls <- final[,1]

for(i in urls){
  
  html <- tryCatch(read_html(i),
                   error = function(e) {NA})
  # get headline of article
  mem$Title <- ifelse(is.na(html), NA_character_, get_title_rnd(html)) 
  
  # get publishing date
  mem$Date <- ifelse(is.na(html), NA_character_, get_date_rnd(html))
  
  mem$Text <- ifelse(is.na(html), NA_character_, get_text_rnd(html))
  
  mem$Lead <- ifelse(is.na(html), NA_character_, get_lead_rnd(html))
  
  rndOpi <- rbind(rndOpi, mem)
  
  print(i)
  Sys.sleep(rnorm(1, 1, 0.5)^2)
}

# remove first row that is empty
rndOpi <- rndOpi  %>% slice(-1)

# assign links to scraped articles
rndOpi$link <- final$website_url

# clean date column
rndOpi$Date <- as.Date(rndOpi$Date)

write.xlsx(rndOpi, file= "C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/Scraping_rnd/rndOpi.xlsx", row.names = FALSE)
