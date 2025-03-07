library(rvest)
library(dplyr)
library(jsonlite)
library(xlsx)
library(tidyverse) 
library(xml2)  
library(data.table)

output1 <- data.frame("website_url" = NA)
output2 <- data.frame("content_code" = NA)

t <- seq(0,2000,100)

# guide to find a hidden API:
# visit rnd.de topic site (like economy) and open the browsers developer tools and navigate to network
# then click the load more button on the page and watch which new entries appear in the network list (look for the typ "fetch")
# copy the link which is queried by the webpage to load new content and insert it in a browser tab and look if the data is what you want (use chrome extension like jsonviewer to make sense of json output)
# keep attention to the format of the link encoded/decoded -> copy link from previous step (from browser tab) into R to get it encoded (needed by rvest)
for (i in t){
  
  test <- jsonlite::fromJSON(paste0("https://www.rnd.de/pf/api/v3/content/fetch/Themenfeeds?query=%7B%22contentTypes%22%3A%22%22%2C%22offset%22%3A%22",i ,"%22%2C%22size%22%3A100%2C%22sortfield%22%3A%22display_date%22%2C%22tags%22%3A%22migration%22%7D&d=310&_website=rndde"), simplifyVector = TRUE)
  
  link <- as.data.frame(test[["teasers"]][["href"]])
  content_type <- as.data.frame(test[["teasers"]][["isRequiringPayment"]])
  
  colnames(link) <- "website_url"
  colnames(content_type) <- "content_code"
  
  output1 <- rbind(output1, link)
  output2 <- rbind(output2, content_type)

  Sys.sleep(rnorm(1, 1, 0.5)^2)
  
}


# bind result datasets from loop above together
final <- cbind(output1, output2)

# delete empty row
final <- final[-1,]

# complete the url
for (t in 1:nrow(final)){
  
  final$website_url[t] <- paste0("https://www.rnd.de", final$website_url[t])
  
} 

# "NA" = Video / "plus" = paid article
linksMig <- final %>%
  filter(content_code == "FALSE")




#create functions for scraping
get_date_rnd <- function(html) {
  html %>% 
    html_node("[data-testid=timestamp]") %>% 
    html_attr("datetime")
}


get_text_rnd <- function(html) {
  html %>% 
    html_nodes(".cKKmGm , .fMxgIC") %>% 
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
    html_elements(".ArticleHeadstyled__ArticleSubHeadline-sc-1xd2qac-8") %>% 
    html_text %>% 
    str_c(collapse = " ")
  
}


# create empty dataframes where results are written to
rndMig <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA)
mem <- data.frame("Date" = NA, "Title" = NA, "Text" = NA, "Lead" = NA)

# write links to object for loop
urls <- linksMig[,1]

for(i in urls){

  html <- tryCatch(read_html(i),
                   error = function(e) {NA})
    # get headline of article
    mem$Title <- ifelse(is.na(html), NA_character_, get_title_rnd(html)) 
    
    # get publishing date
    mem$Date <- ifelse(is.na(html), NA_character_, get_date_rnd(html))
    
    mem$Text <- ifelse(is.na(html), NA_character_, get_text_rnd(html))
    
    mem$Lead <- ifelse(is.na(html), NA_character_, get_lead_rnd(html))
    
    rndMig <- rbind(rndMig, mem)
    
    print(i)
    #Sys.sleep(rnorm(1, 1, 0.5)^2)
  }

# remove first row that is empty
rndMig <- rndMig  %>% slice(-1)

# assign links to scraped articles
rndMig$link <- linksMig$website_url

# clean date column
rndMig$Date <- as.Date(rndMig$Date)

write.xlsx(rndMig, file= "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/rnd/rndMig.xlsx", rowNames = FALSE)
