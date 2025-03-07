library(xlsx)
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)
library(stringr)


#### prep DW articles (Climate/Opinion)
tazAll <- read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/taz/tazAll.xlsx")
tazOpi <- read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/taz/tazOpi.xlsx")
tazMig <- read_excel("C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/taz/tazMig.xlsx")


# format dates in col Date2 in tazMig & tazAll and combine columns
tazMig$Date2 <- as.Date(tazMig$Date2, "%d.%m.%Y")
tazAll$Date2 <- as.Date(tazAll$Date2, "%d.%m.%Y")

tazMig <- unite(tazMig, Date, c(Date, Date2), na.rm = T)
tazAll <- unite(tazAll, Date, c(Date, Date2), na.rm = T)

# remove rows with emtpy cells
tazMig <- na.omit(tazMig)
tazAll <- na.omit(tazAll)
tazOpi <- na.omit(tazOpi)

# convert links of tazOpi/Mig to format of tazAll
tazOpi <- tazOpi %>% 
  separate(link, into = c("part1", "part2"), sep = "&") %>%
  mutate(link = as.character(part1)) %>%
  select(Date, Title, Title2, Text, Lead, link)

tazOpi <- tazOpi %>%
  distinct() 

tazMig <- tazMig %>% 
  separate(link, into = c("part1", "part2"), sep = "&") %>%
  mutate(link = as.character(part1)) %>%
  select(Date, Title, Title2, Text, Lead, link)

tazMig <- tazMig %>%
  distinct()


# filter on all rows in the first data frame that do not have a matching team in the second data frame
tazAll_opi <- anti_join(tazAll, tazOpi, by = "link")
tazAll_mig <- anti_join(tazAll, tazMig, by = "link")


# filter out articles longer than 30000 characters (typ: longread)
tazAll_opi$cnt_characters <- str_count(tazAll_opi$Text)
tazAll_mig$cnt_characters <- str_count(tazAll_mig$Text)
tazOpi$cnt_characters <- str_count(tazOpi$Text)
tazMig$cnt_characters <- str_count(tazMig$Text)

tazAll_opi <- tazAll_opi[tazAll_opi$cnt_characters <= 30000, ]
tazAll_mig <- tazAll_mig[tazAll_mig$cnt_characters <= 30000, ]
tazOpi     <- tazOpi[tazOpi$cnt_characters <= 30000, ]
tazMig     <- tazMig[tazMig$cnt_characters <= 30000, ]

tazAll_opi <- tazAll_opi[,-7]
tazAll_mig <- tazAll_mig[,-7]
tazOpi     <- tazOpi[,-7]
tazMig     <- tazMig[,-7]



# get sample out of dwOpi matching the obs. of dwAll
tazAll_mig_sample <- sample_n(tazAll_mig, size = nrow(tazMig))
tazAll_opi_sample <- sample_n(tazAll_opi, size = nrow(tazOpi))


# construct flags for fine-tuning BERT
tazAll_opi_sample$opinion <- 0
tazOpi$opinion <- 1

tazAll_mig_sample$migration<- 0
tazMig$migration<- 1 

tazOpi$Date <- as.Date(tazOpi$Date)

taz_final_opi <- rbind(tazOpi, tazAll_opi_sample)
taz_final_mig <- rbind(tazMig, tazAll_mig_sample)

openxlsx::write.xlsx(taz_final_opi, "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/taz/taz_final_opi.xlsx", rowNames = F)
openxlsx::write.xlsx(taz_final_mig, "C:/Users/deanl/Desktop/MASTERARBEIT/BERT models/Migration/taz/taz_final_mig.xlsx", rowNames = F)

