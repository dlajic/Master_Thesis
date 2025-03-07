library(readxl)
library(dplyr)

augsburger_allgemeine_final_cli <- read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/augsburger_allgemeine_final_cli.xlsx")
BILD_final_cli <- read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/BILD_final_cli.xlsx")
dwFinal_Env <- read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/dwFinal_Env.xlsx")
ntv_final_cli <- read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/ntv_final_cli.xlsx")
rnd_final_cli <- read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/rnd_final_cli.xlsx")
spiegelFinal_Env <- read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/spiegelFinal_Env.xlsx")
sz_final_cli <- read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/sz_final_cli.xlsx")
tagesspiegel_final_cli <- read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/tagesspiegel_final_cli.xlsx")
taz_final_cli <- read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/taz_final_cli.xlsx")
tonline_final_cli <- read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/tonline_final_cli.xlsx")
WELT_final_cli <- read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/WELT_final_cli.xlsx")

BILD_final_cli$Lead <- NA
ntv_final_cli$Title2 <- NULL
sz_final_cli$Title2 <- NULL
taz_final_cli$Title2 <- NULL

ntv_final_cli <- ntv_final_cli %>%
  rename(Title = Title1)

sz_final_cli <- sz_final_cli %>%
  rename(Title = Title1)

spiegelFinal_Env <- spiegelFinal_Env %>%
  rename(link = Links)

dwFinal_Env <- dwFinal_Env %>%
  rename(link = Links)

tonline_final_cli <- tonline_final_cli %>%
  rename(link = Links)

articles_env_final <- rbind(augsburger_allgemeine_final_cli,
                            BILD_final_cli,
                            dwFinal_Env, 
                            ntv_final_cli,
                            rnd_final_cli,
                            spiegelFinal_Env,
                            sz_final_cli,
                            tagesspiegel_final_cli,
                            taz_final_cli,
                            tonline_final_cli,
                            WELT_final_cli)

openxlsx::write.xlsx(articles_env_final, "C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/Scraping/articles_env_final.xlsx", rowNames = F)
