library(pacman)

p_load(stringr,
       dplyr,
       tidyr,
       purrr,
       readxl,
       wordcloud,
       quanteda,
       quanteda.textstats,
       quanteda.textplots,
       quanteda.textmodels)


# Data preparation ----
#### count how often NERs appear in all real articles
# load dataset containing scraped articles limited to articles classified as environment/climate by our classifier 

news_categorised_final_all_NER <- read_excel("D:/MASTERARBEIT/Analysis/data/news_categorised_final_all_Mig_NER_BERTopic.xlsx")
#list_titles_up_to_wave_3 <- read_excel("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2022/Master thesis/News consumption_polarization/list_titles_up_to_wave_3_encoded.xlsx")

# filter out articles for which no entities exist
NER_column <- data.frame(Entities = news_categorised_final_all_NER %>%
                           filter(!grepl("\\[\\]", .$Entities)) %>% # remove emtpy ones
                           distinct(title, .keep_all = T) %>% # remove articles with duplicates to get the correct count of all words in all consumed articles 
                           #filter(title %in% list_titles_up_to_wave_3$title) %>%
                           select(Entities, is_alt_news, opinion))

colnames(NER_column) <- c("Entities", "is_alt_news", "opinion")


#replace "double" words 
# strategy: checking every word if an other word belongs to this
NER_column <- NER_column %>%
  mutate(Entities = gsub("\\<dpa\\>|\\<BILD\\>|\\<t-online\\>|\\<WELT\\>|\\<VBUI\\>|\\<SPIEGEL\\>|\\<FOCUS Online\\>|\\<Bild\\>|\\<Handelsblatt\\>|\\<Redaktionsnetzwerk Deutschland\\>|\\<Spiegel\\>|\\<taz\\>|\\<IPPEN.MEDIA\\>|\\<RND\\>|\\<ZEIT ONLINE\\>|\\<Welt\\>|\\<Rheinischen Post\\>|\\<AFP\\>|\\<afp\\>|\\<wetter.net\\>|\\<ntv\\>|\\<Merkur.de\\>|\\<Deutschlandfunk\\>|\\<Reuters\\>|\\<Funke-Mediengruppe\\>|\\<Deutschen Presse-Agentur\\>|\\.de| am Sonntag|\\<ka\\>|\\<Tagesspiegel\\>|\\<MOPO\\>|\\< AM SONNTAG\\>|\\<Luisamneubauer\\>|\\b-Zeitung\\b", "", Entities)) %>% # remove news sites since often the outlets are named at the end of an article
  mutate(Entities = gsub("\\<COVID\\>|\\<Covid-19\\>|\\<Covid\\>|\\<COVID-19\\>|\\<SARS\\>|\\<SARS-CoV-2\\>|\\<Sars-Cov-2\\>|\\<Coronavirus\\>|\\<Sars-CoV-2\\>|\\<Corona-Virus\\>", "Corona", Entities)) %>%
  mutate(Entities = gsub("\\<Fridays-for-Future-Bewegung\\>|\\<Friday for Future\\>|\\<Fridays for future\\>|\\<Fridays-for-Future\\>|\\<Friday For Future\\>|\\<Fridays for Future-Bewegung\\>|\\<FridaysforFuture\\>|\\<Fridays For Future\\>|\\<Fridays für Future\\>|\\<Fridays for Future Germany\\>|\\<Fridays for Future Deutschland\\>", "Fridays for Future", Entities)) %>%
  mutate(Entities = gsub("\\<Benlpekteup\\>|\\<GtlOeveupet\\>|\\<Blueeul\\>|\\<Pnupeplealelnua\\>|\\<Blueuelunluetlpleu\\>|\\<GtlOepeknle\\>", "", Entities)) %>%
  mutate(Entities = gsub("\\<Nordstream 2\\>|\\<Nordstream2\\>|\\<Nord-Stream-2-Pipeline\\>|\\<Nord-Stream 2\\>|\\<NordStream 2\\>|\\<Nordstream II\\>", "Nord Stream 2", Entities)) %>%
  mutate(Entities = gsub("\\<Zweite Weltkrieg\\>|\\<Zweiten Weltkrieges\\>|\\<Zweiten Weltkriegs\\>|\\<2. Weltkrieg\\>|\\<zweiten Weltkrieg\\>|\\<zweiten Weltkriegs\\>", "Zweiten Weltkrieg", Entities)) %>%
  mutate(Entities = gsub("\\<1. Weltkrieg\\>|\\<Ersten Weltkriegs\\>", "Ersten Weltkrieg", Entities)) %>%
  mutate(Entities = gsub("\\<Bitcoins\\>", "Bitcoin", Entities)) %>%
  mutate(Entities = gsub("\\<€\\>|\\<Euro\\.\\>|\\<Euros\\>", "Euro", Entities)) %>%
  mutate(Entities = gsub("\\<kenfm\\>", "KenFM", Entities)) %>%
  mutate(Entities = gsub("\\<climate change\\>|\\<Climate Change\\>", "Klimawandel", Entities)) %>%
  mutate(Entities = gsub("\\<Weltwirtschaftsforum\\>|\\<Weltwirtschaftsforums\\>|\\<Davoser Weltwirtschaftsforum\\>|\\<World Economic Forum\\>|\\<World Economy Forum\\>", "WEF", Entities)) %>%
  mutate(Entities = gsub("\\<RT Deutsch\\>|\\<rt de\\>|\\<RT\\>", "RT DE", Entities)) %>%
  mutate(Entities = gsub("\\<Deutschen Bundestag\\>|\\<Bundestags\\>|\\<Bundestages\\>|\\<Deutschen Bundestages\\>|\\<BundestagAmpel\\>|\\<Deutschen Bundestags\\>|\\<Deutsche Bundestag\\>|\\<Reichstag\\>|\\<Reichstags\\>", "Bundestag", Entities)) %>%
  mutate(Entities = gsub("\\<Europäischen Green Deals\\>|\\<Europäischen Green Deal\\>|\\<European Green Deals\\>|\\<European Green Deal\\>|\\<Green Deals\\>", "Green Deal", Entities)) %>%
  mutate(Entities = gsub("\\<Cum-ex-Skandal\\>|\\<Cum-ex-Affäre\\>|\\<Cum-Ex-Steuerskandal\\>|\\<Cum-Ex-Skandal\\>|\\<Cum-Ex-Komplex\\>|\\<Cum-Ex-Deals\\>|\\<Cum-Ex-Affäre\\>", "Cum-Ex", Entities)) %>%
  mutate(Entities = gsub("\\<Europäischen Kommission\\>", "Europäische Kommission", Entities)) %>%
  mutate(Entities = gsub("\\<Europäische Union-Kommission\\>", "Europäische Kommission", Entities)) %>%
  mutate(Entities = gsub("\\<Europas\\>", "Europa", Entities)) %>%
  mutate(Entities = gsub("\\<New York Citys\\>|\\<New Yorks\\>|\\<New York City\\>", "New York", Entities)) %>%
  mutate(Entities = gsub("\\<UN-Umweltprogramm\\>|\\<UN-Umweltprogramms\\>|\\<Unep\\>|\\<Uno-Umweltprogramm\\>", "UNEP", Entities)) %>%
  mutate(Entities = gsub("\\<Apples\\>", "Apple", Entities)) %>%
  mutate(Entities = gsub("\\<Clokes\\>", "Cloke", Entities)) %>%
  mutate(Entities = gsub("\\<Griechenlands\\>", "Griechenland", Entities)) %>%
  mutate(Entities = gsub("\\<Italiens\\>", "Italien", Entities)) %>%
  mutate(Entities = gsub("\\<Mercedes\\>|\\<Mercedes Benz\\>|\\<Mercedes-Benz AG\\>", "Mercedes-Benz", Entities)) %>%
  mutate(Entities = gsub("\\<Nasa\\>", "NASA", Entities)) %>%
  mutate(Entities = gsub("\\<Aldis\\>", "Aldi", Entities)) %>%
  mutate(Entities = gsub("\\<EDEKA\\>", "Edeka", Entities)) %>%
  mutate(Entities = gsub("\\<Meteorologischen Weltorganisation\\>|\\<Weltorganisation für Meteorologie\\>", "WMO", Entities)) %>%
  mutate(Entities = gsub("\\<Bundesamt für Bevölkerungsschutz und Katastrophenhilfe\\>|\\<Bundesamtes für Bevölkerungsschutz und Katastrophenhilfe\\>|\\<Bundesamts für Bevölkerungsschutz und Katastrophenhilfe\\>|\\<Bundesamt für Bevölkerungsschutz\\>|\\<Bundesamtes für Bevölkerung und Katastrophenschutz\\>|\\<Bundesamtes für Bevölkerungsschutz\\>", "BKI", Entities)) %>%
  mutate(Entities = gsub("\\<autolanddeutschland\\>|\\<autoland\\>|\\<Autoland\\>", "Autoland", Entities)) %>%
  mutate(Entities = gsub("\\<Deutschen Instituts für Wirtschaftsforschung\\>|\\<Deutschen Institut für Wirtschaftsforschung\\>|\\<Deutsche Institut für Wirtschaftsforschung\\>|\\<Deutschen Instituts fürWirtschaftsforschung\\>", "DIW", Entities)) %>%
  mutate(Entities = gsub("\\<Robert Koch-Institut\\>|\\<Robert-Koch-Institut\\>|\\<Robert Koch-Instituts\\>|\\<Robert-Koch-Insituts\\>|\\<Robert-Koch Institut\\>", "RKI", Entities)) %>%
  mutate(Entities = gsub("\\<Potsdam-Institut für Klimafolgenforschung\\>|\\<Potsdam-Instituts für Klimafolgenforschung\\>|\\<Potsdam Institut für Klimafolgenforschung\\>|\\<Potsdamer Institut für Klimafolgenforschung\\>|\\<Potsdam-Institut für Klimaforschung\\>|\\<Potsdamer Klimaforschungsinstituts\\>|\\<Potsdaminstitut für Klimafolgenforschung\\>", "PIK", Entities)) %>%
  mutate(Entities = gsub("\\<aldi\\>", "Aldi", Entities)) %>%
  mutate(Entities = gsub("\\<Siemens-AG\\>|\\<Siemens AG\\>", "Siemens", Entities)) %>%
  mutate(Entities = gsub("\\<Westdeutsche Rundfunk\\>|\\<Westdeutschen Rundfunk\\>", "WDR", Entities)) %>%
  mutate(Entities = gsub("\\<Norddeutsche Rundfunk\\>|\\<Norddeutschen Rundfunk\\>", "NDR", Entities)) %>%
  mutate(Entities = gsub("\\<Youtube\\>|\\<youtube\\>", "YouTube", Entities)) %>%
  mutate(Entities = gsub("\\<Instituts der Deutschen Wirtschaft\\>|\\<Institutes der Deutschen Wirtschaft\\>|\\<Institutes der deutschen Wirtschaft\\>|\\<Instituts der deutschen Wirtschaft\\>|\\<Institut der Deutschen Wirtschaft\\>|\\<Institut für Deutsche Wirtschaft\\>|\\<Institut für deutsche Wirtschaft\\>|\\<Institut der deutschen Wirtschaft\\>", "IW", Entities)) %>%
  mutate(Entities = gsub("\\<Daimler AG\\>|\\<Daimlers\\>", "Daimler", Entities)) %>%
  mutate(Entities = gsub("\\<Kreditanstalt für Wiraufbau\\>", "KfW", Entities)) %>%
  mutate(Entities = gsub("\\<Amazons\\>", "Amazon", Entities)) %>%
  mutate(Entities = gsub("\\<Russlands\\>", "Russland", Entities)) %>%
  mutate(Entities = gsub("\\<RT DE DE\\>|\\<RT DE\\>", "RT", Entities)) %>%
  mutate(Entities = gsub("\\<National Oceanic and Atmospheric Administration\\>|\\<National Oceanic und Atmospheric Administration\\>|\\<Nationale Ozean- und Atmosphärenbehö\\>", "NOAA", Entities)) %>%
  mutate(Entities = gsub("\\<ExtinctionRebellion\\>", "Extinction Rebellion", Entities)) %>%
  mutate(Entities = gsub("\\<Barack Obama\\>", "Obama", Entities)) %>%
  mutate(Entities = gsub("\\<Rewe\\>", "REWE", Entities)) %>%
  mutate(Entities = gsub("\\<Klaus Schwab\\>", "Schwab", Entities)) %>%
  mutate(Entities = gsub("\\<Peter Daszak\\>", "Daszak", Entities)) %>%
  mutate(Entities = gsub("\\<Marco Pino\\>", "Pino", Entities)) %>%
  mutate(Entities = gsub("\\<Paulina Brünger\\>|\\<Brüngers\\>", "Brünger", Entities)) %>%
  mutate(Entities = gsub("\\<Alexander Wallasch\\>", "Wallasch", Entities)) %>%
  mutate(Entities = gsub("\\<Boris Reitschuster\\>|\\<Reitschusters\\>", "Reitschuster", Entities)) %>%
  mutate(Entities = gsub("\\<Christine Lagarde\\>|\\<Lagardes\\>", "Lagarde", Entities)) %>%
  mutate(Entities = gsub("\\<Beatrix von Storch\\>|\\<von Storchs\\>", "von Storch", Entities)) %>%
  mutate(Entities = gsub("\\<Helmut Lussi\\>", "Lussi", Entities)) %>%
  mutate(Entities = gsub("\\<Jennifer Granholm\\>", "Granholm", Entities)) %>%
  mutate(Entities = gsub("\\<Ricarda Lang\\>", "Lang", Entities)) %>%
  mutate(Entities = gsub("\\<Heiko Maas\\>", "Maas", Entities)) %>%
  mutate(Entities = gsub("\\<Hans-Joachim Maaz\\>", "Maaz", Entities)) %>%
  mutate(Entities = gsub("\\<Stefan Rahmstorf\\>", "Rahmstorf", Entities)) %>%
  mutate(Entities = gsub("\\<Jair Bolsonaro\\>|\\<Bolsonaros\\>", "Bolsonaro", Entities)) %>%
  mutate(Entities = gsub("\\<Joschka Fischer\\>", "Fischer", Entities)) %>%
  mutate(Entities = gsub("\\<Fritz Vahrenholt\\>", "Vahrenholt", Entities)) %>%
  mutate(Entities = gsub("\\<Leonhard Birnbaum\\>", "Birnbaum", Entities)) %>%
  mutate(Entities = gsub("\\<Pauline Brünger\\>", "Brünger", Entities)) %>%
  mutate(Entities = gsub("\\<Tina Hassel\\>", "Hassel", Entities)) %>%
  mutate(Entities = gsub("\\<Michael Hüther\\>|\\<Hüthers\\>", "Hüther", Entities)) %>%
  mutate(Entities = gsub("\\<Steffen Seibert\\>", "Seibert", Entities)) %>%
  mutate(Entities = gsub("\\<Stefan Wolf\\>", "Wolf", Entities)) %>%
  mutate(Entities = gsub("\\<Josep Borrell\\>", "Borrell", Entities)) %>%
  mutate(Entities = gsub("\\<Jochen Flasbarth\\>", "Flasbarth", Entities)) %>%
  mutate(Entities = gsub("\\<Markus Blume\\>", "Blume", Entities)) %>%
  mutate(Entities = gsub("\\<Mojib Latif\\>", "Mojib Latif", Entities)) %>%
  mutate(Entities = gsub("\\<Markus Wipperfürth\\>", "Wipperfürth", Entities)) %>%
  mutate(Entities = gsub("\\<Pauline Brünger\\>", "Brünger", Entities)) %>%
  mutate(Entities = gsub("\\<Stefan Rahmstorf\\>", "Rahmstorf", Entities)) %>%
  mutate(Entities = gsub("\\<Boris Reitschuster\\>", "Reitschuster", Entities)) %>%
  mutate(Entities = gsub("\\<Thilo Sarrazin\\>", "Sarrazin", Entities)) %>%
  mutate(Entities = gsub("\\<Rasmus Andresen\\>", "Andresen", Entities)) %>%
  mutate(Entities = gsub("\\<Katarina Barley\\>", "Barley", Entities)) %>%
  mutate(Entities = gsub("\\<Christoph Bertram\\>", "Bertram", Entities)) %>%
  mutate(Entities = gsub("\\<Britta Haßelmann\\>", "Haßelmann", Entities)) %>%
  mutate(Entities = gsub("\\<Detlef Flintz\\>", "Flintz", Entities)) %>%
  mutate(Entities = gsub("\\<Jean Pütz\\>", "Pütz", Entities)) %>%
  mutate(Entities = gsub("\\<Helmut Kohl\\>", "Kohl", Entities)) %>%
  mutate(Entities = gsub("\\<Markus Jerger\\>", "Jerger", Entities)) %>%
  mutate(Entities = gsub("\\<Jörg Kachelmann\\>", "Kachelmann", Entities)) %>%
  mutate(Entities = gsub("\\<Jürgen Pföhler\\>", "Pföhler", Entities)) %>%
  mutate(Entities = gsub("\\<Konstantin von Notz\\>", "von Notz", Entities)) %>%
  mutate(Entities = gsub("\\<Wolfgang Kubicki\\>", "Kubicki", Entities)) %>%
  mutate(Entities = gsub("\\<Jochen Flasbarth\\>", "Flasbarth", Entities)) %>%
  mutate(Entities = gsub("\\<Hannah Cloke\\>", "Cloke", Entities)) %>%
  mutate(Entities = gsub("\\<Jürgen Todenhöfer\\>", "Todenhöfer", Entities)) %>%
  mutate(Entities = gsub("\\<Arvind Kejriwa\\>", "Kejriwa", Entities)) %>%
  mutate(Entities = gsub("\\<John F. Kennedy\\>", "Kennedy", Entities)) %>%
  mutate(Entities = gsub("\\<Dmitri Peskow\\>", "Peskow", Entities)) %>%
  mutate(Entities = gsub("\\<Nato\\>", "NATO", Entities)) %>%
  mutate(Entities = gsub("\\<Nabu\\>|\\<NABUs\\>|\\<Naturschutzbund Deutschland\\>|\\<Naturschutzbund\\>|\\<Naturschutzbund Deutschland e. V.\\>|\\<Naturschutzbunds Deutschland\\>", "NABU", Entities)) %>%
  mutate(Entities = gsub("\\<statistischem Bundesamt\\>|\\<Bundesamt für Statistik\\>|\\<Statistischen Bundesamts\\>|\\<Statistischen Bundesamtes\\>|\\<Statistischen Bundesamt\\>|\\<Statistischem Bundesamt\\>", "Statistische Bundesamt", Entities)) %>%
  mutate(Entities = gsub("\\<Bund für Umwelt und Naturschutz Deutschland\\>|\\<Bund für Umwelt und Naturschutz\\>|\\<Bund für Umwelt- und Naturschutz\\>|\\<Bund für Umwelt- und Naturschutz Deeutschland\\>", "BUND", Entities)) %>%
  mutate(Entities = gsub("\\<Technischen Hilfswerks\\>|\\<Technische Hilfswerk\\>|\\<Technischen Hilfswerk\\>|\\<Technischem Hilfswerk\\>|\\<Technisches Hilfswerk\\>", "THW", Entities)) %>%
  mutate(Entities = gsub("\\<DUH\\>|\\<Deutschen Umwelthilfe\\>|\\<Umwelthilfe\\>|\\<Deutscher Umwelthilfe\\>", "Deutsche Umwelthilfe", Entities)) %>%
  mutate(Entities = gsub("\\<Deutsche Deutsche Umwelthilfe\\>", "Deutsche Umwelthilfe", Entities)) %>%
  mutate(Entities = gsub("\\<foodwatch\\>|\\<Foodwatch Deutschland\\>|\\<foodwatch Deutschland\\>", "Foodwatch", Entities)) %>%
  mutate(Entities = gsub("\\<World Wide Fund For Nature\\>|\\<World Wildlife Fund\\>", "WWF", Entities)) %>%
  mutate(Entities = gsub("\\<World Health Organisation\\>|\\<Weltgesundheitsorganisation\\>", "WHO", Entities)) %>%
  mutate(Entities = gsub("\\<Vereinte Nationen\\>|\\<Vereinten Nationen\\>", "UN", Entities)) %>%
  mutate(Entities = gsub("\\<Allgemeinen Deutschen Automobil-Club\\>", "ADAC", Entities)) %>%
  mutate(Entities = gsub("\\<Ökotest\\>", "Öko-Test", Entities)) %>%
  mutate(Entities = gsub("\\<Volkswagen\\>|\\<Volkswagen AG\\>|\\<Volkswagen-Konzern\\>|\\<Volkswagen-Konzerns\\>|\\<Volkswagens\\>|\\<VWs\\>", "VW", Entities)) %>%
  mutate(Entities = gsub("\\<Intergovernmental Panel on Climate Change\\>|\\<Intergovernmental Panel on\\>|\\<klimarat\\>|\\<klimarats\\>|\\<klimarates\\>|\\<Klimarat\\>|\\<Uno-klimarates\\>|\\<Weltklimarat\\>|\\<Weltklimarats\\>|\\<Weltklimarates\\>|\\<Uno-Weltklimarates\\>|\\<Weltklimarats\\>", "IPCC", Entities)) %>%
  mutate(Entities = gsub("\\<Erneuerbare-Energien-Gesetz\\>|\\<Erneuerbare-Energien-Gesetzes\\>|\\<Erneuerbare Energien-Gesetz\\>|\\<Erneuerbare Energien Gesetz\\>|\\<Erneuerbaren-Energien-Gesetz\\>|\\<Erneuerbaren Energie Gesetz\\>|\\<Erneuerbaren Energien Gesetzes\\>", "EEG", Entities)) %>%
  mutate(Entities = gsub("\\<Super E10\\>|\\<E10\\>", "Super E10", Entities)) %>%
  mutate(Entities = gsub("\\<Großbritanniens\\>|\\<Vereinigten Königreich\\>|\\<Vereinigte Königreich\\>|\\<Vereinigten Königreichs\\>|\\<Vereinigten Königreiches\\>", "Großbritannien", Entities)) %>%
  mutate(Entities = gsub("\\<Europäische Zentralbank\\>|\\<Europäischen Zentralbank\\>", "EZB", Entities)) %>%
  mutate(Entities = gsub("\\<Coronakrise\\>", "Corona-Krise", Entities)) %>%
  mutate(Entities = gsub("\\<Coronapandemie\\>", "Corona-Pandemie", Entities)) %>%
  mutate(Entities = gsub("\\<US-Dollar\\>|\\<Dollar\\>", "US-Dollar", Entities)) %>%
  mutate(Entities = gsub("\\<Nobelpreise\\>|\\<Nobelpreisen\\>|\\<Nobelpreises\\>", "Nobelpreis", Entities)) %>%
  mutate(Entities = gsub("\\<Hartz-IV\\>|\\<Hartz 4\\>", "Hartz IV", Entities)) %>%
  mutate(Entities = gsub("\\<BMW Group\\>", "BMW", Entities)) %>%
  mutate(Entities = gsub("\\<Iter\\>", "ITER", Entities)) %>%
  mutate(Entities = gsub("\\<Lea\\>", "Lea Bonasera", Entities)) %>%
  mutate(Entities = gsub("\\<BioNTech\\>", "Biontech", Entities)) %>%
  mutate(Entities = gsub("\\<D-Mark\\>|\\<Mark\\>", "Deutsche Mark", Entities)) %>%
  mutate(Entities = gsub("\\<IAA Mobility\\>|\\<Internationale Automobilausstellung\\>|\\<Internationale Automobil-Ausstellung\\>|\\<Internationalen Automobil-Ausstellung\\>|\\<Internationalen Automobilausstellung\\>", "IAA", Entities)) %>%
  mutate(Entities = gsub("\\<Pariser Klimavertrags\\>|\\<Pariser Klimavertrages\\>|\\<Pariser Klima-Abkommen\\>|\\<Abkommen von Paris\\>|\\<Klimaschutzabkommen von Paris\\>|\\<Pariser Klimaabkommens\\>|\\<Paris-Abkommen\\>|\\<Pariser Klimavertrag\\>|\\<Pariser Abkommen\\>|\\<Pariser Klima-Abkommens\\>|\\<Klimaabkommen von Paris\\>|\\<Paris Klimaabkommen\\>|\\<Pariser Klimaabkommen\\>|\\<Pariser Klimaschutzabkommens\\>|\\<Pariser Abkommens\\>", "Pariser Klimaschutzabkommen", Entities)) %>%
  mutate(Entities = gsub("\\<Pariser Klimaschutzabkommens\\>", "Pariser Klimaschutzabkommen", Entities)) %>%
  mutate(Entities = gsub("(?i)Grüne\\/Die Grünen|\\<Grünen\\>|\\<Die Grüne\\>|\\<DIE GRÜNEN\\>|\\<BÜNDNIS 90/DIE GRÜNEN\\>|\\<Bündnis 90/Die Grüne\\>|\\<Bündnis 90\\>|\\<Bündnis 90 /\\>|\\<Bündnis 90 / Die Grüne\\>|\\<BÜNDNIS 90 / DIE GRÜNEN\\>|\\<Bündnis 90/\\>|\\<BÜNDNIS 90/\\>|\\<Bündnis 90/ Die Grüne\\>|\\<Bündnis90/Die Grüne\\>", "Grüne", Entities)) %>%
  mutate(Entities = gsub("\\<Sozialdemokraten\\>|\\<Sozialdemokratischen Partei Deutschland\\>", "SPD", Entities)) %>%
  mutate(Entities = gsub("\\<Bundes-FDP\\>|\\<fdp\\>|\\<Liberalen\\>", "FDP", Entities)) %>%
  mutate(Entities = gsub("\\<wiebke Winter\\>|\\<Wiebke Winter\\>", "Winter", Entities)) %>%
  mutate(Entities = gsub("\\<Lionel Messi\\>|\\<Lionel Messis\\>|\\<Messis\\>", "Messi", Entities)) %>%
  mutate(Entities = gsub("\\<Karsten Brandt\\>", "Brandt", Entities)) %>%
  mutate(Entities = gsub("\\<stroeer\\>", "Ströer", Entities)) %>%
  mutate(Entities = gsub("\\<CDU Deutschland\\>", "CDU", Entities)) %>%
  mutate(Entities = gsub("\\<AFD\\>|\\<Alternative für Deutschland\\>", "AfD", Entities)) %>%
  mutate(Entities = gsub("\\<Teslas\\>", "Tesla", Entities)) %>%
  mutate(Entities = gsub("\\<LINKEN\\>|\\<DIE LINKE\\>|\\<die Linke\\>|\\<Die Linke\\>|\\<Die Linken\\>|\\<Linken\\>", "Linke", Entities)) %>%
  mutate(Entities = gsub("\\<Deutsche Wetterdienstes\\>|\\<deutsche Wetterdienst\\>|\\<Deutscher Wetterdienst\\>|\\<Deutsche Wetterdienst\\>|\\<Deutschen Wetterdienstes\\>|\\<Deutschen Wetterdienst\\>|\\<Deutschem Wetterdienst\\>|\\<Deutschen Wetterdiensts\\>", "DWD", Entities)) %>%
  mutate(Entities = gsub("\\<Deutschen Bahn\\>", "Deutsche Bahn", Entities)) %>%
  mutate(Entities = gsub("\\<Europäischen Union\\>|\\<EU\\>", "Europäische Union", Entities)) %>%
  mutate(Entities = gsub("\\<Nordrhein-Westfahlen\\>|\\<NRWs\\>|\\<NRW\\>|\\<Nordrhein-Westfalens\\>", "Nordrhein-Westfalen", Entities)) %>%
  mutate(Entities = gsub("\\<Niederlanden\\>|\\<Holland\\>|\\<Hollands\\>", "Niederlande", Entities)) %>%
  mutate(Entities = gsub("\\<Vereinigten Staaten\\>|\\<Vereinigte Staaten\\>|\\<Vereinigen Staaten\\>", "USA", Entities)) %>%
  mutate(Entities = gsub("\\<Bayerns\\>", "Bayern", Entities)) %>% # Bayerns is not related to FC Bayern
  mutate(Entities = gsub("\\<Linkspartei\\>", "Linke", Entities)) %>%
  mutate(Entities = gsub("\\<Deutschland.\\>|\\<Deutschlands\\>|\\<Bundesrepublik Deutschland\\>", "Deutschland", Entities)) %>%
  mutate(Entities = gsub("\\<MÜNCHEN\\>|\\<Münchens\\>", "München", Entities)) %>%
  mutate(Entities = gsub("\\<Hamburgs\\>", "Hamburg", Entities)) %>%
  mutate(Entities = gsub("\\<Afrikas\\>", "Afrika", Entities)) %>%
  mutate(Entities = gsub("\\<Athens\\>", "Athen", Entities)) %>%
  mutate(Entities = gsub("\\<Sachsens\\>", "Sachsen", Entities)) %>%
  mutate(Entities = gsub("\\<Österreichs\\>", "Österreich", Entities)) %>%
  mutate(Entities = gsub("\\<Frankfurt am Main\\>|\\<Frankfurts\\>", "Frankfurt", Entities)) %>%
  mutate(Entities = gsub("\\<BERLIN\\>|\\<Berlins\\>", "Berlin", Entities)) %>%
  mutate(Entities = gsub("\\<Glasgows\\>|\\<GLASGOW\\>", "Glasgow", Entities)) %>%
  mutate(Entities = gsub("\\<Spaniens\\>", "Spanien", Entities)) %>%
  mutate(Entities = gsub("\\<Leipzigs\\>", "Leipzig", Entities)) %>%
  mutate(Entities = gsub("\\<Frankreichs\\>", "Frankreich", Entities)) %>%
  mutate(Entities = gsub("\\<Afghanistans\\>", "Afghanistan", Entities)) %>%
  mutate(Entities = gsub("\\<Volksrepublik China\\>|\\<Chinas\\>", "China", Entities)) %>%
  mutate(Entities = gsub("\\<Bad Neuenahr-Ahrweiler\\>|\\<Ahrweilers\\>", "Ahrweiler", Entities)) %>%
  mutate(Entities = gsub("\\<Ahrtals\\>", "Ahrtal", Entities)) %>%
  mutate(Entities = gsub("\\<Baden-Württembergs\\>|\\<Baden Württemberg\\>", "Baden-Württemberg", Entities)) %>%
  mutate(Entities = gsub("\\<Brandenburgs\\>", "Brandenburg", Entities)) %>%
  mutate(Entities = gsub("\\<Olaf Scholz\\>|\\<Scholz’\\>", "Scholz", Entities)) %>%
  mutate(Entities = gsub("\\<Angela Merkel\\>|\\<Merkel\\>|\\<Merkels\\>|\\<Angela Merkels\\>", "Merkel", Entities)) %>%
  mutate(Entities = gsub("\\<Laschet\\>|\\<Armin Laschet\\>|\\<Laschets\\>|\\<Armin Laschets\\>|\\<ArminLaschet\\>|\\<Laschet.\\>", "Laschet", Entities)) %>%
  mutate(Entities = gsub("\\<Annalena\\>|\\<Annalena Baerbock\\>|\\<Baerbock\\>|\\<Baerbocks\\>|\\<Annalena Baerbocks\\>|\\<A. Baerbock\\>", "Baerbock", Entities)) %>%
  mutate(Entities = gsub("\\<Robert Habeck\\>|\\<Habeck\\>|\\<Habecks\\>|\\<Habeck.\\>|\\<Rober Habeck\\>|\\<Robert Habecks\\>|\\<Habeck, \\>", "Habeck", Entities)) %>%
  mutate(Entities = gsub("\\<Christian Lindner\\>|\\<Lindner\\>|\\<Lindners\\>", "Lindner", Entities)) %>%
  mutate(Entities = gsub("\\<Luisa Neubauer\\>|\\<Neubauer\\>|\\<Neubauers\\>|\\<Luisa Neubauers\\>|\\<LUISA NEUBAUER\\>", "Neubauer", Entities)) %>%
  mutate(Entities = gsub("\\<Joe Biden\\>|\\<Biden\\>|\\<Joe Bidens\\>|\\<Bidens\\>", "Biden", Entities)) %>%
  mutate(Entities = gsub("\\<Markus Söder\\>|\\<Söder\\>|\\<Söders\\>|\\<Markus Söders\\>|\\<SÖDER\\>", "Söder", Entities)) %>%
  mutate(Entities = gsub("\\<Dominik Jung\\>", "Jung", Entities)) %>%
  mutate(Entities = gsub("\\<GretaThunberg\\>|\\<Greta Thunberg\\>|\\<Thunberg\\>|\\<Greta Thunbergs\\>|\\<Thunbergs\\>|\\<Greta\\>|\\<Gretas\\>", "Thunberg", Entities)) %>%
  mutate(Entities = gsub("\\<Andreas Scheuer\\>|\\<Scheuer\\>|\\<Scheuers\\>|\\<Andi Scheuer\\>", "Scheuer", Entities)) %>%
  mutate(Entities = gsub("\\<Svenja Schulze\\>|\\<Schulze\\>|\\<Schulzes\\>", "Schulze", Entities)) %>%
  mutate(Entities = gsub("\\<Peter Altmaier\\>|\\<Altmaier\\>|\\<Altmaiers\\>|\\<peter Altmaiers\\>", "Altmaier", Entities)) %>%
  mutate(Entities = gsub("\\<Merz,\\>|\\<Friedrich Merz\\>|\\<Merz\\>|\\<Friedrich Merz.\\>|\\<Merz, \\>", "Merz", Entities)) %>%
  mutate(Entities = gsub("\\<Axel Bojanowski\\>|\\<Bojanowskis\\>", "Bojanowski", Entities)) %>%
  mutate(Entities = gsub("\\<Markus Lanz\\>", "Lanz", Entities)) %>%
  mutate(Entities = gsub("\\<Nenas\\>", "Nena", Entities)) %>%
  mutate(Entities = gsub("\\<Donald Trump\\>|\\<Trump\\>|\\<Trumps\\>|\\<Donald Trumps\\>|\\<Trumps\\>|\\<Donald J. Trump\\>|\\<Donald J. Trumps\\>", "Trump", Entities)) %>%
  mutate(Entities = gsub("\\<Malu Dreyer\\>|\\<Dreyers\\>", "Dreyer", Entities)) %>%
  mutate(Entities = gsub("\\<Elon Musk\\>|\\<Elon Musks\\>|\\<Elon Musk.\\>|\\<Musk\\>|\\<Musk, \\>|\\<Musks\\>", "Musk", Entities)) %>%
  mutate(Entities = gsub("\\<Volker Wissing\\>|\\<Wissing\\>|\\<Wissings\\>", "Wissing", Entities)) %>%
  mutate(Entities = gsub("\\<Karl Lauterbach\\>|\\<Lauterbach\\>|\\<Lauterbachs\\>|\\<Karl_Lauterbach\\>", "Lauterbach", Entities)) %>%
  mutate(Entities = gsub("\\<Horst Seehofer\\>|\\<Seehofer\\>|\\<Horst Seehofers\\>|\\<Seehofers\\>", "Seehofer", Entities)) %>%
  mutate(Entities = gsub("\\<Ursula von der Leyen\\>|\\<von der Leyen\\>|\\<Von der Leyen\\>|\\<von der Leyens\\>|\\<Von der Leyens\\>|\\<Ursula von der Leyens\\>", "von der Leyen", Entities)) %>%
  mutate(Entities = gsub("\\<Frank-Walter Steinmeier\\>|\\<Steinmeier\\>|\\<Steinmeiers\\>|\\<Frank Walter Steinmeier\\>", "Steinmeier", Entities)) %>%
  mutate(Entities = gsub("\\<Niklas Boers\\>|\\<Boers\\>|\\<Boer\\>", "Boers", Entities)) %>%
  mutate(Entities = gsub("\\<Cem Özdemir\\>|\\<Özdemirs\\>", "Özdemir", Entities)) %>%
  mutate(Entities = gsub("\\<Palmers\\>|\\<Boris Palmer\\>", "Palmer", Entities)) %>%
  mutate(Entities = gsub("\\<Lars Klingbeil\\>|\\<Klingbeils\\>", "Klingbeil", Entities)) %>%
  mutate(Entities = gsub("\\<Saskia Esken\\>|\\<Eskens\\>", "Esken", Entities)) %>%
  mutate(Entities = gsub("\\<Queen Elizabeth II.\\>|\\<Queen\\>|\\<Queen Elizabeth\\>|\\<Elizabeth\\>|\\<Elizabeth II.\\>|\\<Elizabeths\\>|\\<Queen Elisabeth\\>|\\<Elisabeth II\\>", "Queen Elizabeth II", Entities)) %>%
  mutate(Entities = gsub("\\<Queen Elizabeth II II\\>", "Queen Elizabeth II", Entities)) %>%
  mutate(Entities = gsub("\\<Maischbergers\\>|\\<Sandra Maischberger\\>", "Maischberger", Entities)) %>%
  mutate(Entities = gsub("\\<Herbert Diess\\>", "Diess", Entities)) %>%
  mutate(Entities = gsub("\\<Michaela Koschak\\>", "", Entities)) %>% # weather moderator
  mutate(Entities = gsub("\\<Emmanuel Macron\\>|\\<Macrons\\>", "Macron", Entities)) %>%
  mutate(Entities = gsub("\\<Gerhard Schröder\\>|\\<Gerhard Schröders\\>|\\<Schröders\\>", "Schröder", Entities)) %>%
  mutate(Entities = gsub("\\<Jens Spahn\\>|\\<Spahns\\>", "Spahn", Entities)) %>%
  mutate(Entities = gsub("\\<Jürgen Trittin\\>|\\<Trittins\\>", "Trittin", Entities)) %>%
  mutate(Entities = gsub("\\<Meghan Markle\\>|\\<Meghans\\>", "Meghan", Entities)) %>%
  mutate(Entities = gsub("\\<Wladimir Putin\\>|\\<Putins\\>|\\<Wladimir Putins\\>", "Putin", Entities)) %>%
  mutate(Entities = gsub("\\<Boris Johnson\\>", "Johnson", Entities)) %>%
  mutate(Entities = gsub("\\<Winfried Kretschmann\\>", "Kretschmann", Entities)) %>%
  mutate(Entities = gsub("\\<Kevin Kühnert\\>|\\<Kühnerts\\>", "Kühnert", Entities)) %>%
  mutate(Entities = gsub("\\<Julia Klöckner\\>|\\<Klöckners\\>", "Klöckner", Entities)) %>%
  mutate(Entities = gsub("\\<Bill Gates\\>|\\<Gates\\>", "Gates", Entities)) %>%
  mutate(Entities = gsub("\\<Michael Kellner\\>", "Kellner", Entities)) %>%
  mutate(Entities = gsub("\\<Bundeszentrale für politische Bildung\\>", "bpb", Entities)) %>%
  mutate(Entities = gsub("\\<Leibniz-Institut für Wirtschaftsforschung\\>", "RWI", Entities)) %>%
  mutate(Entities = gsub("\\<Nuklearia e.V.\\>|\\<Nuklearia e. V.\\>|\\<Verein Nuklearia e.V.\\>|\\<AG Nuklearia\\>", "Nuklearia", Entities)) %>%
  mutate(Entities = gsub("\\<Alexander Lukaschenko\\>", "Lukaschenko", Entities)) %>%
  mutate(Entities = gsub("\\<Mario Draghi\\>", "Draghi", Entities)) %>%
  mutate(Entities = gsub("\\<Mathias Middelberg\\>", "Middelberg", Entities)) %>%
  mutate(Entities = gsub("\\<Narendra Modi\\>", "Modi", Entities)) %>%
  mutate(Entities = gsub("\\<Michael Theurer\\>", "Theurer", Entities)) %>%
  mutate(Entities = gsub("\\<Detlef Scheele\\>", "Scheele", Entities)) %>%
  mutate(Entities = gsub("\\<Steven Schuurman\\>", "Steven Schuurman", Entities)) %>%
  mutate(Entities = gsub("\\<Christine Wernicke\\>", "Wernicke", Entities)) %>%
  mutate(Entities = gsub("\\<Peter Ahmels\\>", "Ahmels", Entities)) %>%
  mutate(Entities = gsub("\\<Albert Scherr\\>", "Scherr", Entities)) %>%
  mutate(Entities = gsub("\\<Kjetil B. Alstadheim\\>", "Alstadheim", Entities)) %>%
  mutate(Entities = gsub("\\<Arvind Kejriwal\\>", "Kejriwal", Entities)) %>%
  mutate(Entities = gsub("\\<David Bendels\\>", "Bendels", Entities)) %>%
  mutate(Entities = gsub("\\<Bodo Ramelow\\>", "Ramelow", Entities)) %>%
  mutate(Entities = gsub("\\<Christian Kullmann\\>", "Kullmann", Entities)) %>%
  mutate(Entities = gsub("\\<Claudia Roth\\>", "Roth", Entities)) %>%
  mutate(Entities = gsub("\\<Stephan Grünewald\\>", "Grünewald", Entities)) %>%
  mutate(Entities = gsub("\\<Helmut Kohl\\>", "Kohl", Entities)) %>%
  mutate(Entities = gsub("\\<Hubert Aiwanger\\>", "Aiwanger", Entities)) %>%
  mutate(Entities = gsub("\\<Jürgen Pföhler\\>", "Pföhler", Entities)) %>%
  mutate(Entities = gsub("\\<Kwasi Kwarteng\\>", "Kwarteng", Entities)) %>%
  mutate(Entities = gsub("\\<Lars Feld\\>", "Feld", Entities)) %>%
  mutate(Entities = gsub("\\<Lenins\\>", "Lenin", Entities)) %>%
  mutate(Entities = gsub("\\<Leo DiCaprio\\>|\\<Leonardo DiCaprio\\>", "DiCaprio", Entities)) %>%
  mutate(Entities = gsub("\\<Leonhard Birnbaum\\>", "Birnbaum", Entities)) %>%
  mutate(Entities = gsub("\\<Linus Steinmetz\\>", "Steinmetz", Entities)) %>%
  mutate(Entities = gsub("\\<Tanja Loitz\\>", "Loitz", Entities)) %>%
  mutate(Entities = gsub("\\<Malena Ernman\\>", "Ernman", Entities)) %>%
  mutate(Entities = gsub("\\<Reinhard Sager\\>", "Sager", Entities)) %>%
  mutate(Entities = gsub("\\<Sebastian Kurz\\>", "Kurz", Entities)) %>%
  mutate(Entities = gsub("\\<Sigmar Gabriel\\>", "Gabriel", Entities)) %>%
  mutate(Entities = gsub("\\<Stephanie Töwe\\>", "Töwe", Entities)) %>%
  mutate(Entities = gsub("\\<Anton Hofreiter\\>", "Hofreiter", Entities)) %>%
  mutate(Entities = gsub("\\<Hans-Georg Maaßen\\>|\\<Maaßens\\>|\\<Hans Georg-Maaßen\\>|\\<Hans Georg Maaßen\\>|\\<Maaßen.\\>", "Maaßen", Entities)) %>%
  mutate(Entities = gsub("\\<António Guterres\\>|\\<Antonio Guterres\\>", "Guterres", Entities)) %>%
  mutate(Entities = gsub("\\<Xis\\>|\\<Xi\\>", "Xi Jinping", Entities)) %>%
  mutate(Entities = gsub("\\<Herbert Reul\\>", "Reul", Entities)) %>%
  mutate(Entities = gsub("\\<Frank Plasberg\\>", "Plasberg", Entities)) %>%
  mutate(Entities = gsub("\\<Franziska Giffey\\>|\\<Giffeys\\>|\\<Franziska Giffey.Es\\>", "Giffey", Entities)) %>%
  mutate(Entities = gsub("\\<Recep Tayyip Erdogan\\>|\\<Erdogans\\>", "Erdogan", Entities)) %>%
  mutate(Entities = gsub("\\<Ralph Brinkhaus\\>", "Brinkhaus", Entities)) %>%
  mutate(Entities = gsub("\\<Jeff Bezos\\>|\\<Jeffrey Bezos\\>", "Bezos", Entities)) %>%
  mutate(Entities = gsub("\\<Sahra Wagenknecht\\>|\\<Wagenknechts\\>|\\<Sarah Wagenknecht\\>", "Wagenknecht", Entities)) %>%
  mutate(Entities = gsub("\\<Claudia Kemfert\\>|\\<Kemfert\\>|\\<Kemferts\\>|\\<Claudie Kemfert\\>", "Kemfert", Entities)) %>%
  mutate(Entities = gsub("\\<Henning Jeschke\\>", "Jeschke", Entities)) %>%
  mutate(Entities = gsub("\\<Dietmar Bartsch\\>", "Bartsch", Entities)) %>%
  mutate(Entities = gsub("\\<Alice Weidel\\>|\\<Weidels\\>", "Weidel", Entities)) %>%
  mutate(Entities = gsub("\\<Kyriakos Mitsotakis\\>", "Mitsotakis", Entities)) %>%
  mutate(Entities = gsub("\\<Katrin Göring-Eckardt\\>|\\<Katrin Göring-Eckhardt\\>|\\<Göring\\>|\\<Göring-Eckardts\\>|\\<Göring-Eckart\\>", "Göring-Eckardt", Entities)) %>%
  mutate(Entities = gsub("\\<Norbert Walter-Borjans\\>|\\<Walter-Borjans\\>|\\<Norbert-Walter Borjans\\>|\\<Norbert Peter Walter-Borjans\\>|\\<Walter-Norbert Borjans\\>|\\<Walter Borjans\\>", "Borjans", Entities)) %>%
  mutate(Entities = gsub("\\<Laschet,\\>|\\<Laschet]\\>", "Laschet", Entities)) %>%
  mutate(Entities = gsub("\\<Elizabeth II,\\>", "Elizabeth II", Entities)) %>%
  mutate(Entities = gsub("\\<Elizabeth II \\>", "Elizabeth II", Entities)) %>%
  mutate(Entities = gsub("\\<Göring-Eckardt-Eckardt\\>", "Göring-Eckardt", Entities)) %>%
  mutate(Entities = gsub("\\<Xi Jinping Jinping\\>", "Xi Jinping", Entities)) %>%
  mutate(Entities = gsub("\\<Habeck,\\>", "Habeck", Entities)) %>%
  mutate(Entities = gsub("\\<Maaßen, \\>", "Maaßen", Entities)) %>%
  mutate(Entities = gsub("\\<Merz,\\>", "Merz", Entities)) %>%
  mutate(Entities = gsub("\\<Musk,\\>", "Musk", Entities)) %>%
  mutate(Entities = gsub("\\<Laschet \\>", "Laschet", Entities)) %>%
  mutate(Entities = gsub("\\<Habeck \\>", "Habeck", Entities)) %>%
  mutate(Entities = gsub("\\<Merz \\>", "Merz", Entities)) %>%
  mutate(Entities = gsub("\\<Musk \\>", "Musk", Entities)) %>%
  mutate(Entities = gsub("\\<Weltklimakonferenz\\>|\\<Cop 26\\>|\\<COP26-Gipfel\\>|\\<Cop26\\>|\\<COP26-Klimakonferenz\\>|\\<COP 26\\>", "COP26", Entities)) %>%
  mutate(Entities = gsub("\\<Atlantic Meridional Overturning Circulation\\>|\\<Atlantische Umwälzströmung\\>|\\<Atlantischen Meridionalen Umwälzströmung\\>", "AMOC", Entities)) %>%
  mutate(Entities = gsub("\\<Bundesumweltministerium\\>|\\<Bundesumweltministeriums\\>|\\<Bundesumweltamt\\>|\\<Umwelt-Bundesamt\\>|\\<Bundesumweltamtes\\>|\\<umweltbundesamt\\>|\\<Umweltbundesamts\\>|\\<Umweltbundesamtes\\>|\\<Berliner Umweltbundesamts\\>|\\<UBA\\>", "Umweltbundesamt", Entities)) %>%
  mutate(Entities = gsub("\\<Australiens\\>","Australien", Entities)) %>%
  mutate(Entities = gsub("\\<Siziliens\\>","Sizilien", Entities)) %>%
  mutate(Entities = gsub("\\<Indiens\\>","Indien", Entities)) %>%
  mutate(Entities = gsub("\\<Polens\\>","Polen", Entities)) %>%
  mutate(Entities = gsub("\\<Hessens\\>","Hessen", Entities)) %>%
  mutate(Entities = gsub("\\<Ukraines\\>","Ukraine", Entities)) %>%
  mutate(Entities = gsub("\\<Rostocks\\>","Rostock", Entities)) %>%
  mutate(Entities = gsub("\\<Roms\\>","Rom", Entities)) %>%
  mutate(Entities = gsub("\\<Kaliforniens\\>", "Kalifornien", Entities)) %>%
  mutate(Entities = gsub("\\<Euböas\\>","Euböa", Entities)) %>%
  mutate(Entities = gsub("\\<Rheins\\>","Rhein", Entities)) %>%
  mutate(Entities = gsub("\\<Mittelmeers\\>","Mittelmeer", Entities)) %>%
  mutate(Entities = gsub("\\<Antarktis\\>", "Arktis", Entities)) %>%
  mutate(Entities = gsub("\\<Niedersachsens\\>","Niedersachsen", Entities)) %>%
  mutate(Entities = gsub("\\<Schleswig-Holsteins\\>","Schleswig-Holstein", Entities)) %>%
  mutate(Entities = gsub("\\<Belgiens\\>","Belgien", Entities)) %>%
  mutate(Entities = gsub("\\<Mecklenburg-Vorpommerns\\>","Mecklenburg-Vorpommern", Entities)) %>%
  mutate(Entities = gsub("\\<Londons\\>","London", Entities)) %>%
  mutate(Entities = gsub("\\<Asiens\\>","Asien", Entities)) %>%
  mutate(Entities = gsub("\\<Twitter.Mehr\\>","Twitter", Entities)) %>%
  mutate(Entities = gsub("\\<Greifswalds\\>","Greifswald", Entities)) %>%
  mutate(Entities = gsub("\\<Dresdens\\>","Dresden", Entities)) %>%
  mutate(Entities = gsub("\\<Sachsen-Anhalts\\>|\\<SACHSEN-ANHALT\\>|\\<sachsen-anhalt\\>","Sachsen-Anhalt", Entities)) %>%
  mutate(Entities = gsub("\\<Japans\\>","Japan", Entities)) %>%
  mutate(Entities = gsub("\\<Brasiliens\\>","Brasilien", Entities)) %>%
  mutate(Entities = gsub("\\<Schwedens\\>","Schweden", Entities)) %>%
  mutate(Entities = gsub("\\<Kanadas\\>","Kanada", Entities)) %>%
  mutate(Entities = gsub("\\<Israels\\>","Israel", Entities)) %>%
  mutate(Entities = gsub("\\<Mallorcas\\>|\\<MALLORCA\\>","Mallorca", Entities)) %>%
  mutate(Entities = gsub("\\<Thüringens\\>","Thüringen", Entities)) %>%
  mutate(Entities = gsub("\\<Mitteleuropas\\>","Mitteleuropa", Entities)) %>%
  mutate(Entities = gsub("\\<Mittelmeerraums\\>|\\<Mittelmeerraumes\\>|\\<Mittelmeerraum.\\>","Mittelmeer", Entities)) %>%
  mutate(Entities = gsub("\\<Washingtons\\>|\\<Washington D.C\\>|\\<Washington D.C.\\>|\\<Washington DC\\>","Washington", Entities)) %>%
  mutate(Entities = gsub("\\<Dänemarks\\>","Dänemark", Entities)) %>%
  mutate(Entities = gsub("\\<Norwegens\\>","Norwegen", Entities)) %>%
  mutate(Entities = gsub("\\<Atlantiks\\>","Atlantik", Entities)) %>%
  mutate(Entities = gsub("\\<Amerikas\\>","Amerika", Entities)) %>%
  mutate(Entities = gsub("\\<Grönlands\\>","Grönland", Entities)) %>%
  mutate(Entities = gsub("\\<Louisianas\\>","Louisiana", Entities)) %>%
  mutate(Entities = gsub("\\<Westdeutschlands\\>", "Westdeutschland", Entities)) %>%
  mutate(Entities = gsub("\\<Englands\\>","England", Entities)) %>%
  mutate(Entities = gsub("\\<Brandenburger Tors\\>","Brandenburger Tor", Entities)) %>%
  mutate(Entities = gsub("\\<Südamerikas\\>","Südamerika", Entities)) %>%
  mutate(Entities = gsub("\\<Südeuropas\\>","Südeuropa", Entities)) %>%
  mutate(Entities = gsub("\\<Neuseelands\\>","Neuseeland", Entities)) %>%
  mutate(Entities = gsub("\\<Saarlandes\\>|\\<Saarlands\\>","Saarland", Entities)) %>%
  mutate(Entities = gsub("\\<Blessem\\>", "Erftstadt-Blessem", Entities)) %>%
  mutate(Entities = gsub("\\<Erftstadt-Erftstadt-Blessem\\>", "Erftstadt-Blessem", Entities)) %>%
  mutate(Entities = gsub("\\<Kabuls\\>","Kabul", Entities)) %>%
  mutate(Entities = gsub("\\<Tübingens\\>","Tübingen", Entities)) %>%
  mutate(Entities = gsub("\\<Bremens\\>","Bremen", Entities)) %>%
  mutate(Entities = gsub("\\<Sardiniens\\>","Sardinien", Entities)) %>%
  mutate(Entities = gsub("\\<Ägyptens\\>","Ägypten", Entities)) %>%
  mutate(Entities = gsub("\\<Potsdams\\>","Potsdam", Entities)) %>%
  mutate(Entities = gsub("\\<Schuld an der Ahr\\>", "Schuld", Entities)) %>%
  mutate(Entities = gsub("\\<Freiburgs\\>","Freiburg", Entities)) %>%
  mutate(Entities = gsub("\\<Stockholms\\>","Stockholm", Entities)) %>%
  mutate(Entities = gsub("\\<Tschechische Republik\\>|\\<Tschechiens\\>", "Tschechien", Entities)) %>%
  mutate(Entities = gsub("\\<Ungarns\\>","Ungarn", Entities)) %>%
  mutate(Entities = gsub("\\<Äthiopiens\\>","Äthiopien", Entities)) %>%
  mutate(Entities = gsub("\\<Mexikos\\>","Mexiko", Entities)) %>%
  mutate(Entities = gsub("\\<Hambacher Forsts\\>|\\<Hambacher Forstes\\>|\\<Hambacher Wald\\>|\\<Hambacher Waldes\\>|\\<Hambacher Walds\\>", "Hambacher Forst", Entities)) %>%
  mutate(Entities = gsub("\\<La Palmas\\>","La Palma", Entities)) %>%
  mutate(Entities = gsub("\\<Nordamerikas\\>","Nordamerika", Entities)) %>%
  mutate(Entities = gsub("\\<Nürnbergs\\>","Nürnberg", Entities)) %>%
  mutate(Entities = gsub("\\<Dortmunds\\>","Dortmund", Entities)) %>%
  mutate(Entities = gsub("\\<Osteuropas\\>","Osteuropa", Entities)) %>%
  mutate(Entities = gsub("\\<Musk \\>", "Musk", Entities))


# Function to extract the entities and the words
extract_entity_und_word <- function(text) {

  separated_entity_words <- str_match_all(text, "\\b([A-Z]+): ([^']+)")
  
  entity <- separated_entity_words[[1]][,2]
  words <- separated_entity_words[[1]][,3]
  data.frame(entity = unlist(entity), words = unlist(words), stringsAsFactors = FALSE)
  
}



# all articles
# Extract entities and words from the NER_column for each row
prep_count_NER_all <- NER_column %>%
  mutate(prep_count_NER_all = map(Entities, extract_entity_und_word)) %>%
  pull(prep_count_NER_all)

# convert list to df
list_to_df_all <- do.call(rbind, prep_count_NER_all)
  
# Count the words by entity
count_NER_all <- list_to_df_all %>%
  group_by(entity, words) %>%
  summarise(count = n()) %>%
  ungroup()
  
# convert results in new clearer df
final_count_NER_all <- pivot_wider(count_NER_all, names_from = entity, values_from = count, values_fill = 0)

# get frequencies of entities over all articles
sum(final_count_NER_all$PER > 0)
sum(final_count_NER_all$LOC > 0)
sum(final_count_NER_all$ORG > 0)
sum(final_count_NER_all$MISC > 0)

# word clouds over all articles
# filter data for different entities
loc_words_all <- final_count_NER_all %>%
  select(words, LOC) %>%
  filter(LOC > 0) %>%
  arrange(desc(LOC))

per_words_all <- final_count_NER_all %>%
  select(words, PER) %>%
  filter(PER > 0) %>%
  arrange(desc(PER))

misc_words_all <- final_count_NER_all %>%
  select(words, MISC) %>%
  filter(MISC > 0) %>%
  arrange(desc(MISC))

org_words_all <- final_count_NER_all %>%
  select(words, ORG) %>%
  filter(ORG > 0) %>%
  arrange(desc(ORG))

# for reproducible word clouds
set.seed(123)

wordcloud(words = loc_words_all$words, freq = loc_words$LOC, min.freq = 50, max.words = 150, random.order=FALSE, colors=brewer.pal(7, "Paired")[2:7])
wordcloud(words = per_words_all$words, freq = per_words$PER, min.freq = 50, max.words = 150, random.order=FALSE, colors=brewer.pal(7, "Paired")[2:7])
wordcloud(words = misc_words_all$words, freq = misc_words$MISC, min.freq = 20, max.words = 150, random.order=FALSE, colors=brewer.pal(7, "Paired")[2:7])
wordcloud(words = org_words_all$words, freq = org_words$ORG, min.freq = 50, max.words = 150, random.order=FALSE, colors=brewer.pal(7, "Paired")[2:7])



# alternative news articles
# Extract entities and words from the NER_column for each row
NER_alt <- NER_column %>%
  filter(is_alt_news == 1)

# Extract entities and words from the NER_column for each row
prep_count_NER_alt <- NER_alt %>%
  mutate(prep_count_NER_alt = map(Entities, extract_entity_und_word)) %>%
  pull(prep_count_NER_alt)

# convert list to df
list_to_df_alt <- do.call(rbind, prep_count_NER_alt)

# Count the words by entity
count_NER_alt <- list_to_df_alt %>%
  group_by(entity, words) %>%
  summarise(count = n()) %>%
  ungroup()

# convert results in new clearer df
final_count_NER_alt <- pivot_wider(count_NER_alt, names_from = entity, values_from = count, values_fill = 0)

# filter data for different entities
loc_words_alt <- final_count_NER_alt %>%
  select(words, LOC) %>%
  filter(LOC > 0) %>%
  arrange(desc(LOC))

per_words_alt <- final_count_NER_alt %>%
  select(words, PER) %>%
  filter(PER > 0) %>%
  arrange(desc(PER))

misc_words_alt <- final_count_NER_alt %>%
  select(words, MISC) %>%
  filter(MISC > 0) %>%
  arrange(desc(MISC))

org_words_alt <- final_count_NER_alt %>%
  select(words, ORG) %>%
  filter(ORG > 0) %>%
  arrange(desc(ORG))


# conventional news articles
# Extract entities and words from the NER_column for each row
NER_conv <- NER_column %>%
  filter(is_alt_news == 0)

# Extract entities and words from the NER_column for each row
prep_count_NER_conv <- NER_conv %>%
  mutate(prep_count_NER_conv = map(Entities, extract_entity_und_word)) %>%
  pull(prep_count_NER_conv)

# convert list to df
list_to_df_conv <- do.call(rbind, prep_count_NER_conv)

# Count the words by entity
count_NER_conv <- list_to_df_conv %>%
  group_by(entity, words) %>%
  summarise(count = n()) %>%
  ungroup()

# convert results in new clearer df
final_count_NER_conv <- pivot_wider(count_NER_conv, names_from = entity, values_from = count, values_fill = 0)

# filter data for different entities
loc_words_conv <- final_count_NER_conv %>%
  select(words, LOC) %>%
  filter(LOC > 0) %>%
  arrange(desc(LOC))

per_words_conv <- final_count_NER_conv %>%
  select(words, PER) %>%
  filter(PER > 0) %>%
  arrange(desc(PER))

misc_words_conv <- final_count_NER_conv %>%
  select(words, MISC) %>%
  filter(MISC > 0) %>%
  arrange(desc(MISC))

org_words_conv <- final_count_NER_conv %>%
  select(words, ORG) %>%
  filter(ORG > 0) %>%
  arrange(desc(ORG))


## word clouds ----

# for reproducible word clouds
set.seed(123)

#showing plots alt vs. normal views
# loc
par(mfrow = c(1, 2))  # Divides the graphic area into 1 row and 2 columns
wordcloud(words = loc_words_alt$words, freq = loc_words_alt$LOC, 
          min.freq = 10, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(7, "Paired")[2:7])

wordcloud(words = loc_words_conv$words, freq = loc_words_conv$LOC, 
          min.freq = 10, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(7, "Paired")[2:7])

#Persons
wordcloud(words = per_words_alt$words, freq = per_words_alt$PER, 
          min.freq = 10, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(7, "Paired")[2:7])

wordcloud(words = per_words_conv$words, freq = per_words_conv$PER, 
          min.freq = 10, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(7, "Paired")[2:7])

#Misc
wordcloud(words = misc_words_alt$words, freq = misc_words_alt$MISC, 
          min.freq = 10, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(7, "Paired")[2:7])

wordcloud(words = misc_words_conv$words, freq = misc_words_conv$MISC, 
          min.freq = 10, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(7, "Paired")[2:7])

#Organisations
wordcloud(words = org_words_alt$words, freq = org_words_alt$ORG, 
          min.freq = 10, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(7, "Paired")[2:7])

wordcloud(words = org_words_conv$words, freq = org_words_conv$ORG, 
          min.freq = 10, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(7, "Paired")[2:7])





## opinion vs. descriptive ----
### opinion ----
# Extract entities and words from the NER_column for each row
NER_opi <- NER_column %>%
  filter(opinion == 1)

prep_count_NER_opi <- NER_opi %>%
  mutate(prep_count_NER_df = map(Entities, extract_entity_und_word)) %>%
  pull(prep_count_NER_df)

# convert list to df
list_to_df_opi <- do.call(rbind, prep_count_NER_opi)

# Count the words by entity
count_NER_opi <- list_to_df_opi %>%
  group_by(entity, words) %>%
  summarise(count = n()) %>%
  ungroup()

# convert results in new clearer df
final_count_NER_opi <- pivot_wider(count_NER_opi, names_from = entity, values_from = count, values_fill = 0)

# filter data for different entities
loc_words_opi <- final_count_NER_opi %>%
  select(words, LOC) %>%
  filter(LOC > 0) %>%
  arrange(desc(LOC))

per_words_opi <- final_count_NER_opi %>%
  select(words, PER) %>%
  filter(PER > 0) %>%
  arrange(desc(PER))

misc_words_opi <- final_count_NER_opi %>%
  select(words, MISC) %>%
  filter(MISC > 0) %>%
  arrange(desc(MISC))

org_words_opi <- final_count_NER_opi %>%
  select(words, ORG) %>%
  filter(ORG > 0) %>%
  arrange(desc(ORG))



### descriptive ----
NER_desc <- NER_column %>%
  filter(opinion == 0)

prep_count_NER_desc <- NER_desc %>%
  mutate(prep_count_NER_desc = map(Entities, extract_entity_und_word)) %>%
  pull(prep_count_NER_desc)

# convert list to df
list_to_df_desc <- do.call(rbind, prep_count_NER_desc)

# Count the words by entity
count_NER_desc <- list_to_df_desc %>%
  group_by(entity, words) %>%
  summarise(count = n()) %>%
  ungroup()

# convert results in new clearer df
final_count_NER_desc <- pivot_wider(count_NER_desc, names_from = entity, values_from = count, values_fill = 0)

# filter data for different entities
loc_words_desc <- final_count_NER_desc %>%
  select(words, LOC) %>%
  filter(LOC > 0) %>%
  arrange(desc(LOC))

per_words_desc <- final_count_NER_desc %>%
  select(words, PER) %>%
  filter(PER > 0) %>%
  arrange(desc(PER))

misc_words_desc <- final_count_NER_desc %>%
  select(words, MISC) %>%
  filter(MISC > 0) %>%
  arrange(desc(MISC))

org_words_desc <- final_count_NER_desc %>%
  select(words, ORG) %>%
  filter(ORG > 0) %>%
  arrange(desc(ORG))



## word clouds ----

# for reproducible word clouds
set.seed(123)

#showing plots alt vs. normal views
# loc
par(mfrow = c(1, 2))  # Divides the graphic area into 1 row and 2 columns
wordcloud(words = loc_words_opi$words, freq = loc_words_opi$LOC, 
          min.freq = 10, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(7, "Paired")[2:7])

wordcloud(words = loc_words_desc$words, freq = loc_words_desc$LOC, 
          min.freq = 50, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(7, "Paired")[2:7])

#Persons
wordcloud(words = per_words_opi$words, freq = per_words_opi$PER, 
          min.freq = 10, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(7, "Paired")[2:7])

wordcloud(words = per_words_desc$words, freq = per_words_desc$PER, 
          min.freq = 50, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(7, "Paired")[2:7])

#Misc
wordcloud(words = misc_words_opi$words, freq = misc_words_opi$MISC, 
          min.freq = 10, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(7, "Paired")[2:7])

wordcloud(words = misc_words_desc$words, freq = misc_words_desc$MISC, 
          min.freq = 50, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(7, "Paired")[2:7])

#Organisations
wordcloud(words = org_words_opi$words, freq = org_words_opi$ORG, 
          min.freq = 10, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(7, "Paired")[2:7])

wordcloud(words = org_words_desc$words, freq = org_words_desc$ORG, 
          min.freq = 50, max.words = 150, random.order = FALSE, 
          colors = brewer.pal(7, "Paired")[2:7])















# keyness comparisons text ----

#just distinct articles
news_categorised_final_all_NER <- news_categorised_final_all_NER %>%
  distinct(title, .keep_all = TRUE)

# Definiere die Stopwörter, die ignoriert werden sollen
custom_stopwords <- c("ple", "pel", "nup","\\*","peu","tel","–","lu","pep","lpl","en","ulekl","oll","ep","lo","ent","uuu",">","plek","elue","enek","\\+")

# Convert the text data into a quanteda document
doc <- corpus(news_categorised_final_all_NER$text)

# Pre-processing der Textdaten, einschließlich Entfernung von Stopwörtern
doc_processed <- doc %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(stopwords("german")) %>%
  tokens_remove(custom_stopwords)  # Manuelle Entfernung der individuellen Stopwörter

## keyness comparison for conventional news vs alternative news ----
# Erstelle die Document-Feature-Matrix (dfm)
dfm_conv_alt <- dfm(doc_processed, groups = news_categorised_final_all_NER$is_alt_news)

# Trimme die dfm, um leere Dokumente zu entfernen
dfm_conv_alt <- dfm_trim(dfm_conv_alt)

# Überprüfe, ob die dfm mindestens einen nicht-null Wert enthält
if (sum(dfm_conv_alt) == 0) {
  stop("Die dfm enthält keine nicht-null Werte. Überprüfe deine Stopwort-Entfernungslogik.")
}

# Perform Keyness-Analyse
keyness_results_conv_alt <- textstat_keyness(dfm_conv_alt)

# plot results
textplot_keyness(keyness_results_conv_alt)


## keyness comparison for opinion news vs despriptive news ----
# Erstelle die Document-Feature-Matrix (dfm)
dfm_conv_opi <- dfm(doc_processed, groups = news_categorised_final_all_NER$opinion)

# Trimme die dfm, um leere Dokumente zu entfernen
dfm_conv_opi <- dfm_trim(dfm_conv_opi)

# Überprüfe, ob die dfm mindestens einen nicht-null Wert enthält
if (sum(dfm_conv_opi) == 0) {
  stop("Die dfm enthält keine nicht-null Werte. Überprüfe deine Stopwort-Entfernungslogik.")
}

# Perform Keyness-Analyse
keyness_results_conv_opi <- textstat_keyness(dfm_conv_opi)

# plot results
textplot_keyness(keyness_results_conv_opi)





# keyness comparisons Entities ----

# Definiere die Stopwörter, die ignoriert werden sollen
custom_stopwords <- c("dts","gru","bri","te","\\''","")

# Convert the text data into a quanteda document
doc <- corpus(news_categorised_final_all_NER$Entities)

# Pre-processing der Textdaten, einschließlich Entfernung von Stopwörtern
doc_processed <- doc %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(custom_stopwords)  # Manuelle Entfernung der individuellen Stopwörter


## keyness comparison for conventional news vs alternative news ----

# Erstelle die Document-Feature-Matrix (dfm)
dfm_conv_alt <- dfm(doc_processed, groups = news_categorised_final_all_NER$is_alt_news)

# Trimme die dfm, um leere Dokumente zu entfernen
dfm_conv_alt <- dfm_trim(dfm_conv_alt)

# Überprüfe, ob die dfm mindestens einen nicht-null Wert enthält
if (sum(dfm_conv_alt) == 0) {
  stop("Die dfm enthält keine nicht-null Werte. Überprüfe deine Stopwort-Entfernungslogik.")
}

# Perform Keyness-Analyse
keyness_results_conv_alt <- textstat_keyness(dfm_conv_alt)

# plot results
textplot_keyness(keyness_results_conv_alt)




## keyness comparison for opinion news vs despriptive news ----
# Erstelle die Document-Feature-Matrix (dfm)
dfm_conv_opi <- dfm(doc_processed, groups = news_categorised_final_all_NER$opinion)

# Trimme die dfm, um leere Dokumente zu entfernen
dfm_conv_opi <- dfm_trim(dfm_conv_opi)

# Überprüfe, ob die dfm mindestens einen nicht-null Wert enthält
if (sum(dfm_conv_opi) == 0) {
  stop("Die dfm enthält keine nicht-null Werte. Überprüfe deine Stopwort-Entfernungslogik.")
}

# Perform Keyness-Analyse
keyness_results_conv_opi <- textstat_keyness(dfm_conv_opi)

# plot results
textplot_keyness(keyness_results_conv_opi)





## keyness comparison for descriptive news vs opinion news ----

# Convert the text data into a quanteda document
dfm_descr_opi <- dfm(news_categorised_final_all_NER$Entities)

# Convert to a dfm object
dfm_descr_opi <- dfm(dfm_descr_opi, groups = news_categorised_final_all_NER$opinion)

# Perform pre-processing of the text data (e.g. tokenization, removal of stop words, etc.)
keyness_results_descr_opi <- textstat_keyness(dfm_descr_opi)

# plot results
textplot_keyness(keyness_results_descr_opi)

