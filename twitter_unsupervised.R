# uploads ####
options(scipen = 999)
library(gdata)
# for crawling Twitter data 
# library(academictwitteR)
# library(rtweet)
library(corpus)
library(quanteda)
library(udpipe)
library(stopwords)
library(corpustools)
# # for data wrangling. very helpful for preparing nodes and edges data
library(dplyr)
library(tidyverse)
library(lubridate)
library(tokenizers)
library(tidytext)
library(stringi)
library(readtext)
library(parallel)
library(stringr)
# library(stm)


# 
 setwd("//watt.bss.bremen-social-sciences.de/rpaolillo/R/twitter_unsupervised/act_pol")
# 
#  
# Old collected new keywords ####
# 
#  get_all_tweets(
#    query = c("kfw kredite"),
#    exact_phrase = T,
#    start_tweets = "2020-01-01T00:00:00z",
#    end_tweets = "2021-12-31T23:59:00z",
#    lang = c("de"),
#    is_retweet = NULL,
#    n = 500000,
#    data_path = "de_kfwkredite",
#    bearer_token = bearer_token
#  )
#  
#  # new keywords kfw-sonderprogram, kfw-kredite, soforthilfe only corona
#  
#   df_kn <- bind_tweets(data_path = "de_kfwsonder", output_format = "tidy")
#  df_kd <- bind_tweets(data_path = "de_kfwkredite", output_format = "tidy")
# 
#  
#  df <- bind_tweets(data_path = "test_tweet",output_format = "tidy")
#  
#  # added new keywords
#  load("df_tw.Rdata") # all tweets on policies from everyone 2020-2021 both countries
#  
 # df_kd$country <- "Germany"
 # df_kn$country <- "Germany"
#  
 #  df_tw <- df_tw[,-33]
 # df_tw <- rbind(df_tw,df_kd,df_kn)
# save(df_tw,file="df_tw.Rdata")
# 

 # df_sof <- bind_tweets(data_path = "de_sofor", output_format = "tidy") # added sofort
 # df_sof <- df_sof[!duplicated(df_sof$tweet_id),]
 # df_sof$country <- "Germany"
 # df_sof$month <- strftime(df_sof$created_at,format = "%m-%y")
 # df_sof <- df_sof %>% filter((sourcetweet_type != "retweeted") %>% replace_na(TRUE))
 # save(df_sof,file="df_sof.Rdata")
 
 # load("df_tw.Rdata")
 # load("df_sof.Rdata")
 # 
 # 
 # keywords_de <- read.xls("keywords.xls",sheet=2)
 # k_de <-  paste(do.call(c, keywords_de),collapse="|")
 # df_sof <- df_sof %>% filter(str_detect(df_sof$text, paste0("corona|covid|konjunkt|sars",k_de)))
 # 
 # df_tw <- rbind(df_tw,df_sof)
 # df_tw <- df_tw[!duplicated(df_tw$tweet_id),]
 # save(df_tw,file="df_tw.Rdata")
 # sof_act <- df_sof %>% filter(created_at >= "2020-01-01") %>% filter(tweet_id %in% df_tot1821$tweet_id) to check if match only with soforthilfe keywords
 
 
# Back source to compile and clean corpus ####

# load("df_tw.Rdata") # all tweets on policies from everyone 2020-2021 both countries
# load("df_tot1821.Rdata") # all tweets TA, TU, POL 2018-2021
# # #
#  # list of TA,TU, POL in each country
# load("de_pol.Rdata")
# load("de_ta.Rdata")
# load("de_tu.Rdata")
# load("it_pol.Rdata")
# load("it_ta.Rdata")
# load("it_tu.Rdata")
# 
# df_tw <- df_tw[!duplicated(df_tw$tweet_id),]
# df_tw <- df_tw %>% filter((sourcetweet_type != "retweeted") %>% replace_na(TRUE))
# df_actpol <- df_tot1821 %>% filter(tweet_id %in% df_tw$tweet_id)
# df_actpol <- df_actpol[!duplicated(df_actpol$tweet_id),]
# df_actpol$actor <- ifelse((df_actpol$user_name %in% de_pol) | (df_actpol$user_name %in% it_pol),
#                           "POL",
#                           ifelse((df_actpol$user_name %in% de_ta) | (df_actpol$user_name %in% it_ta),
#                                  "TA",
#                                  "TU"))
# 
# # add variable on dataset
# df_actpol$date <- as_date(df_actpol$created_at)
# df_actpol$datenum <- as.integer(df_actpol$date)
# df_actpol$ID <- rownames(df_actpol)
# # 
# # # for general cleaning
# spc <- pull(read.csv("special_characters.csv", encoding = "UTF-8"),1) # delete emoticon, keep ASCII
# spct <- paste(spc,collapse="")
# chrmt <- paste0("[^\x01-\x7F",spct,"]")
# 
# df_actpol$text <- tolower(df_actpol$text)
# df_actpol$text <- gsub("_","-",df_actpol$text)
# df_actpol$text <- gsub("#"," ",df_actpol$text)
# df_actpol$text <- gsub(chrmt," ", df_actpol$text)  # for not ASCII (emoticon), keeps normal characthers + spct list
# df_actpol$text <- gsub("&amp;", " ", df_actpol$text) # remove how ";" is translated
# df_actpol$text <- gsub("\\s+", " ", df_actpol$text)
# df_actpol$text <- gsub("\n", " ",  df_actpol$text)
# df_actpol$text <- gsub("\t", " ",  df_actpol$text)
# df_actpol$text <- gsub("'", " ",  df_actpol$text)
# df_actpol$text <- gsub("’", " ",  df_actpol$text)
# 
# # # annotation keywords policies Germany ####
# # 
# pattern_umlaut <- c("ueberbrueck","überbrueck","ueberbrück","grossbürg","großbuerg","grossbürg",
#                     "buergschaf")
# replace_umlaut <- c("überbrück","überbrück","überbrück","großbürg","großbürg","großbürg","bürgschaf")
# names(replace_umlaut) <- pattern_umlaut
# 
# df_actpol[df_actpol$lang=="de",]$text <- str_replace_all(
#   df_actpol[df_actpol$lang=="de",]$text,regex(replace_umlaut,ignore_case = T))
# 
# # reshape some words roots (regex of composed)
# pattern_keywords <- c("überbrückungshilfeprogramms","überbrückungshilfeprogramm")
# replace_keywords <- c("überbrückungshilfe","überbrückungshilfe")
# names(replace_keywords) <- pattern_keywords
# 
# df_actpol[df_actpol$lang=="de",]$text <- str_replace_all(
#   df_actpol[df_actpol$lang=="de",]$text,regex(replace_keywords,ignore_case = T))
# 
# # # lemmatization policies and other words
# df_de <- read.xls("annotation_de_keywords.xls")
# results_de = list()
# for (i in df_de$lem_de_keywords) {
#   b_de <- df_de[df_de$lem_de_keywords==i,]
#   x_de <- paste0("\\b",b_de$tok_de_keywords,"\\b")
#   xa_de <- paste0(x_de,collapse="|")
#   results_de[[i]] = xa_de
# }
# 
# df_actpol[df_actpol$lang=="de",]$text <- stri_replace_all_regex(
#   df_actpol[df_actpol$lang=="de",]$text,
#                               pattern=results_de,
#                               replacement=names(results_de),
#                               vectorize_all=FALSE)
# # 
# # 
# # # annotation keywords policies Italy ####
# # # lemmatization policies and other words
# df_it <- read.xls("annotation_it_keywords.xls")
# results_it = list()
# for (i in df_it$lem_it_keywords) {
#   b_it <- df_it[df_it$lem_it_keywords==i,]
#   x_it <- paste0("\\b",b_it$tok_it_keywords,"\\b")
#   xa_it <- paste0(x_it,collapse="|")
#   results_it[[i]] = xa_it
# }
# 
# df_actpol[df_actpol$lang=="it",]$text <- stri_replace_all_regex(
#   df_actpol[df_actpol$lang=="it",]$text,
#   pattern=results_it,
#   replacement=names(results_it),
#   vectorize_all=FALSE)
# # 
# # # 
# save(df_actpol, file= "df_actpol.Rdata")

# Cleaning doubles using cosine similarity ####
# library(quanteda.textstats)
# load("dfm_deap.Rdata")
# load("dfm_itap.Rdata")
# 
# dfm_df <- dfm_subset(dfm_deap,user_username == "BMWK")
# 
# de_sim <- textstat_simil(dfm_df,margin="documents",
#                          method="cosine",min_simil = 0.95)
# as.list(de_sim, diag = FALSE)
# 
# df_actpol <- df_actpol %>% filter(!(user_username == "VDMAonline" &
#  str_detect(text,"der maschinenbau im einsatz gegen die corona-krise: vdma-mitgliedsfirmen handeln.|der maschinenbau im einsatz gegen die corona-krise: vdma-mitgliedsfirmen und partner handeln."))) %>%
#  filter(!(user_username == "BMWK" &
#  str_detect(text,"corona-ticker|corona- ticker|corona-blog|jetzt live|unternehmen stehen vor großen herausforderungen. mit unterstützungsprogrammen versuchen wir zu helfen, schnell und unbürokratisch.|zur überbrückungshilfe3 wurden bisher rund|servicetweet|zur überbrückungshilfe3 wurden bisher gut"))) %>%
#   filter(!(user_username == "MISE_GOV" & str_detect(text,"covid curaitalia: 50 milioni per le aziende che vogliono produrre dispositivi medici e di protezione individuale incentivi per sostenere aziende italiane che vogliono ampliare o riconvertire propria attività"))) %>%
#   filter(!(user_username == "FABI_News" & str_detect(text,"speciale covid"))) %>%
#   filter(!(user_username == "BMWK" & date == "2021-08-18")) %>%
# filter(!(user_username == "BMWK" & date == "2021-07-16")) %>%
# filter(!(user_username == "BMWK" & str_detect(text,"anträge auf neustarthilfen plus natürlicher personen für den förderzeitraum juli bis september"))) %>%
# filter(!(user_username == "BMWK" & date == "2021-06-18")) %>%
# filter(!(user_username == "BMWK" & str_detect(text,"alle aktuellen coronahilfen, die direkte zuschüsse beinhalten, befinden sich mittlerweile im sogenannten fachverfahren"))) %>%
# filter(!(user_username == "BMWK" & date == "2021-04-09"))
# 
# 
# save(df_actpol,file="df_actpol.Rdata")




# Bigrams, trigrams (not used) ####

#load("df_actpol.Rdata")
# 
# 
# bigrams_tx_de <- df_actpol %>% filter(lang=="de") %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
# bigrams_tx_de %>% dplyr::count(bigram, sort = TRUE)
# bigrams_separate_de <- bigrams_tx_de %>% separate(bigram,c("word1","word2"),sep=" ")
# bigrams_filtered_de <- bigrams_separate_de %>%
#   filter(!word1 %in% stopwords_de) %>%
#   filter(!word2 %in% stopwords_de)
# bigrams_filtered_de %>%  dplyr::count(word1, word2, sort = TRUE)
# bigrams_united_de <- bigrams_filtered_de %>% unite(bigram, word1, word2, sep = " ")
# bigrams_united_de <- bigrams_united_de$bigram
# #
# write.csv(bigrams_united_de,"bigrams_de.csv")
# # #
# # # #
# trigrams_tx_de <- df_actpol %>% filter(lang=="de") %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)
# trigrams_tx_de %>% dplyr::count(trigram, sort = TRUE)
# trigrams_separate_de <- trigrams_tx_de %>% separate(trigram,c("word1","word2","word3"),sep=" ")
# trigrams_filtered_de <- trigrams_separate_de %>%
#   filter(!word1 %in% stopwords_de) %>%
#   filter(!word2 %in% stopwords_de) %>%
#   filter(!word3 %in% stopwords_de)
# trigrams_filtered_de %>%  dplyr::count(word1, word2,word3, sort = TRUE)
# trigrams_united_de <- trigrams_filtered_de %>% unite(trigram, word1, word2,word3, sep = " ")
# trigrams_united_de <- trigrams_united_de$trigram
# 
#   write.csv(trigrams_united_de,"trigrams_de.csv")
# #

# Italy
# load("df_it.Rdata")
#
# bigrams_tx_it <- df_actpol %>% filter(lang=="it") %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
# bigrams_tx_it %>% dplyr::count(bigram, sort = TRUE)
# bigrams_separate_it <- bigrams_tx_it %>% separate(bigram,c("word1","word2"),sep=" ")
# bigrams_filtered_it <- bigrams_separate_it %>%
#   filter(!word1 %in% stopwords_it) %>%
#   filter(!word2 %in% stopwords_it)
# bigrams_filtered_it %>%  dplyr::count(word1, word2, sort = TRUE)
# bigrams_united_it <- bigrams_filtered_it %>% unite(bigram, word1, word2, sep = " ")
# bigrams_united_it <- bigrams_united_it$bigram
# 
# write.csv(bigrams_united_it,"bigrams_it.csv")
# # #
# # # #
# trigrams_tx_it <- df_actpol %>% filter(lang=="it") %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)
# trigrams_tx_it %>% dplyr::count(trigram, sort = TRUE)
# trigrams_separate_it <- trigrams_tx_it %>% separate(trigram,c("word1","word2","word3"),sep=" ")
# trigrams_filtered_it <- trigrams_separate_it %>%
#   filter(!word1 %in% stopwords_it) %>%
#   filter(!word2 %in% stopwords_it) %>%
#   filter(!word3 %in% stopwords_it)
# trigrams_filtered_it %>%  dplyr::count(word1, word2,word3, sort = TRUE)
# trigrams_united_it <- trigrams_filtered_it %>% unite(trigram, word1, word2,word3, sep = " ")
# trigrams_united_it <- trigrams_united_it$trigram
# # #
# write.csv(trigrams_united_it, "trigrams_it.csv")


# selection from actors
 
load("df_actpol.Rdata")
 load("df_tot1821.Rdata")
 
 spc <- pull(read.csv("special_characters.csv", encoding = "UTF-8"),1) # delete emoticon, keep ASCII
 spct <- paste(spc,collapse="")
 chrmt <- paste0("[^\x01-\x7F",spct,"]")

 df_tot1821$text <- tolower(df_tot1821$text)
 df_tot1821$text <- gsub("_","-",df_tot1821$text)
 df_tot1821$text <- gsub("#"," ",df_tot1821$text)
 df_tot1821$text <- gsub(chrmt," ", df_tot1821$text)  # for not ASCII (emoticon), keeps normal characthers + spct list
 df_tot1821$text <- gsub("&amp;", " ", df_tot1821$text) # remove how ";" is translated
 df_tot1821$text <- gsub("\\s+", " ", df_tot1821$text)
 df_tot1821$text <- gsub("\n", " ",  df_tot1821$text)
 df_tot1821$text <- gsub("\t", " ",  df_tot1821$text)
 df_tot1821$text <- gsub("'", " ",  df_tot1821$text)
 df_tot1821$text <- gsub("’", " ",  df_tot1821$text)
"%nin%" <- Negate("%in%")

 # # # annotation keywords policies Germany ####
 # # 
 pattern_umlaut <- c("ueberbrueck","überbrueck","ueberbrück","grossbürg","großbuerg","grossbürg",
                     "buergschaf")
 replace_umlaut <- c("überbrück","überbrück","überbrück","großbürg","großbürg","großbürg","bürgschaf")
 names(replace_umlaut) <- pattern_umlaut

 df_tot1821[df_tot1821$lang=="de",]$text <- str_replace_all(
   df_tot1821[df_tot1821$lang=="de",]$text,regex(replace_umlaut,ignore_case = T))

 # reshape some words roots (regex of composed)
 pattern_keywords <- c("überbrückungshilfeprogramms","überbrückungshilfeprogramm")
 replace_keywords <- c("überbrückungshilfe","überbrückungshilfe")
 names(replace_keywords) <- pattern_keywords

 df_tot1821[df_tot1821$lang=="de",]$text <- str_replace_all(
   df_tot1821[df_tot1821$lang=="de",]$text,regex(replace_keywords,ignore_case = T))

 
 
de_key <- read.xls("keywords.xls",sheet = 2)[,1]
de_key <- tolower(de_key)
de_key <- paste0(de_key,collapse="|")
de_key <- paste0("\\b",de_key,"\\b",collapse="|")
de_key <- paste0("(?<![:alpha:])",de_key,"(?![:alpha:])")

df_selctde <- df_tot1821 %>% filter(created_at >= "2020-01-01" & lang == "de") %>%
  filter(str_detect(text, regex(de_key, ignore_case=TRUE)))

df_desof <- df_tot1821 %>% filter(created_at >= "2020-01-01" & lang == "de") %>%
  filter(str_detect(text,(
    regex("(?<![:alpha:])\\bsoforthilfe\\b|\\bsoforthilfen\\b|\\bsofort-hilfe\\b|\\bsofort-hilfen\\b(?![:alpha:])",
                                ignore_case=TRUE))))
df_desof <- df_desof %>%  filter(str_detect(text,
  regex("(?<![:alpha:])\\bkonjunktur\\b|\\bcovid\\b|\\bcorona\\b|\\bsars\\b|\\bcoronakrise\\b(?![:alpha:])", 
                                  ignore_case=TRUE)))


df_check <- df_tot1821 %>% filter(created_at >= "2020-01-01" & lang == "de") 
# %>%  filter(str_detect(text, (regex("coronahilf", 
#                                  ignore_case=TRUE))))

unique(str_extract_all(df_check$text, regex("\\bkonjunktur\\-?[:alnum:]*\\-?[:alnum:]*", 
                                     ignore_case = TRUE)))


# df_dekfw <- df_selctde %>% filter(str_detect(text,
#     regex("(?<![:alpha:])\\bkonjunktur\\b|\\bcovid\\b|\\bcorona\\b|
#   \\bcoronavirusde\\b|\\bcoronavirus\\b|\\bcovid19de\\b|\\bcovid19\\b|
#   \\bcovid2019\\b|\\bcovid2019de\\b|\\bcoronapandemie\\b|\\bcoronaviruspandemie\\b|
#          \\bsars\\b|\\bcoronakrise\\b(?![:alpha:])", 
#                                                    ignore_case=TRUE)))

df_sofcheck <- df_desof %>% filter(str_detect(text,
   regex("(?<![:alpha:])\\bcovid\\b|\\bcorona\\b|\\bcoronavirusde\\b|\\bcoronavirus\\b|\\bcovid19de\\b|\\bcovid19\\b|\\bcovid2019\\b|\\bcovid2019de\\b|\\bcoronapandemie\\b|\\bcoronaviruspandemie\\b|\\bsars\\b|\\bcoronakrise\\b|\\bcoronavirusdeutschland\\b(?![:alpha:])",
                                                           ignore_case=TRUE)))


df_kfwcheckreg <- df_dekfw %>% filter(str_detect(text,
   regex("(?<![:alpha:])\\bpandemie\\b(?![:alpha:])", 
                                                   ignore_case=TRUE)))

df_sofdouble <- df_sofcheckreg %>% filter(text %nin% df_sofcheck$text)



de_actTOslc <- df_actpol[df_actpol$lang=="de",] %>% filter(tweet_id %nin% df_selctde$tweet_id)



df_selctde <- rbind(df_selctde,df_desof)

"%nin%" <- Negate("%in%")
de_slcTOact <- df_selctde %>% 
  filter(tweet_id %nin% df_actpol[df_actpol$lang=="de",]$tweet_id)

# italy

it_key <- read.xls("keywords.xls",sheet = 3)[,1]
it_key <- paste0(it_key,collapse="|")
it_key <- tolower(it_key)
df_selctit <- df_tot1821 %>% filter(created_at >= "2020-01-01" & lang == "it") %>%
  filter(str_detect(text,(regex(it_key,ignore_case=TRUE))))

df_select <- rbind(df_selctde,df_selctit)

"%nin%" <- Negate("%in%")
df_extra <- df_actpol %>% filter(tweet_id %nin% df_select$tweet_id)

df_select <- df_select %>% filter(!(user_username == "VDMAonline" &
                                       str_detect(text,"der maschinenbau im einsatz gegen die corona-krise: vdma-mitgliedsfirmen handeln.|der maschinenbau im einsatz gegen die corona-krise: vdma-mitgliedsfirmen und partner handeln."))) %>%
                                       filter(!(user_username == "BMWK" &
                                       str_detect(text,"corona-ticker|corona- ticker|corona-blog|jetzt live|unternehmen stehen vor großen herausforderungen. mit unterstützungsprogrammen versuchen wir zu helfen, schnell und unbürokratisch.|zur überbrückungshilfe3 wurden bisher rund|servicetweet|zur überbrückungshilfe3 wurden bisher gut"))) %>%
                                        filter(!(user_username == "MISE_GOV" & str_detect(text,"covid curaitalia: 50 milioni per le aziende che vogliono produrre dispositivi medici e di protezione individuale incentivi per sostenere aziende italiane che vogliono ampliare o riconvertire propria attività"))) %>%
                                        filter(!(user_username == "FABI_News" & str_detect(text,"speciale covid"))) %>%
                                        filter(!(user_username == "BMWK" & date == "2021-08-18")) %>%
                                      filter(!(user_username == "BMWK" & date == "2021-07-16")) %>%
                                      filter(!(user_username == "BMWK" & str_detect(text,"anträge auf neustarthilfen plus natürlicher personen für den förderzeitraum juli bis september"))) %>%
                                      filter(!(user_username == "BMWK" & date == "2021-06-18")) %>%
                                      filter(!(user_username == "BMWK" & str_detect(text,"alle aktuellen coronahilfen, die direkte zuschüsse beinhalten, befinden sich mittlerweile im sogenannten fachverfahren"))) %>%
                                      filter(!(user_username == "BMWK" & date == "2021-04-09"))

# # Corpus compilation ####
# 
#  load("df_actpol.Rdata")
# # #
# corpus_df <- corpus(df_actpol)
# docnames(corpus_df) <- paste0(corpus_df$user_username,"_",corpus_df$date,"_", corpus_df$ID)
# # #
# # # # Germany
# corpus_deap <- corpus_subset(corpus_df, lang == "de")
# save(corpus_deap,file="corpus_deap.Rdata")
# # # bg_de <- pull(read.csv("bigrams_de.csv", encoding = "UTF-8"),2)
# # # trg_de <- pull(read.csv("trigrams_de.csv", encoding = "UTF-8"),2)
#  load("annotation_de.Rdata")
# #
# compound_de <- c("in höhe","passende antragsverfahren finden interessierte",
#                  "bundesweit einheitliche plattform","gemeinsame pressemitteilung","mehr informationen",
#                  "bmf finden","mehr informationen","bitte informieren","weitere informationen")
# rem_de <- c("http*","@*", "bmas-redaktion","|","bitte_informieren","weitere_informationen","€",
#             "rund", "in_höhe","finden","plattform","details","informationen","unserer_webseite",
#             "infos","bitte","seit_beginn","gerne","prozent",
#               "passende_antragsverfahren_finden_interessierte"," + ","gibt",
#             "grundsätzlich","ende","fällen","bundesweit_einheitliche_plattform",
#             "gemeinsame_pressemitteilung","bmf_finden","mehr_informationen", "t.co")
# #
# # # dtm
# dfm_deap <-  tokens(corpus_deap,
#                     remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>%
#   tokens_compound(phrase(c(compound_de))) %>%
#   tokens_remove(c(stopwords("de"),stopwords_de,rem_de,
#                   annotation_de[annotation_de$upos == "ADP",]$token,
#                   annotation_de[annotation_de$upos == "PRON",]$token,
#                   annotation_de[annotation_de$upos == "AUX",]$token,
#                   annotation_de[annotation_de$upos == "DET",]$token,
#                   annotation_de[annotation_de$upos == "SCONJ",]$token)) %>%
#   dfm()
# #
#  save(dfm_deap,file="dfm_deap.Rdata")
# # #
# # # # Italy
# corpus_itap <- corpus_subset(corpus_df, lang == "it")
# save(corpus_itap,file="corpus_itap.Rdata")
# # # bg_it <- pull(read.csv("bigrams_it.csv", encoding = "UTF-8"),2)
# # # trg_it <- pull(read.csv("trigrams_it.csv", encoding = "UTF-8"),2)
#  load("annotation_it.Rdata")
# #
# rem_it <- c("http*","@*","faq","ciglcisluil","SPECIALE","più","già","fra","ufficio_stampa_fabi","ufficio_stampa","asstel",
#             "a_cura_dell_ufficio","qui"," | ")
# #
# compound_it <- phrase(c("ministro giovannini","cassa integrazione","cassa lavoro", "settore terziario", "settore turistico",
#                         "a cura dell ufficio","ufficio stampa fabi","ufficio stampa",
#                         "unione europea", "impresa zombie","imprese zombie", "azienda zombie",
#                         "aziende zombie","salva italia","decreto ristori","decreti ristoro",
#                         "lotto di vaccini","istituto di credito","instituti di credito",
#                         "piano nazionale","sistema produttivo","sistemi produttivi",
#                         "rapporto finanziario","made in italy","tassi di interesse", "tasso di interesse",
#                         "tassi d'interesse","tasso d'interesse","interessi economici","interesse economico",
#                         "ambito finanziario","termini di resa","termini di interesse","ambito lavorativo",
#                         "camera di commercio","ministero dello sviluppo economico","rapport eu",
#                         "stati finanziari","stati di bilancio","stato di emergenza",
#                         "Ferretti Group","Gruppo Ferretti","Ferretti Yachts","occasione per il sud",
#                         "blocco dei licenziamenti","blocco licenziamenti", "blocco_dei_licenziamenti",
#                         "blocco_licenziamenti","risorsa digitale","risorse digitali","dare priorità",
#                         "cura italia","cura-italia","Cura Italia","Cura-Italia","CURA ITALIA",
#                         "sistema economico","sistemi economici","sistema politico","marco granelli",
#                         "marco granelli","sistemi politici","corona virus",
#                         "biblioteca nazionale","osservatorio nazionale","giornata nazionale della salute",
#                         "territorio nazionale","interesse nazionale","servizio sanitario nazionale","lutto nazionale",
#                         "museo nazionale","livello nazionale","corpo nazionale dei vigili del fuoco",
#                         "decreto legge","disegno di legge","banca d italia","banca centrale europea",
#                         "misure urgenti","misure straordinarie","misure per imprese","misure di sostegno",
#                         "milioni di euro","miliardi di euro","reddito di cittadinanza","sostegn* al reddito"))
# 
# dfm_itap <-  tokens(corpus_itap,
#                     remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>%
#   tokens_compound(phrase(c(compound_it))) %>%
#   tokens_remove(c(stopwords("it"),stopwords_it,rem_it,
#                   annotation_it[annotation_it$upos == "ADP",]$token,
#                   annotation_it[annotation_it$upos == "PRON",]$token,
#                   annotation_it[annotation_it$upos == "AUX",]$token,
#                   annotation_it[annotation_it$upos == "DET",]$token,
#                   annotation_it[annotation_it$upos == "SCONJ",]$token)) %>%
#   dfm()
# 
# save(dfm_itap,file="dfm_itap.Rdata")
# 



# K diagnostic #####
library(stm)
library(dplyr)
library(furrr)
library(ggplot2)
library(purrr)
library(tidyr)
plan(multicore)
#
load("dfm_itap.Rdata")
dfm_df <- dfm_itap
lg <- unique(levels(as.factor(dfm_df$country)))

# evaluate K
#
# k_model <- data_frame(K = c(7,10,13,15,16,20,25,30,35,40)) %>%
#    mutate(topic_model = future_map(K, ~stm(dfm_df, K = .,
#           prevalence = ~ (actor * date), verbose = TRUE)))
# save(k_model,file=paste0("k_model_",lg,".Rdata"))
# #
# heldout <- make.heldout(dfm_df)
# save(heldout,file=paste0("heldout_",lg,".Rdata"))

# k result computed
# load(paste0("k_model_",lg,".Rdata"))
# load(paste0("heldout_",lg,".Rdata"))
# # # #
# k_result <- k_model %>%
#   mutate(exclusivity = map(topic_model, exclusivity),
#          semantic_coherence = map(topic_model, semanticCoherence, dfm_df),
#          eval_heldout = map(topic_model, eval.heldout, heldout$missing),
#          residual = map(topic_model, checkResiduals, dfm_df),
#          bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
#          lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
#          lbound = bound + lfact,
#          iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
# save(k_result,file= paste0("k_result_",lg,".Rdata"))


# plotting k results
# 
#  load(paste0("k_result_",lg,".Rdata"))
# k_result %>%
#   transmute(K,
#             `Lower bound` = lbound,
#             Residuals = map_dbl(residual, "dispersion"),
#             `Semantic coherence` = map_dbl(semantic_coherence, mean),
#             `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
#   gather(Metric, Value, -K) %>%
#   ggplot(aes(K, Value, color = Metric)) +
#   geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
#   scale_x_continuous(breaks = c(7,10,13,15,16,20,25,30,35,40),guide = guide_axis(angle = -45) ) + #n.dodge = 2,
#   facet_wrap(~Metric, scales = "free_y") +
#   labs(x = "K (number of topics)",
#        y = NULL,
#        title = "Model diagnostics by number of topics",
#        subtitle = paste0(lg," = actor * time")) + theme_bw()
# ggsave(file=paste0("Kdiag_",lg,".jpg"),width = 5, height= 5)
# # 
# # 
# k_result %>%
#   select(K, exclusivity, semantic_coherence) %>%
#   filter(K %in% c(10,15,20,25,30,35)) %>%
#   unnest() %>%
#   mutate(K = as.factor(K)) %>%
#   ggplot(aes(semantic_coherence, exclusivity, color = K)) +
#   geom_point(size = 2, alpha = 0.7) +
#   labs(x = "Semantic coherence",
#        y = "Exclusivity",
#        title = "Exclusivity and semantic coherence",
#        subtitle = paste0(lg," = actor * time")) + theme_bw()
# ggsave(file=paste0("Kcoher_",lg,".jpg"),height = 5,width = 5)

# TPM  ####
# # 
# library(quanteda)
# library(stm)
# library(parallel)
# library(ggplot2)
# library(dplyr)
# library(tidyverse)
# library(tidyr)
# 
# 
# load("dfm_itap.Rdata")
# dfm_df <- dfm_itap
# stm_df <- quanteda::convert(dfm_df, to = "stm")
# lg <- unique(levels(as.factor(dfm_df$country)))
# #
# cl <- makeCluster(30)
# t_list <- c(10,15,20,25,30,35)
# 
# tpc <- function(nb){
#   stm_m <- stm::stm(stm_df$documents,stm_df$vocab,K = nb, prevalence = ~ (actor * date),
#                     data = stm_df$meta, init.type = "Spectral")
#   save(stm_m, file = paste0("tpm_Italy",nb,".Rdata"))
# }
# 
# parallel::clusterExport(cl,"stm_df") # parallel, cl = cluster to run parallel
# parallel::clusterApply(cl,t_list,tpc)
# # 
# # Beta distribution
# 

# # 
# # # tpm detect
 library(stm)
  load("tpm_Italy25.Rdata")
  load("df_actpol.Rdata")
 key_it <- unique(read.xls("annotation_it_keywords.xls")[,2])
 key_de <- unique(read.xls("annotation_de_keywords.xls")[,2])
#
# #
keyintop <- list()
for (i in key_it) {
  t <- findTopic(stm_m, i)
  keyintop[[i]] = t
}
# # #
# # #
# # #
# # #
tidy(stm_m) %>%
  group_by(topic) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(term =  reorder(term, beta))  %>%
  mutate(topiclab = paste0("Topic ",topic)) %>%
  mutate(topic2 = factor(topiclab, levels = unique(topiclab[order(topic)]))) %>%
  #  filter(term %in% key_de & beta > 0.002) %>%
  ggplot(aes(term,beta, fill = topiclab)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic2, scales = "free") +
  coord_flip() +
  ylab("Probability words belonging to topic") +
  xlab("") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#
# #
 plot(topicCorr(stm_m))
labelTopics(stm_m,19,7) 
thg_de <- findThoughts(stm_m, texts = stm_m$text,n = 6, topics =19)
plotQuote(thg_de)

df <- df_actpol[df_actpol$lang == "it",]

for(i in c(2999,1915,1769,499,982,722)){

  n <- i
  print(c(df[n,]$text,df[n,]$user_name, df[n,]$created_at))

}




