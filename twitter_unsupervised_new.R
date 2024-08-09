# uploads ####
options(scipen = 999)
library(gdata)
# for crawling Twitter data 
library(academictwitteR)
# library(rtweet)
#library(corpus)
library(quanteda)
library(udpipe)
library(stopwords)
library(corpustools)
library(quanteda.textstats)
library(dplyr)
library(tidyverse)
library(lubridate)
library(tokenizers)
library(tidytext)
library(stringi)
library(readtext)
library(parallel)
library(stringr)
library(widyr)
library(irlba)
library(furrr)
library(stm)
library(slider)
library(ggthemes)
library("stminsights")
library("gridExtra")
library("ggpubr")
library(readxl)

# "%nin%" <- Negate("%in%")

setwd("C:/Users/rocpa/OneDrive/Desktop/CNT/twitter_unsupervised/from_server/16_11_2022/")
folder <- "data_new/save_32/"  # for output location

# Dataframe processing and dfm creation (lists for processing in keywords_cnt.xls) ####

load("df_20_22.Rdata")
df_20_22 <- df_20_22[df_20_22$selected_2 == 1,]
# df_20_22 <- rename(df_20_22, original_language = text)
# df_20_22 <- rename(df_20_22, text = en_text)
# save(df_20_22,file="df_20_22.Rdata")

# for cleaning and processing
df_20_22$text <- str_replace_all(df_20_22$text,"\\b's\\b","") 
df_20_22$text <- str_replace_all(df_20_22$text,"\\b-\\b","_") 

# German-specific processing
#key_de <- read.xls("keywords_cnt.xls",sheet = "lemma_de", encoding = "latin1")[,1]
key_de <- read_xls("keywords_cnt.xls",sheet = "lemma_de")[,1]
key_de <- paste0("\\b",key_de,"\\b")
#lemma_de <- read.xls("keywords_cnt.xls",sheet = "lemma_de", encoding="latin1" )[,2]
lemma_de <- read_xls("keywords_cnt.xls",sheet = "lemma_de")[,2]
lemma_de <- paste0("\\b",lemma_de,"\\b")
names(lemma_de) <- key_de
df_20_22[df_20_22$country == "Germany",]$text <- str_replace_all(df_20_22[df_20_22$country == "Germany",]$text,lemma_de) 

# Italian-specific processing
# key_it <- read.xls("keywords_cnt.xls",sheet = "lemma_it", encoding = "latin1")[,1]
key_it <- read_xls("keywords_cnt.xls",sheet = "lemma_it")[,1]
key_it <- paste0("\\b",key_it,"\\b")
#lemma_it <- read.xls("keywords_cnt.xls",sheet = "lemma_it", encoding="latin1" )[,2]
lemma_it <- read_xls("keywords_cnt.xls",sheet = "lemma_it" )[,2]
lemma_it <- paste0("\\b",lemma_it,"\\b")
names(lemma_it) <- key_it
df_20_22[df_20_22$country == "Italy",]$text <- str_replace_all(df_20_22[df_20_22$country == "Italy",]$text,lemma_it) 


# # English common terms
# eng <- read.xls("keywords_cnt.xls",sheet = "lemma_eng", encoding = "latin1")[,1]
eng <- read_xls("keywords_cnt.xls",sheet = "lemma_eng")[,1]
eng <- paste0("\\b",eng,"\\b")
# lemma_eng <- read.xls("keywords_cnt.xls",sheet = "lemma_eng", encoding="latin1" )[,2]
lemma_eng <- read_xls("keywords_cnt.xls",sheet = "lemma_eng")[,2]
lemma_eng <- paste0("\\b",lemma_eng,"\\b")
names(lemma_eng) <- eng
df_20_22$text <- str_replace_all(df_20_22$text,lemma_eng)

# compounds
#cmpd <- read.xls("keywords_cnt.xls",sheet = "compound_dfm", encoding = "latin1")[,1]
cmpd <- read_xls("keywords_cnt.xls",sheet = "compound_dfm")[,1]
cmpd <- paste0("\\b",cmpd,"\\b")
#cmpdlinked <- read.xls("keywords_cnt.xls",sheet = "compound_dfm", encoding="latin1" )[,2]
cmpdlinked <- read_xls("keywords_cnt.xls",sheet = "compound_dfm")[,2]
cmpdlinked <- paste0("\\b",cmpdlinked,"\\b")
names(cmpdlinked) <- cmpd
df_20_22$text <- str_replace_all(df_20_22$text,cmpdlinked)

#

# dfdelete <- read.xls("keywords_cnt.xls",sheet = "usernamedelete", encoding = "latin1")
dfdelete <- read_xls("keywords_cnt.xls",sheet = "usernamedelete")

for (i in dfdelete$usrnm) {
  
  p <- dfdelete[dfdelete$usrnm == i,]$word
  p <- paste0("\\b",p,"\\b")
  d <- rep(" ",length(p))
  names(d) <- p
  
  df_20_22[df_20_22$user_username == i,]$text <- str_replace_all(df_20_22[df_20_22$user_username == i,]$text,d)
  
}

df_20_22$text <- str_squish(df_20_22$text)

#
df_20_22$monthyear <- format(as.Date(df_20_22$created_at),'%m-%Y')
df_20_22$monthyear <- factor(df_20_22$monthyear, levels = c("01-2020","02-2020","03-2020","04-2020","05-2020","06-2020",
                                                            "07-2020","08-2020","09-2020","10-2020","11-2020",
                                                   "12-2020","01-2021","02-2021","03-2021","04-2021",
                                                   "05-2021","06-2021","07-2021", "08-2021","09-2021",
                                                   "10-2021","11-2021", "12-2021","01-2022","02-2022",
                                                   "03-2022","04-2022","05-2022","06-2022","07-2022",
                                                   "08-2022","09-2022", "10-2022","11-2022","12-2022"))

df_20_22$trimester <- "xxx"
 df_20_22[df_20_22$monthyear ==  "01-2020",]$trimester <- "01-2020" 
 df_20_22[df_20_22$monthyear ==     "02-2020",]$trimester <- "01-2020" 
df_20_22[df_20_22$monthyear ==     "03-2020",]$trimester <- "01-2020" 
 df_20_22[df_20_22$monthyear ==       "04-2020",]$trimester <- "02-2020" 
 df_20_22[df_20_22$monthyear ==   "05-2020",]$trimester <-  "02-2020" 
  df_20_22[df_20_22$monthyear ==      "06-2020",]$trimester <- "02-2020" 
 df_20_22[df_20_22$monthyear ==   "07-2020",]$trimester <-  "03-2020" 
 df_20_22[df_20_22$monthyear ==       "08-2020",]$trimester <- "03-2020" 
 df_20_22[df_20_22$monthyear ==      "09-2020",]$trimester <- "03-2020" 
 df_20_22[df_20_22$monthyear == "10-2020",]$trimester <- "04-2020" 
  df_20_22[df_20_22$monthyear ==  "11-2020",]$trimester <- "04-2020" 
 df_20_22[df_20_22$monthyear ==  "12-2020",]$trimester <- "04-2020" 
 df_20_22[df_20_22$monthyear == "01-2021",]$trimester <- "01-2021" 
 df_20_22[df_20_22$monthyear ==   "02-2021",]$trimester <- "01-2021" 
 df_20_22[df_20_22$monthyear ==    "03-2021",]$trimester <- "01-2021" 
 df_20_22[df_20_22$monthyear == "04-2021",]$trimester <- "02-2021" 
 df_20_22[df_20_22$monthyear ==  "05-2021",]$trimester <- "02-2021" 
 df_20_22[df_20_22$monthyear ==  "06-2021",]$trimester <- "02-2021" 
 df_20_22[df_20_22$monthyear ==  "07-2021",]$trimester <- "03-2021" 
 df_20_22[df_20_22$monthyear ==   "08-2021",]$trimester <- "03-2021" 
 df_20_22[df_20_22$monthyear ==  "09-2021",]$trimester <- "03-2021" 
 df_20_22[df_20_22$monthyear ==    "10-2021",]$trimester <- "04-2021" 
 df_20_22[df_20_22$monthyear ==     "11-2021",]$trimester <- "04-2021" 
 df_20_22[df_20_22$monthyear == "12-2021",]$trimester <- "04-2021" 
 df_20_22[df_20_22$monthyear ==   "01-2022",]$trimester <- "01-2022" 
 df_20_22[df_20_22$monthyear ==    "02-2022",]$trimester <-  "01-2022" 
 df_20_22[df_20_22$monthyear ==   "03-2022",]$trimester <- "01-2022" 
 df_20_22[df_20_22$monthyear ==    "04-2022",]$trimester <- "02-2022" 
 df_20_22[df_20_22$monthyear ==  "05-2022",]$trimester <- "02-2022" 
 df_20_22[df_20_22$monthyear ==   "06-2022",]$trimester <- "02-2022" 
 df_20_22[df_20_22$monthyear ==  "07-2022" ,]$trimester <- "03-2022" 
 df_20_22[df_20_22$monthyear ==    "08-2022",]$trimester <- "03-2022" 
 df_20_22[df_20_22$monthyear == "09-2022",]$trimester <- "03-2022" 
 df_20_22[df_20_22$monthyear ==  "10-2022",]$trimester <- "04-2022" 
 df_20_22[df_20_22$monthyear ==      "11-2022",]$trimester <- "04-2022" 
 df_20_22[df_20_22$monthyear ==    "12-2022",]$trimester <- "04-2022" 
#
 df_20_22$trimester <- factor(df_20_22$trimester, levels= c("01-2020","02-2020","03-2020","04-2020",
                                                                      "01-2021","02-2021","03-2021","04-2021",
                                                                      "01-2022","02-2022","03-2022","04-2022"))  

df_20_22 <- df_20_22 %>% filter(! monthyear %in% c("01-2020","02-2020")) # one only for each month and not strict covid
 
# remove words
# rem_dfm <- read.xls("keywords_cnt.xls",sheet = "rem_dfm", encoding = "latin1")[,1]
rem_dfm <- read_xls("keywords_cnt.xls",sheet = "rem_dfm")[,1]
rem_dfm <- unique(rem_dfm)
rem_char <- c("http*","@*","€","+","|","s","faq","=","_","__","~","___")

# List of economic keywords to plot apart or not plot
de_policies <- c(
  "corona_aid",
  "corona_emergency_aid",
  "economic_stabilization_fund",
  "stimulus_package",
  "social_protection_package",
  "social_protection_package_2",
  "social_protection_package_3",
  "bridging_aid",
  "corona_tax_assistance",
  "bridging_aid_2",
  "november_aid",
  "december_aid",
  "november_december_aid",
  "bridging_aid_3",
  "new_start_aid",
  "bridging_aid_3_plus",
  "bridging_aid_4",
  "new_start_aid_plus",
  "new_start_aid22",
  "recoveryplan",
  "digital_pact",
  "nextgeneration",
  "nextgenerationeu",
  "darp",
  "reacteu",
  "esf"
)

it_policies <- c(
  "dc_aiuti",
  "dc_cureitaly",
  "dc_liquidity",
  "dc_relaunch",
  "dc_august",
  "dc_reliefs",
  "dc_reliefs_2",
  "dc_reliefs_3",
  "dc_reliefs_4",
  "dc_supports",
  "dc_supports_2",
  "dc_supports_3",
  "nextgeneration",
  "nextgenerationeu",
  "recoveryplan",
  "pnrr",
  "reacteu",
  "esf",
  "pnrr_resources",
  "pnrr_funds"
)


# For text detection: bigram, trigrams and word extraction ####

bigrams <- read.csv("bigrams_4.csv",sep=";") 
trigrams_can <- trigrams %>% filter(str_starts(trigram,"can"))

write.csv(bigrams_be,file="bigrams_be.csv",row.names = F)

bigrams_tx  <- df_20_22[,-4] %>% filter(country == "Italy") %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
bigrams_tx  %>% dplyr::count(bigram, sort = TRUE)
bigrams_separate  <- bigrams_tx  %>% separate(bigram,c("word1","word2"),sep=" ")
bigrams_filtered  <- bigrams_separate  %>%
  filter(!word1 %in% stopwords_en) %>%
  filter(!word2 %in% stopwords_en)
bigrams_filtered <- bigrams_filtered  %>%  dplyr::count(word1, word2, sort = TRUE)
bigrams_united <- bigrams_filtered  %>% unite(bigram, word1, word2, sep = " ")
# bigrams_united  <- bigrams_united$bigram
bigrams_united <- unique(bigrams_united)

write.csv(bigrams_united,"bigrams_it.csv",row.names= F)

trigrams_tx <- df_20_22[,-4] %>% filter(country == "Italy") %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)
trigrams_tx %>% dplyr::count(trigram, sort = TRUE)
trigrams_separate  <- trigrams_tx %>% separate(trigram,c("word1","word2","word3"),sep=" ")
trigrams_filtered <- trigrams_separate  %>%
  filter(!word1 %in% stopwords_en) %>%
  filter(!word2 %in% stopwords_en) %>%
  filter(!word3 %in% stopwords_en)
trigrams_filtered  <- trigrams_filtered %>%  dplyr::count(word1, word2,word3, sort = TRUE)
trigrams_united  <- trigrams_filtered  %>% unite(trigram, word1, word2,word3, sep = " ")
# trigrams_united <- trigrams_united$trigram
trigrams_united <- unique(trigrams_united)
# 
write.csv(trigrams_united,"trigrams_it.csv",row.names= F)

quadrigrams_tx  <- df_20_22[,-4] %>% filter(country == "Italy") %>% unnest_tokens(quadrigram, text, token = "ngrams", n = 4)
quadrigrams_tx  %>% dplyr::count(quadrigram, sort = TRUE)
quadrigrams_separate  <- quadrigrams_tx  %>% separate(quadrigram,c("word1","word2","word3","word4"),sep=" ")
quadrigrams_filtered  <- quadrigrams_separate  %>%
  filter(!word1 %in% stopwords_en) %>%
  filter(word2 == "and") %>%
  filter(!word3 %in% stopwords_en) %>%
  filter(!word4 %in% stopwords_en)
quadrigrams_filtered <- quadrigrams_filtered  %>%  dplyr::count(word1, word2,word3,word4, sort = TRUE)
quadrigrams_united <- quadrigrams_filtered  %>% unite(quadrigrams, word1, word2,word3,word4, sep = " ") 

write.csv(quadrigrams_united,"quadrigrams_it.csv",row.names= F)


ngram_it <- read.csv("trigrams_it.csv",sep =",")#  %>% filter(n >= 4)
ngram_de <- read.csv("trigrams_de.csv",sep = ",") # %>% filter(n >= 4)

ngram_tot <- rbind(ngram_it,ngram_de)
ngram_totqr <- ngram_tot %>% filter(duplicated(quadrigrams))

ngram_de <- ngram_de %>% filter(trigram %nin% ngram_it$trigram)

write.csv(ngram_de,file="trigrams_de_only.csv")
check <- ngram %>% filter(n >= 3) %>% filter(bigram %nin% cmpd)


tri_it <- read.csv("trigrams_it_only.csv",sep =",")
qua <- qua %>% filter(quadrigram %nin% cmpd)




## unique term
unique(stringr::str_extract_all(df_20_22[df_20_22$selected_2 == 1,]$text,
                                "\\b[:alnum:]*\\-?[:alnum:]*\\-?[:alnum:] nextgeneration_eu\\-?[:alnum:]*\\-?[:alnum:]*\\b"))

wordouble <- unique(stringr::str_extract_all(df_20_22$text,
                                             "\\b\\-?[:alnum:]* \\-?[:alnum:]* extended\\b"))

wordouble <- unlist(wordouble)
wordouble <- unique(wordouble)
wordouble
write.csv(wordouble,file="president_1.csv",row.names = F)

# filter dataframe
txt <- df_20_22 %>% filter(str_detect(text,"\\bstimulus_package\\b") & str_detect(text,"\\bclimate_protection\\b"))

tstcheck <- df_20_22 %>% filter(str_detect(original_language,regex("cigo")))
tstcheck$text

# check feature in dfm
textfreq <- textstat_frequency(dfm_it_en)
textstat_frequency(dfm_it_en) %>% subset(feature %in% "bike_paths")


# Document-feature matrix ####

# dfm_tot <- tokens(corpus(df_20_22),
#                     remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>% 
#   tokens_remove(c("http*","@*","€","+","|","s","faq","=","für","von","l","s","il","la","di",stopwords("en"),stopwords_en,
#                   rem_dfm)) %>%
#   dfm()
# 
# dfm_tot[1,]$ID
# dfm_tot[dfm_tot$ID == "MISE_GOV 483703",] 
# 
# tmod_wf <- textmodel_wordfish(dfm_tot, dir = c(4051, 309))

## German dfm

dfm_de_en <- tokens(corpus(df_20_22[df_20_22$country == "Germany",]),
                remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>% 
  tokens_remove(c("für","von",stopwords("en"),stopwords_en,
                  rem_dfm, rem_char)) %>%
  dfm()

dfm_de_en
topfeatures(dfm_de_en,50)
save(dfm_de_en,file=paste0(folder,"DE/dfm_de_en.Rdata"))
# 
# dfm_de_enWS <- tokens(df2022scoresDE,
#                     remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>% 
#   tokens_remove(c("für","von",stopwords("en"),stopwords_en,
#                   rem_dfm, rem_char)) %>%
#   dfm()
# 
# save(dfm_de_enWS,file="data_new/save_29/DE/dfm_de_enWS.Rdata")


## Italian dfm

dfm_it_en <- tokens(corpus(df_20_22[df_20_22$country == "Italy",]),
                 remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>% 
  tokens_remove(c("l","il","la","di",stopwords("en"),stopwords_en,
                  rem_dfm, rem_char)) %>%
  dfm() 


dfm_it_en
topfeatures(dfm_it_en,50)

save(dfm_it_en,file=paste0(folder,"IT/dfm_it_en.Rdata"))





# Results: sample ####
library(forcats)

deall <-  df_20_22 %>% filter(country == "Germany") %>% 
  mutate(actor = recode(actor,
                            "POL" = "Political\nActors",
                            "TA" = "Trade\nAssociations",
                            "TU" = "Trade\nUnions")) %>%
ggplot(aes(x = fct_rev(fct_infreq(actor)), fill = actor)) + geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 0,hjust = 1, size  = 5) +
  xlab("count tweets") +
  coord_flip() +
  facet_wrap(~ country, scales = "free", nrow = 2) +
  scale_fill_manual(values = c("Political\nActors" = "red","Trade\nAssociations" = "limegreen",
                               "Trade\nUnions" = "purple")) +
  guides(fill="none") + 
  theme_bw() +
  theme(strip.background = element_rect(fill="beige"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size =10),
        strip.text.x = element_text(size = 12)
        )
  
  
detime <- df_20_22 %>% filter(country == "Germany") %>% 
  mutate(actor = recode(actor,
                                   "POL" = "Political Actors",
                                   "TA" = "Trade Associations",
                                   "TU" = "Trade Unions")) %>%
  ggplot(aes(x = monthyear, fill = actor)) + 
  geom_bar(position = position_dodge()) +
#  geom_text(aes(label = ..count..), stat = "count", vjust = 0) +
#  coord_flip() +
  scale_x_discrete(breaks = c("01-2020", "06-2020","01-2021","06-2021","01-2021","06-2021","01-2022","06-2022","12-2022")) + 
  xlab("date") +
#  facet_wrap(~ country, scales = "free", nrow = 2) +
  # facet_wrap(~ segment , scales = "free") +
  scale_fill_manual(values = c("Political Actors" = "red","Trade Associations" = "limegreen",
                               "Trade Unions" = "purple")) +
  guides(fill="none") + 
  theme_bw() +
  theme(axis.title.y = element_blank(), 
        plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm"),
        axis.text.x = element_text(size = 10))


itall <- df_20_22 %>% filter(country == "Italy") %>% 
  mutate(actor = recode(actor,
                        "POL" = "Political\nActors",
                        "TA" = "Trade\nAssociations",
                        "TU" = "Trade\nUnions")) %>%
  ggplot(aes(x = fct_rev(fct_infreq(actor)), fill = actor)) + geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 0,hjust = 1, size  = 5) +
  xlab("count tweets") +
  coord_flip() +
  facet_wrap(~ country, scales = "free", nrow = 2) +
  scale_fill_manual(values = c("Political\nActors" = "red","Trade\nAssociations" = "limegreen",
                               "Trade\nUnions" = "purple")) +
  guides(fill="none") + 
  theme_bw() +
  theme(strip.background = element_rect(fill="beige"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size =10),
        strip.text.x = element_text(size = 12)
  )





ittime <- df_20_22 %>% filter(country == "Italy") %>% 
  mutate(actor = recode(actor,
                        "POL" = "Political Actors",
                        "TA" = "Trade Associations",
                        "TU" = "Trade Unions")) %>%
  ggplot(aes(x = monthyear, fill = actor)) + 
  geom_bar(position = position_dodge()) +
  #  geom_text(aes(label = ..count..), stat = "count", vjust = 0) +
#  coord_flip() +
  scale_x_discrete(breaks = c("01-2020", "06-2020","01-2021","06-2021","01-2021","06-2021","01-2022","06-2022","12-2022")) + 
  xlab("date") +
#  facet_wrap(~ country, scales = "free", nrow = 2) +
  # facet_wrap(~ segment , scales = "free") +
  scale_fill_manual(values = c("Political Actors" = "red","Trade Associations" = "limegreen",
                               "Trade Unions" = "purple")) +
  guides(fill="none") + 
  theme_bw() +
  theme(axis.title.y = element_blank(), 
        plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm"),
        axis.text.x = element_text(size = 10))


ggpubr::ggarrange(deall,detime,itall,ittime,widths = c(0.5, 1))
ggsave(paste0(folder,"figures/sampleNEW.jpg"),width = 13, height = 6)

# Results: tf-idf ####
library(stopwords)
rem_it <- read_xls("keywords_cnt.xls",sheet = "rem_it")[,1]
rem_de <- read_xls("keywords_cnt.xls",sheet = "rem_de")[,1]
df <- df_20_22 %>% unnest_tokens(word, text, token = stringr::str_split, pattern = " ")
df$word <- str_remove_all(df$word, "[^#_[:^punct:]]")

# Germany

dfDE <- df %>% filter(country == "Germany") %>% filter(!word %in% stop_words$word,
                                        !word %in% stopwords("en"), 
#                                        !word %in% stopwords_en,
                                        !word %in% rem_dfm$remove_list,
                                        !word %in% rem_de$rem_de)

dfDE <- replace(dfDE, dfDE =='', NA)
dfDE <- dfDE %>% drop_na(word)

dfDE <- dfDE %>% # filter(country == "Germany") %>%
  filter(!word %in% c(it_policies,de_policies)) %>%
  count(country,actor, word,sort = TRUE)

total_wordsDE <- dfDE %>% 
 group_by(country,actor) %>%
# group_by(actor) %>%
  summarize(total = sum(n))

dfDE <- left_join(dfDE, total_wordsDE)

 dfDE$segment <- paste0(dfDE$country,"_",dfDE$actor) 

df_tf_idfDE <- dfDE %>% bind_tf_idf(word,segment, n)

# Italy

dfIT <- df %>% filter(country == "Italy") %>% filter(!word %in% stop_words$word,
                                                       !word %in% stopwords("en"), 
                                                       #                                        !word %in% stopwords_en,
                                                       !word %in% rem_dfm$remove_list,
                                                       !word %in% rem_it$rem_it)

dfIT <- replace(dfIT, dfIT =='', NA)
dfIT <- dfIT %>% drop_na(word)

dfIT <- dfIT %>% # filter(country == "Germany") %>%
  filter(!word %in% c(it_policies,de_policies)) %>%
  count(country,actor, word,sort = TRUE)

total_wordsIT <- dfIT %>% 
  group_by(country,actor) %>%
  # group_by(actor) %>%
  summarize(total = sum(n))

dfIT <- left_join(dfIT, total_wordsIT)

dfIT$segment <- paste0(dfIT$country,"_",dfIT$actor) 

df_tf_idfIT <- dfIT %>% bind_tf_idf(word,segment, n)
# df_tf_idfDE %>%  arrange(desc(tf_idf))

#df_tf_idfDE <- df_tf_idf
#df_tf_idfIT <- df_tf_idf

df_tf_idf <- rbind(df_tf_idfDE,df_tf_idfIT) 
df_tf_idf <- df_tf_idf %>% mutate(actor = recode(actor,
                                                 "POL" = "Political Actors",
                                                 "TA" = "Trade Associations",
                                                 "TU" = "Trade Unions")) %>%
                          mutate(segment = recode(
                            segment,
                            "Germany_POL" = "Germany\nPolitical Actors",
                            "Italy_POL" = "Italy\nPolitical Actors",
                            "Germany_TA" = "Germany\nTrade Associations",
                            "Italy_TA" = "Italy\nTrade Associations",    
                            "Germany_TU" = "Germany\nTrade Unions",
                            "Italy_TU" = "Italy\nTrade Unions",  
                          ))

#df_tf_idf %>% # filter(country == "Italy") %>%
df_tf_idf %>%
  #  group_by(country, actor) %>% 
  group_by(segment) %>% 
  slice_max(tf_idf, n = 10) %>%
# group_by(country,actor,tf_idf) %>% 
  filter(! word %in% c(it_policies,de_policies,rem_dfm,rem_char,
                       "corona_bonus","corona_special_payment")) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(word,tf_idf),y = tf_idf, fill = actor)) + 
  geom_col() +
  coord_flip() +
  scale_y_continuous(label = scales::percent) +
#  facet_wrap(~ country + actor, scales = "free") +
  # facet_wrap(~ factor(country,c("Germany","Italy")) + 
  #              factor(actor,c("Political Actors","Trade Associations","Trade Unions")), scales = "free", nrow = 3) +
 facet_wrap(~ factor(segment,c("Germany\nPolitical Actors","Italy\nPolitical Actors",
                               "Germany\nTrade Associations","Italy\nTrade Associations",
                               "Germany\nTrade Unions","Italy\nTrade Unions")),
            scales = "free", nrow = 3) +
  scale_fill_manual(values = c("Political Actors" = "red","Trade Associations" = "limegreen",
                               "Trade Unions" = "purple")) +
  theme_light() +
  theme(strip.background = element_rect(fill="beige"), 
        strip.text = element_text(color = "black",size = 13),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=13),
        axis.text.x = element_text(size=11)) +
  guides(fill = FALSE)
ggsave(file=paste0(folder,"figures/tfidf.jpg"),width = 10, height = 10.5)


# Results: Topic extraction ####

load(paste0(folder,"IT/stm_it25.Rdata")) 
load(paste0(folder,"IT/dfm_it_en.Rdata")) 
stm_df_it <- quanteda::convert(dfm_it_en,to = "stm")  
#
load(paste0(folder,"DE/stm_de25.Rdata"))
load(paste0(folder,"DE/dfm_de_en.Rdata")) 
stm_df_de <- quanteda::convert(dfm_de_en,to = "stm")  


# selection used later for adapting code to Germany or Italy

numm <- 25
stm_m <- stm_it25
stm_df <- stm_df_it
titleplot <- "Italy"
dfb <- df_20_22[df_20_22$country == titleplot & df_20_22$selected_2 == 1,] 

# Results: summary ####

sg <- sageLabels(stm_m,10)
sg_prob <- tibble(topic = 1:numm,sg$marginal$prob) # marginal probability
sg_frex <- tibble(topic = 1:numm,sg$marginal$frex) # marginal frex
sg_prob_pol <- tibble(topic = 1:numm,sg$covnames[[1]],sg$cov.betas[[1]]$problabels) # POL prob
sg_prob_ta <- tibble(topic = 1:numm,sg$covnames[[2]],sg$cov.betas[[2]]$problabels) # TA probl
sg_prob_tu <- tibble(topic = 1:numm,sg$covnames[[3]],sg$cov.betas[[3]]$problabels) # TU prob
sg_frex_pol <- tibble(topic = 1:numm,sg$covnames[[1]],sg$cov.betas[[1]]$frexlabels) # POL frex
sg_frex_ta <- tibble(topic = 1:numm,sg$covnames[[2]],sg$cov.betas[[2]]$frexlabels) # TA frex
sg_frex_tu <- tibble(topic = 1:numm,sg$covnames[[3]],sg$cov.betas[[3]]$frexlabels) # TU frex


thoughts <- list()
for (i in 1:numm){ # 
  # thg_det <- findThoughts(stm_m, texts = df_de$text,n = 3, topics =i)$docs[[1]]
  thought_text = list()
  dates <- findThoughts(stm_m, texts = dfb$text,n = 20, topics =i)$index[[1]] # 
  for (n in dates) {
    txx <-  print(c(paste0(" ACT: ", dfb[n,]$actor,
                           " ACTsj: ", dfb[n,]$user_username," TXT: ",dfb[n,]$text," DATE: ", dfb[n,]$created_at,
                           " TWID: ",dfb[n,]$tweet_id," CNID ",dfb[n,]$conversation_id)))
    thought_text[[n]] <- txx
    thought_textfin <- do.call(rbind.data.frame, thought_text)
  }
  thoughts[[i]] <- thought_textfin
  
 }
bind_rows(thoughts)
thoughts <- do.call(rbind.data.frame, lapply(thoughts,'[[',1))
colnames(thoughts) = c("1", "2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20") # columns where texts go
thoughts <- cbind(topic = 1:numm,thoughts)

# combining final report pieces and write excel
long_report <- cbind(sg_prob,sg_frex,sg_prob_pol,sg_prob_ta,sg_prob_tu,sg_frex_pol,sg_frex_ta,sg_frex_tu,thoughts)
# long_report <- cbind(sg_prob,sg_frex,thoughts)

write.csv(long_report,file=paste0(folder,"IT/",titleplot,numm,".csv"),row.names = F, col.names=T,  sep=";",  fileEncoding = "UTF-8")


# Results: Topic Proportion global ####

td_gamma <- tidy(stm_m, matrix = "gamma")
ID_row <- names(stm_df$documents) # the name of documents gets lost, the row number is reported
td_gamma <- cbind(td_gamma,ID_row) # Here I map each document to its name via row, I checked with content, it works
td_gamma <- cbind(td_gamma,dfb) # merge the gamma matrix with the dataframe via ID, so the variables to sort documents can be used


top_terms <- tidy(stm_m) %>%
  group_by(topic,term) %>%
  summarise(beta = mean(beta))  %>%
  arrange(beta) %>%
  top_n(5, beta) %>% #8
  arrange(-beta)%>%
  select(topic, term) %>%
  summarise(terms = list(unique(term))) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic")


gamma_terms$country <- titleplot
gamma_terms <- gamma_terms[order(-gamma_terms$gamma),]
gamma_terms$rank <- 1:25
IT_gammaterms <- gamma_terms


# Adding label and transformative/restorative policy

 IT_gammaterms <- IT_gammaterms %>% mutate(label = recode(topic,
                                                          "1" = "1 Dc\nSupports", # brought cisl
                                                           "2" = "2 Pnrr\nInvestments"  , #, "transitions",
                                                           "3" = "3 Funds\nAllocations", #*
                                                           "4" = "4 Dc\nRelaunch", # "life\nquality",
                                                           "5" = "5 Emergency\nCompany"  , #"emergency\naid", # companies
                                                           "6" = "6 Liquidity",
                                                           "7" = "7 Agribusiness", # "agribusiness" , # "dc\naugust",
                                                           "8" = "8 Tourism", # agorarai
                                                           "9" = "9 Social\nConfrontations",
                                                           "10" = "10 European\nFunding",
                                                           "11" = "11 National\nRecovery" , # "sustainability",
                                                           "12" = "12 Investments\nBoost", # confederal_secretary, front_page
                                                           "13" = "13 Sustainable\nGrowth", #  # fenealuil #spoke
                                                           "14" = "14 Pa\nSimplification",
                                                           "15" = "15 Pnrr\nImplementation", # confsal check all
                                                           "16" = "16 Funds\nAnnouncement", #?governance?
                                                           "17" = "17 Recoveryplan" , # "restart\ndecree",
                                                           "18" = "18 Contracts",
                                                           "19" = "19 Employment\nEquality", #*
                                                           "20" = "20 South", # credit
                                                         "21" = "21 Funding\nAccess",
                                                         "22" = "22 Public\nAdministration",
                                                         "23" = "23 Actors", #infrastructure
                                                         "24" = "24 Recovery\nInterventions", # thousand
                                                         "25" = "25 Pnrr" )) #"industry"))

 IT_gammaterms <- IT_gammaterms %>% mutate(policy = recode(topic,
                                                         "1" = "restorative", # brought cisl
                                                         "2" = "transformative"  , #, "transitions",
                                                         "3" = "transformative", #*
                                                         "4" = "restorative", # "life\nquality",
                                                         "5" = "restorative"  , #"emergency\naid", # companies
                                                         "6" = "restorative",
                                                         "7" = "generic", # "agribusiness" , # "dc\naugust",
                                                         "8" = "restorative", # agorarai
                                                         "9" = "generic",
                                                         "10" = "transformative",
                                                         "11" = "transformative" , # "sustainability",
                                                         "12" = "generic", # confederal_secretary, front_page
                                                         "13" = "transformative", #  # fenealuil #spoke
                                                         "14" = "transformative",
                                                         "15" = "transformative", # confsal check all
                                                         "16" = "generic", #?governance?
                                                         "17" = "transformative" , # "restart\ndecree",
                                                         "18" = "generic",
                                                         "19" = "transformative", #*
                                                         "20" = "transformative", # credit
                                                         "21" = "generic",
                                                         "22" = "generic",
                                                         "23" = "generic", #infrastructure
                                                         "24" = "restorative", # thousand
                                                         "25" = "generic" )) #"industry"))
 
IT_gammaterms <- IT_gammaterms %>% mutate(labelcol = recode(topic,
                                                             "5" = 5,
                                                             "10" = 10,
                                                             "2" = 2,
                                                             "15" = 15,
                                                             "20" = 20,
                                                             "9" = 9,
                                                             "24" = 24,
                                                             "19" = 19))
 
IT_gammaterms["labelcol"][is.na(IT_gammaterms["labelcol"])] <- 0
 
save(IT_gammaterms, file = paste0(folder,"IT/IT_gammaterms.Rdata"))



 DE_gammaterms <- DE_gammaterms %>% mutate(label = recode(topic,
                                                         "1" = "1 Bridging\nAid", # intended consequences
                                                         "2" = "2 Stimulus\nPackage", # "stimulus\package",  # "growth",
                                                         "3" = "3 Families",  # "families", #*
                                                         "4" = "4 Work\nSecurity",
                                                         "5" = "5 Corona\nAid" , # "business", minimum short_time_allowance
                                                         "6" = "6 Electricity\nPrice", # electricity\nprice
                                                         "7" = "7 Extension\nAid",
                                                         "8" = "8 Retail\nSector", # "Companies\nRecovery
                                                         "9" = "9 Economic\nBoost", # Sustainable Goals
                                                         "10" = "10 Companies\nRecovery",
                                                         "11" = "11 European\nFunding",
                                                         "12" = "12 Climate\nProtection", # Families
                                                         "13" = "13 Digitalization",
                                                         "14" = "14 Bridging\nAid3",
                                                         "15" = "15 Basic\nSecurity", #creative industry, economic stabilisation_fund
                                                         "16" = "16 Special\nCorona Aid",
                                                         "17" = "17 Lockdown" , # "self\nemployed",
                                                         "18" = "18 Economic\nStabilisation",
                                                         "19" = "19 Wumms", # "recession",
                                                         "20" = "20 Social\nProtection" , #"lockdown",
                                                         "21" = "21 Debates",
                                                         "22" = "22 Germany\nin Eu",
                                                         "23" = "23 Startups",
                                                         "24" = "24 Gender\nEquality", # voluntary shopping, voluntary initiatives
                                                         "25" = "25 Tax\nEstimates"))

  DE_gammaterms <- DE_gammaterms %>% mutate(policy = recode(topic,
                                                          "1" = "restorative", # brought cisl
                                                          "2" = "transformative"  , #, "transitions",
                                                          "3" = "restorative", #*
                                                          "4" = "restorative", # "life\nquality",
                                                          "5" = "restorative"  , #"emergency\naid", # companies
                                                          "6" = "restorative",
                                                          "7" = "restorative", # "agribusiness" , # "dc\naugust",
                                                          "8" = "restorative", # agorarai
                                                          "9" = "transformative",
                                                          "10" = "restorative",
                                                          "11" = "transformative" , # "sustainability",
                                                          "12" = "transformative", # confederal_secretary, front_page
                                                          "13" = "transformative", #  # fenealuil #spoke
                                                          "14" = "restorative",
                                                          "15" = "transformative", # confsal check all
                                                          "16" = "restorative", #?governance?
                                                          "17" = "restorative" , # "restart\ndecree",
                                                          "18" = "restorative",
                                                          "19" = "restorative", #*
                                                          "20" = "restorative", # credit
                                                          "21" = "restorative",
                                                          "22" = "transformative",
                                                          "23" = "transformative", #infrastructure
                                                          "24" = "transformative", # thousand
                                                          "25" = "restorative" )) #"industry"))

 
  DE_gammaterms <- DE_gammaterms %>% mutate(labelcol = recode(topic,
                                                              "2" = 2,
                                                              "13" = 13,
                                                              "20" = 20,
                                                              "19" = 19,
                                                              "5" = 5,
                                                              "9" = 9,
                                                              "8" = 8,
                                                              "24" = 24))
  
  DE_gammaterms["labelcol"][is.na(DE_gammaterms["labelcol"])] <- 0
  
save(DE_gammaterms, file = paste0(folder,"DE/DE_gammaterms.Rdata"))



df <- rbind(DE_gammaterms,IT_gammaterms)

# Topic proportion national level

# Germany
#tpde <- # df %>% filter(country == "Germany") %>%
load(paste0(folder,"DE/DE_gammaterms.Rdata"))
tpde <-  DE_gammaterms %>%
 ggplot(aes(reorder(label,gamma), gamma, fill = as.factor(labelcol))) + #, fill = policy) + 
 # ggplot(aes(reorder(topic,gamma), gamma)) + #, fill = policy)) + 
 geom_col(color = "black") + #, width=.4
  geom_text(aes(label = terms ), hjust = 0, y = 0.001, size = 9,# nudge_y = 0.00005, size = 5, # 0.0005
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.10) ,
                     labels = scales::percent_format()) +
facet_wrap(~ country , scales = "free", dir = "v") +
  # theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  # labs(x = NULL, y = expression(gamma))
  labs(x = NULL, y = "Topic Proportion") +
    scale_fill_manual(values = c("2" = "salmon",
                                 "13" = "green",
                                 "20" = "lightblue",
                                 "19" = "cyan",
                                 "5" = "purple",
                                 "9" = "brown2",
                                 "8" = "lightgreen",
                                 "24" = "tan1",
                                 "0" = "white")) +
  theme_bw() +
    theme(axis.title.x = element_text(size = 22),
          axis.text.x = element_text(size = 19),
      axis.text.y = element_text(size = 24),
         # legend.text = element_text(size=12),
          strip.background = element_rect(fill="beige"), 
          strip.text.x = element_text(color = "black",size = 26),
      plot.margin = unit(c(0.2, 0.37, 0.2, 0.2), 
                         "inches")) +
    guides(fill = "none")
save(tpde,file = paste0(folder,"DE/tpde.Rdata"))

# Italy
# tpit <- df %>% filter(country == "Italy") %>%
load(paste0(folder,"IT/IT_gammaterms.Rdata"))
tpit <- IT_gammaterms %>% 
  ggplot(aes(reorder(label,gamma), gamma, fill = as.factor(labelcol))) + #, fill = policy) + 
  geom_col(color = "black") + # , width=.4
  geom_text(aes(label = terms ), hjust = 0, y = 0.001, size = 9,# nudge_y = 0.00005, size = 5, # 0.0005
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.10) ,
                     labels = scales::percent_format()) +
  facet_wrap(~ country , scales = "free", dir = "v") +
  # theme(legend.position="bottom",
  #       axis.text.y = element_text(size = 20),
  #       legend.text = element_text(size=12),
  #       strip.background = element_rect(fill="beige"), 
  #       strip.text = element_text(color = "black",size = 16)) +
  # labs(x = NULL, y = expression(gamma))
  labs(x = NULL, y = "Topic Proportion") +
  scale_fill_manual(values = c("5" = "salmon",
                              "10" = "green",
                              "2" = "lightblue",
                              "15" = "cyan",
                              "20" = "purple",
                              "9" = "brown2",
                              "24" = "lightgreen",
                              "19" = "tan1",
                              "0" = "white")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 22),
        axis.text.x = element_text(size = 19),
        axis.text.y = element_text(size = 24),
        # legend.text = element_text(size=12),
        strip.background = element_rect(fill="beige"), 
        strip.text.x = element_text(color = "black",size = 26),
        plot.margin = unit(c(0.2, 0.37, 0.2, 0.2), 
                           "inches")) +
  guides(fill = "none")
save(tpit,file = paste0(folder,"IT/tpit.Rdata"))

# # to prepare trimester-break visualization ####
# 
# gamma_termsmy <- td_gamma %>% 
#   group_by(topic,trimester) %>%
#   summarise(gamma = mean(gamma)) %>%
#   arrange(desc(gamma))  
# 
# IT_gamma_termsmy <- gamma_termsmy %>% mutate(country = "Italy") %>% 
#   mutate(textlabel = topic)
# 
# DE_gamma_termsmy <- gamma_termsmy %>% mutate(country = "Germany") %>% 
#   mutate(textlabel = topic)
# 
# IT_gamma_termsmy <- IT_gamma_termsmy %>% mutate(label = recode(topic,
#                                                          "1" = "1 dc\nsupports", # brought cisl
#                                                          "2" = "2 pnrr\ninvestments"  , #, "transitions",
#                                                          "3" = "3 funds\nallocations", #*
#                                                          "4" = "4 dc_relaunch", # "life\nquality",
#                                                          "5" = "5 emergency\ncompanies"  , #"emergency\naid", # companies
#                                                          "6" = "6 liquidity",
#                                                          "7" = "7 agribusiness", # "agribusiness" , # "dc\naugust",
#                                                          "8" = "8 tourism", # agorarai
#                                                          "9" = "9 social\nconfrontations",
#                                                          "10" = "10 european\nfunding",
#                                                          "11" = "11 national\nrecovery" , # "sustainability",
#                                                          "12" = "12 active\npolicies", # confederal_secretary, front_page
#                                                          "13" = "13 sustainable\ngrowth", #  # fenealuil #spoke
#                                                          "14" = "14 pa\nsimplification",
#                                                          "15" = "15 pnrr\nimplementation", # confsal check all
#                                                          "16" = "16 dc\nreliefs2", #?governance?
#                                                          "17" = "17 intervention\nuncertainty" , # "restart\ndecree",
#                                                          "18" = "18 contracts",
#                                                          "19" = "19 employment\nequity", #*
#                                                          "20" = "20 south", # credit
#                                                          "21" = "21 funding\naccess",
#                                                          "22" = "22 public\nadministration",
#                                                          "23" = "23 pnrr", #infrastructure
#                                                          "24" = "24 recovery\ninterventions", # thousand
#                                                          "25" = "25 workers" )) #"industry")) 
# 
# IT_gamma_termsmy <- IT_gamma_termsmy %>% mutate(policy = recode(topic,
#                                                           "1" = "restorative", # brought cisl
#                                                           "2" = "transformative"  , #, "transitions",
#                                                           "3" = "transformative", #*
#                                                           "4" = "restorative", # "life\nquality",
#                                                           "5" = "restorative"  , #"emergency\naid", # companies
#                                                           "6" = "restorative",
#                                                           "7" = "restorative", # "agribusiness" , # "dc\naugust",
#                                                           "8" = "restorative", # agorarai
#                                                           "9" = "transformative",
#                                                           "10" = "transformative",
#                                                           "11" = "restorative" , # "sustainability",
#                                                           "12" = "restorative", # confederal_secretary, front_page
#                                                           "13" = "transformative", #  # fenealuil #spoke
#                                                           "14" = "transformative",
#                                                           "15" = "transformative", # confsal check all
#                                                           "16" = "restorative", #?governance?
#                                                           "17" = "restorative" , # "restart\ndecree",
#                                                           "18" = "restorative",
#                                                           "19" = "transformative", #*
#                                                           "20" = "transformative", # credit
#                                                           "21" = "restorative",
#                                                           "22" = "restorative",
#                                                           "23" = "transformative", #infrastructure
#                                                           "24" = "restorative", # thousand
#                                                           "25" = "restorative" )) #"industry")) 
# 
# 
# 
# 
# DE_gamma_termsmy <- DE_gamma_termsmy %>% mutate(label = recode(topic,
#                                                         "1" = "1 bridging\naid", # intended consequences
#                                                          "2" = "2 stimulus_package", # "stimulus\package",  # "growth",
#                                                          "3" = "3 families",  # "families", #*
#                                                          "4" = "4 work\nsecurity",
#                                                          "5" = "5 corona\naid" , # "business", minimum short_time_allowance
#                                                          "6" = "6 electricity\nprice", # electricity\nprice
#                                                          "7" = "7 extension\naid",
#                                                          "8" = "8 retail\nsector", # "smes", # ver.di_chairman
#                                                          "9" = "9 economic\nboost", 
#                                                          "10" = "10 schools",
#                                                          "11" = "11 european\nfunding",
#                                                          "12" = "12 climate\nprotection",
#                                                          "13" = "13 digitalization",
#                                                          "14" = "14 bridging\naid3",
#                                                          "15" = "15 basic\nsecurity", #creative industry, economic stabilisation_fund
#                                                          "16" = "16 special\ncorona aid",
#                                                          "17" = "17 lockdown" , # "self\nemployed",
#                                                          "18" = "18 economic\nstabilisation",
#                                                          "19" = "19 tax\nreduction", # "recession", 
#                                                          "20" = "20 social\nprotection" , #"lockdown",
#                                                          "21" = "21 insurance\nprotection",
#                                                          "22" = "22 germany\nin eu",
#                                                          "23" = "23 wumms",
#                                                          "24" = "24 gender\nequality", # voluntary shopping, voluntary initiatives
#                                                          "25" = "25 national\neconomy")) 
# 
# DE_gamma_termsmy <- DE_gamma_termsmy %>% mutate(policy = recode(topic,
#                                                           "1" = "restorative", # brought cisl
#                                                           "2" = "transformative"  , #, "transitions",
#                                                           "3" = "restorative", #*
#                                                           "4" = "restorative", # "life\nquality",
#                                                           "5" = "restorative"  , #"emergency\naid", # companies
#                                                           "6" = "restorative",
#                                                           "7" = "restorative", # "agribusiness" , # "dc\naugust",
#                                                           "8" = "restorative", # agorarai
#                                                           "9" = "transformative",
#                                                           "10" = "restorative",
#                                                           "11" = "transformative" , # "sustainability",
#                                                           "12" = "transformative", # confederal_secretary, front_page
#                                                           "13" = "transformative", #  # fenealuil #spoke
#                                                           "14" = "restorative",
#                                                           "15" = "transformative", # confsal check all
#                                                           "16" = "restorative", #?governance?
#                                                           "17" = "restorative" , # "restart\ndecree",
#                                                           "18" = "restorative",
#                                                           "19" = "restorative", #*
#                                                           "20" = "restorative", # credit
#                                                           "21" = "restorative",
#                                                           "22" = "transformative",
#                                                           "23" = "transformative", #infrastructure
#                                                           "24" = "transformative", # thousand
#                                                           "25" = "restorative" )) #"industry")) 


# for labeling trimester-time with most cited topic

# Germany

#  a <- DE_gamma_termsmy %>% mutate(country = "Germany") %>% 
#    group_by(trimester) %>%
#    top_n(4,gamma) %>%
#    mutate(textlabel = topic) %>%
#    mutate(sel = paste0(topic,trimester))
#  
#  DE_gamma_termsmy <- DE_gamma_termsmy %>% 
#   mutate(sel = paste0(topic,trimester)) %>%
#    mutate(textlabel  = ifelse(sel %in% a$sel,topic,""))
# 
#  tptmde4q <- DE_gamma_termsmy %>% 
#  ggplot(aes(x = trimester, y = gamma, 
#              fill = as.factor(policy) )) +
#   geom_col( color = "black") + 
#  geom_text(aes(label = textlabel), color = "black", position = position_stack(vjust = 0.5 )) +
#   labs(x = "Time", y = "Topic Proportion") + 
#   theme_bw() +
#    theme(legend.position = "none")
#  save(tptmde4q,file=paste0(folder,"DE/tptmde4q.Rdata"))
#  
# 
# # Italy
#  
#  a <- IT_gamma_termsmy %>% mutate(country = "Italy") %>% 
#    group_by(trimester) %>%
#    top_n(4,gamma) %>%
#    mutate(textlabel = topic) %>%
#    mutate(sel = paste0(topic,trimester))
#  
#  IT_gamma_termsmy <- IT_gamma_termsmy %>% 
#    mutate(sel = paste0(topic,trimester)) %>%
#    mutate(textlabel  = ifelse(sel %in% a$sel,topic,""))
#  
#  tptmit4q <- IT_gamma_termsmy %>% 
#    ggplot(aes(x = trimester, y = gamma, 
#               fill = as.factor(policy)
#    )) +
#    geom_col( color = "black") +  
#    geom_text(aes(label = textlabel), color = "black", position = position_stack(vjust = 0.5 )) +
#    labs(x = "Time", y = "Topic Proportion") + 
#    theme_bw() +
#    theme(legend.position = "none")
#  save(tptmit4q,file=paste0(folder,"IT/tptmit4q.Rdata"))

# interactive to test
# plotly::ggplotly(tptmit4q)

# combine pictures
# ggarrange(tpit,tpde,tptmit4q,tptmde4q, common.legend = TRUE, legend = "bottom", heights = c(1.5,0.7))  #  widths = c(1.4, 0.6)) # 
# ggsave( paste0(folder,"figures/tp14Q.jpg"),width = 29.5,height = 16) # 12.5)

# Different topic proportion actorsXcountry ####

td_gamma <- tidy(stm_m, matrix = "gamma")
ID_row <- names(stm_df$documents) # the name of documents gets lost, the row number is reported
td_gamma <- cbind(td_gamma,ID_row) # Here I map each document to its name via row, I checked with content, it works
td_gamma <- cbind(td_gamma,dfb) # merge the gamma matrix with the dataframe via ID, so the variables to sort documents can be used


top_terms <- tidy(stm_m) %>%
  group_by(topic,actor,term) %>%
  summarise(beta = mean(beta))  %>%
  arrange(beta) %>%
  top_n(8, beta) %>%
  arrange(-beta)%>%
  select(topic, term) %>%
  summarise(terms = list(unique(term))) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()
# 
# gamma_terms <- td_gamma %>%
#   group_by(topic,actor) %>%
#   summarise(gamma = mean(gamma)) %>%
#   arrange(desc(gamma)) %>%
#   left_join(top_terms, by = "topic")
# 
# 
# gamma_terms$country <- titleplot
# gamma_terms <- gamma_terms[order(-gamma_terms$gamma),]
# gamma_terms$rank <- 1:25
# IT_gammaterms <- gamma_terms


IT_gamma_termsact <- gamma_terms_act <- td_gamma %>%
  group_by(topic,actor) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(actor = recode(actor,
                        "POL" = "Political Actors",
                        "TA" = "Trade Associations",
                        "TU" = "Trade Unions")) %>%
  mutate(country = "Italy")

 IT_gamma_termsact <- gamma_terms_act
# 
# IT_gamma_termsact <- IT_gamma_termsact %>% mutate(label = recode(topic,
#                                                          "1" = "1 Dc\nSupports", # brought cisl
#                                                          "2" = "2 Pnrr\nInvestments"  , #, "transitions",
#                                                          "3" = "3 Funds\nAllocations", #*
#                                                          "4" = "4 Dc\nRelaunch", # "life\nquality",
#                                                          "5" = "5 Emergency\nCompany"  , #"emergency\naid", # companies
#                                                          "6" = "6 Liquidity",
#                                                          "7" = "7 Agribusiness", # "agribusiness" , # "dc\naugust",
#                                                          "8" = "8 Tourism", # agorarai
#                                                          "9" = "9 Social\nConfrontations",
#                                                          "10" = "10 European\nFunding",
#                                                          "11" = "11 National\nRecovery" , # "sustainability",
#                                                          "12" = "12 Investments\nBoost", # confederal_secretary, front_page
#                                                          "13" = "13 Sustainable\nGrowth", #  # fenealuil #spoke
#                                                          "14" = "14 Pa\nSimplification",
#                                                          "15" = "15 Pnrr\nImplementation", # confsal check all
#                                                          "16" = "16 Funds\nAnnouncement", #?governance?
#                                                          "17" = "17 Recoveryplan" , # "restart\ndecree",
#                                                          "18" = "18 Contracts",
#                                                          "19" = "19 Employment\nEquality", #*
#                                                          "20" = "20 South", # credit
#                                                          "21" = "21 Funding\nAccess",
#                                                          "22" = "22 Public\nAdministration",
#                                                          "23" = "23 Actors", #infrastructure
#                                                          "24" = "24 Recovery\nInterventions", # thousand
#                                                          "25" = "25 Pnrr" )) #"industry"))
# 
# IT_gamma_termsact <- IT_gamma_termsact %>% mutate(policy = recode(topic,
#                                                           "1" = "restorative", # brought cisl
#                                                           "2" = "transformative"  , #, "transitions",
#                                                           "3" = "transformative", #*
#                                                           "4" = "restorative", # "life\nquality",
#                                                           "5" = "restorative"  , #"emergency\naid", # companies
#                                                           "6" = "restorative",
#                                                           "7" = "generic", # "agribusiness" , # "dc\naugust",
#                                                           "8" = "restorative", # agorarai
#                                                           "9" = "generic",
#                                                           "10" = "transformative",
#                                                           "11" = "transformative" , # "sustainability",
#                                                           "12" = "generic", # confederal_secretary, front_page
#                                                           "13" = "transformative", #  # fenealuil #spoke
#                                                           "14" = "transformative",
#                                                           "15" = "transformative", # confsal check all
#                                                           "16" = "generic", #?governance?
#                                                           "17" = "transformative" , # "restart\ndecree",
#                                                           "18" = "generic",
#                                                           "19" = "transformative", #*
#                                                           "20" = "transformative", # credit
#                                                           "21" = "generic",
#                                                           "22" = "generic",
#                                                           "23" = "generic", #infrastructure
#                                                           "24" = "restorative", # thousand
#                                                           "25" = "generic" )) #"industry"))
# 
# IT_gamma_termsact <- IT_gamma_termsact %>% mutate(labelcol = recode(topic,
#                                                             "5" = 5,
#                                                             "10" = 10,
#                                                             "2" = 2,
#                                                             "15" = 15,
#                                                             "20" = 20,
#                                                             "9" = 9,
#                                                             "24" = 24,
#                                                             "19" = 19))
# 
# IT_gamma_termsact["labelcol"][is.na(IT_gamma_termsact["labelcol"])] <- 0
# 
# save(IT_gamma_termsact, file = paste0(folder,"IT/IT_gamma_termsact.Rdata"))



# DE_gammatermsact <- DE_gammatermsact %>% mutate(label = recode(topic,
#                                                          "1" = "1 Bridging\nAid", # intended consequences
#                                                          "2" = "2 Stimulus\nPackage", # "stimulus\package",  # "growth",
#                                                          "3" = "3 Families",  # "families", #*
#                                                          "4" = "4 Work\nSecurity",
#                                                          "5" = "5 Corona\nAid" , # "business", minimum short_time_allowance
#                                                          "6" = "6 Electricity\nPrice", # electricity\nprice
#                                                          "7" = "7 Extension\nAid",
#                                                          "8" = "8 Retail\nSector", # "Companies\nRecovery
#                                                          "9" = "9 Economic\nBoost", # Sustainable Goals
#                                                          "10" = "10 Companies\nRecovery",
#                                                          "11" = "11 European\nFunding",
#                                                          "12" = "12 Climate\nProtection", # Families
#                                                          "13" = "13 Digitalization",
#                                                          "14" = "14 Bridging\nAid3",
#                                                          "15" = "15 Basic\nSecurity", #creative industry, economic stabilisation_fund
#                                                          "16" = "16 Special\nCorona Aid",
#                                                          "17" = "17 Lockdown" , # "self\nemployed",
#                                                          "18" = "18 Economic\nStabilisation",
#                                                          "19" = "19 Wumms", # "recession",
#                                                          "20" = "20 Social\nProtection" , #"lockdown",
#                                                          "21" = "21 Debates",
#                                                          "22" = "22 Germany\nin Eu",
#                                                          "23" = "23 Startups",
#                                                          "24" = "24 Gender\nEquality", # voluntary shopping, voluntary initiatives
#                                                          "25" = "25 Tax\nEstimates"))
# 
# DE_gammatermsact <- DE_gammatermsact %>% mutate(policy = recode(topic,
#                                                           "1" = "restorative", # brought cisl
#                                                           "2" = "transformative"  , #, "transitions",
#                                                           "3" = "restorative", #*
#                                                           "4" = "restorative", # "life\nquality",
#                                                           "5" = "restorative"  , #"emergency\naid", # companies
#                                                           "6" = "restorative",
#                                                           "7" = "restorative", # "agribusiness" , # "dc\naugust",
#                                                           "8" = "restorative", # agorarai
#                                                           "9" = "transformative",
#                                                           "10" = "restorative",
#                                                           "11" = "transformative" , # "sustainability",
#                                                           "12" = "transformative", # confederal_secretary, front_page
#                                                           "13" = "transformative", #  # fenealuil #spoke
#                                                           "14" = "restorative",
#                                                           "15" = "transformative", # confsal check all
#                                                           "16" = "restorative", #?governance?
#                                                           "17" = "restorative" , # "restart\ndecree",
#                                                           "18" = "restorative",
#                                                           "19" = "restorative", #*
#                                                           "20" = "restorative", # credit
#                                                           "21" = "restorative",
#                                                           "22" = "transformative",
#                                                           "23" = "transformative", #infrastructure
#                                                           "24" = "transformative", # thousand
#                                                           "25" = "restorative" )) #"industry"))
# 
# 
# DE_gammatermsact <- DE_gammatermsact %>% mutate(labelcol = recode(topic,
#                                                             "2" = 2,
#                                                             "13" = 13,
#                                                             "20" = 20,
#                                                             "19" = 19,
#                                                             "5" = 5,
#                                                             "9" = 9,
#                                                             "8" = 8,
#                                                             "24" = 24))
# 
# DE_gammatermsact["labelcol"][is.na(DE_gammatermsact["labelcol"])] <- 0


df <- rbind(IT_gamma_termsact,DE_gamma_termsact)

DE_gamma_termsact %>%
  ggplot(aes(x = actor, y = gamma, fill = as.factor(labelcol))) +
  geom_col(color = "black") +
 # coord_flip() +
  geom_text(aes(label = ifelse(gamma >= 0.019584737,topic,"") ),
            color = "black", position = position_stack(vjust = 0.5 )) +
  scale_fill_manual(values = c("2" = "salmon",
                                "13" = "green",
                                "20" = "lightblue",
                                "19" = "cyan",
                                "5" = "purple",
                                "9" = "brown2",
                                "8" = "lightgreen",
                                "24" = "tan1",
                                "0" = "white")) + 
  facet_wrap(~ country) +
  labs(y = "Topic proporiton") +
  theme_bw() +
  theme(strip.background = element_rect(fill="beige"), 
        strip.text = element_text(color = "black",size = 16), 
        axis.title.x = element_blank(),
        legend.position = "none")
ggsave(file=paste0(folder,"figures/both_tp2.jpg"),width = 8, height = 5.5)

# Specific section : differences between actors for each country (terms and estimate) ####
# stm_m must be set to either to stm_de25 or stm_it25

td_gamma <- tidy(stm_m, matrix = "gamma")
ID_row <- names(stm_df$documents) # the name of documents gets lost, the row number is reported
td_gamma <- cbind(td_gamma,ID_row) # Here I map each document to its name via row, I checked with content, it works
td_gamma <- cbind(td_gamma,dfb) # merge the gamma matrix with the dataframe via ID, so the variables to sort documents can be used


top_terms_act <- tidy(stm_m) %>%
  rename(actor = y.level) %>% 
group_by(topic,actor,term) %>%
   summarise(beta = mean(beta))  %>%
  arrange(beta) %>% 
 filter(!term %in% de_policies) %>%   # to filter out either it_policies or de_policies
  top_n(5, beta) %>%
  arrange(-beta)%>%
  select(topic, term) %>%
  summarise(terms = list(unique(term))) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms_act <- td_gamma %>%
  group_by(topic,actor) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms_act, by = c("topic","actor")) %>%
  mutate(actor = recode(actor,
                        "POL" = "Political Actors",
                        "TA" = "Trade Associations",
                        "TU" = "Trade Unions")) # %>%
  

# Germany

DE_gamma_termsact <- gamma_terms_act %>% mutate(country = "Germany")

DE_gamma_termsact <- DE_gamma_termsact %>% mutate(label = recode(topic,
                                                               "1" = "1 Bridging\nAid", # intended consequences
                                                               "2" = "2 Stimulus\nPackage", # "stimulus\package",  # "growth",
                                                               "3" = "3 Families",  # "families", #*
                                                               "4" = "4 Work\nSecurity",
                                                               "5" = "5 Corona\nAid" , # "business", minimum short_time_allowance
                                                               "6" = "6 Electricity\nPrice", # electricity\nprice
                                                               "7" = "7 Extension\nAid",
                                                               "8" = "8 Retail\nSector", # "Companies\nRecovery
                                                               "9" = "9 Economic\nBoost", # Sustainable Goals
                                                               "10" = "10 Companies\nRecovery",
                                                               "11" = "11 European\nFunding",
                                                               "12" = "12 Climate\nProtection", # Families
                                                               "13" = "13 Digitalization",
                                                               "14" = "14 Bridging\nAid3",
                                                               "15" = "15 Basic\nSecurity", #creative industry, economic stabilisation_fund
                                                               "16" = "16 Special\nCorona Aid",
                                                               "17" = "17 Lockdown" , # "self\nemployed",
                                                               "18" = "18 Economic\nStabilisation",
                                                               "19" = "19 Wumms", # "recession",
                                                               "20" = "20 Social\nProtection" , #"lockdown",
                                                               "21" = "21 Debates",
                                                               "22" = "22 Germany\nin Eu",
                                                               "23" = "23 Startups",
                                                               "24" = "24 Gender\nEquality", # voluntary shopping, voluntary initiatives
                                                               "25" = "25 Tax\nEstimates"))

DE_gamma_termsact <- DE_gamma_termsact %>% mutate(policy = recode(topic,
                                                                "1" = "restorative", # brought cisl
                                                                "2" = "transformative"  , #, "transitions",
                                                                "3" = "restorative", #*
                                                                "4" = "restorative", # "life\nquality",
                                                                "5" = "restorative"  , #"emergency\naid", # companies
                                                                "6" = "restorative",
                                                                "7" = "restorative", # "agribusiness" , # "dc\naugust",
                                                                "8" = "restorative", # agorarai
                                                                "9" = "transformative",
                                                                "10" = "restorative",
                                                                "11" = "transformative" , # "sustainability",
                                                                "12" = "transformative", # confederal_secretary, front_page
                                                                "13" = "transformative", #  # fenealuil #spoke
                                                                "14" = "restorative",
                                                                "15" = "transformative", # confsal check all
                                                                "16" = "restorative", #?governance?
                                                                "17" = "restorative" , # "restart\ndecree",
                                                                "18" = "restorative",
                                                                "19" = "restorative", #*
                                                                "20" = "restorative", # credit
                                                                "21" = "restorative",
                                                                "22" = "transformative",
                                                                "23" = "transformative", #infrastructure
                                                                "24" = "transformative", # thousand
                                                                "25" = "restorative" )) #"industry"))


DE_gamma_termsact <- DE_gamma_termsact %>% mutate(labelcol = recode(topic,
                                                                  "2" = 2,
                                                                  "13" = 13,
                                                                  "20" = 20,
                                                                  "19" = 19,
                                                                  "5" = 5,
                                                                  "9" = 9,
                                                                  "8" = 8,
                                                                  "24" = 24))

DE_gamma_termsact["labelcol"][is.na(DE_gamma_termsact["labelcol"])] <- 0


# Interaction

prepDE <- estimateEffect(1:numm ~ actor * s(datenum), stm_m, metadata = stm_df$meta, uncertainty = "Global")
save(prepDE,file=paste0(folder,"DE/prepDE.Rdata"))
 
# load(paste0(folder,"DE/prepDE.R"))

load(paste0(folder,"DE/prepDE.Rdata"))
effects_intDE <- get_effects(estimates = prepDE,
                           variable = 'datenum',
                           type = 'continuous',
                           moderator = 'actor',
                           modval = "POL") %>%
  bind_rows(
    get_effects(estimates = prepDE,
                variable = 'datenum',
                type = 'continuous',
                moderator = 'actor',
                modval = "TA") %>%
      
      bind_rows(
        get_effects(estimates = prepDE,
                    variable = 'datenum',
                    type = 'continuous',
                    moderator = 'actor',
                    modval = "TU")
      )
  )


effects_intDE <- effects_intDE %>%
  mutate(moderator = recode(moderator,
                            "POL" = "Political Actors",
                            "TA" = "Trade Associations",
                            "TU" = "Trade Unions"))

# keep_ita <- c(5,6,4,24,2,20,15,19)


depl <- effects_intDE %>%  filter(topic %in% c(2,13,20,19,5,9,8,24)) %>%
  ggplot(aes(x = value, y =  proportion, color = as.factor(topic) #, color = moderator # ,
             #   group = moderator, fill = moderator
  )) +
  geom_line() + 
  scale_x_continuous(breaks = c(18290,18414,18628,18841,18993,19250,19357),
                     labels = c("18290" = "JAN 2020","18414" = "JUN 2020","18628" = "JAN 2021","18841" = "AUG 2021",
                                "18993" = "JAN 2022","19250" = "SEP 2022","19357" = "DEC 2022")) +
   ylab("Expected Proportion") +
  scale_color_manual(values = c("2" = "salmon",
                                "13" = "green",
                                "20" = "lightblue",
                                "19" = "cyan",
                                "5" = "purple",
                                "9" = "brown2",
                                "8" = "lightgreen",
                                "24" = "tan1"),
                     name = "Topic") +
  facet_wrap(~ moderator, scales = "free_y", nrow = 3) +
  theme_light() +
  theme(axis.text.x = element_text(vjust = 0.6,angle=45, size = 16), axis.title.x = element_blank(),
        axis.text.y = element_text(size =  14), 
        axis.title.y = element_text(size =  16),
        strip.background = element_rect(fill="beige"), 
        strip.text = element_text(color = "black",size = 21),
        legend.position = "none",
        plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), 
                           "inches")
        )


load(paste0(folder,"DE/DE_gamma_termsact.Rdata"))
trdeac <- DE_gamma_termsact %>% # ggplot(aes(-rank, gamma, label = terms, fill = country)) +
   filter(topic %in% c(2,13,20,19,5,9,8,24)) %>%
   ggplot(aes(x = reorder_within(topic,gamma,actor), y = gamma, 
              fill = as.factor(topic))) +
  geom_col() +
  geom_text(aes(label = frex ), hjust = 0, y = 0.001,# nudge_y = 0.00005, size = 5, # 0.0005 #terms
           size = 6, family = "IBMPlexSans") +
  scale_fill_manual(values = c("2" = "salmon",
                               "13" = "green",
                               "20" = "lightblue",
                               "19" = "cyan",
                               "5" = "purple",
                               "9" = "brown2",
                               "8" = "lightgreen",
                               "24" = "tan1"),
                    labels = c("2" = "2: Stimulus package",
                               "13" = "13: Digitalization",
                               "20" = "20: Social protection",
                               "19" = "19: Wumms",
                               "5" = "5: Corona aid",
                               "9" = "9: Economic boost",
                               "8" = "8: Retail sector",
                               "24" = "24: Gender equality"),
                     name = "Topic") + 
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.105) ,
                     labels = scales::percent_format()) +
  scale_x_reordered() +
  facet_wrap(~ actor, scales = "free_y", nrow = 3 ) +
  # theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme_light() +
  theme( strip.background = element_rect(fill="beige"), 
         strip.text = element_text(color = "black",size = 19),
        plot.subtitle = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 17),
        legend.position="bottom",
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 20),
        plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), 
                           "inches")
        ) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = NULL, y = "Topic Proportion")

deintpl<- ggarrange(trdeac,depl, ncol = 2, widths = c(1.4,1))
deintpl <- annotate_figure(deintpl, top = text_grob("Germany", 
                                                    color = "black", face = "bold", size = 23))
ggsave(deintpl, file = paste0(folder,"figures/actDE.jpg"),width = 19, height = 13)

# Italy



IT_gamma_termsact <- gamma_terms_act

IT_gamma_termsact <- IT_gamma_termsact %>% mutate(label = recode(topic,
                                                                 "1" = "1 Dc\nSupports", # brought cisl
                                                                 "2" = "2 Pnrr\nInvestments"  , #, "transitions",
                                                                 "3" = "3 Funds\nAllocations", #*
                                                                 "4" = "4 Dc\nRelaunch", # "life\nquality",
                                                                 "5" = "5 Emergency\nCompany"  , #"emergency\naid", # companies
                                                                 "6" = "6 Liquidity",
                                                                 "7" = "7 Agribusiness", # "agribusiness" , # "dc\naugust",
                                                                 "8" = "8 Tourism", # agorarai
                                                                 "9" = "9 Social\nConfrontations",
                                                                 "10" = "10 European\nFunding",
                                                                 "11" = "11 National\nRecovery" , # "sustainability",
                                                                 "12" = "12 Investments\nBoost", # confederal_secretary, front_page
                                                                 "13" = "13 Sustainable\nGrowth", #  # fenealuil #spoke
                                                                 "14" = "14 Pa\nSimplification",
                                                                 "15" = "15 Pnrr\nImplementation", # confsal check all
                                                                 "16" = "16 Funds\nAnnouncement", #?governance?
                                                                 "17" = "17 Recoveryplan" , # "restart\ndecree",
                                                                 "18" = "18 Contracts",
                                                                 "19" = "19 Employment\nEquality", #*
                                                                 "20" = "20 South", # credit
                                                                 "21" = "21 Funding\nAccess",
                                                                 "22" = "22 Public\nAdministration",
                                                                 "23" = "23 Actors", #infrastructure
                                                                 "24" = "24 Recovery\nInterventions", # thousand
                                                                 "25" = "25 Pnrr" )) #"industry"))

IT_gamma_termsact <- IT_gamma_termsact %>% mutate(policy = recode(topic,
                                                                  "1" = "restorative", # brought cisl
                                                                  "2" = "transformative"  , #, "transitions",
                                                                  "3" = "transformative", #*
                                                                  "4" = "restorative", # "life\nquality",
                                                                  "5" = "restorative"  , #"emergency\naid", # companies
                                                                  "6" = "restorative",
                                                                  "7" = "generic", # "agribusiness" , # "dc\naugust",
                                                                  "8" = "restorative", # agorarai
                                                                  "9" = "generic",
                                                                  "10" = "transformative",
                                                                  "11" = "transformative" , # "sustainability",
                                                                  "12" = "generic", # confederal_secretary, front_page
                                                                  "13" = "transformative", #  # fenealuil #spoke
                                                                  "14" = "transformative",
                                                                  "15" = "transformative", # confsal check all
                                                                  "16" = "generic", #?governance?
                                                                  "17" = "transformative" , # "restart\ndecree",
                                                                  "18" = "generic",
                                                                  "19" = "transformative", #*
                                                                  "20" = "transformative", # credit
                                                                  "21" = "generic",
                                                                  "22" = "generic",
                                                                  "23" = "generic", #infrastructure
                                                                  "24" = "restorative", # thousand
                                                                  "25" = "generic" )) #"industry"))

IT_gamma_termsact <- IT_gamma_termsact %>% mutate(labelcol = recode(topic,
                                                                    "5" = 5,
                                                                    "10" = 10,
                                                                    "2" = 2,
                                                                    "15" = 15,
                                                                    "20" = 20,
                                                                    "9" = 9,
                                                                    "24" = 24,
                                                                    "19" = 19))

IT_gamma_termsact["labelcol"][is.na(IT_gamma_termsact["labelcol"])] <- 0

IT_gamma_termsact <- IT_gamma_termsact %>% mutate(country = "Italy")

# save(IT_gamma_termsact, file = paste0(folder,"IT/IT_gamma_termsact.Rdata"))

load(paste0(folder,"IT/IT_gamma_termsact.Rdata"))
tritac <- IT_gamma_termsact %>% # ggplot(aes(-rank, gamma, label = terms, fill = country)) +
   filter(topic %in% c(5,10,2,15,20,9,24,19)) %>%
  ggplot(aes(x = reorder_within(topic,gamma,actor), y = gamma, 
             fill = as.factor(topic))) +
  geom_col() +
  geom_text(aes(label = frex ), hjust = 0, y = 0.001,# nudge_y = 0.00005, size = 5, # 0.0005
            size = 6, family = "IBMPlexSans") +
  scale_fill_manual(values = c("5" = "salmon",
                               "10" = "green",
                               "2" = "lightblue",
                               "15" = "cyan",
                               "20" = "purple",
                               "9" = "brown2",
                               "24" = "lightgreen",
                               "19" = "tan1"),
                    labels = c("5" = "5: Emergency companies",
                               "10" = "10: European funding",
                               "2" = "2: Pnrr investments",
                               "15" = "15: Pnrr implementation",
                               "20" = "20: South",
                               "9" = "9: Social confrontations",
                               "24" = "24: Recovery interventions",
                               "19" = "19: Employment equality"),
                    name = "Topic") + 
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.105) ,
                     labels = scales::percent_format()) +
  scale_x_reordered() +
  facet_wrap(~ actor, scales = "free_y", nrow = 3 ) +
  # theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme_light() +
  theme( strip.background = element_rect(fill="beige"), 
         strip.text = element_text(color = "black",size = 19),
         plot.subtitle = element_text(size = 18),
         axis.text.x = element_text(size = 16),
         axis.text.y = element_text(size = 16),
         axis.title.x = element_text(size = 17),
         legend.position="bottom",
         legend.text = element_text(size = 17),
         legend.title = element_text(size = 20),
         plot.margin = unit(c(0.15, 0.4, 0.15, 0.15), 
                            "inches")
  ) +
  guides(fill = guide_legend(nrow = 3)) +
  labs(x = NULL, y = "Topic Proportion")

# Interaction estimate


prepIT <- estimateEffect(1:numm ~ actor * s(datenum), stm_m, metadata = stm_df$meta, uncertainty = "Global")
save(prepIT,file=paste0(folder,"IT/prepIT.Rdata"))

load(paste0(folder,"IT/prepIT.Rdata"))
effects_intIT <- get_effects(estimates = prepIT,
                             variable = 'datenum',
                             type = 'continuous',
                             moderator = 'actor',
                             modval = "POL") %>%
  bind_rows(
    get_effects(estimates = prepIT,
                variable = 'datenum',
                type = 'continuous',
                moderator = 'actor',
                modval = "TA") %>%
      
      bind_rows(
        get_effects(estimates = prepIT,
                    variable = 'datenum',
                    type = 'continuous',
                    moderator = 'actor',
                    modval = "TU")
      )
  )


effects_intIT <- effects_intIT %>%
  mutate(moderator = recode(moderator,
                            "POL" = "Political Actors",
                            "TA" = "Trade Associations",
                            "TU" = "Trade Unions"))


itpl <- effects_intIT %>%  filter(topic %in% c(5,10,2,15,20,9,24,19)) %>%
   ggplot(aes(x = value, y =  proportion, color = as.factor(topic) #, color = moderator # ,
             #   group = moderator, fill = moderator
  )) +
  geom_line() +
   scale_x_continuous(breaks = c(18290,18414,18628,18841,18993,19250,19357),
                     labels = c("18290" = "JAN 2020","18414" = "JUN 2020","18628" = "JAN 2021","18841" = "AUG 2021",
                                "18993" = "JAN 2022","19250" = "SEP 2022","19357" = "DEC 2022")) +
   ylab("Expected Proportion") +
   scale_color_manual(values = c("5" = "salmon",
                               "10" = "green",
                               "2" = "lightblue",
                               "15" = "cyan",
                               "20" = "purple",
                               "9" = "brown2",
                               "24" = "lightgreen",
                               "19" = "tan1"),
                    labels = c("5" = "Emergency companies",
                               "10" = "European funding",
                               "2" = "Pnrr investments",
                               "15" = "Pnrr implementation",
                               "20" = "South",
                               "9" = "Social confrontations",
                               "24" = "Recovery interventions",
                               "19" = "Employment equality"),
                    name = "Topic") +
  facet_wrap(~ moderator, scales = "free_y", nrow = 3) +
  theme_light() +
  theme(axis.text.x = element_text(vjust = 0.6,angle=45, size = 16), axis.title.x = element_blank(),
        axis.text.y = element_text(size =  14), 
        axis.title.y = element_text(size =  16),
        strip.background = element_rect(fill="beige"), 
        strip.text = element_text(color = "black",size = 21),
        legend.position = "none",
        plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), 
                           "inches")
  )

itintpl <- ggarrange(tritac,itpl, ncol = 2, widths = c(1.4,1))
itintpl <- annotate_figure(itintpl, top = text_grob("Italy", 
                                      color = "black", face = "bold", size = 23))
ggsave(itintpl, file = paste0(folder,"figures/actIT.jpg"), width = 19, height = 13)
 
# Results: wordfish ####
# 
# require(quanteda)
# require(quanteda.textmodels)
# require(quanteda.textplots)
# load(paste0(folder,"DE/tmod_DE.Rdata"))
# 
# textplot_scale1d(tmod_DE, margin = "features", 
#                  highlighted = de_policies)
# 
# # top_terms_df <- tidy(stm_m) %>%
# #   arrange(beta) %>%
# #   group_by(topic) %>%
# #   # filter(!term %in% policies) %>%   # to filter out either it_policies or de_policies
# #   top_n(10, beta) %>%
# #   arrange(-beta)
# 
# term <- tmod_DE$features 
# betawf <- tmod_DE$beta
# dfwd <- data.frame(term,betawf)
# 
# top_terms_df <- tidy(stm_m) %>%
#   arrange(beta) %>%
#   group_by(topic,term) %>%
#   summarise(beta = mean(beta)) %>%
#   # filter(!term %in% policies) %>%   # to filter out either it_policies or de_policies
#   top_n(30, beta) %>%
#   arrange(-beta) # %>%
#   # select(topic, term) %>%
#   # summarise(terms = list(unique(term))) %>%
#   # mutate(terms = map(terms, paste, collapse = ", ")) %>% 
#   # unnest()
# 
# dfwd2 <- dfwd %>% filter(dfwd$term %in% top_terms_df$term)
# 
# dfwd2$quantile <- 0
# dfwd2[dfwd2$betawf >= -6.05469949 & dfwd2$betawf <= -0.86420861,]$quantile <- 1
# dfwd2[dfwd2$betawf > -0.86420861 & dfwd2$betawf <= -0.43808321,]$quantile <- 2
# dfwd2[dfwd2$betawf > -0.43808321 & dfwd2$betawf <= -0.14864741,]$quantile <- 3
# dfwd2[dfwd2$betawf > -0.14864741 & dfwd2$betawf <= 0.02654563,]$quantile <- 4
# 
# top_terms_df <-  merge(dfwd2,top_terms_df,by = "term")
# 
# top_terms_df2 <- top_terms_df %>% group_by(topic) %>% summarise(gradient = sum(betawf))
# 
# top_terms_df <-  merge(top_terms_df,top_terms_df2,by = "topic")
# 
# DE_gammaterms <- merge(DE_gammaterms,top_terms_df2,by = "topic")
# save(IT_gammaterms, file = paste0(folder,"IT/IT_gammaterms.Rdata"))

############
# 
# load("wordembedding456.Rdata")
# 
# wordembedding %>% 
#   filter(! word %in% c(stopwords_en,rem_char,rem_dfm)) %>%
#   ggplot(aes(x=V2, y=V9, label=word))+
#   geom_text(aes(label=word),hjust=0, vjust=0, color="blue")+
#   theme_minimal()+
#   xlab("First Dimension Created by SVD")+
#   ylab("Second Dimension Created by SVD")


############
# all actors prob per topic

stm_m <- stm_it25
tidystm <- tidy(stm_m)
tidystm <- rename(tidystm, actor = y.level)
stm_df <- stm_df_it
titleplot <- "Italy"
numm <- 25
# policies <- de_policies

prep <- estimateEffect(1:numm ~ actor * s(datenum), stm_m, metadata = stm_df$meta, uncertainty = "Global")

effects_int <- get_effects(estimates = prep,
                           variable = 'datenum',
                           type = 'continuous',
                           moderator = 'actor',
                           modval = "POL") %>%
  bind_rows(
    get_effects(estimates = prep,
                variable = 'datenum',
                type = 'continuous',
                moderator = 'actor',
                modval = "TA") %>%
      
      bind_rows(
        get_effects(estimates = prep,
                    variable = 'datenum',
                    type = 'continuous',
                    moderator = 'actor',
                    modval = "TU")
      )
  )



for (i in c(1:numm)) {
  
  tm <-  effects_int %>%  filter(topic == i) %>%
    # mutate(moderator = as.factor(moderator)) %>%
    # filter(moderator == "TU") %>%
    ggplot(aes(x = value, y = proportion, color = moderator # ,
               #   group = moderator, fill = moderator
    )) +
    geom_line() +
    # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
    scale_x_continuous(breaks = c(18290,18414,18628,18841,18993,19250,19357),
                       labels = c("18290" = "JAN 2020","18414" = "JUN 2020","18628" = "JAN 2021","18841" = "AUG 2021",
                                  "18993" = "JAN 2022","19250" = "SEP 2022","19357" = "DEC 2022")) +
    ggtitle(paste(titleplot, "Topic: ",i)) + 
    ylab("Expected Proportion") +
    # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
    # facet_wrap(~ topic_f,labeller=labeller(topic_f = lbs), scales = "free") +  # c("6" ="6: Digital Education","9" = "9: Families"))) +
    scale_color_manual(labels = c("Political\nActors","Trade\nAssociations","Trade\nUnions"),
                       values = c("red","green","blue")) +
    theme_light() +
    theme(axis.text.x = element_text(vjust = 0.6,angle=45), axis.title.x = element_blank(),
          strip.background = element_rect(fill="beige"), 
          strip.text = element_text(color = "black",size = 12),
          legend.position = "bottom") 
  #plotly::ggplotly(pl)
  # ggsave(paste0(folder,"results25/",i,titleplot,"_time.jpg"),width = 12,height = 7)
  
  
  wd <-  tidystm %>% filter(topic == i) %>%
  #  filter(! term %in% policies) %>%
    group_by(actor) %>%
    arrange(-beta) %>%
    top_n(10,beta) %>%
    ggplot(aes(reorder(term,beta),beta,fill = actor)) +
    geom_col(show.legend = FALSE) +
    scale_y_continuous(label = scales::percent ) +
    #  scale_y_continuous(breaks = ~ c(min(.x), max(.x))) +
    facet_wrap(~ actor, scales = "free",
               labeller = labeller(actor = c("TU" = "Trade Unions","TA" = "Trade Associations", "POL" = "Political Actors"))) +         
    coord_flip() +
    # ggtitle(paste("Topic: ",i)) +
    xlab("") +
    ylab("Probability words per actor") +
    theme(axis.title.x = element_blank()) +
    theme_bw()
  #  ggsave(paste0(folder,"/results/",titleplot,"_wordactor",i,".jpg"),width = 11,height = 5) 
  
  cm <- grid.arrange(tm,wd,ncol = 2)
  ggsave(cm,file = paste0(folder,"IT/IT25/",titleplot,"comb_",i,".jpg"),width = 14, height = 3.5)
  
  
} 

# Main effects ####

# Italy #####

load(paste0(folder,"IT/stm_it25.Rdata")) 
load(paste0(folder,"IT/dfm_it_en.Rdata")) 
stm_df_it <- quanteda::convert(dfm_it_en,to = "stm")  
numm <- 25
#

prepITmain <- estimateEffect(1:numm ~ actor * s(datenum), stm_it25, metadata = stm_df_it$meta, uncertainty = "Global")
save(prepITmain,file=paste0(folder,"IT/prepITmain.Rdata"))

load(paste0(folder,"IT/prepITmain.Rdata"))
effects_IT <- get_effects(estimates = prepITmain,
                             variable = 'datenum',
                             type = 'continuous')

# for (i in c(1:numm)){
  
pl <-  effects_IT %>% filter(topic == i) %>%
    ggplot(aes(x = value, y = proportion)) +
    geom_line() +
    # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
    scale_x_continuous(breaks = c(18290,18414,18628,18841,18993,19250,19357),
                       labels = c("18290" = "JAN 2020","18414" = "JUN 2020","18628" = "JAN 2021","18841" = "AUG 2021",
                                  "18993" = "JAN 2022","19250" = "SEP 2022","19357" = "DEC 2022")) +
    ggtitle(paste(titleplot, "Topic: ",i)) + 
    ylab("Expected Proportion") +
    # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
    # facet_wrap(~ topic_f,labeller=labeller(topic_f = lbs), scales = "free") +  # c("6" ="6: Digital Education","9" = "9: Families"))) +
    # scale_color_manual(labels = c("Political\nActors","Trade\nAssociations","Trade\nUnions"),
    #                    values = c("red","green","blue")) +
    theme_light() +
    theme(axis.text.x = element_text(vjust = 0.6,angle=45, size = 13), 
          axis.title.x = element_blank(),
          strip.background = element_rect(fill="beige"), 
          strip.text = element_text(color = "black",size = 13),
          legend.position = "bottom") 
ggsave(pl,file = paste0(folder,"IT/IT25/",titleplot,"main_",i,".jpg"))
  
# }

mina_itpl <- effects_IT %>% filter(topic %in% c(5,10,2,15,20,9,24,19)) %>%
  ggplot(aes(x = value, y = proportion, color = as.factor(topic))) +
  geom_line(linewidth=1.1) +
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
  scale_x_continuous(breaks = c(18322,18414,18628,18841,18993,19250,19357),
                     labels = c("18322" = "MAR 2020","18414" = "JUN 2020","18628" = "JAN 2021","18841" = "AUG 2021",
                                "18993" = "JAN 2022","19250" = "SEP 2022","19357" = "DEC 2022")) +
 # ggtitle("Italy") + 
  ylab("Expected Proportion") +
 scale_color_manual(values = c("5" = "salmon",
                               "10" = "green",
                               "2" = "lightblue",
                               "15" = "cyan",
                               "20" = "purple",
                               "9" = "brown2",
                               "24" = "lightgreen",
                               "19" = "tan1"),
                    labels = c("5" = "Emergency companies",
                               "10" = "European funding",
                               "2" = "Pnrr investments",
                               "15" = "Pnrr implementation",
                               "20" = "South",
                               "9" = "Social confrontations",
                               "24" = "Recovery interventions",
                               "19" = "Employment equality"),
                    name = "Topic") +
                    
  theme_light() +
  theme(axis.text.x = element_text(vjust = 0.6,angle=45, size = 21), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 21),
        axis.title.y = element_text(size = 22),
        strip.background = element_rect(fill="beige"), 
        strip.text = element_text(color = "black",size = 12),
        legend.position = "bottom") +
  guides(color = "none")
ggsave(file=paste0(folder,"IT/IT25/main_it.jpg"),width = 10,height = 6)

# Germany

load(paste0(folder,"DE/stm_de25.Rdata")) 
load(paste0(folder,"DE/dfm_de_en.Rdata")) 
stm_df_de <- quanteda::convert(dfm_de_en,to = "stm")  
numm <- 25
#

prepDEmain <- estimateEffect(1:numm ~ actor * s(datenum), stm_de25, metadata = stm_df_de$meta, uncertainty = "Global")
save(prepDEmain,file=paste0(folder,"DE/prepDEmain.Rdata"))

load(paste0(folder,"DE/prepDEmain.Rdata"))
effects_DE <- get_effects(estimates = prepDEmain,
                          variable = 'datenum',
                          type = 'continuous')


mina_depl <- effects_DE %>% filter(topic %in% c(2,13,20,19,5,9,8,24)) %>%
  ggplot(aes(x = value, y = proportion, color = as.factor(topic))) +
  geom_line(linewidth=1.1) +
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)  +
  scale_x_continuous(breaks = c(18322,18414,18628,18841,18993,19250,19357),
                     labels = c("18322" = "MAR 2020","18414" = "JUN 2020","18628" = "JAN 2021","18841" = "AUG 2021",
                                "18993" = "JAN 2022","19250" = "SEP 2022","19357" = "DEC 2022")) +
#  ggtitle("Germany") + 
  ylab("Expected Proportion") +
  scale_color_manual(values = c("2" = "salmon",
                               "13" = "green",
                               "20" = "lightblue",
                               "19" = "cyan",
                               "5" = "purple",
                               "9" = "brown2",
                               "8" = "lightgreen",
                               "24" = "tan1"),
                    labels = c("2" = "Stimulus package",
                               "13" = "Digitalization",
                               "20" = "Social protection",
                               "19" = "Wumms",
                               "5" = "Corona aid",
                               "9" = "Economic boost",
                               "8" = "Retail sector",
                               "24" = "Gender equality"),
                    name = "Topic") +
  theme_light() +
  theme(axis.text.x = element_text(vjust = 0.6,angle=45, size = 21), 
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 21),
        axis.title.y = element_text(size = 22),
        strip.background = element_rect(fill="beige"), 
        strip.text = element_text(color = "black",size = 12),
        legend.position = "bottom") +
  guides(color = "none")
ggsave(file=paste0(folder,"DE/DE25/main_de.jpg"),width = 10,height = 6)


ggarrange(tpde,tpit,mina_depl,mina_itpl,nrow=2,ncol =2, heights = c(3,1), widths = c(1.1,1))
# ggarrange(tpde,tpit,mina_depl,mina_itpl,nrow=2,ncol =2, widths = c(2.5,1))
ggsave(paste0(folder,"figures/topicgen.jpg"), width = 33, heigh = 28)


# FREX

# DE_gamma_termsact <- DE_gamma_termsact[,-9]
frex_de <- read.csv(paste0(folder,"DE/frex_DE25_2.csv"),sep = ";")
DE_gamma_termsact <- merge(DE_gamma_termsact,frex_de,by = c("topic", "actor" ))
save(DE_gamma_termsact, file = paste0(folder,"DE/DE_gamma_termsact.Rdata"))

# IT_gamma_termsact <- IT_gamma_termsact[,-9]
frex_it <- read.csv(paste0(folder,"IT/frex_IT25_2.csv"),sep = ";")
IT_gamma_termsact <- merge(IT_gamma_termsact,frex_it,by = c("topic", "actor" ))
save(IT_gamma_termsact, file = paste0(folder,"IT/IT_gamma_termsact.Rdata"))
