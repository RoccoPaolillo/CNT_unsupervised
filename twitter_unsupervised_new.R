# uploads ####
options(scipen = 999)
library(gdata)
# for crawling Twitter data 
library(academictwitteR)
# library(rtweet)
library(corpus)
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

# "%nin%" <- Negate("%in%")

setwd("C:/Users/rocpa/OneDrive/Desktop/CNT/twitter_unsupervised/from_server/16_11_2022/")

# Dataframe processing and dfm creation (lists for processing in keywords_cnt.xls)

load("df_20_22.Rdata")
df_20_22 <- df_20_22[df_20_22$selected_2 == 1,]
# df_20_22 <- rename(df_20_22, original_language = text)
# df_20_22 <- rename(df_20_22, text = en_text)
# save(df_20_22,file="df_20_22.Rdata")

# for cleaning and processing
df_20_22$text <- str_replace_all(df_20_22$text,"\\b's\\b","") 
df_20_22$text <- str_replace_all(df_20_22$text,"\\b-\\b","_") 

# German-specific processing
key_de <- read.xls("keywords_cnt.xls",sheet = "lemma_de", encoding = "latin1")[,1]
key_de <- paste0("\\b",key_de,"\\b")
lemma_de <- read.xls("keywords_cnt.xls",sheet = "lemma_de", encoding="latin1" )[,2]
# lemma_de <- paste0("\\b",lemma_de,"\\b")
names(lemma_de) <- key_de
df_20_22[df_20_22$country == "Germany",]$text <- str_replace_all(df_20_22[df_20_22$country == "Germany",]$text,lemma_de) 

# Italian-specific processing
key_it <- read.xls("keywords_cnt.xls",sheet = "lemma_it", encoding = "latin1")[,1]
key_it <- paste0("\\b",key_it,"\\b")
lemma_it <- read.xls("keywords_cnt.xls",sheet = "lemma_it", encoding="latin1" )[,2]
# lemma_de <- paste0("\\b",lemma_de,"\\b")
names(lemma_it) <- key_it
df_20_22[df_20_22$country == "Italy",]$text <- str_replace_all(df_20_22[df_20_22$country == "Italy",]$text,lemma_it) 


# # English common terms
eng <- read.xls("keywords_cnt.xls",sheet = "lemma_eng", encoding = "latin1")[,1]
eng <- paste0("\\b",eng,"\\b")
lemma_eng <- read.xls("keywords_cnt.xls",sheet = "lemma_eng", encoding="latin1" )[,2]
# lemma_de <- paste0("\\b",lemma_de,"\\b")
names(lemma_eng) <- eng
df_20_22$text <- str_replace_all(df_20_22$text,lemma_eng)

# compounds
cmpd <- read.xls("keywords_cnt.xls",sheet = "compound_dfm", encoding = "latin1")[,1]
cmpd <- paste0("\\b",cmpd,"\\b")
cmpdlinked <- read.xls("keywords_cnt.xls",sheet = "compound_dfm", encoding="latin1" )[,2]
names(cmpdlinked) <- cmpd
df_20_22$text <- str_replace_all(df_20_22$text,cmpdlinked)

#

dfdelete <- read.xls("keywords_cnt.xls",sheet = "usernamedelete", encoding = "latin1")

for (i in dfdelete$usrnm) {
  
  p <- dfdelete[dfdelete$usrnm == i,]$word
  p <- paste0("\\b",p,"\\b")
  d <- rep(" ",length(p))
  names(d) <- p
  
  df_20_22[df_20_22$user_username == i,]$text <- str_replace_all(df_20_22[df_20_22$user_username == i,]$text,d)
  
}

df_20_22$text <- str_squish(df_20_22$text)

#


#


# remove words
rem_dfm <- read.xls("keywords_cnt.xls",sheet = "rem_dfm", encoding = "latin1")[,1]
rem_dfm <- unique(rem_dfm)

## German dfm

dfm_de_en <- tokens(corpus(df_20_22[df_20_22$country == "Germany",]),
                remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>% 
  tokens_remove(c("http*","@*","€","+","|","s","faq","=","für","von",stopwords("en"),stopwords_en,
                  rem_dfm)) %>%
  dfm()

dfm_de_en
topfeatures(dfm_de_en,50)
save(dfm_de_en,file="data_new/save_27/dfm_de_en.Rdata")

## Italian dfm

dfm_it_en <- tokens(corpus(df_20_22[df_20_22$country == "Italy",]),
                 remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>% 
  tokens_remove(c("http*","@*","€","+","|","faq","l","s","il","la","=","di",stopwords("en"),stopwords_en,
                  rem_dfm)) %>%
  dfm() 


dfm_it_en
topfeatures(dfm_it_en,50)

save(dfm_it_en,file="data_new/save_27/dfm_it_en.Rdata")



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
unique(stringr::str_extract_all(df_20_22[df_20_22$selected_2 == 1 & df_20_22$country == "Italy",]$text,
                                "\\b[:alnum:]*\\-?[:alnum:]*\\-?[:alnum:] sized supplier\\-?[:alnum:]*\\-?[:alnum:]*\\b"))

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



# Results ####

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
  "next_generation",
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
  "next_generation",
  "nextgenerationeu",
  "recoveryplan",
  "pnrr",
  "reacteu",
  "esf"
)



folder <- "data_new/save_27/"  # for output location

# sample

# load("df_20_22.Rdata")
# df_20_22 <- df_20_22[df_20_22$selected_2 == 1,]

ggplot(df_20_22,aes(x = actor)) + geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 0) +
  facet_wrap(~country) +
  theme_bw()
ggsave(paste0(folder,"results/sample.jpg"),width = 5, height = 4)

df <- df_20_22 %>% unnest_tokens(word, text, token = stringr::str_split, pattern = " ")
df$word <- str_remove_all(df$word, "[^#_[:^punct:]]")
df <- df %>% filter(!word %in% stop_words$word,
                                       # !word %in% rem_char,
                                        !word %in% stopwords("en"), 
                                        !word %in% stopwords_en,
                                        !word %in% rem_dfm)
#  !word %in% "^[0-9]*$")

df <- replace(df, df =='', NA)
df <- df %>% drop_na(word)

df <- df %>% 
  filter(!word %in% c(it_policies,de_policies)) %>%
  count(user_username,country, actor, word,sort = TRUE)

total_words <- df %>% 
  group_by(user_username) %>% 
 group_by(country, actor) %>%
  summarize(total = sum(n))

df <- left_join(df, total_words)

 df$segment <- paste0(df$country,"_",df$actor) 

df_tf_idf <- df %>% bind_tf_idf(word,segment, n)

df_tf_idf %>%  arrange(desc(tf_idf))

# df_tf_idf[df_tf_idf$word == "corona",]

df_tf_idf %>%
  group_by(country, actor) %>%
  slice_max(tf_idf, n = 10) %>%
  group_by(segment,tf_idf) %>%
#  mutate(labelling = paste0(word,collapse = ", ")) %>%
  # df_tf_idf %>%
  #   group_by(country, monthyear) %>%
  #   slice_max(tf_idf, n = 1) 
  ungroup() %>%
  ggplot(aes(x = reorder(word,tf_idf),y = tf_idf)) + 
  geom_col() +
  coord_flip() +
  facet_wrap(~ segment, scales = "free")




# Topic extraction

load(paste0(folder,"DE/stm_de25.Rdata")) 

load(paste0(folder,"DE/dfm_de_en.Rdata")) 

stm_m <- stm_de25
stm_m 
stm_m$settings$call # to check stm
numm <- 25 # number of topics
# tpreg <- "cn" # for topic formulation identification
titleplot <- "Germany"
stm_df <- quanteda::convert(dfm_de_en,to = "stm")  
policies <- de_policies

# dataframe used for findthoughts (make sure titleplot is the country needed)
dfb <- df_20_22[df_20_22$country == titleplot & df_20_22$selected_2 == 1,]


# Extraction for the report

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
write.csv(long_report,file=paste0(folder,"DE/longrep_",titleplot,numm,".csv"),row.names = F, col.names=T,  sep=";",  fileEncoding = "UTF-8")

# # Transformative / Restorative
# 
# td_beta <- tidy(stm_m) %>% group_by(y.level,topic)  %>% filter(topic == 2) %>% top_n(30, beta)

# Topic Proportion

td_gamma <- tidy(stm_m, matrix = "gamma")
ID_row <- names(stm_df$documents) # the name of documents gets lost, the row number is reported
td_gamma <- cbind(td_gamma,ID_row) # Here I map each document to its name via row, I checked with content, it works
td_gamma <- cbind(td_gamma,dfb) # merge the gamma matrix with the dataframe via ID, so the variables to sort documents can be used



# tidy global topic proportion (filter if filtering out labels for it_policies or de_policies)

top_terms <- tidy(stm_m) %>%
  arrange(beta) %>%
  group_by(topic) %>%
 # filter(!term %in% policies) %>%   # to filter out either it_policies or de_policies
  top_n(7, beta) %>%
  arrange(-beta)%>%
  select(topic, term) %>%
  summarise(terms = list(unique(term))) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

# gamma_terms <- td_gamma %>%
#   group_by(topic) %>%
#  # filter(word %nin% it_policies) %>%
#   summarise(gamma = mean(gamma)) %>%
#   arrange(desc(gamma)) %>%
#   left_join(top_terms, by = "topic") %>%
#   mutate(gamma = paste0("(",round(gamma,4)*100,"%)")) 


gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic")
# %>%
#   mutate(topic = paste0("Topic ", topic),
#          topic = reorder(topic, gamma))

gamma_terms$country <- "Germany"
gamma_terms$policy <- "transformative"
gamma_terms[c(4,3,16,20,1,21,25),]$policy<- "restorative"
gamma_terms <- gamma_terms[order(-gamma_terms$gamma),]
gamma_terms$rank <- 1:25
DE_gammaterms <- gamma_terms

save(DE_gammaterms, file = paste0(folder,"test/DE_gammaterms.Rdata"))

# 

df <- rbind(DE_gammaterms,IT_gammaterms)

df %>% ggplot(aes(-rank, gamma, label = terms, fill = country)) +
 geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, y = 0.001,# nudge_y = 0.00005, size = 5, # 0.0005
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.10) ,
                     labels = scales::percent_format()) +
facet_wrap(~ country , scales = "free") +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 20,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13),
        axis.text.y = element_text(size = 15)) +
  labs(x = NULL, y = expression(gamma))









#

gamma_terms %>%
  # top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
   geom_text(hjust = 0, y = 0.001,# nudge_y = 0.00005, size = 5, # 0.0005
           family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
            limits = c(0, 0.10) , 
  labels = scales::percent_format()) +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 20,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13),
        axis.text.y = element_text(size = 15)) +
  labs(x = NULL, y = expression(gamma))



# ggsave("topic_de.jpg",width = 18,height = 9)



# gamma_terms %>%
#   # top_n(20, gamma) %>%
#   ggplot(aes(topic, gamma, label = terms, fill = topic)) +
#   geom_col(show.legend = FALSE) +
#   # geom_text(hjust = 0.2, nudge_y = 0.00005, size = 5, # 0.0005
#   #           family = "IBMPlexSans") +
#   geom_text() +
#   coord_flip() +
#   scale_y_continuous(expand = c(0,0),
#                      limits = c(0, 0.10) , 
#                      labels = scales::percent_format() ) +
#   theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
#   theme(plot.title = element_text(size = 20,
#                                   family="IBMPlexSans-Bold"),
#         plot.subtitle = element_text(size = 13),
#         axis.text.y = element_text(size = 15)) +
#   labs(x = NULL, y = expression(gamma))

# tidy(stm_m) %>%
#   group_by(topic) %>%
#   top_n(10) %>%
#  # ungroup %>%
#  # mutate(term = reorder_within(term, beta, topic)) %>%
#   mutate(term =  reorder(term, beta))  %>%
#   mutate(topiclab = paste0("Topic ",topic)) %>%
#   mutate(topic2 = factor(topiclab, levels = unique(topiclab[order(topic)]))) %>%
#   #  filter(term %in% key_de & beta > 0.002) %>%
#   ggplot(aes(beta,term, fill = topiclab)) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic2, scales = "free") +
#   scale_y_reordered() +
#   # coord_flip() +
#   xlab("Probability words belonging to topic") +
#   # ylab("") +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, 
#                                                                           vjust = 1, hjust=1))
# plot(betapic)













# SANKEY
library(networkD3)
library(dplyr)

tidystm <- tidy(stm_m)  %>% group_by(term,topic) %>%  summarise(beta = mean(beta))
# tidystm <- rename(tidystm, actor = y.level)

a <- tidystm %>% filter(term %in% policies) # select policies to label on starting column: Italian or German
a$topic <- as.character(a$topic)
gamma_terms$terms <- as.character(gamma_terms$terms)

for (i in c(1:numm)){
  
  a[a$topic == i,]$topic <- paste(a[a$topic == i,]$topic,gamma_terms[gamma_terms$topic == i,]$gamma,":", gamma_terms[gamma_terms$topic == i,]$terms)
  
}


# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(a$term), 
         as.character(a$topic)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
a$IDsource <- match(a$term, nodes$name)-1 
a$IDtarget <- match(a$topic, nodes$name)-1


# Make the Network
sankeyNetwork(Links = a, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "beta", NodeID = "name",
              fontSize=16,
              sinksRight =FALSE)


# TIME

tidystm <- tidy(stm_m)
tidystm <- rename(tidystm, actor = y.level)

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
    filter(! term %in% policies) %>%
    group_by(actor) %>%
    arrange(-beta) %>%
    top_n(7,beta) %>%
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
ggsave(cm,file = paste0(folder,"DE/25/",titleplot,"comb_",i,".jpg"),width = 14, height = 3.5)


} 



# Deleted: topic proportion by actor (no time) ####


# td_gamma %>%
#   group_by(actor,topic) %>%
#   summarise(gamma = mean(gamma)) %>%
#   arrange(desc(gamma)) %>%
#  # top_n(7, gamma) %>%
#  # filter(actor == "POL") %>%
#   ggplot(aes(reorder(factor(topic), gamma),gamma, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   # geom_text(hjust = 0.32, nudge_y = 0.00005, size = 8, # 0.0005
#   #           family = "IBMPlexSans") +
#   facet_wrap(~ actor, scales = "free_x",
#   labeller = labeller(actor = c("TU" = "Trade Unions","TA" = "Trade Associations", "POL" = "Political Actors"))) +
#   xlab("Topic") + ylab("Avg expect. proportion by actors") +
#   coord_flip() +
#   ggtitle(titleplot) +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 15),
#         axis.text.x = element_text(size = 15),
#         strip.text.x = element_text(size = 15))
# ggsave(paste0(folder,"/results/",titleplot,"_actor",numm,".jpg"),width = 11,height = 5)

#
# td_gamma %>%
#   group_by(actor,topic) %>%
#   summarise(gamma = mean(gamma))  %>%
#   ggplot(aes(x = actor,y = gamma, fill = factor(topic), labels = factor(topic))) +
#   geom_bar(position="fill", stat="identity")
# 
# td_gamma %>%
#   group_by(actor,topic) %>%
#    summarise(gamma = mean(gamma))  %>%
#   ggplot(aes(x = topic,y = gamma, fill = factor(topic), labels = factor(topic))) +
#   geom_bar( stat="identity") + facet_wrap(~ actor)



# topic all corpus proportion
# 
# top_terms <- tidy(stm_m) %>%
#   arrange(beta) %>%
#   group_by(topic) %>%
#   top_n(7, beta) %>%
#   arrange(-beta) %>%
#   select(topic, term) %>%
#   summarise(terms = list(term)) %>%
#   mutate(terms = map(terms, paste, collapse = ", ")) %>% 
#   unnest()

# gamma_terms <- td_gamma %>%
#   group_by(topic) %>%
#   summarise(gamma = mean(gamma)) %>%
#   arrange(desc(gamma)) %>%
#   left_join(top_terms, by = "topic") %>%
#   mutate(topic = paste0("Topic ", topic),
#          topic = reorder(topic, gamma))
# 
# gamma_terms %>%
#   # top_n(20, gamma) %>%
#   ggplot(aes(topic, gamma, label = terms, fill = topic)) +
#   geom_col(show.legend = FALSE) +
#   geom_text(hjust = 0.2, nudge_y = 0.00005, size = 5, # 0.0005
#             family = "IBMPlexSans") +
#   coord_flip() +
#   scale_y_continuous(expand = c(0,0),
#                      limits = c(0, 0.10) , 
#                      labels = scales::percent_format() ) +
#   theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
#   theme(plot.title = element_text(size = 20,
#                                   family="IBMPlexSans-Bold"),
#         plot.subtitle = element_text(size = 13),
#         axis.text.y = element_text(size = 15)) +
#   labs(x = NULL, y = expression(gamma))
# # ggsave("topic_de.jpg",width = 18,height = 9)

























# topic correlation

library(stm)
library(ggraph)
library(quanteda)

stm_corrs <- get_network(model = stm_it30cn,
                         method = 'simple',
                         labels = paste('Topic', 1:30),
                         cutoff = 0.001,
                         cutiso = TRUE)

stm_corrs <- get_network(model = stm_corrs,
                         method = 'simple',
                         labels = paste('Topic', 1:numm),
                         cutoff = 0.001,
                         cutiso = TRUE)

ggraph(stm_corrs, layout = 'fr') +
  geom_edge_link(
    aes(edge_width = weight),
    label_colour = '#fc8d62',
    edge_colour = '#377eb8') +
  geom_node_point(size = 4, colour = 'black')  +
  geom_node_label(
    aes(label = name, size = props),
    colour = 'black',  repel = TRUE, alpha = 0.85) +
  scale_size(range = c(2, 10), labels = scales::percent) +
  labs(size = 'Topic Proportion',  edge_width = 'Topic Correlation') +
  scale_edge_width(range = c(1, 3)) +
  theme_graph()

#####

cmpd <- read.xls("keywords_cnt.xls",sheet = "compound_dfm", encoding = "latin1")[1:2397,1]
bi_de <- read.csv("bigrams_de.csv",sep=",") %>% filter(bigram >= 5)

bde_cmpd <- bi_de %>% filter(bigram %nin% cmpd)




