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
library(widyr)
library(irlba)
library(furrr)
library(stm)

 setwd("//watt.bss.bremen-social-sciences.de/rpaolillo/R/twitter_unsupervised/actpol")

# test with old dataframe ####
# load("df_det.Rdata")
# load("df_detxt_t.Rdata")
# load("df_actpol.Rdata")
# df_actpol <- df_actpol %>% filter(country=="Germany")
# 
# detxNOactpol <- df_detxt_t %>% filter(tweet_id %nin% df_actpol$tweet_id)
# actpolNOdetx <- df_actpol %>% filter(tweet_id %nin% df_detxt_t$tweet_id)
# detxYEactpol <- df_detxt_t %>% filter(tweet_id %in% df_actpol$tweet_id)
# actpolYEOdetx <- df_actpol %>% filter(tweet_id %in% df_detxt_t$tweet_id)



# Data Collection ####
# 
# actors <- read.csv("actors.csv",sep=";")
 
 #  pol_it <- actors$POL_IT[1:11]
 
 # get_all_tweets(
 #   users = pol_it,
 #   start_tweets = "2020-01-01T00:00:00z",
 #   end_tweets = "2021-12-31T23:59:59z",
 #  # lang = c("DE"),
 #   is_retweet = NULL,
 #   n = Inf,
 #   data_path = "POL_DE",
 #   bearer_token = bearer_token
 # )
 

#   de_pol <- bind_tweets(data_path = "POL_DE", output_format = "tidy")
#   de_pol <- de_pol %>% filter(lang=="de")
#   de_pol$actor <- "POL"
#   save(de_pol,file="df_pol_de.Rdata")
#   load("df_pol_de.Rdata")
#  
# load("df_pol_de.Rdata")
# load("df_ta_de.Rdata")
# load("df_tu_de.Rdata")
# load("df_pol_it.Rdata")
# load("df_ta_it.Rdata")
# load("df_tu_it.Rdata")
# 
# df_act20 <- rbind(de_pol,de_ta,de_tu,it_pol,it_ta,it_tu)
# df_act20 <- df_act20 %>% filter((sourcetweet_type != "retweeted") %>% replace_na(TRUE))
# df_act20 <- df_act20[!duplicated(df_act20$tweet_id),]
# df_act20$date <- as_date(df_act20$created_at)
# df_act20$datenum <- as.integer(df_act20$date)
# df_act20$ID <- paste(df_act20$user_username,rownames(df_act20))
# df_act20 <- df_act20 %>% relocate(ID,.before=tweet_id)
# # 
# pattern_umlaut <- c("ueberbrueck","überbrueck","ueberbrück","grossbürg","großbuerg","grossbürg",
#                     "buergschaf")
# replace_umlaut <- c("überbrück","überbrück","überbrück","großbürg","großbürg","großbürg","bürgschaf")
# names(replace_umlaut) <- pattern_umlaut
# 
# df_act20[df_act20$lang=="de",]$text <- str_replace_all(
#   df_act20[df_act20$lang=="de",]$text,regex(replace_umlaut,ignore_case = T))
 
#  pattern_keywords <- c("überbrückungshilfeprogramms","überbrückungshilfeprogramm")
#  replace_keywords <- c("überbrückungshilfe","überbrückungshilfe")
#  names(replace_keywords) <- pattern_keywords
#  # 
#  df_act20[df_act20$lang=="de",]$text <- str_replace_all(
#    df_act20[df_act20$lang=="de",]$text,regex(replace_keywords,ignore_case = T))
# # 
#  save(df_act20,file="df_act20.Rdata")

# cleaning corpus
load("df_act20.Rdata")
# 
spc <- pull(read.csv("special_characters.csv", encoding = "UTF-8"),1) # delete emoticon, keep ASCII
spct <- paste(spc,collapse="")
chrmt <- paste0("[^\x01-\x7F",spct,"]")

df_act20$text <- tolower(df_act20$text)
df_act20$text <- gsub("_","-",df_act20$text)
df_act20$text <- gsub("#"," ",df_act20$text)
df_act20$text <- gsub(chrmt," ", df_act20$text)  # for not ASCII (emoticon), keeps normal characthers + spct list
df_act20$text <- gsub("&amp;", " ", df_act20$text) # remove how ";" is translated
df_act20$text <- gsub("\\s+", " ", df_act20$text)
df_act20$text <- gsub("\n", " ",  df_act20$text)
df_act20$text <- gsub("\t", " ",  df_act20$text)
df_act20$text <- gsub("'", " ",  df_act20$text)
df_act20$text <- gsub("’", " ",  df_act20$text)
df_act20$text <- str_replace_all(df_act20$text,
regex("http[s]?:/[/]?(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",
  ignore_case = TRUE)," ")
df_act20$text <- str_replace_all(df_act20$text, regex("(?<=^|\\s)@[^\\s]+",ignore_case=TRUE)," ")
# df_act20$text <- str_replace_all(df_act20$text,
#    regex("[a-zA-Z]*@[a-zA-Z]*",ignore_case=TRUE)," ")
df_act20[df_act20$lang=="it",]$text <- gsub("á","à",   df_act20[df_act20$lang=="it",]$text)
df_act20[df_act20$lang=="it",]$text <- gsub("é","è",   df_act20[df_act20$lang=="it",]$text)
df_act20[df_act20$lang=="it",]$text <- gsub( "í","ì",  df_act20[df_act20$lang=="it",]$text)
df_act20[df_act20$lang=="it",]$text <- gsub( "ó","ò",  df_act20[df_act20$lang=="it",]$text)
df_act20[df_act20$lang=="it",]$text <- gsub( "ú","ù",  df_act20[df_act20$lang=="it",]$text)
df_act20$text <- str_trim(df_act20$text)
"%nin%" <- Negate("%in%")

# # # annotation keywords policies Germany ####
# # 


# reshape some words roots (regex of composed) # here to not repeat in the csv of keywords


# check keywords term regex

 df_word <- unique(stringr::str_extract_all(df_act20[df_act20$lang=="de",]$text, 
   regex("\\b[:alnum:]*\\-?[:alnum:]*\\-?[:alnum:]* üh \\-?[:alnum:]*\\-?[:alnum:]*", 
         ignore_case = TRUE)))
# 
# 





# subset corpus Germany

# policies
de_key <- read.xls("keywords.xls",sheet = "de")[,1]
de_key <- de_key[!duplicated(de_key)]
de_key <- paste0("\\b",de_key,"\\b",collapse="|")
save(de_key,file="de_key.Rdata")
# # # de_key <- paste0("(?<![:alpha:])",de_key,"(?![:alpha:])")
# # 
 df_de <- df_act20 %>% filter(lang == "de") %>%
  filter(str_detect(text, regex(de_key, ignore_case=TRUE)))
 df_de <- df_de[!duplicated(df_de$tweet_id),]
save(df_de,file="df_de.Rdata")

  a <- df_de %>% filter(str_detect(text,
     regex("_",ignore_case=T)))
  
# # # #  check terms mention and if any missing (avoidable)
 de_key <- read.xls("keywords.xls",sheet = "de")[,1]
 de_key <- de_key[!duplicated(de_key)]
# # #  
notincl <- list()
 for (i in de_key) {
 db = tibble(keyterm = i, vl = max(str_count(df_de$text,i )))
 notincl[[i]] = db
#   print(c(i,max(str_count(df_de$text,i ))))
 }
#
 dbfin <- bind_rows(notincl)
# # # 
de_key2round <- dbfin[dbfin$vl == 0,]$keyterm
 de_key2round <- de_key2round[!duplicated(de_key2round)]
 de_key2round <- paste0("\\b",de_key2round,"\\b",collapse="|")
save(de_key2round,file="de_key2round.Rdata")
# load("de_key2round.Rdata")
  df_de2 <- df_act20 %>% filter(lang == "de") %>%
  filter(str_detect(text, regex(de_key2round, ignore_case=TRUE)))
  save(df_de2,file="df_de2.Rdata")

# extra terms

 de_xtr <- read.xls("keywords.xls",sheet = "extra_de")[,1]
  de_xtr <- paste0("\\b",de_xtr,"\\b",collapse="|")
# # de_xtr <- paste0("(?<![:alpha:])",de_xtr,"(?![:alpha:])")
# 
 de_cvd <- read.xls("keywords.xls",sheet = "covid_de")[,1]
# de_cvd <- paste0("\\b",de_cvd,"\\b",collapse="|")
  de_cvd <- paste0("\\b",de_cvd,collapse="|")
# # 
 df_extra_de <- df_act20 %>% filter(lang=="de") %>%
  filter(str_detect(text, regex(de_xtr, ignore_case=TRUE)))
# #   
 df_extra_de <- df_extra_de %>% 
   filter(str_detect(text, regex(de_cvd, ignore_case=TRUE)))
 df_extra_de <- df_extra_de[!duplicated(df_extra_de$tweet_id),]
# # 
  df_de <- rbind(df_de,df_extra_de)
 df_de <- df_de[!duplicated(df_de$tweet_id),]
# # 
   save(df_de,file="df_de.Rdata")
 # load("df_de.Rdata")
 # df_de$ID <-  paste(df_de$user_username,rownames(df_de))
 # save(df_de,file="df_de.Rdata")
#
 
# duplicate text Germany
 load("df_de.Rdata")
 df_de <- df_de[!duplicated(df_de$tweet_id),]
  corpus_de <- corpus(df_de)
 docnames(corpus_de) <- df_de$ID
 dfm_de <- tokens(corpus_de) %>% dfm()
# 
   de_sim <- textstat_simil(dfm_de,
  method="jaccard",margin="documents",min_simil = 0.95)
#   
 de_simlist <- as.list(de_sim,diag=FALSE)
# 
 results_sim = list()
 for (i in names(de_simlist)) {
   txt <- df_de[df_de$ID == i,]$text
   id <- df_de[df_de$ID == i,]$tweet_id
   b <- tibble(name = names(de_simlist[i]),id = id, double = length(de_simlist[[i]]),
               text = txt)
   results_sim[[i]] = b
 }
# results_sim
 dfres <- bind_rows(results_sim, .id = "name")
#  
 df_double <- df_de %>% filter(tweet_id %in% dfres$id)
 save(df_double, file="df_double.Rdata")
 
 df_de <- df_de %>% filter(tweet_id %nin% df_double$tweet_id)
 save(df_de,file="df_de_nodouble.Rdata")

 # lemmatization cleaning
 load("df_de_nodouble.Rdata")
 
 de_ptn <- read.xls("keywords.xls",sheet = "lemma_de")[,1]
 de_ptn <- paste0("\\b",de_ptn,"\\b")
 de_rpl <- read.xls("keywords.xls",sheet = "lemma_de")[,2]
 
  df_de$text <- stri_replace_all_regex(
     df_de$text,
    pattern=de_ptn,
    replacement=de_rpl,
    vectorize_all=FALSE)

  a <- df_de %>% filter(str_detect(text,
      regex("woche",ignore_case=T)))
  
df_de$ID <-  paste(df_de$actor,"DE",df_de$user_username,df_de$date,rownames(df_de),sep=".")
  
save(df_de,file="df_de_annotated.Rdata")
load("annotation_de.Rdata")
a <- annotation_de %>% filter(token %in% de_rpl)
a <- annotation_de %>% filter(upos == "PRON")

# word embedding ####

load("df_de_annotated.Rdata")
rem_de <- read.xls("keywords.xls",sheet = "rem_de")[,1]
de_key <- read.xls("keywords.xls",sheet = "de")[,2]
we_tx <- df_de %>% filter(actor=="POL") %>% select(c("text"))
# we_tx$text <- str_replace_all(we_tx$text,"(?<=^|\\s)@[^\\s]+","")
# we_tx$text <- str_replace_all(we_tx$text,"(?<=^|\\s)https[^\\s]+","")
# we_tx$text <- str_replace_all(we_tx$text,stopwords_de,"")

we_tx$postID<-row.names(we_tx)

tidy_text <- we_tx %>%
  select(postID, text) %>%
  unnest_tokens(word, text) %>%
  add_count(word) %>%
  filter(n >= 40) %>% 
  filter(!word %in% stopwords_de) %>% 
  filter(!word %in% rem_de) %>% 
  select(-n)

nested_words <- tidy_text %>%
  nest(words = c(word))

# install.packages("slider")


slide_windows <- function(tbl, window_size) {
  skipgrams <- slider::slide(
    tbl, 
    ~.x, 
    .after = window_size - 1, 
    .step = 1, 
    .complete = TRUE
  )
  
  safe_mutate <- safely(mutate)
  
  out <- map2(skipgrams,
              1:length(skipgrams),
              ~ safe_mutate(.x, window_id = .y))
  
  out %>%
    transpose() %>%
    pluck("result") %>%
    compact() %>%
    bind_rows()
}


plan(multisession)  ## for parallel processing

tidy_pmi <- nested_words %>%
  mutate(words = future_map(words, slide_windows, 4L)) %>%
  unnest(words) %>%
  unite(window_id, postID, window_id) %>%
  pairwise_pmi(word, window_id)

tidy_pmi

tidy_word_vectors <- tidy_pmi %>%
  widely_svd(
    item1, item2, pmi,
    nv = 2 #, maxit = 1000
  )

#pl <- as.data.frame(tidy_word_vectors)

pl_wd <- as.data.frame(tidy_word_vectors) %>% 
  reshape(idvar = "item1", timevar = "dimension", direction = "wide")

ggplot(pl_wd,aes(pl_wd$value.1,pl_wd$value.2)) + geom_point() +
  geom_text(aes(label=item1),hjust=0, vjust=0,
          color= ifelse(pl_wd$item1 %in% unique(de_key),"red","blue")
  ) + xlab("Dimension1") + ylab("Dimension2") + 
  ggtitle("Germany, POL") + 
  theme_bw()
ggsave("DE_POL_we.jpg",width = 15,height=11)




# TPM ####
# dfm
load("df_de_annotated.Rdata")
df_de <- df_de[!duplicated(df_de$tweet_id),]

compound_de <- read.xls("keywords.xls",sheet = "compound_de")[,1]
rem_de <- read.xls("keywords.xls",sheet = "rem_de")[,1]
#                   
corpus_de <- corpus(df_de)
docnames(corpus_de) <- corpus_de$ID



 dfm_de <-  tokens(corpus_de,
                    remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>%
   tokens_compound(phrase(c(compound_de))) %>%
   tokens_remove(c("http*","@*","€","gut",stopwords("de"),stopwords_de,rem_de)) %>%
  dfm()
 topfeatures(dfm_de,100)
 save(dfm_de,file="dfm_de.Rdata")
 
# TPM analysis ####
 
 # keyintop <- list()
 # for (i in key_de) {
 #   t <- findTopic(stm_m, i)
 #   keyintop[[i]] = t
 # }
 # 
 # 
 labelTopics(stm_m,1,7)
 thg_de <- findThoughts(stm_m, texts = stm_m$text,n = 3, topics =8)
 plotQuote(thg_de)

 thg_det <- findThoughts(stm_m, texts = df_de$text,n = 2, topics =1:30)$docs[[1]]
 plotQuote(thg_det)
 for(i in c(3097,1843,3135)){

   n <- i
   print(c(df_de[n,]$text,df_de[n,]$user_name, df_de[n,]$created_at))

 }
 # 
 load("TPM/tpmde_pr_act_i_date10.Rdata")
 load("df_de_annotated.Rdata")
 load("dfm_de.Rdata")
 
 tb <- labelTopics(stm_m,1:10,7)
 df <- tibble(topic = tb$topicnums,prob = tb$prob,frex = tb$frex)

 thoughts <- list()
 for (i in 1:30) {
  # thg_det <- findThoughts(stm_m, texts = df_de$text,n = 3, topics =i)$docs[[1]]
   thought_text = list()
   dates <- findThoughts(stm_m, texts = df_de$text,n = 3, topics =i)$index[[1]]
   for (n in dates) {
   txx <-  print(c(paste0(" TXT: ",df_de[n,]$text," ACT: ",df_de[n,]$user_username," DATE: ", df_de[n,]$created_at)))
   thought_text[[n]] <- txx
   thought_textfin <- do.call(rbind.data.frame, thought_text)
   }
  thoughts[[i]] <- thought_textfin
   
 }
 bind_rows(thoughts)
 thoughts <- do.call(rbind.data.frame, lapply(thoughts,'[[',1))
 colnames(thoughts) = c("1", "2","3")
 
  df <- cbind(df,thoughts)
 
  # thoughts <- list()
  # for (i in 1:30) {
  #   thg_det <- findThoughts(stm_m, texts = df_de$text,n = 3, topics =i)$docs[[1]]
  #   thg_detf <- as.data.frame(thg_det)
  #   thg_detf$name <- rownames(thg_detf)
  #   thg_detf <- tibble(spread(thg_detf, name, thg_det),"topic" = i)
  #   thoughts[[i]] <- thg_detf
  # }
  # bind_rows(thoughts)
  # thoughts <- do.call(rbind.data.frame, thoughts)
  # dffin <- cbind(df,thoughts)
  
  
  
  
 
 # # # #
 tidy(stm_m) %>%
   group_by(topic) %>%
   top_n(7) %>%
   ungroup %>%
   mutate(term = reorder_within(term, beta, topic)) %>%
   # mutate(term =  reorder(term, beta))  %>%
   mutate(topiclab = paste0("Topic ",topic)) %>%
   mutate(topic2 = factor(topiclab, levels = unique(topiclab[order(topic)]))) %>%
   #  filter(term %in% key_de & beta > 0.002) %>%
   ggplot(aes(beta,term, fill = topiclab)) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~ topic2, scales = "free") +
   scale_y_reordered() +
  # coord_flip() +
   xlab("Probability words belonging to topic") +
   # ylab("") +
   theme_bw() +
   theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
 #
 
td_gamma <- tidy(stm_m, matrix = "gamma")
docs_name <- names(stm_df$documents)
td_gamma <- cbind(td_gamma,docs_name)
td_gamma <- td_gamma %>% separate(docs_name,c("actor","country","date","rn"),
                                  sep=".",convert = TRUE)

td_gamma %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ actor) +
  labs(x = "topic", y = expression(gamma))


# to take average of actors
td_gammaplot <- td_gamma %>% group_by(actor) %>% summarize(mean(gamma))
colnames(td_gammaplot) <- c("actor","topic","gamma")
ggplot(td_gammaplot,aes(factor(toppic),gamma)) + geom_point() + facet_wrap(~actor)

plot(stm_m) #<- mean of gamma of all topics, i.e. mean topic proportion (prevalence) for all documents
td_gammaplottopic <- td_gamma %>% group_by(topic) %>% summarize(mean(gamma))


library(ggthemes)

top_terms <- tidy(stm_m) %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  # top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
   geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
          family = "IBMPlexSans") +
  coord_flip() +
   scale_y_continuous(expand = c(0,0),
                     #limits = c(0, 0.09)# , 
                     labels = scales::percent_format() ) +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 25,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma))



 # 
 # ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
 #   geom_histogram(alpha = 0.8, show.legend = FALSE) +
 #   ggtitle("Count documents per topic, Germany") +
 #   facet_wrap(~ topic, ncol = 6) 
 # 
 # # theta same as gamma
 # td_theta <- tidy(stm_m, matrix = "theta")
 # theta_red <- td_theta[td_theta$document%in%c(1:15),] #to first 15 topics
 #  thetaplot1<-ggplot(theta_red, aes(y=gamma, x=as.factor(topic), fill = as.factor(topic))) +
 #    geom_bar(stat="identity",alpha = 0.8, show.legend = FALSE) +
 #      facet_wrap(~ document, ncol = 3) +
 #    ggtitle("Probability topic per document, Germany") +
 #     labs(title = "Theta values per document",  y = expression(theta), x = "Topic")
 # ggsave(file="theta_de.jpg",plot = last_plot())
 
 
 
 
 
 
 
 
 #################################################################
 
 
# 
# ## subset corpus ITA
# 
# # policies
# 
# it_key <- read.xls("keywords.xls",sheet = "it")[,1]
# it_key <- tolower(it_key)
# it_key <- paste0("\\b",it_key,"\\b",collapse="|")
# it_key <- paste0("(?<![:alpha:])",it_key,"(?![:alpha:])")
# 
# df_it <- df_act20 %>% filter(lang == "it") %>%
#   filter(str_detect(text, regex(it_key, ignore_case=TRUE)))
# 
# # 
# 
# it_xtr <- read.xls("keywords.xls",sheet = "extra_it")[,1]
# it_xtr <- tolower(it_xtr)
# it_xtr <- it_xtr[1:8]
# it_xtr <- paste0("\\b",it_xtr,"\\b",collapse="|")
# it_xtr <- paste0("(?<![:alpha:])",it_xtr,"(?![:alpha:])")
# 
# it_cvd <- read.xls("keywords.xls",sheet = "extra_it")[,2]
# it_cvd <- it_cvd[1:13]
# it_cvd <- tolower(it_cvd)
# #de_cvd <- paste0("\\b",de_cvd,"\\b",collapse="|")
# it_cvd <- paste0("\\b",it_cvd,collapse="|")
# it_cvd <- paste0("(?<![:alpha:])",it_cvd,"(?![:alpha:])")
# 
# df_extra_it <- df_act20 %>% filter(lang == "it") %>%
#   filter(str_detect(text, regex(it_xtr, ignore_case=TRUE)))
# 
# df_extra_it <- df_extra_it  %>% 
#   filter(str_detect(text, regex(it_cvd, ignore_case=TRUE)))
# 
# # compile corpus IT
# 
# df_it <- rbind(df_it,df_extra_it) 
# df_it <- df_it[!duplicated(df_it$tweet_id),]
# 
# keywords_it <- read.xls("annotation_it_keywords.xls")
# results_it = list()
# for (i in keywords_it$lem_it_keywords) {
#   b_it <- keywords_it[keywords_it$lem_it_keywords==i,]
#   x_it <- paste0("\\b",b_it$tok_it_keywords,"\\b")
#   xa_it <- paste0(x_it,collapse="|")
#   results_it[[i]] = xa_it
# }
# 
# df_it$text <- stri_replace_all_regex(
#   df_it$text,
#   pattern=results_it,
#   replacement=names(results_it),
#   vectorize_all=FALSE)
# 
# save(df_it,file="df_it.Rdata")
# 
# # to detect similars with cosine similarity
# 
# # load("df_de.Rdata")
# # corpus_de <- corpus(df_de)
# # docnames(corpus_de) <- corpus_de$ID
# # 
# # dfm_de <- tokens(corpus_de) %>% dfm()
# # 
# # library(quanteda.textstats)
# # 
# # dfm_df <- dfm_subset(dfm_de,user_username == "BMWK")
# # # 
# # de_sim <- textstat_simil(dfm_df,margin="documents",
# #                          method="cosine",min_simil = 0.99)
# # as.list(de_sim, diag = FALSE)
# 
# # DFM Germany
# load("df_de.Rdata")
#  load("annotation_de.Rdata")
#  
# corpus_de <- corpus(df_de)
# docnames(corpus_de) <- corpus_de$ID
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
# dfm_de <-  tokens(corpus_de,
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
#  save(dfm_de,file="dfm_de.Rdata")
# 
#  # DFM Italy
#  load("df_it.Rdata")
#   load("annotation_it.Rdata")
#   
#   corpus_it <- corpus(df_it)
#   docnames(corpus_it) <- corpus_it$ID
#  #
#  rem_it <- c("http*","@*","faq","ciglcisluil","SPECIALE","più","già","fra","ufficio_stampa_fabi","ufficio_stampa","asstel",
#              "a_cura_dell_ufficio","qui"," | ")
#  #
#  compound_it <- phrase(c("ministro giovannini","cassa integrazione","cassa lavoro", "settore terziario", "settore turistico",
#                          "a cura dell ufficio","ufficio stampa fabi","ufficio stampa",
#                          "unione europea", "impresa zombie","imprese zombie", "azienda zombie",
#                          "aziende zombie","salva italia","decreto ristori","decreti ristoro",
#                          "lotto di vaccini","istituto di credito","instituti di credito",
#                          "piano nazionale","sistema produttivo","sistemi produttivi",
#                          "rapporto finanziario","made in italy","tassi di interesse", "tasso di interesse",
#                          "tassi d'interesse","tasso d'interesse","interessi economici","interesse economico",
#                          "ambito finanziario","termini di resa","termini di interesse","ambito lavorativo",
#                          "camera di commercio","ministero dello sviluppo economico","rapport eu",
#                          "stati finanziari","stati di bilancio","stato di emergenza",
#                          "Ferretti Group","Gruppo Ferretti","Ferretti Yachts","occasione per il sud",
#                          "blocco dei licenziamenti","blocco licenziamenti", "blocco_dei_licenziamenti",
#                          "blocco_licenziamenti","risorsa digitale","risorse digitali","dare priorità",
#                          "cura italia","cura-italia","Cura Italia","Cura-Italia","CURA ITALIA",
#                          "sistema economico","sistemi economici","sistema politico","marco granelli",
#                          "marco granelli","sistemi politici","corona virus",
#                          "biblioteca nazionale","osservatorio nazionale","giornata nazionale della salute",
#                          "territorio nazionale","interesse nazionale","servizio sanitario nazionale","lutto nazionale",
#                          "museo nazionale","livello nazionale","corpo nazionale dei vigili del fuoco",
#                          "decreto legge","disegno di legge","banca d italia","banca centrale europea",
#                          "misure urgenti","misure straordinarie","misure per imprese","misure di sostegno",
#                          "milioni di euro","miliardi di euro","reddito di cittadinanza","sostegn* al reddito"))
# 
#  dfm_it <-  tokens(corpus_it,
#                      remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>%
#    tokens_compound(phrase(c(compound_it))) %>%
#    tokens_remove(c(stopwords("it"),stopwords_it,rem_it,
#                    annotation_it[annotation_it$upos == "ADP",]$token,
#                    annotation_it[annotation_it$upos == "PRON",]$token,
#                    annotation_it[annotation_it$upos == "AUX",]$token,
#                    annotation_it[annotation_it$upos == "DET",]$token,
#                    annotation_it[annotation_it$upos == "SCONJ",]$token)) %>%
#    dfm()
# 
#  save(dfm_it ,file="dfm_it.Rdata")
# 
# 
# 
# 
# 
# #####################
#   #  df_kd <- bind_tweets(data_path = "de_kfwkredite", output_format = "tidy")
# # 
# #  
# #  df <- bind_tweets(data_path = "test_tweet",output_format = "tidy")
# #  
# #  # added new keywords
# #  load("df_tw.Rdata") # all tweets on policies from everyone 2020-2021 both countries
# #  
#  # df_kd$country <- "Germany"
#  # df_kn$country <- "Germany"
# #  
#  #  df_tw <- df_tw[,-33]
#  # df_tw <- rbind(df_tw,df_kd,df_kn)
# # save(df_tw,file="df_tw.Rdata")
# # 
# 
#  # df_sof <- bind_tweets(data_path = "de_sofor", output_format = "tidy") # added sofort
#  # df_sof <- df_sof[!duplicated(df_sof$tweet_id),]
#  # df_sof$country <- "Germany"
#  # df_sof$month <- strftime(df_sof$created_at,format = "%m-%y")
#  # df_sof <- df_sof %>% filter((sourcetweet_type != "retweeted") %>% replace_na(TRUE))
#  # save(df_sof,file="df_sof.Rdata")
#  
#  # load("df_tw.Rdata")
#  # load("df_sof.Rdata")
#  # 
#  # 
#  # keywords_de <- read.xls("keywords.xls",sheet=2)
#  # k_de <-  paste(do.call(c, keywords_de),collapse="|")
#  # df_sof <- df_sof %>% filter(str_detect(df_sof$text, paste0("corona|covid|konjunkt|sars",k_de)))
#  # 
#  # df_tw <- rbind(df_tw,df_sof)
#  # df_tw <- df_tw[!duplicated(df_tw$tweet_id),]
#  # save(df_tw,file="df_tw.Rdata")
#  # sof_act <- df_sof %>% filter(created_at >= "2020-01-01") %>% filter(tweet_id %in% df_tot1821$tweet_id) to check if match only with soforthilfe keywords
#  
#  
# # Back source to compile and clean corpus ####
# 
# # load("df_tw.Rdata") # all tweets on policies from everyone 2020-2021 both countries
# # load("df_tot1821.Rdata") # all tweets TA, TU, POL 2018-2021
# # # #
# #  # list of TA,TU, POL in each country
# # load("de_pol.Rdata")
# # load("de_ta.Rdata")
# # load("de_tu.Rdata")
# # load("it_pol.Rdata")
# # load("it_ta.Rdata")
# # load("it_tu.Rdata")
# # 
# # df_tw <- df_tw[!duplicated(df_tw$tweet_id),]
# # df_tw <- df_tw %>% filter((sourcetweet_type != "retweeted") %>% replace_na(TRUE))
# # df_actpol <- df_tot1821 %>% filter(tweet_id %in% df_tw$tweet_id)
# # df_actpol <- df_actpol[!duplicated(df_actpol$tweet_id),]
# # df_actpol$actor <- ifelse((df_actpol$user_name %in% de_pol) | (df_actpol$user_name %in% it_pol),
# #                           "POL",
# #                           ifelse((df_actpol$user_name %in% de_ta) | (df_actpol$user_name %in% it_ta),
# #                                  "TA",
# #                                  "TU"))
# # 
# # # add variable on dataset
# # df_actpol$date <- as_date(df_actpol$created_at)
# # df_actpol$datenum <- as.integer(df_actpol$date)
# # df_actpol$ID <- rownames(df_actpol)
# # # 
# # # # for general cleaning
# # spc <- pull(read.csv("special_characters.csv", encoding = "UTF-8"),1) # delete emoticon, keep ASCII
# # spct <- paste(spc,collapse="")
# # chrmt <- paste0("[^\x01-\x7F",spct,"]")
# # 
# # df_actpol$text <- tolower(df_actpol$text)
# # df_actpol$text <- gsub("_","-",df_actpol$text)
# # df_actpol$text <- gsub("#"," ",df_actpol$text)
# # df_actpol$text <- gsub(chrmt," ", df_actpol$text)  # for not ASCII (emoticon), keeps normal characthers + spct list
# # df_actpol$text <- gsub("&amp;", " ", df_actpol$text) # remove how ";" is translated
# # df_actpol$text <- gsub("\\s+", " ", df_actpol$text)
# # df_actpol$text <- gsub("\n", " ",  df_actpol$text)
# # df_actpol$text <- gsub("\t", " ",  df_actpol$text)
# # df_actpol$text <- gsub("'", " ",  df_actpol$text)
# # df_actpol$text <- gsub("’", " ",  df_actpol$text)
# # 
# # # # annotation keywords policies Germany ####
# # # 
# # pattern_umlaut <- c("ueberbrueck","überbrueck","ueberbrück","grossbürg","großbuerg","grossbürg",
# #                     "buergschaf")
# # replace_umlaut <- c("überbrück","überbrück","überbrück","großbürg","großbürg","großbürg","bürgschaf")
# # names(replace_umlaut) <- pattern_umlaut
# # 
# # df_actpol[df_actpol$lang=="de",]$text <- str_replace_all(
# #   df_actpol[df_actpol$lang=="de",]$text,regex(replace_umlaut,ignore_case = T))
# # 
# # # reshape some words roots (regex of composed)
# # pattern_keywords <- c("überbrückungshilfeprogramms","überbrückungshilfeprogramm")
# # replace_keywords <- c("überbrückungshilfe","überbrückungshilfe")
# # names(replace_keywords) <- pattern_keywords
# # 
# # df_actpol[df_actpol$lang=="de",]$text <- str_replace_all(
# #   df_actpol[df_actpol$lang=="de",]$text,regex(replace_keywords,ignore_case = T))
# # 
# # # # lemmatization policies and other words
# # df_de <- read.xls("annotation_de_keywords.xls")
# # results_de = list()
# # for (i in df_de$lem_de_keywords) {
# #   b_de <- df_de[df_de$lem_de_keywords==i,]
# #   x_de <- paste0("\\b",b_de$tok_de_keywords,"\\b")
# #   xa_de <- paste0(x_de,collapse="|")
# #   results_de[[i]] = xa_de
# # }
# # 
# # df_actpol[df_actpol$lang=="de",]$text <- stri_replace_all_regex(
# #   df_actpol[df_actpol$lang=="de",]$text,
# #                               pattern=results_de,
# #                               replacement=names(results_de),
# #                               vectorize_all=FALSE)
# # # 
# # # 
# # # # annotation keywords policies Italy ####
# # # # lemmatization policies and other words
# # df_it <- read.xls("annotation_it_keywords.xls")
# # results_it = list()
# # for (i in df_it$lem_it_keywords) {
# #   b_it <- df_it[df_it$lem_it_keywords==i,]
# #   x_it <- paste0("\\b",b_it$tok_it_keywords,"\\b")
# #   xa_it <- paste0(x_it,collapse="|")
# #   results_it[[i]] = xa_it
# # }
# # 
# # df_actpol[df_actpol$lang=="it",]$text <- stri_replace_all_regex(
# #   df_actpol[df_actpol$lang=="it",]$text,
# #   pattern=results_it,
# #   replacement=names(results_it),
# #   vectorize_all=FALSE)
# # # 
# # # # 
# # save(df_actpol, file= "df_actpol.Rdata")
# 
# # Cleaning doubles using cosine similarity ####
# # library(quanteda.textstats)
# # load("dfm_deap.Rdata")
# # load("dfm_itap.Rdata")
# # 
# # dfm_df <- dfm_subset(dfm_deap,user_username == "BMWK")
# # 
# # de_sim <- textstat_simil(dfm_df,margin="documents",
# #                          method="cosine",min_simil = 0.95)
# # as.list(de_sim, diag = FALSE)
# # 
# # df_actpol <- df_actpol %>% filter(!(user_username == "VDMAonline" &
# #  str_detect(text,"der maschinenbau im einsatz gegen die corona-krise: vdma-mitgliedsfirmen handeln.|der maschinenbau im einsatz gegen die corona-krise: vdma-mitgliedsfirmen und partner handeln."))) %>%
# #  filter(!(user_username == "BMWK" &
# #  str_detect(text,"corona-ticker|corona- ticker|corona-blog|jetzt live|unternehmen stehen vor großen herausforderungen. mit unterstützungsprogrammen versuchen wir zu helfen, schnell und unbürokratisch.|zur überbrückungshilfe3 wurden bisher rund|servicetweet|zur überbrückungshilfe3 wurden bisher gut"))) %>%
# #   filter(!(user_username == "MISE_GOV" & str_detect(text,"covid curaitalia: 50 milioni per le aziende che vogliono produrre dispositivi medici e di protezione individuale incentivi per sostenere aziende italiane che vogliono ampliare o riconvertire propria attività"))) %>%
# #   filter(!(user_username == "FABI_News" & str_detect(text,"speciale covid"))) %>%
# #   filter(!(user_username == "BMWK" & date == "2021-08-18")) %>%
# # filter(!(user_username == "BMWK" & date == "2021-07-16")) %>%
# # filter(!(user_username == "BMWK" & str_detect(text,"anträge auf neustarthilfen plus natürlicher personen für den förderzeitraum juli bis september"))) %>%
# # filter(!(user_username == "BMWK" & date == "2021-06-18")) %>%
# # filter(!(user_username == "BMWK" & str_detect(text,"alle aktuellen coronahilfen, die direkte zuschüsse beinhalten, befinden sich mittlerweile im sogenannten fachverfahren"))) %>%
# # filter(!(user_username == "BMWK" & date == "2021-04-09"))
# # 
# # 
# # save(df_actpol,file="df_actpol.Rdata")
# 
# 
# 
# 
# # Bigrams, trigrams (not used) ####
# 
# #load("df_actpol.Rdata")
# # 
# # 
# # bigrams_tx_de <- df_actpol %>% filter(lang=="de") %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
# # bigrams_tx_de %>% dplyr::count(bigram, sort = TRUE)
# # bigrams_separate_de <- bigrams_tx_de %>% separate(bigram,c("word1","word2"),sep=" ")
# # bigrams_filtered_de <- bigrams_separate_de %>%
# #   filter(!word1 %in% stopwords_de) %>%
# #   filter(!word2 %in% stopwords_de)
# # bigrams_filtered_de %>%  dplyr::count(word1, word2, sort = TRUE)
# # bigrams_united_de <- bigrams_filtered_de %>% unite(bigram, word1, word2, sep = " ")
# # bigrams_united_de <- bigrams_united_de$bigram
# # #
# # write.csv(bigrams_united_de,"bigrams_de.csv")
# # # #
# # # # #
# # trigrams_tx_de <- df_actpol %>% filter(lang=="de") %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)
# # trigrams_tx_de %>% dplyr::count(trigram, sort = TRUE)
# # trigrams_separate_de <- trigrams_tx_de %>% separate(trigram,c("word1","word2","word3"),sep=" ")
# # trigrams_filtered_de <- trigrams_separate_de %>%
# #   filter(!word1 %in% stopwords_de) %>%
# #   filter(!word2 %in% stopwords_de) %>%
# #   filter(!word3 %in% stopwords_de)
# # trigrams_filtered_de %>%  dplyr::count(word1, word2,word3, sort = TRUE)
# # trigrams_united_de <- trigrams_filtered_de %>% unite(trigram, word1, word2,word3, sep = " ")
# # trigrams_united_de <- trigrams_united_de$trigram
# # 
# #   write.csv(trigrams_united_de,"trigrams_de.csv")
# # #
# 
# # Italy
# # load("df_it.Rdata")
# #
# # bigrams_tx_it <- df_actpol %>% filter(lang=="it") %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
# # bigrams_tx_it %>% dplyr::count(bigram, sort = TRUE)
# # bigrams_separate_it <- bigrams_tx_it %>% separate(bigram,c("word1","word2"),sep=" ")
# # bigrams_filtered_it <- bigrams_separate_it %>%
# #   filter(!word1 %in% stopwords_it) %>%
# #   filter(!word2 %in% stopwords_it)
# # bigrams_filtered_it %>%  dplyr::count(word1, word2, sort = TRUE)
# # bigrams_united_it <- bigrams_filtered_it %>% unite(bigram, word1, word2, sep = " ")
# # bigrams_united_it <- bigrams_united_it$bigram
# # 
# # write.csv(bigrams_united_it,"bigrams_it.csv")
# # # #
# # # # #
# # trigrams_tx_it <- df_actpol %>% filter(lang=="it") %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)
# # trigrams_tx_it %>% dplyr::count(trigram, sort = TRUE)
# # trigrams_separate_it <- trigrams_tx_it %>% separate(trigram,c("word1","word2","word3"),sep=" ")
# # trigrams_filtered_it <- trigrams_separate_it %>%
# #   filter(!word1 %in% stopwords_it) %>%
# #   filter(!word2 %in% stopwords_it) %>%
# #   filter(!word3 %in% stopwords_it)
# # trigrams_filtered_it %>%  dplyr::count(word1, word2,word3, sort = TRUE)
# # trigrams_united_it <- trigrams_filtered_it %>% unite(trigram, word1, word2,word3, sep = " ")
# # trigrams_united_it <- trigrams_united_it$trigram
# # # #
# # write.csv(trigrams_united_it, "trigrams_it.csv")
# 
# 
# # selection from actors
#  
# load("df_actpol.Rdata")
#  load("df_tot1821.Rdata")
#  
# #  spc <- pull(read.csv("special_characters.csv", encoding = "UTF-8"),1) # delete emoticon, keep ASCII
# #  spct <- paste(spc,collapse="")
# #  chrmt <- paste0("[^\x01-\x7F",spct,"]")
# # 
# #  df_tot1821$text <- tolower(df_tot1821$text)
# #  df_tot1821$text <- gsub("_","-",df_tot1821$text)
# #  df_tot1821$text <- gsub("#"," ",df_tot1821$text)
# #  df_tot1821$text <- gsub(chrmt," ", df_tot1821$text)  # for not ASCII (emoticon), keeps normal characthers + spct list
# #  df_tot1821$text <- gsub("&amp;", " ", df_tot1821$text) # remove how ";" is translated
# #  df_tot1821$text <- gsub("\\s+", " ", df_tot1821$text)
# #  df_tot1821$text <- gsub("\n", " ",  df_tot1821$text)
# #  df_tot1821$text <- gsub("\t", " ",  df_tot1821$text)
# #  df_tot1821$text <- gsub("'", " ",  df_tot1821$text)
# #  df_tot1821$text <- gsub("’", " ",  df_tot1821$text)
# # "%nin%" <- Negate("%in%")
# # 
# #  # # # annotation keywords policies Germany ####
# #  # # 
# #  pattern_umlaut <- c("ueberbrueck","überbrueck","ueberbrück","grossbürg","großbuerg","grossbürg",
# #                      "buergschaf")
# #  replace_umlaut <- c("überbrück","überbrück","überbrück","großbürg","großbürg","großbürg","bürgschaf")
# #  names(replace_umlaut) <- pattern_umlaut
# # 
# #  df_tot1821[df_tot1821$lang=="de",]$text <- str_replace_all(
# #    df_tot1821[df_tot1821$lang=="de",]$text,regex(replace_umlaut,ignore_case = T))
# # 
# #  # reshape some words roots (regex of composed)
# #  pattern_keywords <- c("überbrückungshilfeprogramms","überbrückungshilfeprogramm")
# #  replace_keywords <- c("überbrückungshilfe","überbrückungshilfe")
# #  names(replace_keywords) <- pattern_keywords
# # 
# #  df_tot1821[df_tot1821$lang=="de",]$text <- str_replace_all(
# #    df_tot1821[df_tot1821$lang=="de",]$text,regex(replace_keywords,ignore_case = T))
# 
#  
#  
# de_key <- read.xls("keywords.xls",sheet = 2)[,1]
# de_key <- tolower(de_key)
# de_key <- paste0(de_key,collapse="|")
# de_key <- paste0("\\b",de_key,"\\b",collapse="|")
# de_key <- paste0("(?<![:alpha:])",de_key,"(?![:alpha:])")
# 
# df_selctde <- df_tot1821 %>% filter(created_at >= "2020-01-01" & lang == "de") %>%
#   filter(str_detect(text, regex(de_key, ignore_case=TRUE)))
# 
# df_desof <- df_tot1821 %>% filter(created_at >= "2020-01-01" & lang == "de") %>%
#   filter(str_detect(text,(
#     regex("(?<![:alpha:])\\bsoforthilfe\\b|\\bsoforthilfen\\b|\\bsofort-hilfe\\b|\\bsofort-hilfen\\b(?![:alpha:])",
#                                 ignore_case=TRUE))))
# df_desof <- df_desof %>%  filter(str_detect(text,
#   regex("(?<![:alpha:])\\bkonjunktur\\b|\\bcovid\\b|\\bcorona\\b|\\bsars\\b|\\bcoronakrise\\b(?![:alpha:])", 
#                                   ignore_case=TRUE)))
# 
# 
# df_check <- df_tot1821 %>% filter(created_at >= "2020-01-01" & lang == "it") 
# # %>%  filter(str_detect(text, (regex("coronahilf", 
# #                                  ignore_case=TRUE))))
# 
# 
# df_word <- unique(stringr::str_extract_all(df_check$text, 
#       regex("\\b[:alnum:]*\\-?[:alnum:]*\\-?[:alnum:]*cura italia\\-?[:alnum:]*\\-?[:alnum:]*", 
#                                                        ignore_case = TRUE)))
# 
# a<- df_check %>% filter(str_detect(text,"aufbauplan"))
# a$text
# # df_dekfw <- df_selctde %>% filter(str_detect(text,
# #     regex("(?<![:alpha:])\\bkonjunktur\\b|\\bcovid\\b|\\bcorona\\b|
# #   \\bcoronavirusde\\b|\\bcoronavirus\\b|\\bcovid19de\\b|\\bcovid19\\b|
# #   \\bcovid2019\\b|\\bcovid2019de\\b|\\bcoronapandemie\\b|\\bcoronaviruspandemie\\b|
# #          \\bsars\\b|\\bcoronakrise\\b(?![:alpha:])", 
# #                                                    ignore_case=TRUE)))
# 
# df_sofcheck <- df_desof %>% filter(str_detect(text,
#    regex("(?<![:alpha:])\\bcovid\\b|\\bcorona\\b|\\bcoronavirusde\\b|\\bcoronavirus\\b|\\bcovid19de\\b|\\bcovid19\\b|\\bcovid2019\\b|\\bcovid2019de\\b|\\bcoronapandemie\\b|\\bcoronaviruspandemie\\b|\\bsars\\b|\\bcoronakrise\\b|\\bcoronavirusdeutschland\\b(?![:alpha:])",
#                                                            ignore_case=TRUE)))
# 
# 
# df_kfwcheckreg <- df_dekfw %>% filter(str_detect(text,
#    regex("(?<![:alpha:])\\bpandemie\\b(?![:alpha:])", 
#                                                    ignore_case=TRUE)))
# 
# df_sofdouble <- df_sofcheckreg %>% filter(text %nin% df_sofcheck$text)
# 
# 
# 
# de_actTOslc <- df_actpol[df_actpol$lang=="de",] %>% filter(tweet_id %nin% df_selctde$tweet_id)
# 
# 
# 
# df_selctde <- rbind(df_selctde,df_desof)
# 
# "%nin%" <- Negate("%in%")
# de_slcTOact <- df_selctde %>% 
#   filter(tweet_id %nin% df_actpol[df_actpol$lang=="de",]$tweet_id)
# 
# # italy
# 
# it_key <- read.xls("keywords.xls",sheet = 3)[,1]
# it_key <- paste0(it_key,collapse="|")
# it_key <- tolower(it_key)
# df_selctit <- df_tot1821 %>% filter(created_at >= "2020-01-01" & lang == "it") %>%
#   filter(str_detect(text,(regex(it_key,ignore_case=TRUE))))
# 
# df_select <- rbind(df_selctde,df_selctit)
# 
# "%nin%" <- Negate("%in%")
# df_extra <- df_actpol %>% filter(tweet_id %nin% df_select$tweet_id)
# 
# df_select <- df_select %>% filter(!(user_username == "VDMAonline" &
#                                        str_detect(text,"der maschinenbau im einsatz gegen die corona-krise: vdma-mitgliedsfirmen handeln.|der maschinenbau im einsatz gegen die corona-krise: vdma-mitgliedsfirmen und partner handeln."))) %>%
#                                        filter(!(user_username == "BMWK" &
#                                        str_detect(text,"corona-ticker|corona- ticker|corona-blog|jetzt live|unternehmen stehen vor großen herausforderungen. mit unterstützungsprogrammen versuchen wir zu helfen, schnell und unbürokratisch.|zur überbrückungshilfe3 wurden bisher rund|servicetweet|zur überbrückungshilfe3 wurden bisher gut"))) %>%
#                                         filter(!(user_username == "MISE_GOV" & str_detect(text,"covid curaitalia: 50 milioni per le aziende che vogliono produrre dispositivi medici e di protezione individuale incentivi per sostenere aziende italiane che vogliono ampliare o riconvertire propria attività"))) %>%
#                                         filter(!(user_username == "FABI_News" & str_detect(text,"speciale covid"))) %>%
#                                         filter(!(user_username == "BMWK" & date == "2021-08-18")) %>%
#                                       filter(!(user_username == "BMWK" & date == "2021-07-16")) %>%
#                                       filter(!(user_username == "BMWK" & str_detect(text,"anträge auf neustarthilfen plus natürlicher personen für den förderzeitraum juli bis september"))) %>%
#                                       filter(!(user_username == "BMWK" & date == "2021-06-18")) %>%
#                                       filter(!(user_username == "BMWK" & str_detect(text,"alle aktuellen coronahilfen, die direkte zuschüsse beinhalten, befinden sich mittlerweile im sogenannten fachverfahren"))) %>%
#                                       filter(!(user_username == "BMWK" & date == "2021-04-09"))
# 
# # # Corpus compilation ####
# # 
# #  load("df_actpol.Rdata")
# # # #
# # corpus_df <- corpus(df_actpol)
# # docnames(corpus_df) <- paste0(corpus_df$user_username,"_",corpus_df$date,"_", corpus_df$ID)
# # # #
# # # # # Germany
# # corpus_deap <- corpus_subset(corpus_df, lang == "de")
# # save(corpus_deap,file="corpus_deap.Rdata")
# # # # bg_de <- pull(read.csv("bigrams_de.csv", encoding = "UTF-8"),2)
# # # # trg_de <- pull(read.csv("trigrams_de.csv", encoding = "UTF-8"),2)
# #  load("annotation_de.Rdata")
# # #
# # compound_de <- c("in höhe","passende antragsverfahren finden interessierte",
# #                  "bundesweit einheitliche plattform","gemeinsame pressemitteilung","mehr informationen",
# #                  "bmf finden","mehr informationen","bitte informieren","weitere informationen")
# # rem_de <- c("http*","@*", "bmas-redaktion","|","bitte_informieren","weitere_informationen","€",
# #             "rund", "in_höhe","finden","plattform","details","informationen","unserer_webseite",
# #             "infos","bitte","seit_beginn","gerne","prozent",
# #               "passende_antragsverfahren_finden_interessierte"," + ","gibt",
# #             "grundsätzlich","ende","fällen","bundesweit_einheitliche_plattform",
# #             "gemeinsame_pressemitteilung","bmf_finden","mehr_informationen", "t.co")
# # #
# # # # dtm
# # dfm_deap <-  tokens(corpus_deap,
# #                     remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>%
# #   tokens_compound(phrase(c(compound_de))) %>%
# #   tokens_remove(c(stopwords("de"),stopwords_de,rem_de,
# #                   annotation_de[annotation_de$upos == "ADP",]$token,
# #                   annotation_de[annotation_de$upos == "PRON",]$token,
# #                   annotation_de[annotation_de$upos == "AUX",]$token,
# #                   annotation_de[annotation_de$upos == "DET",]$token,
# #                   annotation_de[annotation_de$upos == "SCONJ",]$token)) %>%
# #   dfm()
# # #
# #  save(dfm_deap,file="dfm_deap.Rdata")
# # # #
# # # # # Italy
# # corpus_itap <- corpus_subset(corpus_df, lang == "it")
# # save(corpus_itap,file="corpus_itap.Rdata")
# # # # bg_it <- pull(read.csv("bigrams_it.csv", encoding = "UTF-8"),2)
# # # # trg_it <- pull(read.csv("trigrams_it.csv", encoding = "UTF-8"),2)
# #  load("annotation_it.Rdata")
# # #
# # rem_it <- c("http*","@*","faq","ciglcisluil","SPECIALE","più","già","fra","ufficio_stampa_fabi","ufficio_stampa","asstel",
# #             "a_cura_dell_ufficio","qui"," | ")
# # #
# # compound_it <- phrase(c("ministro giovannini","cassa integrazione","cassa lavoro", "settore terziario", "settore turistico",
# #                         "a cura dell ufficio","ufficio stampa fabi","ufficio stampa",
# #                         "unione europea", "impresa zombie","imprese zombie", "azienda zombie",
# #                         "aziende zombie","salva italia","decreto ristori","decreti ristoro",
# #                         "lotto di vaccini","istituto di credito","instituti di credito",
# #                         "piano nazionale","sistema produttivo","sistemi produttivi",
# #                         "rapporto finanziario","made in italy","tassi di interesse", "tasso di interesse",
# #                         "tassi d'interesse","tasso d'interesse","interessi economici","interesse economico",
# #                         "ambito finanziario","termini di resa","termini di interesse","ambito lavorativo",
# #                         "camera di commercio","ministero dello sviluppo economico","rapport eu",
# #                         "stati finanziari","stati di bilancio","stato di emergenza",
# #                         "Ferretti Group","Gruppo Ferretti","Ferretti Yachts","occasione per il sud",
# #                         "blocco dei licenziamenti","blocco licenziamenti", "blocco_dei_licenziamenti",
# #                         "blocco_licenziamenti","risorsa digitale","risorse digitali","dare priorità",
# #                         "cura italia","cura-italia","Cura Italia","Cura-Italia","CURA ITALIA",
# #                         "sistema economico","sistemi economici","sistema politico","marco granelli",
# #                         "marco granelli","sistemi politici","corona virus",
# #                         "biblioteca nazionale","osservatorio nazionale","giornata nazionale della salute",
# #                         "territorio nazionale","interesse nazionale","servizio sanitario nazionale","lutto nazionale",
# #                         "museo nazionale","livello nazionale","corpo nazionale dei vigili del fuoco",
# #                         "decreto legge","disegno di legge","banca d italia","banca centrale europea",
# #                         "misure urgenti","misure straordinarie","misure per imprese","misure di sostegno",
# #                         "milioni di euro","miliardi di euro","reddito di cittadinanza","sostegn* al reddito"))
# # 
# # dfm_itap <-  tokens(corpus_itap,
# #                     remove_punct = TRUE, remove_numbers = TRUE,remove_url = TRUE) %>%
# #   tokens_compound(phrase(c(compound_it))) %>%
# #   tokens_remove(c(stopwords("it"),stopwords_it,rem_it,
# #                   annotation_it[annotation_it$upos == "ADP",]$token,
# #                   annotation_it[annotation_it$upos == "PRON",]$token,
# #                   annotation_it[annotation_it$upos == "AUX",]$token,
# #                   annotation_it[annotation_it$upos == "DET",]$token,
# #                   annotation_it[annotation_it$upos == "SCONJ",]$token)) %>%
# #   dfm()
# # 
# # save(dfm_itap,file="dfm_itap.Rdata")
# # 
# 
# 
# 
# # K diagnostic #####
# library(stm)
# library(dplyr)
# library(furrr)
# library(ggplot2)
# library(purrr)
# library(tidyr)
# plan(multicore)
# #
# # load("dfm_itap.Rdata")
# # dfm_df <- dfm_itap
# # lg <- unique(levels(as.factor(dfm_df$country)))
# 
# # evaluate K
# #
# # k_model <- data_frame(K = c(7,10,13,15,16,20,25,30,35,40)) %>%
# #    mutate(topic_model = future_map(K, ~stm(dfm_df, K = .,
# #           prevalence = ~ (actor * date), verbose = TRUE)))
# # save(k_model,file=paste0("k_model_",lg,".Rdata"))
# # #
# # heldout <- make.heldout(dfm_df)
# # save(heldout,file=paste0("heldout_",lg,".Rdata"))
# 
# # k result computed
# # load(paste0("k_model_",lg,".Rdata"))
# # load(paste0("heldout_",lg,".Rdata"))
# # # # #
# # k_result <- k_model %>%
# #   mutate(exclusivity = map(topic_model, exclusivity),
# #          semantic_coherence = map(topic_model, semanticCoherence, dfm_df),
# #          eval_heldout = map(topic_model, eval.heldout, heldout$missing),
# #          residual = map(topic_model, checkResiduals, dfm_df),
# #          bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
# #          lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
# #          lbound = bound + lfact,
# #          iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
# # save(k_result,file= paste0("k_result_",lg,".Rdata"))
# 
# 
# # plotting k results
# # 
# #  load(paste0("k_result_de.Rdata"))
# #  lg <- "Germany"
# # k_result %>%
# #   transmute(K,
# #             `Lower bound` = lbound,
# #             Residuals = map_dbl(residual, "dispersion"),
# #             `Semantic coherence` = map_dbl(semantic_coherence, mean),
# #             `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
# #   gather(Metric, Value, -K) %>%
# #   ggplot(aes(K, Value, color = Metric)) +
# #   geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
# #   scale_x_continuous(breaks = c(7,10,13,15,16,20,25,30,35,40),guide = guide_axis(angle = -45) ) + #n.dodge = 2,
# #   facet_wrap(~Metric, scales = "free_y") +
# #   labs(x = "K (number of topics)",
# #        y = NULL,
# #        title = "Model diagnostics by number of topics",
# #        subtitle = paste0(lg," = actor * time")) + theme_bw()
# # ggsave(file=paste0("Kdiag_",lg,".jpg"),width = 5, height= 5)
# # #
# # #
# # k_result %>%
# #   select(K, exclusivity, semantic_coherence) %>%
# #   filter(K %in% c(10,15,20,25,30,35)) %>%
# #   unnest() %>%
# #   mutate(K = as.factor(K)) %>%
# #   ggplot(aes(semantic_coherence, exclusivity, color = K)) +
# #   geom_point(size = 2, alpha = 0.7) +
# #   labs(x = "Semantic coherence",
# #        y = "Exclusivity",
# #        title = "Exclusivity and semantic coherence",
# #        subtitle = paste0(lg," = actor * time")) + theme_bw()
# # ggsave(file=paste0("Kcoher_",lg,".jpg"),height = 5,width = 5)
# 
# # TPM  ####
# # # 
# library(quanteda)
# library(stm)
# library(parallel)
# library(ggplot2)
# library(dplyr)
# library(tidyverse)
# library(tidyr)
# # 
# # 
# load("dfm_de.Rdata")
# dfm_df <- dfm_de
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
# # # Beta distribution
# # 
# 
# # # 
# # # # tpm detect
#  library(stm)
#   load("tpm_Germany30.Rdata")
#   load("df_de.Rdata")
#  key_it <- unique(read.xls("annotation_it_keywords.xls")[,2])
#  key_de <- unique(read.xls("annotation_de_keywords.xls")[,2])
# #
# # #
 
 
 # Word embedding Chris Bail, disgarded
 # tidy_skipgrams <- df_de  %>% select(c("ID","text")) %>%
 #   unnest_tokens(ngram, text, token = "ngrams", n = 3) %>%
 #   mutate(ngramID = row_number()) %>% 
 #   tidyr::unite(skipgramID, ID, ngramID) %>%
 #   unnest_tokens(word, ngram) %>%
 #    filter(!word %in% stopwords_de) %>%
 #   filter(!word %in% rem_de)
 # 
 # #calculate unigram probabilities (used to normalize skipgram probabilities later)
 # unigram_probs <- df_de %>%  select(c("ID","text")) %>%
 #   unnest_tokens(word, text) %>%
 #   count(word, sort = TRUE) %>%
 #   mutate(p = n / sum(n))
 # 
 # #calculate probabilities
 # skipgram_probs <- tidy_skipgrams %>%
 #   pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
 #   mutate(p = n / sum(n))
 # 
 # ### normalize probabilities, filter number words
 # normalized_prob <- skipgram_probs %>%
 #   filter(n > 20) %>%
 #   rename(word1 = item1, word2 = item2) %>%
 #   left_join(unigram_probs %>%
 #               select(word1 = word, p1 = p),
 #             by = "word1") %>%
 #   left_join(unigram_probs %>%
 #               select(word2 = word, p2 = p),
 #             by = "word2") %>%
 #   mutate(p_together = p / p1 / p2)
 # 
 # # pmi matrix
 # pmi_matrix <- normalized_prob %>%
 #   mutate(pmi = log10(p_together)) %>%
 #   cast_sparse(word1, word2, pmi)
 # 
 # 
 # pmi_matrix@x[is.na(pmi_matrix@x)] <- 0
 # pmi_svd <- irlba(pmi_matrix, 2, maxit = 1000)
 # word_vectors <- pmi_svd$u
 # rownames(word_vectors) <- rownames(pmi_matrix)
 # 
 # forplot<-as.data.frame(word_vectors)
 # forplot$word<-rownames(forplot)
 # ggplot(forplot, aes(x=V1, y=V2, label=word))+
 #   geom_text(aes(label=word),hjust=0, vjust=0, color="blue")+
 #   theme_minimal()+
 #   xlab("First Dimension Created by SVD")+
 #   ylab("Second Dimension Created by SVD")
 
 
