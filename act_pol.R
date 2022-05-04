options(scipen = 999)
# for crawling Twitter data 
library(academictwitteR)
library(rtweet)
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
# # for building network and visualization
library(tidygraph)
library(graphlayouts)
library(igraph)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(tidytext)
library(stringi)
library(readtext)
library(parallel)
library(stringr)
library(stm)
# editing
library(ggplot2)
# 
 setwd( )

# In most cases output here presented were already saved as R objects. When possible legally (no Tweet data), these are made available in the repository
# Tidy data from different wrangled samples #####
# load("dfm_depol.Rdata")
#  
# load("df_tw.Rdata") # all tweets on policies from everyone 2020-2021 both countries
# load("df_tot1821.Rdata") # all tweets TA, TU, POL 2018-2021
# # 
# load("df_tw.Rdata") # list of TA,TU, POL in each country
# load("de_pol.Rdata")
# load("de_ta.Rdata")
# load("de_tu.Rdata")
# load("it_pol.Rdata")
# load("it_ta.Rdata")
# load("it_tu.Rdata")
# # 
# # 
# df_actpol <- df_tw %>% filter(tweet_id %in% df_tot1821$tweet_id) # filter TA,TU,POL tweeting about policies
# df_actpol <- df_actpol[!duplicated(df_actpol$tweet_id),]
# df_actpol$actor <- ifelse((df_actpol$user_name %in% de_pol) | (df_actpol$user_name %in% it_pol),
#                    "POL",
#                    ifelse((df_actpol$user_name %in% de_ta) | (df_actpol$user_name %in% it_ta),
#                    "TA",
#                    "TU"))
# 
#  # 
# #df_actpol <- df_actpol %>% filter(created_at >= "2020-01-01")
# df_actpol$datet <- strftime(df_actpol$created_at, format = "%j") # making time a continuous variable
# df_actpol$datet <- as.integer(df_actpol$datet)
# 
# save(df_actpol,file="df_actpol.Rdata")
# rm(df_tw,df_tot1821,de_pol,it_pol,de_ta,it_ta,de_tu,it_tu)

 # #### Identification bigrams and trigrams, filtering out stopwords ####
 # Germany
 # load("df_de.Rdata") 
 #   
 # bigrams_tx_de <- df_de %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
 # bigrams_tx_de %>% dplyr::count(bigram, sort = TRUE)
 # bigrams_separate_de <- bigrams_tx_de %>% separate(bigram,c("word1","word2"),sep=" ")
 # bigrams_filtered_de <- bigrams_separate_de %>%
 #   filter(!word1 %in% stopwords_de) %>%
 #   filter(!word2 %in% stopwords_de)
 # bigrams_filtered_de %>%  dplyr::count(word1, word2, sort = TRUE)
 # bigrams_united_de <- bigrams_filtered_de %>% unite(bigram, word1, word2, sep = " ")
 # bigrams_united_de <- bigrams_united_de$bigram
 # 
 # write.csv(bigrams_united_de,"bigrams_de.csv")
 # #
 # # #
 # trigrams_tx_de <- df_de %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)
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
 # bigrams_tx_it <- df_it %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
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
 # #
 # # #
 # trigrams_tx_it <- df_it %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)
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

 

# Lemmatization with UDPIPE ####
 
  # Germany
  #  udmodel_de <- udpipe_download_model(language = "german")
  #  # # udpipe in parallel
  #  path <- udmodel_de$file_model
  #  annotation_de <- udpipe(x = df_de$text, path , parallel.cores = 40, parallel.chunksize = 10)
  # 
  # # udmodel_de <- udpipe_load_model(file = udmodel_de$file_model) # no parallel
  # # annotation_de <- udpipe_annotate(udmodel_de, x = df$text, doc_id = df$tweet_id) # no parallel
  # annotation_de <- as.data.frame(annotation_de) %>% filter(!is.na(lemma))
  # save(annotation_de,file = "annotation_de.Rdata")
  # saveRDS(annotation_de, file = "annotation_de.rds")
 
  # Italy
  # udmodel_it <- udpipe_download_model(language = "italian")
  # # # udpipe in parallel
  # path <- udmodel_it$file_model
  # annotation_it <- udpipe(x = df_it$text, path , parallel.cores = 40, parallel.chunksize = 10)
  # 
  # # udmodel_de <- udpipe_load_model(file = udmodel_it$file_model) # no parallel
  # # annotation_de <- udpipe_annotate(udmodel_it, x = df_it$text, doc_id = df_it$tweet_id) # no parallel
  # annotation_it <- as.data.frame(annotation_it) %>% filter(!is.na(lemma))
  # save(annotation_it,file = "annotation_it.Rdata")
  # saveRDS(annotation_it, file = "annotation_it.rds")
 
 
# Corpus Building ####
 
#  load("df_tot1821.Rdata")
#  load("df_tw.Rdata")
#  load("df_policies.Rdata")

# # #  Bigrams and trigrams saved
# bg_it <- pull(read.csv("it_bigrams.csv", encoding = "UTF-8"),2)
# trg_it <- pull(read.csv("it_trigrams.csv", encoding = "UTF-8"),2)
# bg_de <- pull(read.csv("de_bigrams.csv", encoding = "UTF-8"),2)
# trg_de <- pull(read.csv("de_trigrams.csv", encoding = "UTF-8"),2)
# load("annotation_de.Rdata")
# load("annotation_it.Rdata")
# 
# df_policies <- df_policies %>% filter(created_at >= "2020-01-01")
# df_policies$datet <- strftime(df_policies$created_at, format = "%j")
# df_policies$datet <- as.integer(df_policies$datet)
#
# Total corpus
 load("df_actpol.Rdata")
 spc <- pull(read.csv("special_characters.csv", encoding = "UTF-8"),1) # special characters not ASCII to maintain
 spct <- paste(spc,collapse="")
 chrmt <- paste0("[^\x01-\x7F",spct,"]") 
 "%!in%" <- Negate("%in%")
 df_actpol <- df_actpol %>% filter(user_name %!in% c("VDMA","FABI"))
# #  
  corpus_df <- corpus(df_actpol)
 corpus_df <- gsub(chrmt," ", corpus_df)  # for not ASCII (emoticon), keeps normal characthers + spct list
 corpus_df <- gsub("&amp;", " ", corpus_df) # remove how ";" is translated
 corpus_df <- gsub("\\s+", " ", corpus_df)
 corpus_df <- gsub("\n", " ",  corpus_df)
 corpus_df <- gsub("\t", " ",  corpus_df)
 corpus_df <- gsub("'", " ",  corpus_df)
 corpus_df <- gsub("’", " ",  corpus_df)
  corpus_df <- gsub("#"," ",corpus_df)

  # filtered corpus by language
 corpus_itap <- corpus_subset(corpus_df, lang == "it")
 corpus_deap <- corpus_subset(corpus_df, lang == "de")

save(corpus_itap,file="corpus_itap.Rdata")
 save(corpus_deap,file="corpus_deap.Rdata")


 
# Germany document feature matrix #### 
 
#  load("corpus_deap.Rdata")
 

  bg_de <- pull(read.csv("de_bigrams.csv", encoding = "UTF-8"),2)
  trg_de <- pull(read.csv("de_trigrams.csv", encoding = "UTF-8"),2)
  load("annotation_de.Rdata")
  annotation_hand_de <-  read.csv("annotation_hand_de.csv", encoding = "UTF-8",sep =";")
  names(annotation_hand_de)[1] <- "token"
# 
  annotation_hand_de$token <- tolower(annotation_hand_de$token)  # lower case, as features appear in dfm
  annotation_hand_de$lemma <- tolower(annotation_hand_de$lemma)
  annotation_de$token <- tolower(annotation_de$token)
  annotation_de$lemma <- tolower(annotation_de$lemma)

 # to remove
   rem_de <- c("http*","@*","1","2","3","4","5","6","7","0","000","co",
               "immer",  "dass","zahl","prozent","viele","mehr","schon","sei","gibt","sagt","sagte","dabei",
              "a" ,"b", "c", "d" ,"e", "f", "g" ,"h" ,"i", "j", "k" ,"l", "m" ,"n" ,"o" ,"p" ,"q", "r" ,
              "s" ,"t" ,"u" ,"v", "w", "x", "y", "z","st",
               "komme", "kommst", "kommt", "kommen","geht","seit","ssen" ,
               "muss","musst","muss","müssen","müsst",
               "soll","sollst","sollt","sollen", "sollt", "sollen",
               "laut","jahr","etwa","etwas","moglich", "allerdings","uhr","ezb","ab",
               "kann","kannst","kann", "können", "könnt", "könnte","könnten","könne",
               "eigentlich","warum",
               "grrr_waf_waf_bark_bark_waf_wsf_grrr_grr_bark_bark_waf_wsf_grrrr_aghrrhrr_bark_bark_barkgrr_bark_auuu_au_bark_bark_waf_ooo_waff_wsff_grrr_barrk_bark_grr_waf_bark_waf_wsf_waf_grr_bark",
               "opel_treffen_wsf_tugema_shop","gleiche_hobby_whatsapp_gruppe_gt", "barrierefreie_umbauten_kfw_vergibt",
               "ja_nein","via","ja","nimmt","bedeutet","drittel","mio_eur","mio_euro","decken","beim","wegen","beim", "mal","mehreit","heute",
               "möglich","beispiel","wer","egal","demo","worum_geht_s","gibt s","gibt_s","rund","more info and english version","more_info_and_english_version",
               "sieht","social.media@bvmw.de","tun","teil","bleibt","wer","vielen dank","vielen_dank","infos","herzlichen glückwunsch","herzlich willkommen",
               "herzlich_willkommen", "viele_grüße","viele grüße","erreichen","geht's","h","hallo","tipps","mehr infos","mehr_infos","sehen","gibt's","gibt","h","s",
               "dgb","igmetall","verdi","bvmw","ig metall","ig_metall","ig","updat",
               "herzlichen_glückwunsch", "gehets","spricht","tag","lang","u.a","uhr live","uhr_live","hoffen_dass","hoffen dass","setzen","setzt","fast","livestream",
               "interview","tages","tag","gehts","gut","sorry","ende","gute","macht","z_b","z b","darf","nein","sicht",
               "bdi-vizepräsidentin","läuft","aktuellen_podcast_fordert_mario_ohoven","ntv-interview",
              "Senden Sie #Stop um Updates abzuschalten","senden sie #stop um updates abzuschalten",
              "senden sie #stop um updates abzuschalten",
              "updat","stop","abzuschalten","information","push-nachricht","ab_sofort_bekommen","ber",
              "innen","nnen","neu","bmg","gesundheit","gr",
               "senden_sie_stop_um_updates_abzuschalten","finden","iii",
  "reifenhäuser_maschinenbauvscorona_zusammengegencorona_maschinenbau_coronahilfe_flattenthecurve_corona_covid2019_coronavirusde covidー19",
  "maschinenbau_im_einsatz_gegen_die_corona-krise_vdma-mitgliedsfirmen_handeln","erfolgen",
  "dauerhaft","mindestens","schwannecke","hdE-huptgeschäftsführer","weiter*_information*","faq","mehr_information*"
              )

# to compound
  compound_de <- phrase(c(
    "Der Maschinenbau im Einsatz gegen die Corona-Krise: VDMA-Mitgliedsfirmen handeln",
    "reifenhäuser maschinenbauvscorona zusammengegencorona maschinenbau coronahilfe _flattenthecurve corona covid2019 coronavirusde covidー19",
  "maschinenbau im einsatz gegen die corona-krise vdma-mitgliedsfirmen handeln",
  "europäische union","europaeische union","zombie firma","zombie unternehmen","zombie firmen",
                           "deutschen banken", "jens ehrhardt",  "carsten dierig","paschal donohoe",
                           "lucas flöther","alexander herzog","flüchtlingswelle 2015","millionen kurzarbeiter",
                           "ifo geschäftsklimaindex","insolvenzen abgewendet","jörg hofmann","mark schieritz",
                           "stefan bratzel",	"isabel schnabel","jan roth","DGB-Vize Elke Hannack",
                           "Senden Sie #Stop um Updates abzuschalten","senden sie stop um updates abzuschalten",
                           "senden sie stop um updates abzuschalten",
                           "ÜH iii","basel iii","weiter* information*","mehr inforation*", "üh ii","sgb ii","alg ii",
                            "säule ii",
                           "corona hilf*","corona hilfsprogra*","konjunktur program*","konjunktur* paket",
                           "corona soforthilf*","sozialschut* paket","sozialschut* paket 2","sozialschut* paket 3",
                           "überbrückungshilf* i","überbrückungshilf* ii","überbrückungshilf* iii",
                           "überbrückungshilf* iii plus","überbrückungshilf* iv",
                           "überbrückungshilf* 1","überbrückungshilf* 2","überbrückungshilf* 3",
                           "überbrückungshilf* 3 plus","überbrückungshilf* 4",
                           "überbrückungshilf* i program*","überbrückungshilf* ii program*","überbrückungshilf* iii program*",
                           "überbrückungshilf* iii plus program*","überbrückungshilf* iv program*",
                           "überbrückungshilf* 1 program*","überbrückungshilf* 2 program*","überbrückungshilf* 3 program*",
                           "überbrückungshilf* 3 plus program*","überbrückungshilf* 4 program*",

                           "überbrueckungshilf* i","überbrueckungshilf* ii","überbrueckungshilf* iii",
                           "überbrueckungshilf* iii plus","überbrueckungshilf* iv",
                           "überbrueckungshilf* 1","überbrueckungshilf* 2","überbrueckungshilf* 3",
                           "überbrueckungshilf* 3 plus","überbrueckungshilf* 4",
                           "überbrueckungshilf* i program*","überbrueckungshilf* ii program*","überbrueckungshilf* iii program*",
                           "überbrueckungshilf* iii plus program*","überbrueckungshilf* iv program*",
                           "überbrueckungshilf* 1 program*","überbrueckungshilf* 2 program*","überbrueckungshilf* 3 program*",
                           "überbrueckungshilf* 3 plus program*","überbrueckungshilf* 4 program*",

                           "ueberbrueckungshilf* i","ueberbrueckungshilf* ii","ueberbrueckungshilf* iii",
                           "ueberbrueckungshilf* iii plus","ueberbrueckungshilf* iv",
                           "ueberbrueckungshilf* 1","ueberbrueckungshilf* 2","ueberbrueckungshilf* 3",
                           "ueberbrueckungshilf* 3 plus","ueberbrueckungshilf* 4",
                           "ueberbrueckungshilf* i program*","ueberbrueckungshilf* ii program*","ueberbrueckungshilf* iii program*",
                           "ueberbrueckungshilf* iii plus program*","ueberbrueckungshilf* iv program*",
                           "ueberbrueckungshilf* 1 program*","ueberbrueckungshilf* 2 program*","ueberbrueckungshilf* 3 program*",
                           "ueberbrueckungshilf* 3 plus program*","ueberbrueckungshilf* 4 program*",
                           "ueberbrückungshilf* i","ueberbrückungshilf* ii","ueberbrückungshilf* iii",
                           "ueberbrückungshilf* iii plus","ueberbrückungshilf* iv",
                           "ueberbrückungshilf* 1","ueberbrückungshilf* 2","ueberbrückungshilf* 3",
                           "ueberbrückungshilf* 3 plus","ueberbrückungshilf* 4",
                           "ueberbrückungshilf* i program*","ueberbrückungshilf* ii program*","ueberbrückungshilf* iii program*",
                           "ueberbrückungshilf* iii plus program*","ueberbrückungshilf* iv program*",
                           "ueberbrückungshilf* 1 program*","ueberbrückungshilf* 2 program*","ueberbrückungshilf* 3 program*",
                           "ueberbrückungshilf* 3 plus program*","ueberbrückungshilf* 4 program*",
                           "neustart* hilf* plus","neustar plus","neustarthilf* plus",
                           "corona steuerhilfegesetz",
                           "sozialschutz* paket*","sozialschutz* paket* 1","sozialschutz* paket* 2","sozialschutz* paket* 3",
                           "sozialschutz* paket*","sozialschutz* paket* i","sozialschutz* paket* ii","sozialschutz* paket* iii",
                           "sozialschutz-paket* ii","sozialschutz-paket* iii",
                           "Kreditanstalt für Wiederaufbau","Kreditanstalt Wiederaufbau","kfw coronahilf*",
                           "bürgschaften der bürgschaftsbanken","buergschaften der buergschaftsbanken",
                           "bürgschaft* der buergschaftsbank*","plattform",
                           "november hilfe*","november-hilfen",
                           "dezember hilf*","dezember-hilfen","dezember-hilfe",
                           "neustart hilf*","neustart hilf* plus","november* und dezemberhilf*",
                           "nov-/dezemberhilfe",
                           "Deutsche* Aufbau- und Resilienzplan","frank werneke","reiner hoffman",
                           "recovery plan",  "react eu", "Europäische Sozialfond*",
                               "eu-sozialfonds" , "eu sozialfonds","next generation",
                           "covid 19","covid 2019","corona virus","corona krise","covid 2019 de","covid 19 de",
  "regulär auszahlung",
                           bg_de,trg_de
                                 ))

  
   dfm_deap <-  tokens(corpus_deap,
                     remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                     remove_numbers = TRUE,remove_url = TRUE) %>%
     tokens_compound(compound_de) %>%
     tokens_remove(c(stopwords("de"),stopwords_de, get_stopwords(language = "de"),rem_de,
                     annotation_de[annotation_de$upos == "PRON",]$token,
                     annotation_de[annotation_de$upos == "SCONJ",]$token,
                     annotation_de[annotation_de$upos == "ADP",]$token,
                     annotation_de[annotation_de$upos == "DET",]$token,
                     annotation_de[annotation_de$upos == "CCONJ",]$token,
                     annotation_de[annotation_de$upos == "AUX",]$token
                     )) %>%
     dfm() %>% dfm_tolower()
#
   # vocabulary transformation for policies and covid
   dfm_deap <- dfm_deap %>% dfm_replace(annotation_hand_de$token,annotation_hand_de$lemma)

   # general lemmatization: filter out the replacement done, i.e. not touch the vocabulary of policies,
   # but lemmatize for any other word

   #`%!in%` <- Negate(`%in%`)  # equal to not in
   # select tokens in automatic notation (annotation_de$token) that are not equal to lemma of hand annotation
   # used for first dictionary conversion of policies and covid (annotation_hand_de$lemma)
   annotation_de <- annotation_de[annotation_de$token %!in% annotation_hand_de$lemma,]
   # lemmatize
   dfm_deap <- dfm_deap %>% dfm_replace(annotation_de$token, annotation_de$lemma)

  save(dfm_deap,file="dfm_deap.Rdata")
  stm_deap <- quanteda::convert(dfm_deap,to="stm")
  save(stm_deap,file="stm_deap.Rdata")

 
# Italy document feature matrix, for details of components: see German document feature matrix ####
   
#  load("corpus_itap.Rdata")
   
   bg_it <- pull(read.csv("it_bigrams.csv", encoding = "UTF-8"),2)
   trg_it <- pull(read.csv("it_trigrams.csv", encoding = "UTF-8"),2)
   load("annotation_it.Rdata")
   annotation_hand_it <-  read.csv("annotation_hand_it.csv", encoding = "UTF-8",sep =";")
   names(annotation_hand_it)[1] <- "token"

   annotation_hand_it$token <- tolower(annotation_hand_it$token)  # lower case, as features appear in dfm
   annotation_hand_it$lemma <- tolower(annotation_hand_it$lemma)
   annotation_it$token <- tolower(annotation_it$token)
   annotation_it$lemma <- tolower(annotation_it$lemma)

   rem_it <- c("http*","@*","1","2","3","4","5","6","7","0","000","co","faq","ciglcisluil","a_cura_dell_ufficio",
                           "circa", "sport","SPECIALE","ufficio","solo","mondo","così","webinar","qui","ora","info","1m2021",
               "a" ,"b", "c", "d" ,"e", "f", "g" ,"h" ,"i", "j", "k" ,"l", "m" ,"n" ,"o" ,"p" ,"q", "r" ,
               "s" ,"t" ,"u" ,"v", "w", "x", "y", "z","più","già","fra","ufficio_stampa_fabi","ufficio_stampa","asstel",
               "a_cura_dell_ufficio")

   compound_it <- phrase(c("ministro giovannini","cassa integrazione","cassa lavoro", "settore terziario", "settore turistico",
                           "a cura dell ufficio","ufficio stampa fabi","ufficio stampa",
                                       "unione europea", "impresa zombie","imprese zombie", "azienda zombie",
                                       "aziende zombie","salva italia","decreto ristori","decreti ristoro",
                                       "lotto di vaccini","istituto di credito","instituti di credito",
                                       "piano nazionale","sistema produttivo","sistemi produttivi",
                                       "rapporto finanziario","made in italy","tassi di interesse", "tasso di interesse",
                                       "tassi d'interesse","tasso d'interesse","interessi economici","interesse economico",
                                       "ambito finanziario","termini di resa","termini di interesse","ambito lavorativo",
                                       "camera di commercio","ministero dello sviluppo economico","rapport eu",
                                       "stati finanziari","stati di bilancio","stato di emergenza",
                                       "Ferretti Group","Gruppo Ferretti","Ferretti Yachts","occasione per il sud",
                                       "blocco dei licenziamenti","blocco licenziamenti", "blocco_dei_licenziamenti",
                                       "blocco_licenziamenti","risorsa digitale","risorse digitali","dare priorità",
                                       "cura italia","cura-italia","Cura Italia","Cura-Italia","CURA ITALIA",
                                       "sistema economico","sistemi economici","sistema politico","marco granelli","marco_granelli",
                                       "Marco Granelli","Marco_Granelli","sistemi politici","corona virus",
                           "Biblioteca Nazionale","osservatorio nazionale","giornata nazionale della salute",
                           "territorio nazionale","interesse nazionale","servizio sanitario nazionale","lutto nazionale",
                           "museo nazionale","livello nazionale","corpo nazionale dei vigili del fuoco",
                           "decreto legge","disegno di legge","banca d italia","banca centrale europea",
                           
                           "aiut* statal*",
                            "aiuti di stato", "aiuti statali","aiuto statale","aiuto di stato",
                            "innovazione digitale",
                           "cura itali*", "decret* cura itali*",
                           "decret* liquidt*", "decret* rilanc*","dl rilanc*",
                           "decret* ristor* 1","decret* ristor* i", "decret* ristor* 2","decret* ristor* ii","decret* ristor* bis",
                           "decret* ristor* 2","decret* ristor* ii","decret* ristor* 3","decret* ristor* iii","decret* ristor* ter",
                           "decret* ristor* 4","decret* ristor* iv","decret* ristor* quater",
                           "decret* sostegn*","decret* sostegn* 1","decret* sostegn* i",
                           "decret* sostegn* 2","decret* sostegn* ii","decret* sostegn* bis",
                           "decret* agost*",
                           "dl liquidt*", "dl rilanc*","dl rilanc*",
                           "dl ristor* 1","dl ristor* i", "dl ristor* 2","dl ristor* ii","dl ristor* bis",
                           "dl ristor* 2","dl ristor* ii","dl ristor* 3","dl ristor* iii","dl ristor* ter",
                           "dl ristor* 4","dl ristor* iv","dl ristor* quater",
                           "dl sostegn*","dl sostegn* 1","dl sostegn* i",
                           "dl sostegn* 2","dl sostegn* ii","dl sostegn* bis",
                           "dl agost*",
                            "cura italia","cura-italia","Cura Italia","Cura_Italia","dl cura italia","dl curaitalia",
                           "dl cura-italia","decreto cura-italia","decreto curaitalia",
                           "decreto liquidità","dl liquidità","decretoliquidità","DECRETO LIQUIDITA","DECRETO LIQUIDITA'",
                           "dl_liquidità","dl-liquidità","decreto-liquidità","decreto_liquidità",
                           "decreto rilancio","decreto-rilancio","decreto_rilancio","dl rilancio","dl_rilancio",
                           "dl-rilancio",
                           "decreto agosto","decreto-agosto","decreto_agosto","dl agosto","dl_agosto",
                           "dl-agosto",
                           "decreto ristori","decreto-ristori","decreto_ristori","dl ristori","dl_ristori","dl-ristori",
                           "decreto ristori2","decreto-ristori2","decreto_ristori2","dl ristori2","dl_ristori2","dl-ristori2",
                           "decreto ristori 2","decreto-ristori 2","decreto_ristori 2","dl ristori 2","dl_ristori 2","dl-ristori 2",
                           "decreto ristori bis","decreto-ristori bis","decreto_ristori bis","dl ristori bis","dl_ristori bis",
                           "decreto ristori3","decreto-ristori3","decreto_ristori3","dl ristori3","dl_ristori3","dl-ristori3",
                           "decreto ristori 3","decreto-ristori 3","decreto_ristori 3","dl ristori 3","dl_ristori 3","dl-ristori 3",
                           "decreto ristori ter","decreto-ristori ter","decreto_ristori ter","dl ristori ter","dl_ristori ter",
                           "decreto ristori4","decreto-ristori4","decreto_ristori4","dl ristori4","dl_ristori4","dl-ristori4",
                           "decreto ristori 4","decreto-ristori 4","decreto_ristori 4","dl ristori 4","dl_ristori 4","dl-ristori 4",
                           "decreto ristori quater","decreto-ristori quater","decreto_ristori quater","dl ristori quater",
                           "dl_ristori quater",
                           "decreto sostegni","decreto-sostegni","decreto_sostegni","dl sostegni",
                           "dl_sostegni",
                           "decreto sostegni bis","decreto-sostegni bis","decreto_sostegni bis","dl sostegni bis",
                           "dl_sostegni bis",
                           "decreto sostegni 2","decreto-sostegni 2","decreto_sostegni 2","dl sostegni 2",
                           "dl_sostegni 2",
                           "next generation",
                           "piano nazional* di ripres* e resilienz*","ripres* e resilienz*", "piano ripresa e resilienza",
                           "fondo sociale europeo","european social fund",
                           "piano nazional* ripresa e resilienza","pian* nazional* di vaccinaz*","piano nazional*",
                           "milion* euro",
                           bg_it,trg_it
                           ))


   dfm_itap <-  tokens(corpus_itap,
                       remove_punct = TRUE, remove_symbols = TRUE, remove_separators = TRUE,
                       remove_numbers = TRUE,remove_url = TRUE) %>%
     tokens_compound(compound_it) %>%
     tokens_remove(c(stopwords("it"),stopwords_it, get_stopwords(language = "it"),rem_it,
                     annotation_it[annotation_it$upos == "PRON",]$token,
                     annotation_it[annotation_it$upos == "SCONJ",]$token,
                     annotation_it[annotation_it$upos == "ADP",]$token,
                     annotation_it[annotation_it$upos == "DET",]$token,
                     annotation_it[annotation_it$upos == "CCONJ",]$token,
                     annotation_it[annotation_it$upos == "AUX",]$token
     )) %>%
     dfm() %>% dfm_tolower()

   # vocabulary transformation for policies and covid
   dfm_itap <- dfm_itap %>% dfm_replace(annotation_hand_it$token,annotation_hand_it$lemma)

   # general lemmatization: filter out the replacement done, i.e. not touch the vocabulary of policies,
   # but lemmatize for any other word

   #`%!in%` <- Negate(`%in%`)  # equal to not in
   # select tokens in automatic notation (annotation_de$token) that are not equal to lemma of hand annotation
   # used for first dictionary conversion of policies and covid (annotation_hand_de$lemma)
   annotation_it <- annotation_it[annotation_it$token %!in% annotation_hand_it$lemma,]
   # lemmatize
   dfm_itap <- dfm_itap %>% dfm_replace(annotation_it$token, annotation_it$lemma)

   save(dfm_itap,file="dfm_itap.Rdata")
   stm_itap <- quanteda::convert(dfm_itap,to="stm")
   save(stm_itap,file="stm_itap.Rdata")


# Stats descriptives ####
  
  load("df_actpol.Rdata")
  
  # categories
  sam_tot <- data.frame(act = c("POL","TA","TU","POL","TA","TU"),
                        country = c("Germany","Germany","Germany","Italy","Italy","Italy"),
                        freq = c(14,27,12,21,44,38))
  
  # total sample and tweet activities
  tot_sample <- ggplot(sam_tot,aes(x=act,y=freq,fill=country)) + geom_col(position = "dodge2") + ylab("") + 
    xlab("Actors") + scale_x_discrete(labels = c('POL\nPolitical\n Institutions','TA\nTrade\n Associations','TU\nTrade\n Unions'))+
    scale_fill_manual(values = c("Italy" = "red","Germany" = "darkgreen")) +
    guides(fill=  guide_legend("Country")) + theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size = 15),
          legend.text=element_text(size=15),
          legend.title=element_text(size=15),
          axis.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 16))  
  # ggsave(file="descriptive/actor_sample.jpg",width = 8,height=6)
  
  act_sample <- ggplot(df_actpol,aes(x=actor,fill = country)) + geom_bar(position = "dodge2") + ylab("Tweets") + 
    xlab("Actors") + scale_x_discrete(labels = c('POL\nPolitical\n Institutions','TA\nTrade\n Associations','TU\nTrade\n Unions'))+
    guides(fill=guide_legend("Country")) + theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size = 16),
          legend.text=element_text(size=15),
          legend.title=element_text(size=15),
          axis.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 16))   
  # ggsave(file="tot_sample.jpg",width = 7,height=6)
  
  
  df_actpol$monthfl <- factor(df_actpol$month, levels = c("01-2020","02-2020","03-2020","04-2020","05-2020","06-2020","07-2020","08-2020",
                                                          "09-2020","10-2020","11-2020","12-2020","01-2021","02-2021","03-2021","04-2021",
                                                          "05-2021","06-2021","07-2021","08-2021","09-2021","10-2021","11-2021","12-2021"))
  
  
  activity_actor <-  ggplot(df_actpol,aes(x=monthfl,fill=actor)) + geom_bar() + facet_wrap(~ country,dir = "v") +
    ylab("Tweets") + xlab("Time") +
    guides(fill=guide_legend(title="Actors")) +
    theme_bw() +   theme(axis.text.x = element_text(angle = 330, hjust = 0, vjust = 1,size=12),
                         legend.text=element_text(size=16),
                         legend.title=element_text(size=15),
                         axis.text.y = element_text(size = 14),
                         axis.title.y = element_text(size = 16),
                         axis.title.x = element_text(size=15))  
  # ggsave(file="tweet_time.jpg",width = 8,height = 6)
  
  
  
  ggpubr::ggarrange(act_sample, tot_sample, activity_actor,nrow=2,ncol=2)
  ggsave(file= "descriptive/tot_descpt.jpg",width = 18,height = 15)
  
  load("dfm_deap.Rdata")
  
  # specific policies for category Germany
 plot_dem <- function(i,w,h){
 
  as <- textstat_frequency(dfm_subset(dfm_deap,actor == i),groups=month)
  as <- as[as$feature %in% unique(annotation_hand_de[annotation_hand_de$lemma %!in% c("covid","verlängern","nachtragshaushalt"),]$lemma),]
 # as <- as[as$feature %in% unique(annotation_hand_it[annotation_hand_it$lemma %!in% c("covid","lavoratori"),]$lemma),]
  as$group <- factor(as$group, levels = c("01-2020","02-2020","03-2020","04-2020","05-2020","06-2020","07-2020","08-2020",
                                                "09-2020","10-2020","11-2020","12-2020","01-2021","02-2021","03-2021","04-2021",
                                                "05-2021","06-2021","07-2021","08-2021","09-2021","10-2021","11-2021","12-2021"))
 as$feature <- factor(as$feature,levels = c("coronahilfen","konjunkturpaket", "wsf","soforthilfen","sozialschutzpaket",
                                             "kurzarbeit", "sozialschutzpaket2","sozialschutzpaket3","uberbruckungshilfe",
                                             "uberbruckungshilfe2",
                                 "uberbruckungshilfe3","uberbruckungshilfe3plus","uberbruckungshilfe4","steuerhilfegesetz","großbürgschaften","novemberhilfen",
                                "november/dezemberhilfe","dezemberhilfen","neustarthilfen","neustarthilfenplus",
                                 "nextgen","recoveryplan","react","esf"))

 ggplot(as, aes(x = group, y=frequency, fill = feature)) + geom_col() +
    ylab("Frequency") + xlab("Time") +  ggtitle(paste0("Germany, ",i)) +
    guides(fill=guide_legend(title="Policies")) + theme_minimal() +
    theme(axis.title.x = element_text(size=14), axis.title.y = element_text(size=14),
          axis.text.x = element_text(size = 11,angle= 30, hjust = 1, vjust = 1.5),
          title = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text=element_text(size=14)) 
    ggsave(file=paste0("descriptive/as_de_",i,".jpg"),width = w, height = h)
    
  }
 
  tu_de <- plot_dem("TU",11,5)
  ta_de <- plot_dem("TA",11,5)
  pol_de <- plot_dem("POL",11,5)
 

ggpubr::ggarrange(tu,ta,common.legend = TRUE, legend = "bottom", nrow = 2)
ggsave(file=paste0("tu_ta_de.jpg"),width = 11, height = 12)
  
  
  # specific policies for category Italy
plot_itm <- function(i,w,h){
    
    as <- textstat_frequency(dfm_subset(dfm_itap,actor == i),groups=month)
    as <- as[as$feature %in% unique(annotation_hand_it[annotation_hand_it$lemma %!in% c("covid","lavoratori"),]$lemma),]
    as$group <- factor(as$group, levels = c("01-2020","02-2020","03-2020","04-2020","05-2020","06-2020","07-2020","08-2020",
                                            "09-2020","10-2020","11-2020","12-2020","01-2021","02-2021","03-2021","04-2021",
                                            "05-2021","06-2021","07-2021","08-2021","09-2021","10-2021","11-2021","12-2021"))
    as$feature <- factor(as$feature,levels = c("aiuticovid","aiutidistato","curaitalia","liquidità_dl","rilancio_dl",
                                               "agosto_dl","ristori_dl","ristori2_dl","ristori3_dl","ristori4_dl","sostegni_dl",
                                               "sostegni2_dl","nextgen","recoveryplan","react","esf"))
    
    
    ggplot(as, aes(x = group, y=frequency, fill = feature)) + geom_col() +
      ylab("Frequency") + xlab("Time") +  ggtitle(paste0("Italy, ",i)) +
      guides(fill=guide_legend(title="Policies")) + theme_minimal() +
      theme(axis.title.x = element_text(size=14), axis.title.y = element_text(size=14),
            axis.text.x = element_text(size = 11,angle= 30, hjust = 1, vjust = 1.5),
            title = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text=element_text(size=14)) 
    ggsave(file=paste0("descriptive/as_it_",i,".jpg"),width = w, height = h)
    
  }
tu_ti <- plot_itm("TU",11,5)
ta_ti <- plot_itm("TA",11,5)
pol_ti <- plot_itm("POL",11,5)


  
  # Co-occurrence plots ####
# see https://nballier.github.io/tm4ss.github.io/Tutorial_5_Co-occurrence.html
  
  
# function ####
  
  
  calculateCoocStatistics <- function(coocTerm, binDTM, measure = "DICE"){
    
    # Ensure Matrix (SparseM} or matrix {base} format
    require(Matrix)
    
    # Ensure binary DTM
    if (any(binDTM > 1)) {
      binDTM[binDTM > 1] <- 1
    }
    
    # calculate cooccurrence counts
    coocCounts <- t(binDTM) %*% binDTM
    
    # retrieve numbers for statistic calculation
    k <- nrow(binDTM)
    ki <- sum(binDTM[, coocTerm])
    kj <- colSums(binDTM)
    names(kj) <- colnames(binDTM)
    kij <- coocCounts[coocTerm, ]
    
    # calculate statistics
    switch(measure, 
           DICE = {
             dicesig <- 2 * kij / (ki + kj)
             dicesig <- dicesig[order(dicesig, decreasing=TRUE)]
             sig <- dicesig
           },
           LOGLIK = {
             logsig <- 2 * ((k * log(k)) - (ki * log(ki)) - (kj * log(kj)) + (kij * log(kij)) 
                            + (k - ki - kj + kij) * log(k - ki - kj + kij) 
                            + (ki - kij) * log(ki - kij) + (kj - kij) * log(kj - kij) 
                            - (k - ki) * log(k - ki) - (k - kj) * log(k - kj))
             logsig <- logsig[order(logsig, decreasing=T)]
             sig <- logsig    
           },
           MI = {
             mutualInformationSig <- log(k * kij / (ki * kj))
             mutualInformationSig <- mutualInformationSig[order(mutualInformationSig, decreasing = TRUE)]
             sig <- mutualInformationSig    
           },
           {
             sig <- sort(kij, decreasing = TRUE)
           }
    )
    sig <- sig[-match(coocTerm, names(sig))]
    return(sig)
  }
  
  
  numberOfCoocs <- 10
  
  dfm_df <- dfm_deap
  
  dfm_dfcooc <- dfm_subset(dfm_df, actor == "POL" )
  
  # subset dfm
  coocCounts <- t(dfm_dfcooc) %*% dfm_dfcooc
  
  # key-term

  # coocTerm <- "imprese_zombie"
  

  
  
      coocTerm <- "nextgen"
      
  k <- nrow(dfm_dfcooc)
  ki <- sum(dfm_dfcooc[, coocTerm])
  kj <- colSums(dfm_dfcooc)
  names(kj) <- colnames(dfm_dfcooc)
  kij <- coocCounts[coocTerm, ]
  
  # as.matrix(coocCounts[202:205, 202:205])
  
  #  function
  
  coocs <- calculateCoocStatistics(coocTerm, dfm_dfcooc, measure="LOGLIK")
  # print(coocs[1:numberOfCoocs])
  
  resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  
  
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  
  # Fill the data.frame to produce the correct number of lines
  tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
  
  # Entry of the search word into the first column in all lines
  tmpGraph[, 1] <- coocTerm
  # Entry of the co-occurrences into the second column of the respective line
  tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
  # Set the significances
  tmpGraph[, 3] <- coocs[1:numberOfCoocs]
  
  # Attach the triples to resultGraph
  resultGraph <- rbind(resultGraph, tmpGraph)
  
  # Iteration over the most significant numberOfCoocs co-occurrences of the search term
  for (i in 1:numberOfCoocs){
    
    # Calling up the co-occurrence calculation for term i from the search words co-occurrences
    newCoocTerm <- names(coocs)[i]
    coocs2 <- calculateCoocStatistics(newCoocTerm, dfm_dfcooc, measure="LOGLIK")
    
    #print the co-occurrences
    coocs2[1:10]
    
    # Structure of the temporary graph object
    tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
    tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
    tmpGraph[, 1] <- newCoocTerm
    tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
    tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
    
    #Append the result to the result graph
    resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
  }
  
  # resultGraph[sample(nrow(resultGraph), 6), ]
  
  graphNetwork <- graph.data.frame(resultGraph, directed = F)
  graphVs <- V(graphNetwork)[degree(graphNetwork) < 2]
  graphNetwork <- delete.vertices(graphNetwork, graphVs)
  
  V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange')
  
  # Edges with a significance of at least 50% of the maximum sig- nificance in the graph are drawn in orange
  halfMaxSig <- max(E(graphNetwork)$sig) * 0.5
   E(graphNetwork)$color <- ifelse(E(graphNetwork)$sig > halfMaxSig, "coral", "azure3")
  
  # Disable edges with radius
  E(graphNetwork)$curved <- 0 
  # Size the nodes by their degree of networking
  V(graphNetwork)$size <- log(degree(graphNetwork)) * 5
  
  # All nodes must be assigned a standard minimum-size
  V(graphNetwork)$size[V(graphNetwork)$size < 5] <- 3 
  
  E(graphNetwork)$width <- 2 
 # E(graphNetwork)$width <- scales::rescale(E(graphNetwork)$sig, to = c(1, 10))
  
  # Define the frame and spacing for the plot
   par(mai=c(0,0,0,0)) 
  
  # Finaler Plot
  png(paste0("DEU_POL_",coocTerm,".png"), 600, 600)
  plot(graphNetwork,              
       layout = layout.fruchterman.reingold,  # Force Directed Layout 
       vertex.label.family = "sans",
       vertex.label.cex = 1.5,
       vertex.shape = "circle",
       vertex.label.dist = 0.5,           # Labels of the nodes moved slightly
       vertex.frame.color = 'darkolivegreen',
       vertex.label.color = 'black',      # Color of node names
       vertex.label.font = 1,         # Font of node names
       vertex.label = V(graphNetwork)$name,       # node names
       vertex.label.cex = 1 # font size of node names 
  )
  title(paste0("Germany, POL, ",coocTerm  ),cex.main=2)
 dev.off()

 

# K diagnostics, data objects derived from running on parallel server ####
 
 # K diagnostic objects run on the server ########################################################################
# sample models
  library(stm)
 library(dplyr)
 library(furrr)
 library(ggplot2)
 library(purrr)
 library(tidyr)
 plan(multicore)
 
 # Italy
 k_it_int <- data_frame(K = c(7,10,13,15,16,20,25,30,35,40)) %>%
   mutate(topic_model = future_map(K, ~stm(dfm_itap, K = ., prevalence = ~ (actor * datet), verbose = TRUE)))
 save(k_it_int,file="k_it_int.Rdata")
 
 heldout <- make.heldout(dfm_itap)
 save(heldout,file="heldout_it.Rdata")
 
 k_result_it_int <- k_it_int %>%
   mutate(exclusivity = map(topic_model, exclusivity),
          semantic_coherence = map(topic_model, semanticCoherence, dfm_itap),
          eval_heldout = map(topic_model, eval.heldout, heldout$missing),
          residual = map(topic_model, checkResiduals, dfm_itap),
          bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
          lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
          lbound = bound + lfact,
          iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
 save(k_result_it_int,file="k_result_it_int.Rdata")
 
 # Germany
 
 k_de_int <- data_frame(K = c(7,10,13,15,16,20,25,30,35,40)) %>%
   mutate(topic_model = future_map(K, ~stm(dfm_deap, K = ., prevalence = ~ (actor * datet), verbose = TRUE)))
 save(k_de_int,file="k_de_int.Rdata")
 
 heldout <- make.heldout(dfm_deap)
 save(heldout,file="heldout_de.Rdata")
 
 k_result_de_int <- k_de_int %>%
   mutate(exclusivity = map(topic_model, exclusivity),
          semantic_coherence = map(topic_model, semanticCoherence, dfm_deap),
          eval_heldout = map(topic_model, eval.heldout, heldout$missing),
          residual = map(topic_model, checkResiduals, dfm_deap),
          bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
          lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
          lbound = bound + lfact,
          iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
 save(k_result_de_int,file="k_result_de_int.Rdata")
 
 ###################################################################
 
# Plotting K diagnostic  #######################
 load("k_result_de_int.Rdata")
 load("k_result_it_int.Rdata")

 # Germany
 k_result_de_int %>%
   transmute(K,
             `Lower bound` = lbound,
             Residuals = map_dbl(residual, "dispersion"),
             `Semantic coherence` = map_dbl(semantic_coherence, mean),
             `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
   gather(Metric, Value, -K) %>%
   ggplot(aes(K, Value, color = Metric)) +
   geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
   scale_x_continuous(breaks = c(7,10,13,15,16,20,25,30,35,40),guide = guide_axis(angle = -45) ) + #n.dodge = 2, 
   facet_wrap(~Metric, scales = "free_y") +
   labs(x = "K (number of topics)",
        y = NULL,
        title = "Model diagnostics by number of topics",
        subtitle = "Germany = actor * time") + theme_bw() 
  ggsave(file="TPM/de_diagn.jpg",width = 5, height= 5)
 

  k_result_de_int %>%
   select(K, exclusivity, semantic_coherence) %>%
   filter(K %in% c(20,25,30,35)) %>%
   unnest() %>%
   mutate(K = as.factor(K)) %>%
   ggplot(aes(semantic_coherence, exclusivity, color = K)) +
   geom_point(size = 2, alpha = 0.7) +
   labs(x = "Semantic coherence",
        y = "Exclusivity",
        title = "Exclusivity and semantic coherence",
        subtitle = "Germany, actor * time") + theme_bw()
 ggsave(file="TPM/de_cohexc.jpg",height = 5,width = 5)
 
 
 k_result_it_int %>%
   transmute(K,
             `Lower bound` = lbound,
             Residuals = map_dbl(residual, "dispersion"),
             `Semantic coherence` = map_dbl(semantic_coherence, mean),
             `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
   gather(Metric, Value, -K) %>%
   ggplot(aes(K, Value, color = Metric)) +
   geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
   scale_x_continuous(breaks = c(7,10,13,15,16,20,25,30,35,40),guide = guide_axis(angle = -45) ) + #n.dodge = 2, 
   facet_wrap(~Metric, scales = "free_y") +
   labs(x = "K (number of topics)",
        y = NULL,
        title = "Model diagnostics by number of topics",
        subtitle = "Italy = actor * time") + theme_bw()
 ggsave(file="TPM/it_diagn.jpg",width = 6, height= 6)
 
 
 k_result_it_int %>%
   select(K, exclusivity, semantic_coherence) %>%
   filter(K %in% c(16,20,25,30)) %>%
   unnest() %>%
   mutate(K = as.factor(K)) %>%
   ggplot(aes(semantic_coherence, exclusivity, color = K)) +
   geom_point(size = 2, alpha = 0.7) +
   labs(x = "Semantic coherence",
        y = "Exclusivity",
        title = "Exclusivity and semantic coherence",
        subtitle = "Italy, actor * time") + theme_bw()
 ggsave(file="TPM/it_cohexc.jpg",height = 5,width = 5)
 
# Topic Modelling, topic estimation run on parallel on server ####
 
# Topic estimation on server, example for Italy, same with German document feature matrix #####################
 # sample model
 library(quanteda)
 library(stm)
 library(parallel)
 library(ggplot2)
 library(dplyr)
 library(tidyverse)
 library(tidyr)
 
 load("stm_itap.Rdata")
 
 cl <- makeCluster(50)
 t_list <- c(25,30)
 
 tpc <- function(nb){
   tm_it <- stm::stm(stm_itap$documents,stm_itap$vocab,K= nb, prevalence = ~ (actor *  datet),
                     data= stm_itap$meta,init.type= "Spectral") #,max.em.its= 100)
   save(tm_it, file = paste0("ITint_",nb,".Rdata"))
 }
 
 parallel::clusterExport(cl,"stm_itap") # parallel, cl = cluster to run parallel
 parallel::clusterApply(cl,t_list,tpc)
 
 ###############################################################à
 
 # Topic modelling analysis ############################
 
 
 # Topic modelling beta distribution (words to topic)
 
 load("DEint_20.Rdata")

 png("TPM/DE30_tpplot.png", width = 1500, height = 800)
 tidy(tm_de) %>%
   group_by(topic) %>%
   top_n(10) %>%
   ungroup %>%
   mutate(term =  reorder(term, beta))  %>%
   mutate(topiclab = paste0("Topic ",topic)) %>%
   mutate(topic2 = factor(topiclab, levels = unique(topiclab[order(topic)]))) %>%
   ggplot(aes(term,beta, fill = topiclab)) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~ topic2, scales = "free") +
   coord_flip() +
   ylab("Probability words belonging to topic") +
   xlab("") +
   ggtitle("Germany, actor * time")
 theme_bw() +
   theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
 dev.off()
 
 
# to check within tweets (findThoughts)
 load("stm_deap.Rdata")
 load("corpus_deap.Rdata")  
 tbde <- quanteda::convert(corpus_deap, to = "data.frame")  

 # plot topics overview 
 png("TPM/DE_tp.png", width = 800, height = 600)
 plot(tm_de, main = "Germany, actor * time", n=3, text.cex = 1.2)
 dev.off()
 
 # identify key words for selected topics
 labelTopics(tm_de,18)
 # Findthoughts to identify tweets more likely to use the words of the topic (thoughts)
 thg_de <- findThoughts(tm_de, texts = stm_deap$text,n = 10, topics =18)
 plotQuote(thg_de)
 
 # to read tweets of findthoughts
 for(i in c(2084,4086,4089,4082,105,3775,1586,106,2352,1544)) {
   
 n <- i
 print(c(tbde[n,]$text,tbde[n,]$user_name))

 }
 
# Topical content Germany, a different model with content covariate ~actor was run and here used #########
 load("DEintact_30.Rdata")

 png("TPM/cntDE_30plot.png", width = 900, height = 800)
 tidy(tm_decnt) %>% drop_na() %>%
   group_by(topic) %>%
   top_n(10) %>%
   ungroup %>%
   mutate(term =  reorder(term, beta))  %>%
   mutate(topiclab = paste0("Topic ",topic)) %>%
   mutate(topic2 = factor(topiclab, levels = unique(topiclab[order(topic)]))) %>%
   ggplot(aes(term,beta, fill = topiclab)) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~ topic2, scales = "free") +
   coord_flip() +
   ylab("Probability words belonging to topic") +
   xlab("") +
   ggtitle("Germany, cnt  ~ actor")
 theme_bw() +
   theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
 dev.off()

# Comparison of topical content by actors
 png("TPM/de17ta_tu.png", width = 700, height = 600)
 plot(tm_decnt, type = "perspectives", topics = 17, covarlevels = c("TA","TU"))
 dev.off()

# Italy topic modelling analysis, for details see German topic modelling

 # Beta distribution (words to topic)
 
 load("ITint_30.Rdata")
 
 png("TPM/IT_30.png", width = 800, height = 800)
 tidy(tm_it) %>%
   group_by(topic) %>% drop_na() %>%
   top_n(10) %>%
   ungroup %>%
   mutate(term =  reorder(term, beta))  %>%
   mutate(topiclab = paste0("Topic ",topic)) %>%
   mutate(topic2 = factor(topiclab, levels = unique(topiclab[order(topic)]))) %>%
   ggplot(aes(term,beta, fill = topiclab)) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~ topic2, scales = "free") +
   coord_flip() +
   ylab("Probability words belonging to topic") +
   xlab("") +
   ggtitle("Italy, actor * time")
 theme_bw() +
   theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
 dev.off()
 
 # topic overview
 png("TPM/IT_tp.png", width = 800, height = 600)
 plot(tm_it, main = "Italy, actor * time", n=3, text.cex = 1.2)
 dev.off()
 

 # find thoughts
 load("stm_itap.Rdata")
 load("corpus_itap.Rdata")
 tbit <- quanteda::convert(corpus_itap, to = "data.frame")  
 
 labelTopics(tm_it,1)
 ft_it <- findThoughts(tm_it, texts = stm_itap$text,n = 10, topics =1)
 plotQuote(ft_it)
 
 for(i in c(3199,195,2151,3205,2547,3215,2543,3263,489,3038)) {
   
   n <- i
   print(c(tbit[n,]$text,tbit[n,]$user_name))
   
 }
 
# topical content model 
   load("ITintactt_30.Rdata")
 
 png("TPM/cntIT_30plot.png", width = 900, height = 800)
 tidy(tm_itcnt) %>% drop_na() %>%
   group_by(topic) %>%
   top_n(10) %>%
   ungroup %>%
   mutate(term =  reorder(term, beta))  %>%
   mutate(topiclab = paste0("Topic ",topic)) %>%
   mutate(topic2 = factor(topiclab, levels = unique(topiclab[order(topic)]))) %>%
   ggplot(aes(term,beta, fill = topiclab)) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~ topic2, scales = "free") +
   coord_flip() +
   ylab("Probability words belonging to topic") +
   xlab("") +
   ggtitle("Germany, cnt  ~ actor")
 theme_bw() +
   theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
 dev.off()
 
 # topical content analysis
 
 png("TPM/it5ta_tu.png", width = 700, height = 600)
 plot(tm_itcnt, type = "perspectives", topics = 5, covarlevels = c("TA","TU"))
 dev.off()
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 