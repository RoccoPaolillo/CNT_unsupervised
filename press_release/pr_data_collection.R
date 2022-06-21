library(rvest)
library(tidyverse)
library(tibble)
library(dplyr)
library(stringi)
library(stringr)

setwd("C:/Users/rocpa/OneDrive/Desktop/CNT/press_release/")
# training ####

url_tr = "https://raw.githack.com/ccs-amsterdam/r-course-material/master/miscellaneous/simple_html.html"

read_html(url_tr) %>%
  # html_element("#steve") %>% # id
  # html_element(".someTable") %>% # class, first element
  html_elements(".someTable") %>% # class, all elements
  html_table()
# del <- read.csv("delete_text.csv",sep=";",fileEncoding="UTF-8-BOM")[,1]
# CNT ####



# BVMW #### ok ####
df <- data[data$actor == "BVMW",]
del_t <- paste0(del[del$text == "BVMW",]$delete,collapse="|")
# functions
title <- function(i){read_html(i) %>% html_element("div.col-sm-12") %>% 
    html_element("h1") %>% html_text2()}
subtitle <- function(i){read_html(i) %>% html_element("div.col-sm-12") %>% 
    html_element("p.teaser") %>%
    html_text2()}
text <- function(i){read_html(i) %>% html_element(".text-left") %>% 
    html_text2()}

# BDI #### to do ####

df <- data[data$actor == "BDI",]
i = "https://bdi.eu/artikel/news/eu-erweist-sich-als-handlungsfaehig-und-solidarisch/"
title <- function(i){read_html(i) %>% html_nodes("[property = 'og:title']") %>% 
    html_attr("content")}
subtitle<- function(i){read_html(i) %>% html_element("div.teaser") %>% html_text2()}
text <- function(i){read_html(i) %>% html_elements("div.bdi-content.bdi-ctype__text") %>% 
    html_elements("p") %>% html_text2()}


# BAU ####

df <- data[data$actor == "DTB",]
del_t <- paste0(del[del$text == "DTB",]$delete,collapse="|")

title <- function(i){read_html(i) %>% 
    html_nodes("[property = 'og:title']") %>% html_attr("content")}
subtitle <- "xxxxx"
test <- function(i){read_html(i) %>% html_element("div.news-text-wrap") %>% html_text2()}


# BITKOM ####
# i = "https://www.bitkom.org/Presse/Presseinformation/Jedes-dritte-Startup-hat-Corona-Hilfen-erhalten"
read_html(i) %>% html_nodes("[property = 'og:title']") %>% html_attr("content")
subtitle <- "xxxxx"
read_html(i) %>% html_element("div.wysiwyg__body") %>% html_text2()



# VDA ####
i = "https://www.vda.de/vda/de/presse/Pressemeldungen/200604-Statement-von-VDA-Praesidentin-Hildegard-Mueller-zum-Koalitionsausschuss"
read_html(i) %>% html_nodes("[property = 'og:title']") %>% html_attr("content")
subtitle <- "xxxxx"
read_html(i) %>% html_element("div.RichText_RichText__vPwAm") %>% html_text2()

# Data check #####
data <- read.csv("keyword_data.csv",sep=";",fileEncoding="UTF-8-BOM")
data[is.na(data)] = 0
df <- data[data$actor == "VDA",]
# del <- read.csv("delete_text.csv",sep=";",fileEncoding = "UTF-8-BOM")
# "%nin%" <- Negate("%in%")
df <- df %>% filter(check_run != 1)

# one
i <- "https://www.bitkom.org/Presse/Presseinformation/Corona-Krise-Wo-Startups-jetzt-Hilfe-bekommen"
read_html(i) %>% html_element("div.news-text-wrap") %>% html_text2()


# run-all
for (i in df$link) {
  message('row: ', i)
  t <-  read_html(i) %>% html_nodes("[property = 'og:title']") %>% html_attr("content")
  print(t)
}

# VBI ####
i <- "https://www.vbi.de/presse/news/investitionsrueckgang-24-prozent-der-planungsunternehmen-kaempfen-mit-den-folgen-der-coronakrise/"
read_html(i) %>% html_element("head") %>% html_element("title") %>% html_text2()
read_html(i) %>% html_element("div.custom_intro.mb-3")  %>% 
  html_element("span.intro.w-100.unit.text-dark.fw-medium.fs-125.mb-3") %>% html_text2()
read_html(i) %>% html_element("div.single__content.w-100.no-gutters") %>% 
  html_element("p") %>% html_text2()
# short survey 
read_html(i) %>% html_element("ul.is-style-custom-ul-chevrons.is-style-iconPrimary") %>%
  html_text2()

# BIO-Deutschland ####
i <- "https://www.biodeutschland.org/de/pressemitteilungen/massnahmen-der-bundesregierung-erreichen-den-forschenden-mittelstand-nicht.html"
read_html(i) %>% html_element("div.newsletter") %>% html_element("h1") %>% html_text2()
"xxxxx"
read_html(i) %>% html_element("div.newsletter") %>% html_elements("p") %>%
  html_text2()

# VCI ####

i <-"https://www.vci.de/presse/pressemitteilungen/ueberwindung-der-corona-folgen-braucht-zeit.jsp"
read_html(i) %>% html_nodes("[property = 'og:title']") %>% html_attr("content")
"xxxxx"
read_html(i) %>% html_element("div.component.c-text-image") %>% html_text2()

# ZVEI ####
i <- "https://www.zvei.org/presse-medien/pressebereich/klimabericht-zeigt-gebaeudesektor-muss-endlich-energiewendefaehig-werden"
read_html(i) %>% html_nodes("[property = 'og:title']") %>% html_attr("content")
"xxxxx"
read_html(i) %>% html_element("p.MsoBodyText") %>% html_text2()

# BDE ####
i <- "https://www.bde.de/presse/mitgliedschaft-unternehmensgruen/"
read_html(i) %>% html_nodes("[property = 'og:title']") %>% html_attr("content")
read_html(i) %>% html_nodes("[name = 'description']") %>% html_attr("content")
read_html(i) %>% html_element("div.rich-text") %>% html_text2()

# BDG ####
i <- "https://www.guss.de/organisation/presseinformation/auswirkungen-der-russlandsanktionen"

# ZIA ####
i <- "https://zia-deutschland.de/pressrelease/eu-kommission-macht-weg-fuer-hoehere-hilfen-frei-hilfsprogramme-muessen-nun-zuegig-angepasst-werden/"
read_html(i) %>% html_element("h2.hl1") %>% html_text2()
"xxxxx"
read_html(i) %>% html_element("div.text") %>% html_text2()

# VATM ####  to DO ####
i <- "https://www.vatm.de/vatm-statement-mehrwertsteuersenkung-wird-kundenfreundlich-und-unbuerokratisch-umgesetzt/"
read_html(i) %>% html_element("h1") %>% html_text2()
read_html(i) %>% html_element("body")

# vDMA #### to DO ####
i <- "https://www.vdma.org/viewer/-/v2article/render/1330434"
read_html(i) %>% html_nodes("[property = 'og:title']") %>% html_attr("content")
read_html(i) %>% html_nodes("[property = 'og:description']") %>% html_attr("content")

# BdB ####
i <- "https://bankenverband.de/newsroom/presse-infos/kreditwirtschaft-hilft-grosstes-staatliches-kreditprogramm-umzusetzen/"
read_html(i) %>% html_element("h1") %>% html_text2()
"xxxxx"
p <- read_html(i) %>% html_element("div.row.content.article-content.bv__presstemplate") %>%
  html_elements("p") %>% html_text2()

# ZDH ####
i <- "https://www.zdh.de/presse/veroeffentlichungen/pressemitteilungen/konjunkturpaket-jetzt-schnell-und-buerokratiearm-umsetzen/"
read_html(i) %>% html_nodes("[property = 'og:title']") %>% html_attr("content")
read_html(i) %>% html_element("div.contentHeaderHeader__text") %>% html_text2()
read_html(i) %>% html_element("div.co__rteContent.rteContent") %>% html_text2()

# HDE ####
i <- "https://einzelhandel.de/index.php?option=com_content&view=article&id=12762"
read_html(i) %>% html_element("h1") %>% html_text2
"xxxxx"
read_html(i) %>% html_element("article.item-page") %>% html_elements("p") %>%
  html_text2()

# GDV #### itemlist not working ####
i <- "https://www.gdv.de/de/themen/politische-positionen/stellungnahmen/steuerliche-corona-hilfsmassnahmen-weiter-verbessern-84842"
read_html(i) %>% 
  html_element("h1.title.title--mlarge.color--bordeaux") %>% html_text2()
read_html(i) %>% html_nodes("[property = 'og:description']") %>% html_attr("content")
read_html(i) %>% html_element("div.hide-for-print.columns.small-12.article__image") %>%










# dataset composition ####
df$title <- lapply(df$link,title)
df$subtitle <- lapply(df$link,subtitle)
df$text <- lapply(df$link,text)

str_match(df$text,del_t)
df$text <- str_replace_all(df$text,del_t,"")
df <- apply(df,2,as.character)
write.csv2(df,"compiled/bvmw.csv",row.names = F)

# test df
rm(df)
df <- read.csv("compiled/bvmw.csv",sep=";")
str_match(df$text,del_t)

data[1,]
dff <- data[1,]
d <- paste0(p,collapse="|")



# TEST CODE
i = "https://bdi.eu/artikel/news/verhandlungen-nicht-mit-politischen-zielen-ueberfrachten/"
title <- read_html(i) %>% html_nodes("[property = 'og:title']") %>% html_attr("content")
subtitle <- "xxxxx"
text <- read_html(i) %>% html_element("div.wysiwyg__body") %>% html_text2()
#


  






