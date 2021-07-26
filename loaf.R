library(shinyWidgets)
library(htmltools)
library(dplyr)
library(stringr)

library(DT)
phrase<- read.csv2("tableauregex.csv")
vec_data<- names(phrase%>%select(-Phrases.test)%>%select(-nlpccp5))
vec_data<-vec_data[order(vec_data, decreasing = F)]
regex<-read.csv2("Regex.csv")
regex<- regex%>%filter(tolower(concept)%in%tolower(vec_data))%>%select(type=concept, regex=Regexfrançais )
# regex<- data.frame(type=c('nlpFR','nlpPR'), regex = c("(?i)(\\bFR\\b|facteurs?\\s+rh?umato\\w*)","(?i)\\b(pol[iy]\\s*?-*?\\s*?)?(arth?rites?|arth?ropath?[yi]\\w+|rh?umath?i\\w+)\\s+psoria\\w+\\b"))

regex<- regex%>%arrange(type)%>%pull(regex)
regex<- gsub("\\?\\?","?",regex)
res_regex_patient<- phrase%>%select(-Phrases.test)%>%mutate(id = 1:n() )
library(tidyr)
res_regex_patient<-res_regex_patient%>%select(-nlpccp5)%>%mutate(id= 1:n())
regex_phrases<- phrase%>%pivot_longer(cols = -Phrases.test)%>%filter(value==1)%>%filter(!duplicated(name))%>%
  select(titre=name, phrase=Phrases.test)


phrases_patient <- read_parquet("../../Données patients/entrepot/table_reponse_arrow")
sep_patient<- readRDS("../../Données patients/entrepot/separation_patient")
phrases_patient<- phrases_patient%>%filter(NIPATIENT %in% 
                                             (sep_patient%>%filter(type== "Exploration") %>% pull(NIPATIENT))
) %>%collect

phrases_patient<-phrases_patient%>%select(phrase=REPONSE)%>%mutate(id = 1:n() )
res_regex_patient <- read_parquet("../recherche_regex_explo.parquet")


