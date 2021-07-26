
library(shinyWidgets)
library(htmltools)
library(dplyr)
library(stringr)

library(DT)
# imagevrai<-img("www/faux.png")
# imagefaux<-'<img src="./www/faux.png" height="140" width="400"/>'
# phrase<- read.csv2("tableauregex.csv", fileEncoding = "latin1")
# vec_data<- names(phrase%>%select(-Phrases.test))
# 
# regex<-read.csv2("Regex.csv", fileEncoding = "latin1")
# regex<- regex%>%transmute(type=concept, regex )
# # regex<- data.frame(type=c('nlpFR','nlpPR'), regex = c("(?i)(\\bFR\\b|facteurs?\\s+rh?umato\\w*)","(?i)\\b(pol[iy]\\s*?-*?\\s*?)?(arth?rites?|arth?ropath?[yi]\\w+|rh?umath?i\\w+)\\s+psoria\\w+\\b"))
# 
# regex<- regex%>%filter(type%in% vec_data)%>%arrange(type)%>%pull(regex)
# 
# res_regex_patient<- phrase%>%select(-Phrases.test)%>%mutate(id = 1:n() )
# library(tidyr)
# res_regex_patient<-res_regex_patient%>%select(-nlpccp5)%>%mutate(id= 1:n())
# regex_phrases<- phrase%>%pivot_longer(cols = -Phrases.test)%>%filter(value==1)%>%filter(!duplicated(name))%>%
#   select(titre=name, phrase=Phrases.test)
# # regex_phrases<-data.frame(id = 1:2,titre = vec_data, phrase = c("Découverte d’une arthrite rhumatoïde juvénile chez un adolescent","Dosage des ACCP et du FR augmenté"))
# # res_regex_patient<- data.frame(id = 1:2 ,nlpFR = c(F,T),nlpPR = c(T,F))
# # res_verif<-read.csv("essai.csv")
# 
# res_i<-res_regex_patient[1,]
# res_regex_patient[res_regex_patient==1]<-T
# for(i in dim(res_regex_patient)[2]) res_regex_patient[,i]<- ifelse(res_regex_patient[,i]==1,T,F)
# res_regex_patient$id<- 1:(dim(res_regex_patient)[1])
ligne<- function(x, selected=F){
  
  fluidRow(
    fluidRow(htmlOutput(outputId = paste0("ui_",x))),
    fluidRow(
      
      
      radioButtons(inputId =  paste0("ui_",x,"_val"), label = "",
                   choices = list("Regex matche" = T, "Regex matche pas" = F), 
                   selected = selected)
    )
  )
}


fun_insert <- function(x, pos, insert) {       # Create own function
  gsub(paste0("^(.{", pos, "})(.*)$"),
       paste0("\\1", insert, "\\2"),
       x)
}
highlight_func <- function(text,start, end){
  df<- rbind(data.frame(x=start,y="start"),
             data.frame(x=end,y="end"))
  mark_s<-"<b>"
  mark_e<-"</b>"
  df<-df%>%arrange(x)
  j<-0
  for(i in 1:dim(df)[1]){
    text<- fun_insert(text, df$x[i]+j+ ifelse(df$y[i]=="start",-1,0), ifelse(df$y[i]=="start",mark_s,mark_e))
   j<- ifelse(df$y[i]=="start",j+nchar(mark_s),j+nchar(mark_e))  
    
  }
  
  return(text)
}

highlight_func("ceci est un essai", c(1,2),c(3,4))


