library(shinyWidgets)
library(htmltools)
library(dplyr)
library(stringr)

library(DT)
imagevrai<-img("www/faux.png")
imagefaux<-'<img src="./www/faux.png" height="140" width="400"/>'
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
# regex_phrases<-data.frame(id = 1:2,titre = vec_data, phrase = c("Découverte d’une arthrite rhumatoïde juvénile chez un adolescent","Dosage des ACCP et du FR augmenté"))
# res_regex_patient<- data.frame(id = 1:2 ,nlpFR = c(F,T),nlpPR = c(T,F))
# res_verif<-read.csv("essai.csv")
res_i<-res_regex_patient[1,]
res_verif<-res_regex_patient[0,]
res_regex_patient[res_regex_patient==1]<-T

phrases_patient<-phrase%>%select(phrase=Phrases.test)%>%mutate(id = 1:n() )
  
  
for(i in 1:dim(res_regex_patient)[2]) res_regex_patient[,i]<- ifelse(res_regex_patient[,i]==1,T,F)
res_regex_patient$id<- 1:(dim(res_regex_patient)[1])




server <- function(input, output, session) {
    output$ui_essai <- renderText({
        input$ui_essai_T
    })
    
   
   
    
    res_verif_i <- reactiveValues(df=res_verif)
    
    output$stats <- renderTable({
      type_i<-input$ui_selection
      algo<- res_regex_patient[,type_i]%>%table
      verif<-res_regex_patient[which(res_regex_patient$id %in% res_verif_i$df$id),type_i]%>%table
 
      if(length(verif)==0) verif<- c(0,0)
      print(verif)
      retour<-matrix(c("Match +",verif[1], algo[1],"Match -", verif[2], algo[2]),byrow = T,nrow = 2)
      colnames(retour)<- c(" ","Verifié","Total")
      print(retour%>%as.data.frame())
      return(retour%>%as.data.frame()
      )
    })
    observeEvent(input$goButton, {
        res <- lapply(vec_data, function(i) input[[paste0('ui_', i,"_val")]])%>%unlist
        print(phrase_i())
     print((res))

        
        res_verif_i$df[nrow(res_verif_i$df) + 1,] <-c(res,  phrase_i()$id_i)
        
        
        print(vec_data[names(!vec_data %in% res_verif_i$df)])
      

       write.csv(res_verif_i$df,"essai.csv",row.names = F)
    })    
  
    
    phrase_i<-reactive({
        input$goButton
       

        type_i<-input$ui_selection
        res_regex_patient<-res_regex_patient

        if(length(res_verif_i$df$id)>0)  res_regex_patient<-res_regex_patient%>%filter(!id%in%res_verif_i$df$id)    
        pos<-res_regex_patient[res_regex_patient[,type_i],]$id
       
       
         if(type_i %in%c("nlpRF1")){
        neg<-res_regex_patient[!res_regex_patient[,type_i]&
                                   res_regex_patient[,"rfstandalone"],]$id
        neg2<- res_regex_patient[!res_regex_patient[,type_i]&
                                     !res_regex_patient[,"rfstandalone"],]$id
        neg<-c(neg,neg,neg2)
            
            
        }else if(type_i %in%c("nlpccp")){
        neg<-res_regex_patient[!res_regex_patient[,type_i]&
                                   res_regex_patient[,"ccpstandalone"],]$id
        neg2<-res_regex_patient[!res_regex_patient[,type_i]&
                                    !res_regex_patient[,"ccpstandalone"],]$id
        neg<-c(neg,neg,neg2)
        }else{
       neg<-res_regex_patient[!res_regex_patient[,type_i],]$id
       neg<-c(neg,neg)
       
        }
        dataselection<- c(
            pos, pos, pos, pos, neg)

        id_i<-sample( dataselection,1)
        

        
        phrase_i<- phrases_patient$phrase[phrases_patient$id ==id_i]
        phrase_i<-phrase_i  %>% gsub(","," ",.)%>%gsub("'|’"," ",.)
        return(list(phrase_i=phrase_i, id_i=id_i))
    })
    matches<- reactive({
        phrase_i<- phrase_i()$phrase_i
       
        position<- str_locate(phrase_i,regex)%>%as.data.frame()
  
        position$type = vec_data
       

        position$matche <- !is.na(position$start)
        return(list(highlight = position%>%
                        filter(!is.na(start))%>%
                                                    select(start,end),
                   matches = position%>%select(type, matche)))  
    })
    
    lapply(vec_data, function(i) {
        output[[paste0("ui_", i)]] <- renderText({
          
            HTML(paste0(h3(i,strong(matches()$matches%>%filter(type == i)%>%pull(matche))),
                        "\nExemple de phrase : ",
                        regex_phrases%>%filter(titre==i)%>%pull(phrase) 
                        ))
        })
    
    }
    
)
    
    
    output$phrase_serv <- renderText({
        phrase_i<- phrase_i()$phrase_i
        if(!dim(matches()$highlight)[1]==0){
        
        highlight_func(phrase_i,matches()$highlight$start,matches()$highlight$end)
        }else{
            
            phrase_i
        }
        
    })
    
  

}
