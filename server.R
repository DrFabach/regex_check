library(shinyWidgets)
library(htmltools)
library(dplyr)

vec_data<- c("nlpRF", "nlpG")
regex<- regex%>%filter(type%>% vec_data)%>%pull(regex)
regex_phrases<-data.frame(titre = vec_data, phrase = c("exemple1","exempl2"))
res_regex_patient

server <- function(input, output, session) {
    output$ui_essai <- renderText({
        input$ui_essai_T
    })
    
    
    phrase_i<-reactive({
        type_i<-input$ui_selection
        res_regex_patient<-res_regex_patient() 
        pos<-res_regex_patient[,res_regex_patient[,type_i]]
        if(type_i %in%c("nlpRF1")){
        neg<-res_regex_patient[,!res_regex_patient[,type_i]&
                                   res_regex_patient[,"rfstandalone"]]
        neg2<- res_regex_patient[,!res_regex_patient[,type_i]&
                                     !res_regex_patient[,"rfstandalone"]]
        neg<-bind_rows(neg,neg,neg2)
            
            
        }else if(type_i %in%c("nlpccp")){
        neg<-res_regex_patient[,!res_regex_patient[,type_i]&
                                       res_regex_patient[,"ccpstandalone"]]
        neg2<-res_regex_patient[,!res_regex_patient[,type_i]&
                                   !res_regex_patient[,"ccpstandalone"]]
        neg<-bind_rows(neg,neg,neg2)
        }else{
       neg<-res_regex_patient[,!res_regex_patient[,type_i]]
       neg<-bind_rows(neg,neg)
       
        }
        dataselection<- bind_rows(
            bind_rows(pos, pos, pos, pos, neg, pos)
        )
        sample(1, dataselection$id)
        phrase_i<- phrase$phrase[phrase$id == dataselection$id]
        
    })
    matches<- reactive({
        phrase_i<- phrase_i()
        position<- str_locate(phrase_i,regex)%>%as.data.frame()
        position$type = vec_data
        position$length<- position$end-position$start
        position$matche <- !is.na(position$start)
        return(list(hiligth = position%>%
                        filter(which.max(length, na.rm=T))%>%
                                                    select(start,end)%>%unlist,
                   matches = position%>%select(type, matche)))  
    })
    
    lapply(vec_data, function(i) {
        output[[paste0("ui_", i)]] <- renderText({
            HTML(paste0(h2(i),
                        "\nExemple de phrase : ",
                        regex_phrases%>%filter(titre==i)%>%pull(phrase), 
                        "\nAlgo dit : ",strong(matches()$matches%>%filter(type == i)%>%pull(matche))))
        })
    
    }
)
}
