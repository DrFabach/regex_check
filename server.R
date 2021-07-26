


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
        pos<-sample(pos,1) 
       
         if(type_i %in%c("nlpRF1")){
        neg<-res_regex_patient[!res_regex_patient[,type_i]&
                                   res_regex_patient[,"rfstandalone"],]$id
        neg2<- res_regex_patient[!res_regex_patient[,type_i]&
                                     !res_regex_patient[,"rfstandalone"],]$id
        neg<-c(neg,neg,neg2)
        neg<-sample(neg,1)    
            
        }else if(type_i %in%c("nlpccp")){
        neg<-res_regex_patient[!res_regex_patient[,type_i]&
                                   res_regex_patient[,"ccpstandalone"],]$id
        neg2<-res_regex_patient[!res_regex_patient[,type_i]&
                                    !res_regex_patient[,"ccpstandalone"],]$id
        neg<-c(neg,neg,neg2)
        neg<-sample(neg,1) 
        }else{
       neg<-res_regex_patient[!res_regex_patient[,type_i],]$id
      
       neg<-sample(neg,1) 
       
        }
        dataselection<- c(
            pos, neg)

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
