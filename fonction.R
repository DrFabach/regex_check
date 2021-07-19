ligne<- function(x){
  
  fluidRow(
    fluidRow(htmlOutput(outputId = paste0("ui_",x))),
    fluidRow(
      actionBttn(
        inputId = paste0("ui_",x,"_T"),
        label = "Match"
      ),
      actionBttn(
        inputId =  paste0("ui_",x,"_F"),
        label = "Match Pas"
      )
    )
  )
}
