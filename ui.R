

ui <- fluidPage(br(), br(),sidebarLayout(
    sidebarPanel(selectInput(
        inputId = "ui_selection",
        label = "type",
        choices = vec_data
    ),actionButton("goButton", "Valider!"),br(), 
    tableOutput("stats")),
    mainPanel(fluidPage(
        wellPanel(
            htmlOutput("phrase_serv")),
       lapply(vec_data,ligne)
    )
))
)