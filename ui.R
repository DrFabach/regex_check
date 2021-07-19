

ui <- fluidPage(br(), br(),sidebarLayout(
    sidebarPanel(selectInput(
        inputId = "ui_selection",
        label = "type",
        choices = vec_data
    )),
    mainPanel(fluidPage(
        wellPanel(),
       lapply(vec_data,ligne)
    )
))
)