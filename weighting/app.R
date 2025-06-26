library(shiny)
library(bslib)
library(tidyverse)

ui <- page_sidebar(
  title = "a weighting application",
  sidebar = sidebar(
    helpText(
      "Upload a file to get started. The file should contain a column named 'weighting' with the weights you want to apply."
    ), 
    position = "left",
    fileInput("file", label = NULL),
  ),
  card(
    card_header("Card header"),
    "Card body",
    tableOutput("data_table")
  )
)

server <- function(input, output) {

  data_input <- reactive({
    req(input$file)  # make sure a file is uploaded
    read_csv(input$file$datapath)  # read it
  })

  output$data_table <- renderTable({
    head(data_input(), 10)  # 显示前10行
  })

}


shinyApp(ui = ui, server = server)