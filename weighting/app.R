library(shiny)
library(bslib)
library(tidyverse)

source("helpers.R")

ui <- page_sidebar(
  title = "a weighting application",
  sidebar = sidebar(
    helpText(
      "Upload a file to get started. The file should contain a column named 'weighting' with the weights you want to apply."
    ), 
    position = "left",

    fileInput("file", label = NULL),

    uiOutput("dynamic_checkbox"),

    uiOutput("dynamic_selectbox"),

    uiOutput("dynamic_selectbox_weight"),

  ),
  navset_tab(
    nav_panel("Table View",
      card(
        card_header("Uploaded Table"),
        tableOutput("data_table")
      ),
      card(
        card_header("Variables"),
        textOutput("categorical_vars"),
        textOutput("response_var"),
        textOutput("weight_var")
      )
    ),
    nav_panel("Summary View",
      card(
        card_header("Summary"),
        verbatimTextOutput("summary_output")
      )
    ),
    nav_panel("Plot View",
      card(
        card_header("Histogram (if 'weighting' column exists)"),
        plotOutput("hist_plot")
      )
    ),
    nav_panel("weighting View",
      card(
        card_header("weighting adjustment"),
        helpText("This section will provide options for weighting adjustments based on the selected variables."),
        selectInput(
          "select_weighting",
          "Select method for weighting adjustment:",
          choices = c("Propensity Score", "Post-stratification"),
          selected = "Propensity Score"
        ),
        actionButton("show_table", "Show Table", class = "btn-sm")
      ),
      card(
        card_header("Weighting Adjustment Results"),
        tableOutput("weighting_results")
      )
    )
  )
)

server <- function(input, output) {

  data_input <- reactive({
    req(input$file)  # make sure a file is uploaded
    read_csv(input$file$datapath)  # read it
  })

  output$dynamic_checkbox <- renderUI({
    req(data_input())
    checkboxGroupInput(
      "checkGroup",
      "Select categorical variables:",
      choices = names(data_input()),
      selected = names(data_input())[1] # default selection
    )
  })

  output$dynamic_selectbox <- renderUI({
    req(data_input())
    selectInput(
      "selectGroup",
      "Select response variables:",
      choices = names(data_input()),
      selected = names(data_input())[1] # default selection
    )
  })  

  output$dynamic_selectbox_weight <- renderUI({
    req(data_input())
    selectInput(
      "selectGroup_2",
      "Select weight variables:",
      choices = c("Null", names(data_input())),
      selected = "Null" # default selection
    )
  })  

  output$categorical_vars <- renderText({
    req(input$checkGroup)
    paste("Selected categorical variables:", paste(input$checkGroup, collapse = ", "))
  })

  output$response_var <- renderText({
    req(input$selectGroup)
    paste("Selected response variable:", input$selectGroup)
  })

  output$weight_var <- renderText({
    req(input$selectGroup_2)
    if (input$selectGroup_2 == "Null") {
      "No weighting variable selected."
    } else {
      paste("Selected weight variable:", input$selectGroup_2)
    }
  })
  
  output$data_table <- renderTable({
    req(data_input())
    head(data_input(), 10)  
  })

 output$summary_output <- renderPrint({
    summary(data_input())
  })

  observeEvent(input$show_table, {
  output$weighting_results <- renderTable({
    if(input$select_weighting == "Propensity Score") {
      
      propensity_score_adjustment(data = data_input(), response_var = input$selectGroup, 
      weight_var = input$selectGroup_2, categorical_vars = input$checkGroup)

    } else {
       stop("something went wrong")
    }
  })
})

}


shinyApp(ui = ui, server = server)