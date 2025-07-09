library(shiny)
library(bslib)
library(tidyverse)

source("helpers.R")

ui <- page_navbar(
  title = "a weighting application",

  navset_tab(
    nav_panel("Home Page",
      card(
        card_header("Welcome to the Weighting Application"),
        p("This application allows you to upload a dataset and perform weighting adjustments based on selected variables."),
        p("Please upload your dataset in CSV format and select the appropriate variables for analysis."),
        p("There are two type of data,"),
        p("1. population level data: where each row represents an individual and columns represent individual-level variables."),
        p("2. sample level data: where each row represents a sample and columns represent sample-level variables."),
        p("we recommand to use population level data with a individual-level weighting approach, and sample level data with a post-stratification or raking approach, along with the summary table."),
        fileInput("file", label = NULL)
      ),
      card(
        card_header("Uploaded Table"),
        tableOutput("data_table")
      ),
      card(
        card_header("Summary"),
        verbatimTextOutput("summary_output")
      )
    ),
    nav_panel("Individual level weighting",
        
        card(
          card_header("Variable Selection"),
          helpText("Select the categorical variables, response variable, and weight variable from the uploaded dataset."),
          p("Note: The 'weighting' column is optional. If it exists, it will be used for weighting adjustments."),
          fluidRow(
            column(6, uiOutput("dynamic_checkbox")),
            column(6, uiOutput("dynamic_checkbox_2"))
          ),
          uiOutput("dynamic_selectbox"),
          uiOutput("dynamic_selectbox_weight"),
        ),
        card(
          card_header("Variables"),
          textOutput("categorical_vars"),
          textOutput("continuous_vars"),
          textOutput("response_var"),
          textOutput("weight_var"),
      ),
      card(
        card_header("weighting adjustment"),
        helpText("This section will provide options for weighting adjustments based on the selected variables."),
        fluidRow(
          column(4, selectInput(
          "select_weighting",
          "Select method for weighting adjustment:",
          choices = c("Inverse Propensity Score", "Propensity score stratification", "CHAID algorithm", "classification and regression trees (CART)", "BART package", "xgboost package" ),
          selected = "Inverse Propensity Score"
        )),
          column(8, textOutput("method_description"))
        ),
        actionButton("show_table", "Show Table", class = "btn-sm")
      ),
      card(
        card_header("Weighting Adjustment Results"),
        tableOutput("weighting_results")
      ),
      card(
        card_header("Weighting Adjustment Plot"),
        plotOutput("weighting_plot")
      )
    ),
    nav_panel("post-stratification",
      card(
        card_header("Post-stratification and raking"),
        helpText("This section will provide options for post-stratification adjustments based on the selected variables."),
        p("Note: This is a placeholder for future implementation of post-stratification methods."),
        actionButton("show_table_2", "Show Table", class = "btn-sm")
      ),
      card(
        card_header("Post-stratification Results"),
        tableOutput("post_strat_results")
      )
    ),
    nav_panel("weight triming",
      card(
        card_header("Histogram (if 'weighting' column exists)"),
        plotOutput("hist_plot")
      )
    ),
  )
)

server <- function(input, output) {

  data_input <- reactive({
    req(input$file)  # make sure a file is uploaded
    read_csv(input$file$datapath)  # read it
  })

  output$dynamic_checkbox <- renderUI({
  if (is.null(input$file)) {
    helpText("Please upload a file to enable variable selection.")
  } else {
    checkboxGroupInput(
      "checkGroup",
      "Select categorical variables:",
      choices = names(data_input()),
      selected = names(data_input())[1]
    )
  }
})

  output$dynamic_checkbox_2 <- renderUI({
    checkboxGroupInput(
      "checkGroup_2",
      "Select continuous variables:",
      choices = names(data_input()),
      selected = names(data_input())[1]
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

  output$continuous_vars <- renderText({
    req(input$checkGroup)
    paste("Selected continuous variables:", paste(input$checkGroup_2, collapse = ", "))
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

  output$method_description <- renderText({
    if (input$select_weighting == "Inverse Propensity Score") {
      "The inverse response propensity weighting by fitting a conventional logistic regression model"
    } else if (input$select_weighting == "Propensity score stratification") {
      "The propensity score stratification method involves dividing the dataset into strata based on the propensity scores and then adjusting the weights within each stratum."
    } else if (input$select_weighting == "CHAID algorithm") {
      "The CHAID algorithm is a decision tree method that can be used for classification and regression tasks."
    } else if (input$select_weighting == "classification and regression trees (CART)") {
      "CART is a decision tree algorithm that can be used for both classification and regression tasks."
    } else if (input$select_weighting == "BART package") {
      "BART (Bayesian Additive Regression Trees) is a non-parametric Bayesian regression method that can be used for complex data."
    } else if (input$select_weighting == "xgboost package") {
      "XGBoost is an optimized distributed gradient boosting library designed to be highly efficient, flexible, and portable."
    }
    else {
      "Please select a valid weighting method."
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
    if(input$select_weighting == "Inverse Propensity Score") {
      
      # result  = propensity_score_glm(data = data_input(), response_var = input$selectGroup, 
      # weight_var = input$selectGroup_2, categorical_vars = input$checkGroup, continuous_vars = input$checkGroup_2)

      head(propensity_score_glm(data = data_input(), response_var = input$selectGroup, weight_var = input$selectGroup_2, 
      categorical_vars = input$checkGroup, continuous_vars = input$checkGroup_2), 10)

    } else if (input$select_weighting == "Propensity score stratification") {
      head(propensity_score_stratification(data = data_input(), response_var = input$selectGroup, 
      weight_var = input$selectGroup_2, categorical_vars = input$checkGroup, 
      continuous_vars = input$checkGroup_2), 10)

    } else if (input$select_weighting == "CHAID algorithm") {
      head(chaid_method(data = data_input(), response_var = input$selectGroup, 
      weight_var = input$selectGroup_2, categorical_vars = input$checkGroup), 10)

    } else if (input$select_weighting == "classification and regression trees (CART)") {
      head(CART_method(data = data_input(), response_var = input$selectGroup, 
      weight_var = input$selectGroup_2, categorical_vars = input$checkGroup), 10)

    } else if (input$select_weighting == "BART package") {
      head(BART_method(data = data_input(), response_var = input$selectGroup, 
      weight_var = input$selectGroup_2, categorical_vars = input$checkGroup, continuous_vars = input$checkGroup_2), 10)

    } else if (input$select_weighting == "xgboost package") {
      head(xgboost_method(data = data_input(), response_var = input$selectGroup, 
      weight_var = input$selectGroup_2, categorical_vars = input$checkGroup, continuous_vars = input$checkGroup_2), 10)

    } else {
       stop("something went wrong")
    }
  })
})

  # output$weighting_plot = renderPlot(
  #   result  = propensity_score_glm(data = data_input(), response_var = input$selectGroup, 
  #   weight_var = input$selectGroup_2, categorical_vars = input$checkGroup, continuous_vars = input$checkGroup_2),

  #   plot(result$chaid_model)
  # )

}


shinyApp(ui = ui, server = server)