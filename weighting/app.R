library(shiny)
library(bslib)
library(tidyverse)
library(survey)

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
    nav_panel("nonresponse adjustment",
        
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
        fluidRow(
            column(6, fileInput("file_post", label = "original survy data")),
            column(6, fileInput("file_population", label = "poststratification table"))
          ),
      ),
      card(
        card_header("related questions"),
        helpText("Please select the appropriate parameters based on the type of sampling method, such as whether stratification 
        or clustering is used, whether weights are applied, and whether population-level adjustments are necessary."),
        fluidRow(
            column(4, 
            uiOutput("dynamic_selectbox_stratm"),
            uiOutput("dynamic_selectbox_cluster"),
            uiOutput("dynamic_selectbox_weight_post"),
            # uiOutput("dynamic_selectbox_fpc"),
            uiOutput("dynamic_selectbox_ssu"),
            checkboxInput("nesting_option", "Use nested design (nest = TRUE)", value = TRUE)),
            column(8, 
            verbatimTextOutput("sampling_method_description"), 
            verbatimTextOutput("population_adjustment_description")))
      ),
      card(
        card_header("Post-stratification Results"),
        tableOutput("post_strat_results"),
        div(style = "text-align: right; margin-top: 10px;",
          downloadButton("download_post_strat", "Download Results")
        )
      )
    ),
    nav_panel("weight triming",
      card(
        card_header("Weight Trimming"),
        fileInput("file_trim", "Upload original survey data"),
        uiOutput("dynamic_selectbox_weight_trim"),
        uiOutput("dynamic_selectbox_cluster_trim"),
        uiOutput("dynamic_selectbox_stratm_trim"),
        uiOutput("dynamic_selectbox_ssu_trim"),
        numericInput("trim_upper_factor", "Trim upper factor (multiplier of mean weight):", value = 3, min = 1, step = 0.5),
        checkboxInput("trim_nest", "Use nested design", value = TRUE),
        actionButton("do_trim", "Apply Trimming"),
        tableOutput("trimmed_results"),
        downloadButton("download_trimmed", "Download Trimmed Data")
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

  data_input_post <- reactive({
    req(input$file_post)  # make sure a file is uploaded
    read_csv(input$file_post$datapath)  # read it
  })

  data_input_population = reactive({
    req(input$file_population)  # make sure a file is uploaded
    read_csv(input$file_population$datapath)  # read it
  })

  output$dynamic_selectbox_stratm <- renderUI({
    selectInput(
      "select_stratm",
      "Select stratification variables:",
      choices = c("NULL", names(data_input_post())),
      selected = "NULL" # default selection
    )
  })

  output$dynamic_selectbox_cluster <- renderUI({
    selectInput(
      "select_cluster",
      "Select clustering variables:",
      choices = c(1, names(data_input_post())),
      selected = 1 # default selection
    )
  })

  output$dynamic_selectbox_weight_post <- renderUI({
    selectInput(
      "select_weight_post",
      "Select weight variables:",
      choices = c("NULL", names(data_input_post())),
      selected = "NULL" # default selection
    )
  })

  # output$dynamic_selectbox_fpc <- renderUI({
  #   selectInput(
  #     "select_fpc",
  #     "Select finite population correction variables:",
  #     choices = c("NULL", names(data_input_post())),
  #     selected = "NULL" # default selection
  #   )
  # })

  output$dynamic_selectbox_ssu <- renderUI({
  selectInput(
    "select_ssu",
    "Select second-stage unit (SSU) variable:",
    choices = c("NULL", names(data_input_post())),
    selected = "NULL"
  )
})

  output$population_adjustment_description <- renderText({
    req(input$file_population)
    paste(
      "Population-level adjustments will be made using the variables:",
      paste(names(data_input_population()), collapse = ", "),
      "\nPlease make sure the population level data is in the same format as the survey data, \nthe names of the variables should be the same."
    )
  })

  

  output$sampling_method_description <- renderText({
    sampling_lines <- c()

    if (input$select_stratm != "NULL" && input$select_cluster == "1") {
      sampling_lines <- c(sampling_lines, "Stratified sampling will be applied based on the selected stratification variable.")
    } else if (input$select_stratm == "NULL" && input$select_cluster == "1") {
      sampling_lines <- c(sampling_lines, "Simple random sampling will be applied (no stratification or clustering).")
    } else if (input$select_stratm == "NULL" && input$select_cluster != "1" && input$select_ssu == "NULL") {
      sampling_lines <- c(sampling_lines, "One-stage cluster sampling will be applied based on the selected cluster variable.")
    } else if (input$select_stratm == "NULL" && input$select_cluster != "1" && input$select_ssu != "NULL") {
      sampling_lines <- c(sampling_lines, "Two-stage cluster sampling will be applied based on the selected cluster and second-stage unit (SSU) variables.")
    }else {
      sampling_lines <- c(sampling_lines, "please check your sampling method selection again.")
    }

    if (input$select_weight_post != "NULL") {
      sampling_lines <- c(sampling_lines, "Sampling weights will be applied using the selected weight variable.")
    }

    # if (input$select_fpc != "NULL") {
    #   sampling_lines <- c(sampling_lines, "Finite population correction (FPC) will be applied based on the selected variable.")
    # }

    paste(sampling_lines, collapse = "\n")
  })

  post_strat_data <- reactive({
    req(input$file_post, input$file_population)

    ids_formula <- if (input$select_cluster != "1" && input$select_ssu != "NULL") {
      as.formula(paste0("~", input$select_cluster, " + ", input$select_ssu))
    } else if (input$select_cluster != "1") {
      as.formula(paste0("~", input$select_cluster))
    } else {
      ~1
    }

    strata_formula <- if (input$select_stratm != "NULL") as.formula(paste0("~", input$select_stratm)) else NULL
    weight_formula <- if (input$select_weight_post != "NULL") as.formula(paste0("~", input$select_weight_post)) else NULL
    # fpc_formula    <- if (input$select_fpc != "NULL") as.formula(paste0("~", input$select_fpc)) else NULL
    ssu_formula <- if (input$select_ssu != "NULL") as.formula(paste0("~", input$select_ssu)) else NULL

    

    des_psw <- svydesign(
      data   = data_input_post(),
      ids    = ids_formula,
      strata = strata_formula,
      weights = weight_formula,
      # fpc     = fpc_formula,
      nest = input$nesting_option
    )

    des_psw_p <- postStratify(des_psw, ~educ + age_grp + gender, data_input_population())
    new_weights <- weights(des_psw_p)

    df <- data_input_post()
    df$post_strat_weight <- new_weights
    df
  })

  output$post_strat_results <- renderTable({
    head(post_strat_data(), 10)
  })

  output$download_post_strat <- downloadHandler(
  filename = function() {
    paste0("post_stratification_results_", Sys.Date(), ".csv")
  },
  content = function(file) {
    write_csv(post_strat_data(), file)
  }
)

data_input_trim <- reactive({
    req(input$file_trim)
    read_csv(input$file_trim$datapath)
  })

  output$dynamic_selectbox_weight_trim <- renderUI({
    selectInput("select_weight_trim", "Select weight variable:",
                choices = names(data_input_trim()), selected = names(data_input_trim())[1])
  })

  output$dynamic_selectbox_cluster_trim <- renderUI({
    selectInput("select_trim_cluster", "Select cluster variable:",
                choices = c("NULL", names(data_input_trim())), selected = "NULL")
  })

  output$dynamic_selectbox_stratm_trim <- renderUI({
    selectInput("select_trim_stratm", "Select strata variable:",
                choices = c("NULL", names(data_input_trim())), selected = "NULL")
  })

  output$dynamic_selectbox_ssu_trim <- renderUI({
    selectInput("select_trim_ssu", "Select SSU variable:",
                choices = c("NULL", names(data_input_trim())), selected = "NULL")
  })

  trimmed_data <- eventReactive(input$do_trim, {
    req(input$select_weight_trim)

    ids_formula <- if (input$select_trim_cluster != "NULL" && input$select_trim_ssu != "NULL") {
      as.formula(paste0("~", input$select_trim_cluster, " + ", input$select_trim_ssu))
    } else if (input$select_trim_cluster != "NULL") {
      as.formula(paste0("~", input$select_trim_cluster))
    } else {
      ~1
    }

    strata_formula <- if (input$select_trim_stratm != "NULL") as.formula(paste0("~", input$select_trim_stratm)) else NULL
    weight_formula <- as.formula(paste0("~", input$select_weight_trim))

    design <- svydesign(
      data = data_input_trim(),
      ids = ids_formula,
      strata = strata_formula,
      weights = weight_formula,
      nest = input$trim_nest
    )

    trimmed_design <- trimWeights(design, upper = input$trim_upper_factor * mean(weights(design)), strict = TRUE)
    new_weights <- weights(trimmed_design)

    df <- data_input_trim()
    df$trimmed_weight <- new_weights
    df
  })

  output$trimmed_results <- renderTable({
    req(trimmed_data())
    head(trimmed_data(), 10)
  })

  output$download_trimmed <- downloadHandler(
    filename = function() {
      paste0("trimmed_weights_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(trimmed_data(), file)
    }
)

}


shinyApp(ui = ui, server = server)


# uploda data
# graph useful weights before and after trimming
# calibration (question mark : difference)