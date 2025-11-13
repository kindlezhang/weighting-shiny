# For the whole website

# loading necessary libraries

library(shiny)
library(bslib)
library(tidyverse)
library(survey)
library(shinyjs)
library(dotwhisker)
library(cowplot)
library(rpart.plot)
library(DT)

# load helper functions

source("helpers.R")
source("superlearner.R")

url1 = "https://twitter.com/intent/tweet?text=Hello%20world&url=https://msph.shinyapps.io/nyc-neighborhoods-covid/"
url2 = "https://www.facebook.com/sharer.php?u=https://msph.shinyapps.io/nyc-neighborhoods-covid/"
url3 = "https://www.instagram.com/columbiapublichealth/"
url4 = "https://www.linkedin.com/shareArticle?mini=true&url=https://msph.shinyapps.io/nyc-neighborhoods-covid/&title=&summary=&source="
url5 = "mailto:NYCN-COVID@cumc.columbia.edu?&subject=&body=https://msph.shinyapps.io/nyc-neighborhoods-covid/"
url6 = "whatsapp://send?text=https://msph.shinyapps.io/nyc-neighborhoods-covid/"
url7 = "https://service.weibo.com/share/share.php?url=https://msph.shinyapps.io/nyc-neighborhoods-covid/&title="

# customCSS

custom_card_header_style <- tags$head(
  tags$style(HTML("
    .card-header {
      font-size: 2.0rem;     
      font-weight: bold;      
      color: #0d6efd;          
      background-color: #f8f9fa; 
    }
  "))
)

# Define UI for application

## ui

ui <- 
  navbarPage(
    custom_card_header_style,
    theme = "shiny.css",
    title = div(img(src='whitelogo.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 50)),
    windowTitle = "weighting application",
    id = "menus",

    tabPanel(title = 'Home',
              shinyjs::useShinyjs(),
              fluidRow(
                column(width = 5, offset = 1, div(img(src = "background.png", height = "70%",width = "50%"),
                                                      style="text-align: center;")),
                column(width = 5,  div(img(src = "HomePagepic.png", height = "100%",width = "100%"),
                                           style="text-align: center;"))),
              br(),
              fluidRow(column(width = 10, offset = 1, span(htmlOutput("Hometext"), style="font-size: 15px;line-height:150%"))),
              br(),
              fluidRow(column(width = 10, offset = 1, 
                h3("instruction"),
                span(htmlOutput("Instruction"), style="font-size: 15px;line-height:150%"))),
              br(),
              fluidRow(align="center",
                        span(htmlOutput("bannertext", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
                              letter-spacing: 0.3px;font-size:18px")),
                        #span(htmlOutput("sharetext", style="color:white")),
                        #br(),
                        #img(src='bottomlogo.png', height="20%", width="20%"),
                       h5("Share on", style="color:white;font-size:12px"),
                    actionButton("twitter_index",
                                 label = "",
                                 icon = icon("twitter"),
                                 onclick = sprintf("window.open('%s')", url1),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("fb_index",
                                 label = "",
                                 icon = icon("facebook"),
                                 onclick = sprintf("window.open('%s')", url2),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    #actionButton("ins_index",
                    #             label = "",
                    #             icon = icon("instagram"),
                    #             onclick = sprintf("window.open('%s')", url3),
                    #             style = "border-color: #FFFFFF;"),
                    actionButton("linkedin_index",
                                 label = "",
                                 icon = icon("linkedin"),
                                 onclick = sprintf("window.open('%s')", url4),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("whats_index",
                                 label = "",
                                 icon = icon("whatsapp"),
                                 onclick = sprintf("window.open('%s')", url6),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("email_index",
                                 label = "",
                                 icon = icon("envelope"),
                                 onclick = sprintf("window.open('%s')", url5),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
                    
           )
           
  ),
  tabPanel(title = "Data Upload",
    card(
        card_header("Input data"),
        p("Please upload your dataset in CSV format."),
        fileInput("file", label = NULL)
      ),
      card(
        card_header("Uploaded Table"),
        div(style = "text-align: left; margin-top: 10px;",
                actionButton("show_table_result_1", "Show Table", class = "btn-primary")),
          conditionalPanel(
            condition = "input.show_table_result_1 % 2 == 1", 
            DT::dataTableOutput("data_table")
            )
      ),
      card(
        card_header("Summary"),
        div(style = "text-align: left; margin-top: 10px;",
                actionButton("show_summary", "Show Summary", class = "btn-primary")),
          conditionalPanel(
            condition = "input.show_summary% 2 == 1", 
             verbatimTextOutput("summary_output")
            )
      ),
      br(),
      fluidRow(align="center",
                    span(htmlOutput("bannertext_2", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
                    #span(htmlOutput("sharetext", style="color:white")),
                    #br(),
                    #img(src='bottomlogo.png', height="20%", width="20%"),
                    h5("Share on", style="color:white;font-size:12px"),
                    actionButton("twitter_index",
                                 label = "",
                                 icon = icon("twitter"),
                                 onclick = sprintf("window.open('%s')", url1),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("fb_index",
                                 label = "",
                                 icon = icon("facebook"),
                                 onclick = sprintf("window.open('%s')", url2),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    #actionButton("ins_index",
                    #             label = "",
                    #             icon = icon("instagram"),
                    #             onclick = sprintf("window.open('%s')", url3),
                    #             style = "border-color: #FFFFFF;"),
                    actionButton("linkedin_index",
                                 label = "",
                                 icon = icon("linkedin"),
                                 onclick = sprintf("window.open('%s')", url4),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("whats_index",
                                 label = "",
                                 icon = icon("whatsapp"),
                                 onclick = sprintf("window.open('%s')", url6),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("email_index",
                                 label = "",
                                 icon = icon("envelope"),
                                 onclick = sprintf("window.open('%s')", url5),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
              )
  ),
    tabPanel(title="Nonresponse adjustment",
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
          div(style = "text-align: left; margin-top: 10px;",
              actionButton("show_table", "Generate Analysis", class = "btn-primary"))
          ),
        card(
          card_header("Weighting Adjustment"),
          helpText("This section will provide options for weighting adjustments based on the selected variables."),
            fluidRow(
              column(4, selectInput(
              "select_weighting",
              "Select method for weighting adjustment:",
              choices = c("Inverse Propensity Score", "Propensity score stratification", "CHAID algorithm", "classification and regression trees (CART)", "BART package", "xgboost package" ),
              selected = "Inverse Propensity Score"
            )),
          column(8, textOutput("method_description")),
          # actionButton("show_warning", "warning", class = "btn-sm", style = "margin-left: 10px; height: 35px;")
          uiOutput("chaid_warning_btn")
          ),
          # style = "width: 100%; min-height: 300px; margin: 30px auto;"
          h4("Weighting Adjustment Results", style = "margin-top: 25px;"),
          div(style = "text-align: left; margin-top: 10px;",
                actionButton("show_table_result", "Show Table", class = "btn-primary"),
                downloadButton("download_weighting_table", "Download Results")),
          conditionalPanel(
            condition = "input.show_table_result % 2 == 1", 
            # tableOutput("weighting_results")
            DT::dataTableOutput("weighting_results")
            ),
          h4("Weighting Adjustment Plot", style = "margin-top: 25px;"),
          div(style = "text-align: left; margin-top: 10px;",
                actionButton("show_plot_result", "Show Plot", class = "btn-primary"),
                downloadButton("download_weighting_plot", "Download Plots")),
          conditionalPanel(
            condition = "input.show_plot_result % 2 == 1", 
            plotOutput("weighting_plot")
            ),
          h4("Analysis Plot", style = "margin-top: 25px;"),
            div(style = "text-align: left; margin-top: 10px;",
                actionButton("show_analysis_plot", "Show Plot", class = "btn-primary")),
          conditionalPanel(
            condition = "input.show_analysis_plot % 2 == 1", 
            plotOutput("analysis_plot")
            ),
            h4("Compare Methods", style = "margin-top: 25px;"),
             div(style = "text-align: left; margin-top: 10px;",
                actionButton("show_compare_plot", "Show Plot", class = "btn-primary")),
          conditionalPanel(
            condition = "input.show_compare_plot % 2 == 1", 
            plotOutput("compare_plot")
            )
          ),
      br(),
      fluidRow(align="center",
                    span(htmlOutput("bannertext_3", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
                                      letter-spacing: 0.3px;font-size:18px")),
                    #span(htmlOutput("sharetext", style="color:white")),
                    #br(),
                    #img(src='bottomlogo.png', height="20%", width="20%"),
                    h5("Share on", style="color:white;font-size:12px"),
                    actionButton("twitter_index",
                                 label = "",
                                 icon = icon("twitter"),
                                 onclick = sprintf("window.open('%s')", url1),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("fb_index",
                                 label = "",
                                 icon = icon("facebook"),
                                 onclick = sprintf("window.open('%s')", url2),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    #actionButton("ins_index",
                    #             label = "",
                    #             icon = icon("instagram"),
                    #             onclick = sprintf("window.open('%s')", url3),
                    #             style = "border-color: #FFFFFF;"),
                    actionButton("linkedin_index",
                                 label = "",
                                 icon = icon("linkedin"),
                                 onclick = sprintf("window.open('%s')", url4),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("whats_index",
                                 label = "",
                                 icon = icon("whatsapp"),
                                 onclick = sprintf("window.open('%s')", url6),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("email_index",
                                 label = "",
                                 icon = icon("envelope"),
                                 onclick = sprintf("window.open('%s')", url5),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
           )),
tabPanel(title="Super Learner",
        card(
          card_header("Model Selection"),
          helpText("Select the method you used and compare them directly"),
          fluidRow(
  column(
    width = 6,
    checkboxGroupInput(
      inputId = "selected_methods",
      label = "Select modeling methods:",
      choices = c("propensity score", "Cart", "Random Forest", "XGBoost"),
      selected = NULL
    ),
    # 动态显示Cart参数
    conditionalPanel(
      condition = "input.selected_methods.indexOf('Cart') > -1",
      numericInput("rf_ntree", "Number of trees (Cart)", value = 500, min = 10, max = 2000),
      numericInput("rf_mtry", "mtry (Cart)", value = 3, min = 1, max = 20)
    )
  ),
  column(
    width = 6,
    checkboxInput(
      inputId = "do_cross_validation",
      label = "Perform cross-validation?",
      value = TRUE
    ),
    conditionalPanel(
      condition = "input.do_cross_validation",
      numericInput(
        inputId = "cv_folds",
        label = "Number of folds",
        value = 5,
        min = 2,
        max = 20,
        step = 1
      )
    )
  )
),
             div(style = "text-align: left; margin-top: 10px;",
              actionButton("SL_generate_analysis", "Generate Analysis", class = "btn-primary")
          )
        ),
        card(
          card_header("training performance"),
        ),
        card(
          card_header("predict performance"),
          plotOutput("SL_plot")
        ),
        card(
          card_header("Reference"),
          a("learn more with xgboost", href = "https://www.bilibili.com/", target = "_blank")
        ),
      br(),
      fluidRow(align="center",
                    span(htmlOutput("bannertext_3", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
                                      letter-spacing: 0.3px;font-size:18px")),
                    #span(htmlOutput("sharetext", style="color:white")),
                    #br(),
                    #img(src='bottomlogo.png', height="20%", width="20%"),
                    h5("Share on", style="color:white;font-size:12px"),
                    actionButton("twitter_index",
                                 label = "",
                                 icon = icon("twitter"),
                                 onclick = sprintf("window.open('%s')", url1),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("fb_index",
                                 label = "",
                                 icon = icon("facebook"),
                                 onclick = sprintf("window.open('%s')", url2),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    #actionButton("ins_index",
                    #             label = "",
                    #             icon = icon("instagram"),
                    #             onclick = sprintf("window.open('%s')", url3),
                    #             style = "border-color: #FFFFFF;"),
                    actionButton("linkedin_index",
                                 label = "",
                                 icon = icon("linkedin"),
                                 onclick = sprintf("window.open('%s')", url4),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("whats_index",
                                 label = "",
                                 icon = icon("whatsapp"),
                                 onclick = sprintf("window.open('%s')", url6),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("email_index",
                                 label = "",
                                 icon = icon("envelope"),
                                 onclick = sprintf("window.open('%s')", url5),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
           )),
  tabPanel(title = "post-stratification",
      # card(
      #   card_header("Post-stratification and raking"),
      #   helpText("This section will provide options for post-stratification adjustments based on the selected variables."),
      #   fluidRow(
      #       column(6, fileInput("file_post", label = "original survy data")),
      #       column(6, fileInput("file_population", label = "poststratification table"))
      #     ),
      # ),
      card(
        card_header("Post-stratification and raking"),
        style = "border: 1px solid #dee2e6; margin-bottom: 20px;", 
        helpText("Choose a method and upload the corresponding inputs."),

        tabsetPanel(
          id = "ps_method",
          type = "pills",  # 或 "tabs"
          
          # --- Tab 1: Post-stratification ---
          tabPanel(
            title = "Post-stratification", value = "post",
            helpText("This section will provide options for post-stratification adjustments based on the selected variables."),
            fluidRow(
            column(6, fileInput("file_post", label = "Original survy data")),
            column(6, 
              tags$label("Post-stratification Table", class = "control-label"),
              div(style = "display: flex; align-items: center;",
                         fileInput("file_population", label = NULL, width = "80%"),
                         actionButton("show_ps_example", "example", class = "btn-sm", style = "margin-left: 10px; height: 35px;")
                     )
            )
          )
          ),
          
          # --- Tab 2: Raking ---
          tabPanel(
            title = "Raking", value = "raking",
            helpText("This section will provide options for raking adjustments based on the selected variables and target margins."),
            fluidRow(
              column(6, fileInput("file_rake",       label = "Original survey data")),
              column(6, fileInput("file_rake_margins", label = "Target margins (CSV)"))
            )
          ),
          
          # --- Tab 3: Calibration ---
          tabPanel(
            title = "Calibration", value = "calib",
            helpText("This section will provide options for calibration adjustments based on the selected variables and calibration totals."),
            fluidRow(
              column(6, fileInput("file_calib",        label = "Original survey data")),
              column(6, fileInput("file_calib_totals", label = "Calibration totals/controls (CSV)"))
            )
          )
        )
      ),
      card(
        card_header("Related questions"),
        style = "border: 1px solid #dee2e6; margin-bottom: 20px;",
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
        style = "border: 1px solid #dee2e6; margin-bottom: 20px;",
        uiOutput("post_strat_results_ui"),
        div(style = "text-align: left; margin-top: 10px;",
          actionButton("show_post_strat_table", "Show Table", class = "btn-primary"),
          downloadButton("download_post_strat", "Download Results")
        )
      ),
      card(
        card_header("Post-stratification Plot"),
        style = "border: 1px solid #dee2e6; margin-bottom: 20px;",
        plotOutput("post_strat_plot"),
        plotOutput("population_dist_plot")
      ),
      br(),
      fluidRow(align="center",
                    span(htmlOutput("bannertext_4", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
                    #span(htmlOutput("sharetext", style="color:white")),
                    #br(),
                    #img(src='bottomlogo.png', height="20%", width="20%"),
                    h5("Share on", style="color:white;font-size:12px"),
                    actionButton("twitter_index",
                                 label = "",
                                 icon = icon("twitter"),
                                 onclick = sprintf("window.open('%s')", url1),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("fb_index",
                                 label = "",
                                 icon = icon("facebook"),
                                 onclick = sprintf("window.open('%s')", url2),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    #actionButton("ins_index",
                    #             label = "",
                    #             icon = icon("instagram"),
                    #             onclick = sprintf("window.open('%s')", url3),
                    #             style = "border-color: #FFFFFF;"),
                    actionButton("linkedin_index",
                                 label = "",
                                 icon = icon("linkedin"),
                                 onclick = sprintf("window.open('%s')", url4),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("whats_index",
                                 label = "",
                                 icon = icon("whatsapp"),
                                 onclick = sprintf("window.open('%s')", url6),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("email_index",
                                 label = "",
                                 icon = icon("envelope"),
                                 onclick = sprintf("window.open('%s')", url5),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
           )
  ),
  tabPanel(title = "weight Triming",
    card(
        card_header("Weight Trimming"),
        fileInput("file_trim", "Upload original survey data"),
        uiOutput("dynamic_selectbox_weight_trim"),
        uiOutput("dynamic_selectbox_cluster_trim"),
        uiOutput("dynamic_selectbox_stratm_trim"),
        uiOutput("dynamic_selectbox_ssu_trim"),
        numericInput("trim_upper_factor", "Trim upper factor (multiplier of mean weight):", value = 3, min = 1, step = 0.5),
        # checkboxInput("trim_nest", "Use nested design", value = TRUE),
        actionButton("do_trim", "Apply Trimming"),
        tableOutput("trimmed_results"),
        downloadButton("download_trimmed", "Download Trimmed Data"),
        br(),
        textOutput("nest_flag_text"),
        br(),
        plotOutput("trimmed_plot", height = "300px"),
      ),
      br(),
           fluidRow(align="center",
                    span(htmlOutput("bannertext_5", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
                    #span(htmlOutput("sharetext", style="color:white")),
                    #br(),
                    #img(src='bottomlogo.png', height="20%", width="20%"),
                    h5("Share on", style="color:white;font-size:12px"),
                    actionButton("twitter_index",
                                 label = "",
                                 icon = icon("twitter"),
                                 onclick = sprintf("window.open('%s')", url1),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("fb_index",
                                 label = "",
                                 icon = icon("facebook"),
                                 onclick = sprintf("window.open('%s')", url2),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    #actionButton("ins_index",
                    #             label = "",
                    #             icon = icon("instagram"),
                    #             onclick = sprintf("window.open('%s')", url3),
                    #             style = "border-color: #FFFFFF;"),
                    actionButton("linkedin_index",
                                 label = "",
                                 icon = icon("linkedin"),
                                 onclick = sprintf("window.open('%s')", url4),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("whats_index",
                                 label = "",
                                 icon = icon("whatsapp"),
                                 onclick = sprintf("window.open('%s')", url6),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("email_index",
                                 label = "",
                                 icon = icon("envelope"),
                                 onclick = sprintf("window.open('%s')", url5),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
                    
           )
    
  ),
  tabPanel(title = "About Us",
           fluidRow(column(10, offset = 1, h2("About Us")),
                    column(10, offset = 1, div(img(src = "msph-n-bios2.JPG", height = "100%",width = "100%"),
                                               style="text-align: center;")),
                    column(10, offset = 1,helpText("MSPH photo source: https://globalcenters.columbia.edu/content/yusuf-hamied-fellowships-program")),
                    column(10, offset = 1,span(htmlOutput("abouttext",style = "font-size: 15px; line-height:150%"))),
                    column(10, offset = 1,span(htmlOutput("abouttext2",style = "font-size: 15px; line-height:150%")))),
           br(),
           fluidRow(align="center",
                    span(htmlOutput("bannertext_6", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
                    #span(htmlOutput("sharetext", style="color:white")),
                    #br(),
                    #img(src='bottomlogo.png', height="20%", width="20%"),
                    h5("Share on", style="color:white;font-size:12px"),
                    actionButton("twitter_index",
                                 label = "",
                                 icon = icon("twitter"),
                                 onclick = sprintf("window.open('%s')", url1),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("fb_index",
                                 label = "",
                                 icon = icon("facebook"),
                                 onclick = sprintf("window.open('%s')", url2),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    #actionButton("ins_index",
                    #             label = "",
                    #             icon = icon("instagram"),
                    #             onclick = sprintf("window.open('%s')", url3),
                    #             style = "border-color: #FFFFFF;"),
                    actionButton("linkedin_index",
                                 label = "",
                                 icon = icon("linkedin"),
                                 onclick = sprintf("window.open('%s')", url4),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("whats_index",
                                 label = "",
                                 icon = icon("whatsapp"),
                                 onclick = sprintf("window.open('%s')", url6),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("email_index",
                                 label = "",
                                 icon = icon("envelope"),
                                 onclick = sprintf("window.open('%s')", url5),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
                    
           ))
  )

## server

server <- function(input, output) {
  
  shinyjs::addClass(id = "menus", class = "navbar-right")

  output$Hometext <- renderUI({
    HTML("      <p>Welcome to the <b>Weighting Adjustment Application!</b><br><p>
                This is a weighting application developed by the <a href='https://www.publichealth.columbia.edu/'>Columbia University Mailman School of Public Health</a> team. 
                This application allows users to upload their datasets and perform various weighting adjustments, including inverse propensity score weighting, propensity score stratification, CHAID algorithm, CART, BART, and XGBoost methods. 
                Users can select categorical variables, response variables, and weight variables from their uploaded datasets to conduct the analysis. 
                The application also provides options for post-stratification adjustments based on selected variables. 
                Users can choose parameters based on their sampling methods, such as stratification, clustering, weights, and population-level adjustments. 
                The results of the weighting adjustments and post-stratification can be viewed in tables and downloaded for further analysis. 
                This tool is designed to facilitate data analysis and improve the accuracy of survey results through appropriate weighting techniques.")
  })

  output$Instruction <- renderUI({
    HTML('
      Before you begin, please make sure you understand the type of data you have and what kind of adjustment you intend to perform.
      The workflow generally follows one of the three paths below:</p>

      <h4>1. If you only have individual-level survey data</h4>
      <ul>
        <li>
          Go to the <b>Data Upload</b> tab to upload your dataset in CSV format.
          
          <button id="example_btn" type="button" class="btn btn-danger btn-sm"
                  onclick="Shiny.setInputValue(\'showExample\', Math.random())">
            Data Example
          </button>
          
        </li>
        <li>Then proceed to the <b>Nonresponse Adjustment</b> tab.</li>
        <li>Select relevant variables (e.g., demographic or design variables) for nonresponse weighting adjustment.</li>
        <li>Click <b>Generate Analysis</b> to perform the adjustment and view results in table and plot formats.</li>
      </ul>

      <h4>2. If you also have marginal or population-level data</h4>
      <ul>
        <li>Navigate to the <b>Post-Stratification</b> tab.</li>
        <li>Upload your marginal or population benchmark data files.</li>
        <li>Choose between <b>raking</b> and <b>post-stratification</b> adjustments based on your data.</li>
        <li>Run the analysis and download the adjusted weights.</li>
      </ul>

      <h4>3. If you only want to trim existing weights</h4>
      <ul>
        <li>Go directly to the <b>Weight Trimming</b> tab.</li>
        <li>Upload the dataset containing the existing weights.</li>
        <li>Specify trimming thresholds or criteria, run the procedure, and download the results.</li>
      </ul>

      <h4>Additional Information</h4>
      <p>If you need further assistance or more information about the application,
      please refer to the <b>About Us</b> tab.</p>
    ') })


  observeEvent(input$showExample, {
    showModal(modalDialog(
      title = "Example Dataset",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tagList(
        p("Below is an example of a survey dataset:"),
        p("Make sure your data table has a weight column and a response column."),
        p("1. If your original data does not have a weight column, add one with all values equal to 1. Your response column should be 0/1, where 1 indicates a response. In this example, the response is linked to the HbA1c variable; NaN values correspond to 0 in the response column."),
        p("2. Besides weight and response, the dataset should include at least one categorical or continuous variable."),
        p("3. The dataset should be in CSV format."),
        tableOutput("example_table")
      )
    ))
  })

  output$example_table <- renderTable({
    # 假定文件都放在 www 文件夹下
    csv_path  <- file.path("data", "example_data.csv")
    xls_path  <- file.path("data", "example_data.xls")
    xlsx_path <- file.path("data", "example_data.xlsx")

    # 检查文件并读取
    if (file.exists(csv_path)) {
      df <- readr::read_csv(csv_path, show_col_types = FALSE)
    } else if (file.exists(xlsx_path)) {
      df <- readxl::read_excel(xlsx_path)
    } else if (file.exists(xls_path)) {
      df <- readxl::read_excel(xls_path)
    } else {
      # 若没有找到任何文件，显示提示
      return(data.frame(Message = "⚠️ Example data file not found in www folder."))
    }

    # 返回前10行
    head(df, 10)
  })
  
  output$bannertext = renderText({
    return(
      "studying weighting methods for survey data analysis"
    )
  })

  output$bannertext_2 = renderText({
    return(
      "studying weighting methods for survey data analysis"
    )
  })

  output$bannertext_3 = renderText({
    return(
      "studying weighting methods for survey data analysis"
    )
  })

  output$bannertext_4 = renderText({
    return(
      "studying weighting methods for survey data analysis"
    )
  })

  output$bannertext_5 = renderText({
    return(
      "studying weighting methods for survey data analysis"
    )
  })

    output$bannertext_6 = renderText({
    return(
      "studying weighting methods for survey data analysis"
    )
  })

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
      "Select weight variables if applicable:",
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

  vals <- reactiveValues(results = NULL, objects = NULL)

  # observeEvent(input$show_table, {
  #     # Precompute all results
  #     vals$results <- list(
  #       "Inverse Propensity Score" = propensity_score_glm(
  #         data = data_input(),
  #         response_var   = input$selectGroup,
  #         weight_var     = input$selectGroup_2,
  #         categorical_vars = input$checkGroup,
  #         continuous_vars = input$checkGroup_2
  #       ),
  #       "Propensity score stratification" = propensity_score_stratification(
  #         data = data_input(),
  #         response_var   = input$selectGroup,
  #         weight_var     = input$selectGroup_2,
  #         categorical_vars = input$checkGroup,
  #         continuous_vars = input$checkGroup_2
  #       ),
  #       "CHAID algorithm" = chaid_method(
  #         data = data_input(),
  #         response_var   = input$selectGroup,
  #         weight_var     = input$selectGroup_2,
  #         categorical_vars = input$checkGroup
  #       )[["data"]],
  #       "classification and regression trees (CART)" = CART_method(
  #         data = data_input(),
  #         response_var   = input$selectGroup,
  #         weight_var     = input$selectGroup_2,
  #         categorical_vars = input$checkGroup
  #       )[["data"]],
  #       "BART package" = BART_method(
  #         data = data_input(),
  #         response_var   = input$selectGroup,
  #         weight_var     = input$selectGroup_2,
  #         categorical_vars = input$checkGroup,
  #         continuous_vars = input$checkGroup_2
  #       )[["data"]],
  #       "xgboost package" = xgboost_method(
  #         data = data_input(),
  #         response_var   = input$selectGroup,
  #         weight_var     = input$selectGroup_2,
  #         categorical_vars = input$checkGroup,
  #         continuous_vars = input$checkGroup_2
  #       )[["data"]]
  #     )

  #     vals$objects = list(
  #       "CHAID algorithm" = chaid_method(
  #         data = data_input(),
  #         response_var   = input$selectGroup,
  #         weight_var     = input$selectGroup_2,
  #         categorical_vars = input$checkGroup
  #       )[["chaid_model"]],
  #       "classification and regression trees (CART)" = CART_method(
  #         data = data_input(),
  #         response_var   = input$selectGroup,
  #         weight_var     = input$selectGroup_2,
  #         categorical_vars = input$checkGroup
  #       )[["cart_model"]],
  #       "BART package" = BART_method(
  #         data = data_input(),
  #         response_var   = input$selectGroup,
  #         weight_var     = input$selectGroup_2,
  #         categorical_vars = input$checkGroup,
  #         continuous_vars = input$checkGroup_2
  #       )[["bart_model"]],
  #       "xgboost package" = xgboost_method(
  #         data = data_input(),
  #         response_var   = input$selectGroup,
  #         weight_var     = input$selectGroup_2,
  #         categorical_vars = input$checkGroup,
  #         continuous_vars = input$checkGroup_2
  #       )[["xgboost_model"]]
  #     )
  #   })

  observeEvent(input$show_table, {
  # 整合方法调用
  method_results <- list(
      "Inverse Propensity Score" = propensity_score_glm(
        data = data_input(),
        response_var = input$selectGroup,
        weight_var = input$selectGroup_2,
        categorical_vars = input$checkGroup,
        continuous_vars = input$checkGroup_2
      ),
      "Propensity score stratification" = propensity_score_stratification(
        data = data_input(),
        response_var = input$selectGroup,
        weight_var = input$selectGroup_2,
        categorical_vars = input$checkGroup,
        continuous_vars = input$checkGroup_2
      ),
      "CHAID algorithm" = chaid_method(
        data = data_input(),
        response_var = input$selectGroup,
        weight_var = input$selectGroup_2,
        categorical_vars = input$checkGroup
      ),
      "classification and regression trees (CART)" = CART_method(
        data = data_input(),
        response_var = input$selectGroup,
        weight_var = input$selectGroup_2,
        categorical_vars = input$checkGroup
      ),
      "BART package" = BART_method(
        data = data_input(),
        response_var = input$selectGroup,
        weight_var = input$selectGroup_2,
        categorical_vars = input$checkGroup,
        continuous_vars = input$checkGroup_2
      ),
      "xgboost package" = xgboost_method(
        data = data_input(),
        response_var = input$selectGroup,
        weight_var = input$selectGroup_2,
        categorical_vars = input$checkGroup,
        continuous_vars = input$checkGroup_2
      )
    )

    vals$results <- list(
      "Inverse Propensity Score" = method_results[["Inverse Propensity Score"]],
      "Propensity score stratification" = method_results[["Propensity score stratification"]],
      "CHAID algorithm" = method_results[["CHAID algorithm"]][["data"]],
      "classification and regression trees (CART)" = method_results[["classification and regression trees (CART)"]][["data"]],
      "BART package" = method_results[["BART package"]][["data"]],
      "xgboost package" = method_results[["xgboost package"]][["data"]]
    )

    vals$objects <- list(
      "CHAID algorithm" = method_results[["CHAID algorithm"]][["chaid_model"]],
      "classification and regression trees (CART)" = method_results[["classification and regression trees (CART)"]][["cart_model"]],
      "BART package" = method_results[["BART package"]][["bart_model"]],
      "xgboost package" = method_results[["xgboost package"]][["xgboost_model"]]
    )
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
  
  # observeEvent(input$show_table_result, {
  #     output$weighting_results <- renderTable({
  #       req(input$select_weighting)
  #       head(vals$results[[input$select_weighting]], 10)
  #     })
  #   }
  # )

  output$chaid_warning_btn <- renderUI({
    if (input$select_weighting == "CHAID algorithm") {
      actionButton("show_warning", "warning", class = "btn-sm", style = "margin-left: 10px; height: 35px;")
    }
    # 如果不是CHAID就不显示
  })

  observeEvent(input$show_warning, {
      showModal(modalDialog(
        title = "warning for a CHAID algorithm",
        p("explaination", strong(" 'Freq' "), "..."),
        footer = modalButton("close"),
        easyClose = TRUE 
      ))
    })

  table_open_1 <- reactiveVal(FALSE)

  observeEvent(input$show_table_result_1, {
    # 翻转开关
    table_open_1(!table_open_1())
    # 改变按钮的文字
    if (table_open_1()) {
      updateActionButton(inputId = "show_table_result_1", label = "Close Table")
    } else {
      updateActionButton(inputId = "show_table_result_1", label = "Show Table")
    }
  })

   output$data_table <- DT::renderDataTable({
      req(table_open_1())
      # 表格数据
      df <- data_input()
      DT::datatable(df, options = list(pageLength = 10, lengthMenu = c(10, 50, 100)))
    })


  summary_open <- reactiveVal(FALSE)

  observeEvent(input$show_summary, {
    # 翻转开关
    summary_open(!summary_open())
    # 改变按钮的文字
    if (summary_open()) {
      updateActionButton(inputId = "show_summary", label = "Close Summary")
    } else {
      updateActionButton(inputId = "show_summary", label = "Show Summary")
    }
  })

  output$summary_output <- renderPrint({
    req(summary_open())
    summary(data_input())
  })

  table_open <- reactiveVal(FALSE)

  observeEvent(input$show_table_result, {
    # 翻转开关
    table_open(!table_open())
    # 改变按钮的文字
    if (table_open()) {
      updateActionButton(inputId = "show_table_result", label = "Close Table")
    } else {
      updateActionButton(inputId = "show_table_result", label = "Show Table")
    }
  })

  output$weighting_results <- DT::renderDataTable({
    req(table_open())
    # 表格数据
    # head(vals$results[[input$select_weighting]], 10)
    df <- vals$results[[input$select_weighting]]
    DT::datatable(df, options = list(pageLength = 10, lengthMenu = c(10, 50, 100)))
  })


  plot_open <- reactiveVal(FALSE)

  observeEvent(input$show_plot_result, {
    # 翻转开关
    plot_open(!plot_open())
    # 改变按钮的文字
    if (plot_open()) {
      updateActionButton(inputId = "show_plot_result", label = "Close Plot")
    } else {
      updateActionButton(inputId = "show_plot_result", label = "Show Plot")
    }
  })


  # plotting using only the first result table
  output$weighting_plot <- renderPlot({
      req(plot_open())
      req(vals$results)


      df <- vals$results[[input$select_weighting]]  |> 
        filter(.data[[input$selectGroup]] != 0)
      
      req(is.data.frame(df), nrow(df) > 0, ncol(df) >= 2)

      sec_last_name <- names(df)[ncol(df) - 1]
      last_name     <- names(df)[ncol(df)]
      df$.idx <- seq_len(nrow(df))

      validate(need(is.numeric(df[[last_name]]), "The last column must be numeric to draw a histogram."))
      
      # figure 1: second last column with line at 1
      yval <- df[[sec_last_name]]
      upper <- quantile(yval, 0.99, na.rm = TRUE)
      # upper = 0.5 * (max(yval, na.rm = TRUE)-1) + 1

      p1 <- ggplot(df, aes(x = .idx, y = .data[[sec_last_name]])) +
        geom_line() +
        geom_hline(yintercept = 1, linetype = "dashed") +
        labs(title = paste0("Line plot of ", sec_last_name, " with reference line at 1"),
            x = "Row index", y = sec_last_name) +
        coord_cartesian(ylim = c(min(yval, na.rm = TRUE), upper)) +
        theme_classic()


      # figure 2: last column vs weight (if exists)
      wname <- input$selectGroup_2
      has_weight_col <- !is.null(wname) && wname != "Null" && wname %in% names(df)

      if (has_weight_col) {
        p2 <- ggplot(df, aes(x = .idx)) +
          geom_line(aes(y = .data[[last_name]], color = "last_col")) +
          geom_line(aes(y = .data[[wname]],     color = "weight")) +
          scale_color_manual(
            name   = "Variable",
            values = c("last_col" = "steelblue", "weight" = "tomato"),
            labels = c("last_col" = last_name, "weight" = wname)
          ) +
          labs(title = paste0("Line plots of ", last_name, " (solid) and ", wname, " (dashed)"),
              x = "Row index", y = "Value") +
          theme_classic()
      } else {
        p2 <- ggplot(df, aes(x = .idx, y = .data[[last_name]])) +
          geom_line() +
          labs(title = paste0("Line plot of ", last_name),
              subtitle = "(Weight column not found in result table)",
              x = "Row index", y = last_name) +
          theme_classic()
      }

      # figure 3: new weight vs original weight
        df_new = df  |> 
          mutate(difference = .data[[last_name]] - .data[[wname]])  |> 
          arrange(.data[[wname]])

        p3 <- df_new |> 
          ggplot(aes(x = .data[[wname]], y = difference)) +
          geom_segment(aes(x = .data[[wname]], xend = .data[[wname]],
                     y = min(difference), yend = difference), color = "steelblue") +
          geom_point(color = "tomato", size = 1) +
          labs(title = "Difference Vertical Segments",
          x = "original weight", y = "difference between old and new weight") +
          theme_classic()


     # Left plot (for sec_last_name), with a reversed x-axis to point left
        p_left <- ggplot(df, aes(x = .data[[sec_last_name]])) +
          geom_histogram(bins = 30, color = "white", fill = "salmon") +
          scale_x_reverse() + # This flips the axis
          labs(x = sec_last_name, y = "Count") +
          theme_classic()

      # Right plot (for last_name), with y-axis labels removed for alignment
        p_right <- ggplot(df, aes(x = .data[[last_name]])) +
          geom_histogram(bins = 30, color = "white", fill = "skyblue") +
          labs(x = last_name, y = "Count") + # Y-axis title is not needed
          theme_classic() +
          theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank()
          )

       library(patchwork)

      # Combine the two histograms side-by-side with a shared title
        combined_hist <- p_left + p_right +
          plot_annotation(
            title = "Comparison of Distributions",
            theme = theme(plot.title = element_text(hjust = 0.5))) 

      
      # df_long <- df %>%
      #   pivot_longer(cols = c(sec_last_name, last_name),
      #               names_to = "variable", values_to = "value")

      # combined_hist = 
      #   ggplot(df_long, aes(x = value, fill = variable)) +
      #     geom_histogram(bins = 30, color = "white") +
      #     facet_wrap(~ variable, scales = "free_x") +
      #     labs(x = NULL, y = "Count") +
      #     theme_classic()
      
      p1 / p2 / p3 / combined_hist
    })


# # --- Render the Analysis Plot (Coefficient Plot) ---
#   output$analysis_plot <- renderPlot({
#     # 1. This plot will only render after the main results are calculated
#     #    by pressing the "Show Table" button. We use vals$results as a trigger.
#     req(vals$results)
    
#     # 2. Get the data and selected variables
#     data <- data_input()
#     response_var <- input$selectGroup
#     # Combine selected categorical and continuous variables into one list of predictors
#     predictor_vars <- c(input$checkGroup, input$checkGroup_2)
    
#     # Ensure the response variable is not in the predictor list
#     predictor_vars <- predictor_vars[predictor_vars != response_var]
    
#     # Validate that at least one predictor is selected
#     validate(
#       need(length(predictor_vars) > 0, "Please select at least one predictor variable (categorical or continuous).")
#     )

#     # 3. Build the logistic regression formula dynamically
#     #    Formula will look like: response_var ~ predictor1 + predictor2 + ...
#     formula_str <- paste(response_var, "~", paste(predictor_vars, collapse = " + "))
#     model_formula <- as.formula(formula_str)

#     # 4. Fit the logistic regression model
#     #    The `tryCatch` block prevents the app from crashing if the model fails to converge
#     model <- tryCatch({
#       glm(model_formula, data = data, family = binomial(link = "logit"))
#     }, error = function(e) {
#       # If model fails, return NULL
#       NULL
#     })

#     # 5. Check if the model was fitted successfully
#     validate(
#       need(!is.null(model), "Model could not be fitted. Please check if variables are appropriate (e.g., response variable should be binary).")
#     )

#     # 6. Create the coefficient plot using the 'dotwhisker' package
#     dwplot(model) +
#       geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
#       labs(
#         title = "Coefficient Plot for Logistic Regression",
#         subtitle = paste("Response Variable:", response_var),
#         x = "Coefficient Estimate",
#         y = "Predictor Variable"
#       ) +
#       theme_minimal_grid() + # A clean theme from dotwhisker
#       coord_flip()
#   })

  analysis_plot_open <- reactiveVal(FALSE)

  observeEvent(input$show_analysis_plot, {
    # 翻转开关
    analysis_plot_open(!analysis_plot_open())
    # 改变按钮的文字
    if (analysis_plot_open()) {
      updateActionButton(inputId = "show_analysis_plot", label = "Close Plot")
    } else {
      updateActionButton(inputId = "show_analysis_plot", label = "Show Plot")
    }
  })

  output$analysis_plot <- renderPlot({
    req(analysis_plot_open)
    req(vals$objects)
    if (input$select_weighting == "CHAID algorithm") {
      chaid_model <- vals$objects[["CHAID algorithm"]]
      validate(need(!is.null(chaid_model), "CHAID model is not available."))
      plot(chaid_model)
    } else if (input$select_weighting == "classification and regression trees (CART)") {
       cart_model = vals$objects[["classification and regression trees (CART)"]]

      validate(
         need(!is.null(cart_model) && inherits(cart_model, "rpart"), 
              "CART model is not available or is not a valid rpart object.")
       )

       rpart.plot(cart_model, 
           main = "CART Decision Tree",
           box.palette = "Blues", 
           shadow.col = "gray",
           yesno = 2
          ) 
    } else if (input$select_weighting == "BART package") {
      bart_model = vals$objects[["BART package"]]
      importance <- apply(bart_model$varcount, 2, mean)

      imp_df <- data.frame(
        variable = names(importance),
        importance = as.numeric(importance)
      )

      ggplot(imp_df, aes(x = reorder(variable, importance), y = importance)) +
      geom_bar(stat = "identity", fill = "grey") +
      coord_flip() +
      labs(title = "BART Variable Importance", y = "Importance") +
      theme_classic() +
      theme(axis.text.y = element_text(size = 12))
      # barplot(importance, main="BART Variable Importance", las=2)
    } else if (input$select_weighting == "xgboost package") {
      xgb_model = vals$objects[["xgboost package"]]
      xgb.plot.importance(xgb.importance(model = xgb_model))
    }
    else {
      plot.new()
      text(0.5, 0.5, "There is no picture here for this method", cex = 1.2)
    }
    
  })

  compare_open <- reactiveVal(FALSE)

  observeEvent(input$show_compare_plot, {
    # 翻转开关
    compare_open(!compare_open())
    # 改变按钮的文字
    if (compare_open()) {
      updateActionButton(inputId = "show_compare_plot", label = "Close Plot")
    } else {
      updateActionButton(inputId = "show_compare_plot", label = "Show Plot")
    }
  })

  output$compare_plot <- renderPlot({
    req(compare_open)
    req(vals$results)
    
    
  })

  output$download_weighting_table <- downloadHandler(
      filename = function() {
        paste0("nonres_table_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write_csv(vals$results[[input$select_weighting]], file)
      }
    )

  output$download_weighting_plot <- downloadHandler(
      filename = function() {
        paste0("nonres_plot_", Sys.Date(), ".jpg")
      },
      content = function(file) {
        jpeg(file, width = 800, height = 600) 
        print(p1)  
        dev.off()
      }
    )

  # model_params <- reactive({
  #   params <- list()
  #   methods <- input$selected_methods
  
  #   # 循环生成参数，或者直接针对每种方法
  #   if ("Cart" %in% methods) {
  #     params$Cart <- list(
  #       ntree = input$rf_ntree,
  #       mtry  = input$rf_mtry
  #     )
  #   }
  #   # 模型如Lasso/Ridge，可增加params$Lasso等
  #   params$methods_selected <- methods  # 方法名称列表也可以加进去

  #   params
  # })

  SL_result <- reactiveVal(NULL)

  observeEvent(input$SL_generate_analysis, {
  # 整合方法调用
  
    result = SL_plot(df = data, outcome = input$selectGroup, methods = input$selected_methods)
    SL_result(result)

  })
  
  output$SL_plot = renderPlot({
    # req(SL_result())
    # plot_obj <- SL_result()$plot   
    # plot(plot_obj) + theme_classic()
  })











  

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
      "Select weight variables",
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

  observeEvent(input$show_ps_example, {
    showModal(modalDialog(
      title = "example for a Post-stratification Table",
      
     
      p("explaination", strong(" 'Freq' "), "..."),
      
      renderTable({
        
        data.frame(
          gender = c("male", "male", "male", "female", "female", "female"),
          age_grp = c("18-34", "35-54", "55+", "18-34", "35-54", "55+"),
          educ = c("college", "high school", "college", "high school", "college", "high school"),
          Freq = c(15000, 22000, 18000, 16000, 24000, 20000)
        )
      }),
      
      footer = modalButton("close"),
      easyClose = TRUE 
    ))
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

   observeEvent(input$show_post_strat_table, {
    output$post_strat_results_ui <- renderUI({
      tableOutput("post_strat_results")
    })
  })

  output$post_strat_results <- renderTable({
    head(post_strat_data(), 10)
  })

  output$post_strat_plot <- renderPlot({
    req(post_strat_data())   

    df <- post_strat_data() |> 
      arrange(desc(weight))  |> 
      mutate(order_id = row_number())

    p1 <- ggplot(df) +
      geom_histogram(aes(x = post_strat_weight), 
                    fill = "skyblue", alpha = 0.5, bins = 30) +
      labs(title = "Distribution of Post-stratification Weights",
           x = "Post-stratification Weight",
           y = "Count") +
      theme_minimal()

    # p2

    p2 <- ggplot(df, aes(x = order_id)) +
      geom_line(aes(y = weight,      color = "Original", linewidth = 1.4)) +
      geom_line(aes(y = post_strat_weight,    color = "Post-stratified", linewidth = 0.4)) +
      scale_color_manual(
        name = "Series",
        values = c("Original" = "red", "Post-stratified" = "skyblue")
      ) +
      labs(
        title = "Weights Sorted by Original (Descending)",
        x = "Rank (by original; largest → smallest)",
        y = "Weight"
      ) +
      theme_minimal()

    library(patchwork)
    p1 | p2
  })

output$population_dist_plot <- renderPlot({
    # req(data_input_population())
    # req(post_strat_data())
    # pop_data <- data_input_population()
    
    # df <- post_strat_data() |> 
    #   arrange(desc(weight))  |> 
    #   mutate(order_id = row_number())

    # names_pop <- names(pop_data)
    # names_survey <- names(df)

    # common_vars <- intersect(names_pop, names_survey)

    # plot_list <- lapply(common_vars, function(var_name) {
    #   ggplot(df, aes(x = .data[[var_name]], y = .data[[freq_col_name]])) +
        
    #     geom_col(fill = "#0d6efd", alpha = 0.8) +
    #     labs(
    #       title = paste("按 '", var_name, "' 划分的分布", sep = ""),
    #       x = var_name,
    #       y = weight
    #     ) +
    #     theme_minimal(base_size = 14) +
    #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
    # })

    # library(patchwork)
    # wrap_plots(plot_list, ncol = 3)
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
    selectInput("select_weight_trim", "Select weight variables:",
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

    nest_flag <- {
      # Case 1: two-stage (PSU + SSU) provided → check if SSU repeats across PSUs
      if (input$select_trim_cluster != "NULL" && input$select_trim_ssu != "NULL") {
        df0 <- data_input_trim()

        # Ensure the two ID columns exist
        stopifnot(input$select_trim_cluster %in% names(df0),
                  input$select_trim_ssu     %in% names(df0))

        # Is the same SSU used in more than one PSU?
        dup_check <- df0 %>%
          dplyr::distinct(!!rlang::sym(input$select_trim_cluster), !!rlang::sym(input$select_trim_ssu)) %>%
          dplyr::count(!!rlang::sym(input$select_trim_ssu)) %>%
          dplyr::filter(n > 1)

        # If SSU repeats across PSUs → must be nested
        if (nrow(dup_check) > 0) TRUE else FALSE

      # Case 2: only PSU (one-stage cluster) or no cluster (ids=~1) → nest not meaningful
      } else {
        FALSE
      }
    }

    if (nest_flag) {
      output$nest_flag_text <- renderText("Note: Based on the selected cluster and SSU variables, a nested design (nest = TRUE) will be used.")
    } else {
      output$nest_flag_text <- renderText("Note: Based on the selected cluster and SSU variables, a non-nested design (nest = FALSE) will be used.")
    }

    strata_formula <- if (input$select_trim_stratm != "NULL") as.formula(paste0("~", input$select_trim_stratm)) else NULL
    weight_formula <- as.formula(paste0("~", input$select_weight_trim))

    design <- svydesign(
      data = data_input_trim(),
      ids = ids_formula,
      strata = strata_formula,
      weights = weight_formula,
      # nest = input$trim_nest
      nest = nest_flag
    )

    cutoff <- input$trim_upper_factor * mean(weights(design), na.rm = TRUE)

    trimmed_design <- trimWeights(design, upper = cutoff, strict = TRUE)
    new_weights <- as.numeric(weights(trimmed_design))

    df <- data_input_trim() %>%
      mutate(
        .row_id         = dplyr::row_number(),
        original_weight = .data[[input$select_weight_trim]],
        trimmed_weight  = new_weights,
        trim_cutoff     = cutoff,
        was_trimmed     = (original_weight > cutoff) 
      )

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
    })

  output$trimmed_plot <- renderPlot({
    req(trimmed_data())   

    df <- trimmed_data() |> 
      # arrange(desc(original_weight))  |> 
      mutate(order_id = row_number())

    cutoff <- unique(df$trim_cutoff)
    
    # hist(df$weight, main = "Distribution after Trimming", 
    #     xlab = "Weight", col = "skyblue", border = "white")
    
    p1 <- ggplot(df) +
      geom_histogram(aes(x = original_weight), 
                    fill = "skyblue", alpha = 0.5, bins = 30) +
      geom_histogram(aes(x = trimmed_weight), 
                    fill = "orange", alpha = 0.5, bins = 30) +
      geom_vline(xintercept = cutoff, linetype = "dashed", color = "red") +
      labs(title = "Distribution of Weights (Before vs After Trimming)",
          x = "Weight", y = "Count") +
      theme_minimal()

    p2 <- ggplot(df, aes(x = order_id, y = trimmed_weight)) +
      geom_line(color = "grey40") +
      geom_point(aes(color = was_trimmed, shape = was_trimmed), size = 2) +
      scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
      labs(title = paste0("Trimmed Weights (cutoff = ", round(cutoff,2), ")"),
          x = "Units",
          y = "Weight after trimming",
          color = "Was trimmed", shape = "Was trimmed") +
      theme_minimal()

    # p3

    df_sorted <- df |>
      arrange(desc(original_weight)) |>
      mutate(rank_by_orig = row_number())

    cross_idx <- which(df_sorted$original_weight <= cutoff)[1]
    cross_point <- data.frame(
      x = df_sorted$rank_by_orig[cross_idx],
      y = cutoff
)
    p3 <- ggplot(df_sorted, aes(x = rank_by_orig)) +
      geom_line(aes(y = original_weight, color = "Original")) +
      geom_line(aes(y = trimmed_weight,  color = "Trimmed")) +
      geom_hline(yintercept = cutoff, linetype = "dashed", color = "red") +
      geom_point(data = cross_point, aes(x = x, y = y), color = "red", size = 3) +
      geom_text(data = cross_point,
                aes(x = x, y = y, label = paste0("x=", x)),
                vjust = -1, color = "red") +
      scale_color_manual(
        name   = "Series",
        values = c("Original" = "steelblue", "Trimmed" = "green")
      ) +
      labs(title = "Weights Sorted by Original Weight",
          x = "Rank (by original weight)", y = "Weight") +
      theme_minimal()

    library(patchwork)
    p1 | p2 | p3 
  })
}

shinyApp(ui = ui, server = server)


