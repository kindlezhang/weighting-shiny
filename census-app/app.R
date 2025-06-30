
library(bslib)
library(shiny)
library(maps)
library(mapproj)

counties <- readRDS("data/counties.rds")

source("helpers.R")

ui <- page_sidebar(
  title = "censusVis",

  sidebar = sidebar(
    helpText(
      "Create demographic maps with information from the 2010 US Census."
    ),
    selectInput(
      "var",
      label = "Choose a variable to display",
      choices = c("Percent White",
                  "Percent Black",
                  "Percent Hispanic",
                  "Percent Asian"),
      selected = "Percent White"
    ),
    sliderInput(
      "range",
      label = "Range of interest:",
      min = 0, max = 100, value = c(0, 100)
    )
  ),
  textOutput("selected_var"),
  textOutput("min_max"),
  card(plotOutput("map"))
)

server <- function(input, output) {
  
  output$selected_var <- renderText({
    paste("You have selected", input$var)
  })
  
  output$min_max <- renderText({
    paste("You have chosen a range that goes from",
          input$range[1], "to", input$range[2])
  })

  output$map <- renderPlot({
    args <- switch(input$var,
      "Percent White" = list(counties$white, "darkgreen", "% White"),
      "Percent Black" = list(counties$black, "black", "% Black"),
      "Percent Hispanic" = list(counties$hispanic, "darkorange", "% Hispanic"),
      "Percent Asian" = list(counties$asian, "darkviolet", "% Asian"))

    args$min <- input$range[1]
    args$max <- input$range[2]

    do.call(percent_map, args)
  })
  
}

shinyApp(ui = ui, server = server)