library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "censusVis",
  sidebar = sidebar(
    helpText(
      "Create demographic maps with information from the 2010 US Census."
    ),
    selectInput(
      "var",
      label = "Choose a variable to display",
      choices = 
        list(
          "Percent White", 
          "Percent Black", 
          "Percent Hispanic", 
          "Percent Asian"
        ),
      selected = "Percent White"
    ),
    sliderInput("range",
                label = "Range of interest:",
                min = 0, max = 100, value = c(0, 100))
  )
)

# Define server logic ----
server <- function(input, output) {

}

# Run the app ----
shinyApp(ui = ui, server = server)