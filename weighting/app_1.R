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

ui <-
  navbarPage(
    custom_card_header_style,
    theme = "shiny.css",
    title = div(img(src='whitelogo.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 50)),
    windowTitle = "weighting application",
    id = "menus",
  )