#UI code for AIGHD Moves - shiny app
library(shiny)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(leafem)

distances_orig <- read.csv("Distances.csv",header = T,stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel(h1("AIGHD Moves!",align = "center")),
  splitLayout(leafletOutput("map"),
              plotOutput(outputId = "bar")),
  verticalLayout(textOutput(outputId = "summary"),
                 selectInput(inputId = "select_participants",
                             label = "Participants",
                             choices = unique(distances_orig$Name),
                             multiple = TRUE,
                             selectize = TRUE))
  )