# Load packages
library(shiny)
library(rstudioapi)
library(tidyverse)
library(lubridate)
library(plyr)
library(quantmod)
library(maps)

source("mapHelpers.R")
# Work dir 
setwd(dirname(getActiveDocumentContext()$path ))
cat('Working directory:', getwd())

# Load data ----
shootings <- read_csv("data/shootings.csv")

# Data wrangling
shootings = shootings %>% 
  mutate(date = ymd(date)) %>% 
  mutate_at(vars(date), funs(year, month, day))

shootings$week_day <- wday(shootings$date)

shootings[,c('manner_of_death','armed','gender', 'race', 'city', 'state', 'signs_of_mental_illness', 'threat_level', 'flee', 'body_camera', 'arms_category', 'year', 'month', 'week_day')]  <- lapply(shootings[,c('manner_of_death','armed','gender', 'race', 'city', 'state', 'signs_of_mental_illness', 'threat_level', 'flee', 'body_camera', 'arms_category', 'year', 'month', 'week_day')], factor)
shootings$age <- as.integer(shootings$age)



# User interface ----
ui <- fluidPage(
  titlePanel("MapApp"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Victims' demographic maps."),
      
      selectInput("gender", 
                  label = "Choose gender to display",
                  choices = c("All", 
                              "Male",
                              "Female"),
                  selected = "All"),
      
      selectInput("race", 
                  label = "Choose race to display",
                  choices = c("All", 
                              "Asian",
                              "Black",
                              "Hispanic",
                              "Native",
                              "White",
                              "Other"),
                  selected = "All"),
      
      sliderInput("range", 
                  label = "Age range:",
                  min = min(shootings$age), max = max(shootings$age), value = c(min(shootings$age), max(shootings$age)))
    ),
    mainPanel(plotOutput("map"))
  )
)

# Server logic
server <- function(input, output) {
 
  output$map <- renderPlot({
    
    victims_map(shootings, input$gender, input$race, input$range)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
