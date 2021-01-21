# Load packages
library(shiny)
library(rstudioapi)
library(tidyverse)
library(lubridate)
library(plyr)
library(choroplethr)
library(choroplethrMaps)
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
  titlePanel("Vitims profile"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Take an insight into the victim geographical distribution."),
      helpText("Please, choose victims' demographics."),
      
      selectInput("gender", 
                  label = "Victim's gender",
                  choices = c("Male and Female", 
                              "Male",
                              "Female"),
                  selected = "All"),
      
      selectInput("race", 
                  label = "Victim's race",
                  choices = c("All races", 
                              "Asian",
                              "Black",
                              "Hispanic",
                              "Native",
                              "White",
                              "Other"),
                  selected = "All"),
      
      sliderInput("range", 
                  label = "Victim's age range:",
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
