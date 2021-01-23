# Load packages
library(shiny)
library(rstudioapi)
library(tidyverse)
library(stringr) 
library(lubridate)
library(maps)
library(plyr)
library(choroplethr)
library(choroplethrMaps)
library(plotly)
library(quantmod)


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


g <- list(scope = 'usa',
          projection = list(type = 'albers usa'),
          showland = TRUE,
          showlakes= TRUE,
          landcolor = toRGB("gray85"),
          subunitwidth = 1,
          countrywidth = 1,
          subunitcolor = toRGB("white"),
          countrycolor = toRGB("white"),
          lakecolor = toRGB('white'))
# User interface ----
ui <- fluidPage(
  titlePanel("Geographical exploration"),
  sidebarLayout(
    sidebarPanel(
      h4("US city shootings"),
      helpText("Shooting distribution by cities"),
      helpText("Please, choose the period of time you want to see."),
      sliderInput("years", 
                  label = "Year range:",
                  min = min(2014 + as.integer(shootings$year)), 
                  max = max(2014 + as.integer(shootings$year)), 
                  value = c(min(2014 + as.integer(shootings$year)), max(2014 + as.integer(shootings$year))),
                  sep = "")
    ),
    mainPanel(plotlyOutput("cit_map"))
  ),
  sidebarLayout(
    sidebarPanel(
      h4("Vitims profile"),
      helpText("Take an insight into the geographical distribution of victims with particular demographic features."),
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
    mainPanel(plotlyOutput("vict_map"))
  )
)

# Server logic
server <- function(input, output, session) {

  output$cit_map <- renderPlotly({
    data <- cities_map(shootings, input$years)
    fig <- plot_geo(data, locationmode = 'USA-states', sizes = c(1, 250))
    fig <- fig %>% add_markers(
      x = ~long, y = ~lat, size = ~freq, hoverinfo = "text", 
      text = ~paste(data$city_state, "<br />", data$freq, " shootings")
    )
    fig <- fig %>% layout(title = paste0('US city shootings between', input$years[1], " and ", input$years[2]), geo = g)
    
  })
  
  output$vict_map <- renderPlotly({
   data <- victims_map(shootings, input$gender, input$race, input$range)
   fig <- plot_geo(data, locationmode = 'USA-states', sizes = c(1, 250))
   fig <- fig %>% add_trace(z = ~value, text = ~hover, locations = ~region, color = ~value, colors = 'Blues')
   fig <- fig %>% colorbar(title = "% of victims")
   fig <- fig %>% layout(title = paste0("US police shootings 2015-2020, % with victim's profile:\n", input$race, ", ", input$gender, ", age between ", input$range[1], " and ", input$range[2]), geo = g)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
