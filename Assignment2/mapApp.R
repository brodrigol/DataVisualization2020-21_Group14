# Load packages
library(shiny)
library(rstudioapi)
library(tidyverse)
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


g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

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
      h4("Seasonality"),
      helpText("Does shooting location have a seasonal component? We can compare the specific seasons bteween 2015 and 2020"),
      helpText("Please, choose the period of time you want to see."),
      
      selectInput("seasonality", 
                  label = "Select seasonality",
                  choices = c("Month",
                              "Year quarter",
                              "Season",
                              "Day of the week"),
                  selected = "Month"),
      
      uiOutput("filter")
      
    ),
    mainPanel(plotOutput("seas_map"))
  ),
  sidebarLayout(
    sidebarPanel(
      h4("Vitims profile"),
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
    mainPanel(plotOutput("vict_map"))
  )
)

# Server logic
server <- function(input, output, session) {
  output$filter <- renderUI({
    if(input$seasonality == "Month"){
      selectInput("filter", 
                  label = "Filter",
                  choices = month.name,
                  selected = "January")
    }
    else if (input$seasonality == "Year quarter"){
      selectInput("filter", 
                  label = "Filter",
                  choices = c("Q1: Jan, Feb, Mar", 
                              "Q2: Apr, May, Jun",
                              "Q3: Jul, Aug, Sep",
                              "Q4: Oct, Nov, Dec"),
                  selected = "Q1: Jan, Feb, Mar")
    }
    else if (input$seasonality == "Season"){
      selectInput("filter", 
                  label = "Filter",
                  choices = c("Spring: Mar, Apr, May", 
                              "Summer: Jun, Jul, Aug",
                              "Autumn: Sep, Oct, Nov",
                              "Winter: Dec, Jan, Feb"),
                  selected = "Spring: Mar, Apr, May ")
    }
    else {
      selectInput("filter", 
                  label = "Filter",
                  choices = c("Week days",
                              "Weekend",
                              "Monday", 
                              "Tuesday",
                              "Wednesday",
                              "Thursday",
                              "Friday", 
                              "Saturday",
                              "Sunday"),
                  selected = "Week days: Mon, Tue, Wed, Thu, Fri")
    }
    
  }) 
  
  output$cit_map <- renderPlotly({
    data <- cities_map(shootings, input$years)
    fig <- plot_geo(data, locationmode = 'USA-states', sizes = c(1, 250))
    fig <- fig %>% add_markers(
      x = ~long, y = ~lat, size = ~freq, hoverinfo = "text",
      text = ~paste(data$city_state, "<br />", data$freq, " shootings")
    )
    fig <- fig %>% layout(title = 'US city shootings<br>(Click legend to toggle)', geo = g)
    
  })
  
  output$vict_map <- renderPlot({
    victims_map(shootings, input$gender, input$race, input$range)
  })
  
  output$seas_map <- renderPlot({
    season_map(shootings, input$seasonality, input$filter)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
