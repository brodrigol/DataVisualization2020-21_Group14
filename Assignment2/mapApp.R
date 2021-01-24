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
library(hrbrthemes)
library(viridis)
library(shinyjqui)
library(treemap)
library(d3treeR)
library(rsconnect)
library(VarSelLCM)
library(tibble)


rsconnect::setAccountInfo(name='upmbigdatagroup14',
                          token='B234121BC0C86FB9E950DFAA00A10BBF',
                          secret='W/i9wi+kRcqg2ecGOREOb9OZWctuyLv40kvFlB4D')

source("mapHelpers.R")
# Work dir 
# setwd(dirname(getActiveDocumentContext()$path ))
# cat('Working directory:', getwd())

# Load data ----
shootings <- read_csv("data/shootings.csv")
# Data wrangling
shootings = shootings %>% 
  mutate(date = ymd(date)) %>% 
  mutate_at(vars(date), funs(year, month, day))

shootings$week_day <- wday(shootings$date)


shootings[,c('manner_of_death','armed','gender', 'race', 'city', 'state', 'signs_of_mental_illness', 'threat_level', 'flee', 'body_camera', 'arms_category', 'year', 'month', 'week_day')]  <- lapply(shootings[,c('manner_of_death','armed','gender', 'race', 'city', 'state', 'signs_of_mental_illness', 'threat_level', 'flee', 'body_camera', 'arms_category', 'year', 'month', 'week_day')], factor)
shootings$age <- as.integer(shootings$age)

# Map config
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

corr_option <- shootings %>% select(where(is.factor)) %>% names()
corr_vars <- c(corr_option, 'age')
corr_grid <- c(corr_option, '')

time_option <- c('day', 'week_day', 'month', 'year', 'year_month')
ts_vars <- corr_option[!(corr_option %in% time_option)]

data_cluster<-shootings[,-(1:3)]

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
  ),
  titlePanel("Correlation among variables"),
  sidebarLayout(
    sidebarPanel(
      helpText("Please, choose the variables to analyze"),
      selectInput("x1", 
                  label = "X variable",
                  choices = corr_vars,
                  selected = 'Age'),
      selectInput("y1", 
                  label = "Y variable",
                  choices = corr_vars,
                  selected = 'gender'),
      selectInput("g2", 
                  label = "X grid",
                  choices = corr_grid,
                  selected = ""),
      selectInput("g1", 
                  label = "Y grid",
                  choices = corr_grid,
                  selected = ""),
    ),
    mainPanel(plotlyOutput("corr_plot"))
  ),
  sidebarLayout(
    sidebarPanel(
      helpText("Please, choose the variables for analisys"),
      actionButton("plot_tree", "Plot treemap"),
      br(),
      orderInput('treeS', 'Selected', items = NULL, connect = 'treeV',placeholder = 'Drag items here...'),

      orderInput('treeV', 'Variables', corr_option, connect = 'treeS')
      
      
    ),
    mainPanel(d3tree2Output("tree_plot"))
  ),
  titlePanel("Time series"),
  sidebarLayout(
    sidebarPanel(
      helpText("Please, choose the variables for analisys"),
      selectInput("ts_time", 
                  label = "Time period",
                  choices = time_option,
                  selected = 'year'),
      selectInput("ts_var", 
                  label = "Group variable",
                  choices = ts_vars,
                  selected = 'gender'),

    ),
    mainPanel(plotlyOutput("ts_plot"))
  ),
  titlePanel("Clustering"),
  sidebarLayout(
    sidebarPanel(
      sliderInput('slider1',"Number of clusters:", min =1, max =10, value=4),
      selectInput('score','Select scoring method:',choices = list("BIC"="BIC","AIC"="AIC","MICL"="MICL"), selected="BIC"),
      checkboxInput('varSel',"Automatic variable selection", value=TRUE)
    ),
  mainPanel(
    tabsetPanel(
      tabPanel("Summary",verbatimTextOutput("summary")),
      tabPanel("Discriminative power analysis",plotOutput("discriminative")),
      tabPanel("Distribution per class",
               selectInput('class','Select a class:',choices = list("Day"="day","Month"="month","Year"="year",
                                                                    "Manner of death"="manner_of_death","Armed"="armed",
                                                                    "Age"="age","Gender"="gender","Race"="race","City"="city",
                                                                    "State"="state","Signs of mental illness"="signs_of_mental_illness",
                                                                    "Threat level"="threat_level","Flee"="flee",
                                                                    "Body camera"="body_camera","Category of the arms"="arms_category"), selected="Race")
               ,plotOutput("distribution")),
      tabPanel("Coefficients of categorical variables",verbatimTextOutput("coeffcat")),
      tabPanel("Coefficients of quantitative variables",verbatimTextOutput("coeffcount"))
    ))
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
  output$corr_plot <-
    renderPlotly({GenerateHeatmap(shootings, input$x1, input$y1, input$g1, input$g2)})
  
  observeEvent(input$plot_tree, {
    vect <- isolate(input$treeS_order)
    output$tree_plot <-
      renderD3tree2({
        GenerateTreemap(shootings,vect)
      })
  } , ignoreInit = TRUE)
  
  output$ts_plot <-
    renderPlotly({GenerateTimeLine(shootings, input$ts_time, input$ts_var)})
  
  cluster=reactive({
    VarSelCluster(data_cluster, input$slider1, vbleSelec = input$varSel, nbcores = 2, initModel=40, crit.varsel = input$score)
  })
  output$summary <- renderPrint({
    print(cluster())
  })
  output$discriminative<-renderPlot(
    {
      plot(cluster())
    })
  output$distribution <- renderPlot({
    plot(cluster(),y=input$class)
  })
  output$coeffcat <- renderPrint({
    coef(cluster())@paramCategorical@alpha
  })
  output$coeffcount <- renderPrint({
    coef(cluster())@paramInteger@lambda
  })
}

# Run the app
shinyApp(ui = ui, server = server)
