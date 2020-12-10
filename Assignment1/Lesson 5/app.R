library(shiny)
library(maps)
library(mapproj)
source("/Users/Anita/Documents/ANITA/HMDA/1stSemester/Big data/DataVisualization2020-21_Group14/Assignment1/Lesson 5/helpers.R")
counties <- readRDS("/Users/Anita/Documents/ANITA/HMDA/1stSemester/Big data/DataVisualization2020-21_Group14/Assignment1/Lesson 5/data/counties.rds")


# Define UI ----
ui <- fluidPage(
  
  titlePanel("Lesson 3 - CensusVis"),
  
  sidebarLayout( position = "left",
                
                sidebarPanel(
                  helpText("Create demographic maps with information from the 
                           2010 US Census."),
                  
                  selectInput("var", 
                              label = "Choose a variable to display",
                              choices = c("Percent White", 
                                          "Percent Black",
                                          "Percent Hispanic", 
                                          "Percent Asian"),
                              selected = "Percent White"),
                  
                  sliderInput("range", 
                              label = "Range of interest:",
                              min = 0, max = 100, value = c(0, 100))
                ),
                
                mainPanel(plotOutput("map"))
                
              )
  )


# Define server logic ----
server <- function(input, output) {
  
  output$map <- renderPlot({
    data <- switch(input$var, 
                   "Percent White" = counties$white,
                   "Percent Black" = counties$black,
                   "Percent Hispanic" = counties$hispanic,
                   "Percent Asian" = counties$asian)
    
    color <- switch(input$var, 
                    "Percent White" = "darkgreen",
                    "Percent Black" = "blue",
                    "Percent Hispanic" = "red",
                    "Percent Asian" = "orange")
    
    legend <- switch(input$var, 
                     "Percent White" = "% White",
                     "Percent Black" = "% Black",
                     "Percent Hispanic" = "% Hispanic",
                     "Percent Asian" = "% Asian")
    
    percent_map(data, color, legend, input$range[1], input$range[2])
  })
  
}

# Run the app ----
shinyApp(ui, server)