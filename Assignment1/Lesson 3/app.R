library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Lesson 3 - CensusVis"),
  
  sidebarLayout(position = "left",
                fluidRow(
                sidebarPanel(
                  helpText("Create demographic maps with information from the 2010 US census"),
                  selectInput("select1", 
                                    h3(strong("Choose a variable to display")), 
                                    choices = list("Percent White" = 1, 
                                                   "Percent Latin" = 2, 
                                                  "Percent Black" = 3),
                                    selected = 1),
                    br(),
                    sliderInput("slider1", 
                                label = h3(strong("Range of interest")),
                                min = 0, max = 100, value = c(0, 100))
                )
                ),
                  
                mainPanel(
                  
                )
                
              )
  )


# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)