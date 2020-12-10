library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Lesson 3 - CensusVis"),
  
  sidebarLayout(position = "left",
                sidebarPanel(
                  
                  helpText("Create demographic maps with information from the 2010 US census"),
                  
                  selectInput("select1", 
                                    h3(strong("Choose a variable to display")), 
                                    choices = list("Percent White", 
                                                   "Percent Latin", 
                                                  "Percent Black"),
                                    selected = 1),
                    br(),
                    sliderInput("slider1", 
                                label = h3(strong("Range of interest")),
                                min = 0, max = 100, value = c(0, 100))
                ),
                  
                mainPanel(
                  textOutput("selected_var"),
                  textOutput("slider_var")
                )
                
              )
  )


# Define server logic ----
server <- function(input, output) {
  
  output$selected_var <- renderText ({
    paste("You have selected ", input$select1)
  })
  output$slider_var <- renderText ({
    paste("You have selected a rage that goes from ", input$slider1[1], " to ", input$slider1[2])
  })
  
}

# Run the app ----
shinyApp(ui, server)