library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Lesson 2 - Shiny tutorial"),
  
  sidebarLayout(position = "right",
                sidebarPanel(
                  h3(strong("Data visualization")), align = "right", 
                  h4("Group 14", align = "right"),
                  img(src = "ESCUDO_UPM.png", height = 300, width = 200)
                  ),
                mainPanel(
                  p("This is an example of a Shiny app"),
                  p("you can launch", span("this app", style = "color:red"), "from your console using the following", em("command:")),
                  code("runApp(\"app-1\")"),
                  br(),
                  br(),
                  img(src = "Rstudio.jpg", height = 400)
                )
  )
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)