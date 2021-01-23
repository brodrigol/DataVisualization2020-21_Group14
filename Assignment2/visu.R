# Load packages ----
library(shiny)
library(quantmod)
library(tibble)
library(caret)
library(factoextra)
library(mltools)
library(data.table)
library(FactoMineR)
library(VarSelLCM)
# Source helpers ----


data = read.csv(url("https://raw.githubusercontent.com/anamarine/DataVisualization2020-21_Group14/main/Assignment2/data/shootings.csv"))
data=data[,-1]
year = list()
month = list()
day = list()
for (i in data[,2]){
  l=strsplit(i, split="-")
  year = append(year,l[[1]][1])
  month = append(month,l[[1]][2])
  day = append(day,l[[1]][3])
}
data=add_column(data,year, .after=2)
data=add_column(data,month, .after=2)
data=add_column(data,day, .after=2)
data=data[,-(1:2)]

data$age=as.integer(data$age)
data$year=as.integer(data$year)
data$month=as.integer(data$month)
data$day=as.integer(data$day)

cols=colnames(data)
cols=cols[-(1:3)]
cols=cols[-3]

for (i in cols){
  data[,i]=as.factor(data[,i])
}


ui <- fluidPage(
  titlePanel("Big Data Visualization Project"),
  headerPanel(h1("Principal Component Analysis Clustering")),
  sidebarPanel(
    sliderInput('slider1',"Number of clusters:", min =1, max =10, value=2),
    selectInput('score','Select scoring method:',choices = list("BIC"="BIC","AIC"="AIC","MICL"="MICL"), selected="BIC")
  ),
  mainPanel(
    h2("Clustering"),
    tabsetPanel(
      
      tabPanel("Table View",tableOutput("table")),
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
      tabPanel("Coefficients",verbatimTextOutput("coeff"))
    ))
)

server <- function(input,output) {
  
  output$table <- renderTable({
    data
  })
  output$summary <- renderPrint({
    cluster <- VarSelCluster(data, input$slider1, nbcores = 2, initModel=40, crit.varsel = input$score)
    print(cluster)
  })
  output$discriminative<-renderPlot(
    {
      cluster <- VarSelCluster(data, input$slider1, nbcores = 2, initModel=40, crit.varsel = input$score)
      plot(cluster)
    })
  output$distribution <- renderPlot({
    cluster <- VarSelCluster(data, input$slider1, nbcores = 2, initModel=40, crit.varsel = input$score)
    plot(cluster,y=input$class)
  })
  output$coeff <- renderPrint({
    cluster <- VarSelCluster(data, input$slider1, nbcores = 2, initModel=40, crit.varsel = input$score)
    coef(cluster)
  })
}

shinyApp(ui, server)


  