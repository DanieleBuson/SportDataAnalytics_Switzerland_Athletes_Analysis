################################################################################
# 
# 
# Name of Application: Development of athletics 
# 
# Date: 18.02.2023
#
# Author: Sports Data Analytics
#    
################################################################################

# load packages
library(readr)
library(shiny)
library(pacman)
library(shinythemes)
pacman::p_load(janitor, dplyr, tidyr, ggplot2, plotly)

# load data
olympics <- read.csv("C:/Users/buson/OneDrive/Documenti/R_universita/sportdataanalytics/Data/athletics.csv")
athletics_master <- olympics
  
inputchoiceClass <- athletics_master %>% filter(!Class == "Under 23") %>% distinct(Class)
inputchoiceDiscipline <- athletics_master %>% filter(Is.Olympic.Discipline == "Yes") %>%  distinct(Discipline)
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(theme = shinytheme("superhero"),
  titlePanel("Development of athletics"),
  
  # Sidebar with a slider input 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "agegroup",
        label = "Age Group",
        choices = inputchoiceClass$Class
      ),
      sliderInput(inputId = "rank",
                  label = "Final Rank",
                  min = 1,
                  max = 8,
                  value = 3),
      selectInput(
        inputId = "discipline",
        label = "Choose Discipline",
        choices = inputchoiceDiscipline$Discipline
      ),
      selectInput(
        "gender",
        "Choose Gender",
        choices = list("Men", "Women")
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  # create dynamical data set
  majoreventsreactive <- reactive({
    req(input$agegroup, input$rank, input$discipline, input$gender)
    majorevents <- athletics_master %>% mutate(Result = as.numeric(Result)) %>% filter(Class %in% input$agegroup, Discipline %in% input$discipline, Year >= 1930, Competition %in% c("World Championships", "Olympic Games","World Junior Championships", "World Youth Championships", "Youth Olympic Games"), Gender %in% input$gender, !Result == "", Rank %in% input$rank) %>% select(Year, Competition, Discipline, Gender, Class, Rank, Person.Team, Person, DoB, Age, Result)
  })
  
  
  # selectInput dynamically
  observe({
    updateSelectInput(session, "agegroup", choices = inputchoiceClass$Class)
    updateSelectInput(session, "discipline", choices = inputchoiceDiscipline$Discipline)
  })
  
  
  # create plot for output
  output$distPlot <- renderPlot({
    ggplot(majoreventsreactive(), aes(Year, Result))+
      geom_point()+
      geom_smooth(method = 'auto', formula = y ~ x) +
      theme(plot.background = element_rect(fill = "#d9cec5"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


