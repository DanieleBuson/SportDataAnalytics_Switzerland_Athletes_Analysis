############################################################
# Shiny App - Athletics Trends (World and Switzerland)
# Autor: Joris Kuger
# Datum: 21.02.2023
#############################################################

library(shiny)
library(shinythemes)
library(shinydashboard)

# source has to be called manually first because it doesn't work if we run app directly.
source("C:/Users/buson/OneDrive/Documenti/R_universita/sportdataanalytics/Script/helper.R", local = T)

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title = "Athletics Result Trends"),

    # Navigation bar
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overview Women", tabName = "OverviewWomen"),
        menuItem("Overview Men", tabName = "OverviewMen"),
        menuItem("100m", tabName = "tab100m"),
        menuItem("200m", tabName = "tab200m"),
        menuItem("400m", tabName = "tab400m"),
        menuItem("Long Jump", tabName = "tablongjump")
      )
    ),
    
    dashboardBody(
      tabItems(
        #first tab content
      tabItem(
        tabName = "OverviewWomen",
        fluidRow(
          box(plotOutput("overviewwomen", height = 700, width = 800), width = 12)
        )
      ),
      tabItem(
        tabName = "OverviewMen",
        fluidRow(
          box(plotOutput("overviewmen", height = 700, width = 800), width = 12)
        )
      ),
      tabItem(
        tabName = "tab100m",
        selectInput("gender100m", "Gender", choices = c("Men", "Women")),
        fluidRow(box(plotlyOutput("p100m"), width = 12)),
        fluidRow(box(tableOutput("t100m"), width = 12))
        ),
      tabItem(
        tabName = "tab200m",
        selectInput("gender200m", "Gender", choices = c("Men", "Women")),
        fluidRow(box(plotlyOutput("p200m"), width = 12)),
        fluidRow(box(tableOutput("t200m"), width = 12))
      ),
      tabItem(
        tabName = "tab400m",
        selectInput("gender400m", "Gender", choices = c("Men", "Women")),
        fluidRow(box(plotlyOutput("p400m"), width = 12)),
        fluidRow(box(tableOutput("t400m"), width = 12))
      ),
      tabItem(
        tabName = "tablongjump",
        selectInput("genderlongjump", "Gender", choices = c("Men", "Women")),
        fluidRow(box(plotlyOutput("plongjump"), width = 12)),
        fluidRow(box(tableOutput("tlongjump"), width = 12))
      )
      )
      )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$overviewmen <- renderPlot({
      linetype <-  c("dotted", "dashed", "solid")
      labels <- c("100m Men", "200m Men", "400m Men", "Long Jump Men")
      color <- c("black", "red", "green")
      
      radarchart(mean_reference_men[2:5],
                 title = "Results 2016 - 2022 Compared to World Record (100%)",
                 axistype = 1,
                 vlabels = labels,
                 vlcex = 1.1,
                 pcol = color,
                 plwd = 3,
                 plty = linetype,
                 caxislabels = c("0%", "25%", "50%", "75%", "100%"),
                 axislabcol = 'grey', 
                 cglwd = 0.6) 
      legend("topright",
             legend = c("Mean Gold Medal Results", "Mean Result of Last Finalist", "Swiss Season Best"),
             bty = "n", pch = 20, lty = linetype, fill = color, col = color,
             text.col = "grey25", pt.cex = 2)
    })
    
    output$overviewwomen <- renderPlot({
      linetype <-  c("dotted", "dashed", "solid")
      labels <- c("100m Women", "200m Women", "400m Women", "Long Jump Women")
      color <- c("black", "red", "green")
      
      radarchart(mean_reference_women[2:5],
                 title = "Results 2016 - 2022 Compared to World Record (100%)",
                 axistype = 1,
                 vlabels = labels,
                 vlcex = 1.1,
                 pcol = color,
                 plwd = 3,
                 plty = linetype,
                 caxislabels = c("0%", "25%", "50%", "75%", "100%"),
                 axislabcol = 'grey', 
                 cglwd = 0.6)
      
      legend("topright",
             legend = c("Mean Gold Medal Results", "Mean Result of Last Finalist", "Swiss Season Best"),
             bty = "n", pch = 20, lty = linetype, fill = color, col = color,
             text.col = "grey25", pt.cex = 2)
    })
    
    
    output$p100m <- renderPlotly({
      getplot100m(gender = input$gender100m)
    })
    output$t100m <- renderTable({
      gettable100m(gender = input$gender100m)
    })
    
    output$p200m <- renderPlotly({
      getplot200m(gender = input$gender200m)
    })
    output$t200m <- renderTable({
      gettable200m(gender = input$gender200m)
    })
    
    output$p400m <- renderPlotly({
      getplot400m(gender = input$gender400m)
    })
    output$t400m <- renderTable({
      gettable400m(gender = input$gender400m)
    })
    
    output$plongjump <- renderPlotly({
      getplotlongjump(gender = input$genderlongjump)
    })
    output$tlongjump <- renderTable({
      gettablelongjump(gender = input$genderlongjump)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
