library(shiny)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(fmsb)
source("C:/Users/buson/OneDrive/Documenti/R_universita/sportdataanalytics/Script/sport_data_analytics.R")


ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Athlete Performance"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Men 100m", tabName = "athlete100mM", icon = icon("person-running")),
      menuItem("Women 100m", tabName = "athlete100mW", icon = icon("person-running")),
      menuItem("Men 200m", tabName = "athlete200mM", icon = icon("person-running")),
      menuItem("Women 200m", tabName = "athlete200mW", icon = icon("person-running")),
      menuItem("Men 400m", tabName = "athlete400mM", icon = icon("person-running")),
      menuItem("Women 400m", tabName = "athlete400mW", icon = icon("person-running")),
      menuItem("Long Jump Men", tabName = "athleteLongJumpM", icon = icon("person-running")),
      menuItem("Long Jump Women", tabName = "athleteLongJumpW", icon = icon("person-running"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem("athlete100mM",
              fluidRow(
                box(titlePanel(h1("Men 100 m")),
                    div(h4("The discipline"),
                        p("100m is one the most famous and prestigious disciplines in the sport of athletics. 
                        We are considering only runners that have participated at least three times in the Olympic games or in the World Championships."),
                        p("We are interested in Swiss athletes, comparing them with the rest of the world.
                          In addition, they are divided into classes, in particular, we are analysing “Seniors”, “Juniors” (not a class in the Olympics) and “Youth” classes."),
                        p("The purpose is to make the Federation aware of the current situation in this discipline, providing tools, the graphs, that can be used to make strategic decisions.
                        ")), width = 12)
              ),
              fluidRow(box(plotlyOutput("difference_plot_M"), 
                           div(h3("HOW TO READ IT"),
                               p("The red line is the conjunction of all the results 
                               of the selected athlete (it is possible to select it in the box on the right). On the x-axis there are the years,
                                 on the y-axis there is the result time. For each measurement 
                                 it is possible to see the result directly on the graph. "),
                               p("The blue band is the range in which an athlete has to 
                                 finish in order to have a high probability to receive a 
                                 medal or at least a certificate."),
                               p("To summarise, for each year the red dot is the result 
                                 of the selected athlete in a specific class. On the same 
                                 vertical line there is the blue band. The upper bound of this 
                                 band is the best result of a person that had finished the competition
                                 in the 8th place, while the lower bound is the best result (considering 
                                 data from 1990) of a person that had won.")),
                           width = 7),
                box(
                  selectInput("athlete100mMen", "Athlete: ", 
                              c("Silvan Wicki", "Alex Wilson", "Reto Schenkel", 
                                "Pascal Mancini", "Sylvain Chuard")), 
                  tableOutput("athlete100mM_table"),
                  width = 5
                )),
              fluidRow(box(plotOutput("spidernet100mMen"), 
                           div(h3("HOW TO READ IT"),
                           p("In this representation we want to understand how 
                             the selected athlete is close to the world record and 
                             from the swiss record in a specific class. The 0% is 
                             represented by a fictious value that could be considered 
                             as the worst possible result in a 100m run with professional 
                             athletes. In this case we decided that the baseline had to be 
                             11.19s. The three light blue dots represent it. The 100% is the 
                             best result in each class from 1990 and it is represented by the 
                             three blue vertices. Inside the figure, it is possible to spot a 
                             dashed line in brown. These vertices of the brown triangle are the 
                             Swiss top result. Lastly, the red line is the performance of the selected athlete. "),
                           p("It is important to understand that, sometimes, athletes with
                             good final results have 0% in one or two classes. This is
                             not due to the fact that they were extremely bad at one 
                             point of their career. In fact, we can explain this problem 
                             with the lack of records. It could be that the athlete gained 
                             the chance to participate in one of the major competitions 
                             in a Senior age or that the athlete is still in Youth/Junior 
                             age and he has no records in the Senior class. ")),
                           width = 6),
                       box(plotlyOutput("difference_histogram_M"), 
                           div(h3("HOW TO READ IT"),
                               p("The graph is representing the frequency of top swiss athletes from 
                           the best result of a runner that finished his competition in the 8th place 
                           in a specific class. The black bars are the frequency of the selected athlete in a certain difference. 
                              This graph has two purposes: first of all to make clear how far 
                             is the selected athlete from the minimum result to have a high 
                             probability of receiving a certificate. In second place, it lets the 
                             reader see how far are on average the Swiss athletes from the best 8th 
                             place runner, and, as a consequence, from the certificate."),
                               p("In this case the more the black bars are on the left, the better the result of the athlete.")),
                           width = 6
                       ))
        
      ),
      tabItem("athlete100mW",
              fluidRow(box(titlePanel(h1("Women 100 m")),
                        div(h4("The discipline"),
                            p("100m is one the most famous and prestigious disciplines in the sport of athletics. 
                        We are considering only runners that have participated at least three times in the Olympic games or in the World Championships."),
                            p("We are interested in Swiss athletes, comparing them with the rest of the world.
                          In addition, they are divided into classes, in particular, we are analysing “Seniors”, “Juniors” (not a class in the Olympics) and “Youth” classes."),
                            p("The purpose is to make the Federation aware of the current situation in this discipline, providing tools, the graphs, that can be used to make strategic decisions.
                        ")), width = 12)
              ),
              fluidRow(box(plotlyOutput("difference_plot_W"), 
                           div(h3("HOW TO READ IT"),
                               p("The red line is the conjunction of all the results 
                               of the selected athlete (it is possible to select it in the box on the right). On the x-axis there are the years,
                                 on the y-axis there is the result time. For each measurement 
                                 it is possible to see the result directly on the graph. "),
                               p("The blue band is the range in which an athlete has to 
                                 finish in order to have a high probability to receive a 
                                 medal or at least a certificate."),
                               p("To summarise, for each year the red dot is the result 
                                 of the selected athlete in a specific class. On the same 
                                 vertical line there is the blue band. The upper bound of this 
                                 band is the best result of a person that had finished the competition
                                 in the 8th place, while the lower bound is the best result (considering 
                                 data from 1990) of a person that had won.")),
                           width = 7),
                       box(
                         selectInput("athlete100mWomen", "Athlete: ", 
                                     c("Marisa Lavanchy", "Mujinga Kambundji", "Samantha Dagry", 
                                       "Ajla Del Ponte", "Sarah Atcho", "Salomé Kora",
                                       "Judith Goll", "Cynthia Reinle")), 
                         tableOutput("athlete100mW_table"),
                         width = 5
                       )),
              fluidRow(box(plotOutput("spidernet100mWomen"), 
                           div(h3("HOW TO READ IT"),
                           p("In this representation we want to understand how 
                             the selected athlete is close to the world record and 
                             from the swiss record in a specific class. The 0% is 
                             represented by a fictious value that could be considered 
                             as the worst possible result in a 100m run with professional 
                             athletes. In this case we decided that the baseline had to be 
                             12.43s. The three light blue dots represent it. The 100% is the 
                             best result in each class from 1990 and it is represented by the 
                             three blue vertices. Inside the figure, it is possible to spot a 
                             dashed line in brown. These vertices of the brown triangle are the 
                             Swiss top result. Lastly, the red line is the performance of the selected athlete. "),
                           p("It is important to understand that, sometimes, athletes with
                             good final results have 0% in one or two classes. This is
                             not due to the fact that they were extremely bad at one 
                             point of their career. In fact, we can explain this problem 
                             with the lack of records. It could be that the athlete gained 
                             the chance to participate in one of the major competitions 
                             in a Senior age or that the athlete is still in Youth/Junior 
                             age and he has no records in the Senior class. ")),
                           width = 6),
                       box(plotlyOutput("difference_histogram_W"), 
                           div(h3("HOW TO READ IT"),
                           p("The graph is representing the frequency of top swiss athletes from 
                           the best result of a runner that finished his competition in the 8th place 
                           in a specific class. The black bars are the frequency of the selected athlete in a certain difference. 
                              This graph has two purposes: first of all to make clear how far 
                             is the selected athlete from the minimum result to have a high 
                             probability of receiving a certificate. In second place, it lets the 
                             reader see how far are on average the Swiss athletes from the best 8th 
                             place runner, and, as a consequence, from the certificate."),
                           p("In this case the more the black bars are on the left, the better the result of the athlete.")),
                         width = 6
                       ))
              
      ),
      tabItem("athlete200mM",
              fluidRow(box(titlePanel(h1("Men 200 m")),
                    div(h4("The discipline"),
                        p("The 200 metres, or 200-meter dash, is a sprint running event. On an outdoor 400 metre racetrack, the race begins on the curve and ends on the home straight, so a combination of techniques is needed to successfully run the race.
                        We are considering only runners that have participated at least three times in the Olympic games or in the World Championships."),
                        p("We are interested in Swiss athletes, comparing them with the rest of the world.
                          In addition, they are divided into classes, in particular, we are analysing “Seniors”, “Juniors” (not a class in the Olympics) and “Youth” classes."),
                        p("The purpose is to make the Federation aware of the current situation in this discipline, providing tools, the graphs, that can be used to make strategic decisions.
                        ")), width = 12)
              ),
              fluidRow(box(plotlyOutput("difference_plot_200M"),
                           div(h3("HOW TO READ IT"),
                               p("The red line is the conjunction of all the results 
                               of the selected athlete (it is possible to select it in the box on the right). On the x-axis there are the years,
                                 on the y-axis there is the result time. For each measurement 
                                 it is possible to see the result directly on the graph. "),
                               p("The blue band is the range in which an athlete has to 
                                 finish in order to have a high probability to receive a 
                                 medal or at least a certificate."),
                               p("To summarise, for each year the red dot is the result 
                                 of the selected athlete in a specific class. On the same 
                                 vertical line there is the blue band. The upper bound of this 
                                 band is the best result of a person that had finished the competition
                                 in the 8th place, while the lower bound is the best result (considering 
                                 data from 1990) of a person that had won.")),
                           width = 7),
                       box(
                         selectInput("athlete200mMen", "Athlete: ", 
                                     c("Alex Wilson", "Felix Svensson", "Marc Schneeberger", 
                                       "Reto Schenkel", "Marco Cribari", "Silvan Wicki",
                                       "William Reais")), 
                         tableOutput("athlete200mM_table"),
                         width = 5
                       )),
              fluidRow(box(plotOutput("spidernet200mMen"), 
                           div(h3("HOW TO READ IT"),
                               p("In this representation we want to understand how 
                             the selected athlete is close to the world record and 
                             from the swiss record in a specific class. The 0% is 
                             represented by a fictious value that could be considered 
                             as the worst possible result in a 200m run with professional 
                             athletes. In this case we decided that the baseline had to be 
                             22.97s. The three light blue dots represent it. The 100% is the 
                             best result in each class from 1990 and it is represented by the 
                             three blue vertices. Inside the figure, it is possible to spot a 
                             dashed line in brown. These vertices of the brown triangle are the 
                             Swiss top result. Lastly, the red line is the performance of the selected athlete. "),
                               p("It is important to understand that, sometimes, athletes with
                             good final results have 0% in one or two classes. This is
                             not due to the fact that they were extremely bad at one 
                             point of their career. In fact, we can explain this problem 
                             with the lack of records. It could be that the athlete gained 
                             the chance to participate in one of the major competitions 
                             in a Senior age or that the athlete is still in Youth/Junior 
                             age and he has no records in the Senior class. ")),
                           width = 6),
                       box(plotlyOutput("difference_histogram_200mM"),
                           div(h3("HOW TO READ IT"),
                               p("The graph is representing the frequency of top swiss athletes from 
                           the best result of a runner that finished his competition in the 8th place 
                           in a specific class. The black bars are the frequency of the selected athlete in a certain difference. 
                              This graph has two purposes: first of all to make clear how far 
                             is the selected athlete from the minimum result to have a high 
                             probability of receiving a certificate. In second place, it lets the 
                             reader see how far are on average the Swiss athletes from the best 8th 
                             place runner, and, as a consequence, from the certificate."),
                               p("In this case the more the black bars are on the left, the better the result of the athlete.")),
                           width = 6
                       ))
      ),
      tabItem("athlete200mW",
              fluidRow(box(titlePanel(h1("Women 200 m")),
                    div(h4("The discipline"),
                        p("The 200 metres, or 200-meter dash, is a sprint running event. On an outdoor 400 metre racetrack, the race begins on the curve and ends on the home straight, so a combination of techniques is needed to successfully run the race. 
                        We are considering only runners that have participated at least three times in the Olympic games or in the World Championships."),
                        p("We are interested in Swiss athletes, comparing them with the rest of the world.
                          In addition, they are divided into classes, in particular, we are analysing “Seniors”, “Juniors” (not a class in the Olympics) and “Youth” classes."),
                        p("The purpose is to make the Federation aware of the current situation in this discipline, providing tools, the graphs, that can be used to make strategic decisions.")),
                    width = 12)
              ),
              fluidRow(box(plotlyOutput("difference_plot_200W"), 
                           div(h3("HOW TO READ IT"),
                               p("The red line is the conjunction of all the results 
                               of the selected athlete (it is possible to select it in the box on the right). On the x-axis there are the years,
                                 on the y-axis there is the result time. For each measurement 
                                 it is possible to see the result directly on the graph. "),
                               p("The blue band is the range in which an athlete has to 
                                 finish in order to have a high probability to receive a 
                                 medal or at least a certificate."),
                               p("To summarise, for each year the red dot is the result 
                                 of the selected athlete in a specific class. On the same 
                                 vertical line there is the blue band. The upper bound of this 
                                 band is the best result of a person that had finished the competition
                                 in the 8th place, while the lower bound is the best result (considering 
                                 data from 1990) of a person that had won.")),
                           width = 7),
                       box(
                         selectInput("athlete200mWomen", "Athlete: ", 
                                     c("Valentine Arrieta", "Jacqueline Gasser", "Mujinga Kambundji", 
                                       "Sarah Atcho", "Lea Sprunger", "Cornelia Halbheer",
                                       "Anika Krone", "Nadja Zurlinden")), 
                         tableOutput("athlete200mW_table"),
                         width = 5
                       )),
              fluidRow(box(plotOutput("spidernet200mWomen"), 
                           div(h3("HOW TO READ IT"),
                               p("In this representation we want to understand how 
                             the selected athlete is close to the world record and 
                             from the swiss record in a specific class. The 0% is 
                             represented by a fictious value that could be considered 
                             as the worst possible result in a 200m run with professional 
                             athletes. In this case we decided that the baseline had to be 
                             25.74s. The three light blue dots represent it. The 100% is the 
                             best result in each class from 1990 and it is represented by the 
                             three blue vertices. Inside the figure, it is possible to spot a 
                             dashed line in brown. These vertices of the brown triangle are the 
                             Swiss top result. Lastly, the red line is the performance of the selected athlete. "),
                               p("It is important to understand that, sometimes, athletes with
                             good final results have 0% in one or two classes. This is
                             not due to the fact that they were extremely bad at one 
                             point of their career. In fact, we can explain this problem 
                             with the lack of records. It could be that the athlete gained 
                             the chance to participate in one of the major competitions 
                             in a Senior age or that the athlete is still in Youth/Junior 
                             age and he has no records in the Senior class. ")),
                           width = 6),
                       box(plotlyOutput("difference_histogram_200mW"), 
                           div(h3("HOW TO READ IT"),
                               p("The graph is representing the frequency of top swiss athletes from 
                           the best result of a runner that finished his competition in the 8th place 
                           in a specific class. The black bars are the frequency of the selected athlete in a certain difference. 
                              This graph has two purposes: first of all to make clear how far 
                             is the selected athlete from the minimum result to have a high 
                             probability of receiving a certificate. In second place, it lets the 
                             reader see how far are on average the Swiss athletes from the best 8th 
                             place runner, and, as a consequence, from the certificate."),
                               p("In this case the more the black bars are on the left, the better the result of the athlete.")),
                           width = 6
                       ))
      ),
      tabItem("athlete400mM",
              fluidRow(box(titlePanel(h1("Men 400 m")),
                           div(h4("The discipline"),
                               p("The 400 metres, or 400-meter dash, is a sprint event in track and field competitions. It has been featured in the athletics programme at the Summer Olympics since 1896 for men and since 1964 for women. On a standard outdoor running track, it is one lap around the track. 
                        We are considering only runners that have participated at least three times in the Olympic games or in the World Championships."),
                               p("We are interested in Swiss athletes, comparing them with the rest of the world.
                          In addition, they are divided into classes, in particular, we are analysing “Seniors”, “Juniors” (not a class in the Olympics) and “Youth” classes."),
                               p("The purpose is to make the Federation aware of the current situation in this discipline, providing tools, the graphs, that can be used to make strategic decisions.")), 
                           width = 12)
              ),
              fluidRow(box(plotlyOutput("difference_plot_400M"), 
                           div(h3("HOW TO READ IT"),
                               p("The red line is the conjunction of all the results 
                               of the selected athlete (it is possible to select it in the box on the right). On the x-axis there are the years,
                                 on the y-axis there is the result time. For each measurement 
                                 it is possible to see the result directly on the graph. "),
                               p("The blue band is the range in which an athlete has to 
                                 finish in order to have a high probability to receive a 
                                 medal or at least a certificate."),
                               p("To summarise, for each year the red dot is the result 
                                 of the selected athlete in a specific class. On the same 
                                 vertical line there is the blue band. The upper bound of this 
                                 band is the best result of a person that had finished the competition
                                 in the 8th place, while the lower bound is the best result (considering 
                                 data from 1990) of a person that had won.")),
                           width = 7),
                       box(
                         selectInput("athlete400mMen", "Athlete: ", 
                                     c("Charles Devantay", "Ricky Petrucciani", "Lionel Spitz")),
                         tableOutput("athlete400mM_table"),
                         width = 5
                       )),
              fluidRow(box(plotOutput("spidernet400mMen"), 
                           div(h3("HOW TO READ IT"),
                               p("In this representation we want to understand how 
                             the selected athlete is close to the world record and 
                             from the swiss record in a specific class. The 0% is 
                             represented by a fictious value that could be considered 
                             as the worst possible result in a 400m run with professional 
                             athletes. In this case we decided that the baseline had to be 
                             50.97s. The three light blue dots represent it. The 100% is the 
                             best result in each class from 1990 and it is represented by the 
                             three blue vertices. Inside the figure, it is possible to spot a 
                             dashed line in brown. These vertices of the brown triangle are the 
                             Swiss top result. Lastly, the red line is the performance of the selected athlete. "),
                               p("It is important to understand that, sometimes, athletes with
                             good final results have 0% in one or two classes. This is
                             not due to the fact that they were extremely bad at one 
                             point of their career. In fact, we can explain this problem 
                             with the lack of records. It could be that the athlete gained 
                             the chance to participate in one of the major competitions 
                             in a Senior age or that the athlete is still in Youth/Junior 
                             age and he has no records in the Senior class. ")),
                           width = 6),
                       box(plotlyOutput("difference_histogram_400mM"), 
                           div(h3("HOW TO READ IT"),
                               p("The graph is representing the frequency of top swiss athletes from 
                           the best result of a runner that finished his competition in the 8th place 
                           in a specific class. The black bars are the frequency of the selected athlete in a certain difference. 
                              This graph has two purposes: first of all to make clear how far 
                             is the selected athlete from the minimum result to have a high 
                             probability of receiving a certificate. In second place, it lets the 
                             reader see how far are on average the Swiss athletes from the best 8th 
                             place runner, and, as a consequence, from the certificate."),
                               p("In this case the more the black bars are on the left, the better the result of the athlete.")),
                           width = 6
                       ))
      ),
      tabItem("athlete400mW",
              fluidRow(box(titlePanel(h1("Women 400 m")),
                           div(h4("The discipline"),
                               p("The 400 metres, or 400-meter dash, is a sprint event in track and field competitions. It has been featured in the athletics programme at the Summer Olympics since 1896 for men and since 1964 for women. On a standard outdoor running track, it is one lap around the track. 
                        We are considering only runners that have participated at least three times in the Olympic games or in the World Championships."),
                               p("We are interested in Swiss athletes, comparing them with the rest of the world.
                          In addition, they are divided into classes, in particular, we are analysing “Seniors”, “Juniors” (not a class in the Olympics) and “Youth” classes."),
                               p("The purpose is to make the Federation aware of the current situation in this discipline, providing tools, the graphs, that can be used to make strategic decisions.")), 
                           width = 12)
              ),
              fluidRow(box(plotlyOutput("difference_plot_400W"), 
                           div(h3("HOW TO READ IT"),
                               p("The red line is the conjunction of all the results 
                               of the selected athlete (it is possible to select it in the box on the right). On the x-axis there are the years,
                                 on the y-axis there is the result time. For each measurement 
                                 it is possible to see the result directly on the graph. "),
                               p("The blue band is the range in which an athlete has to 
                                 finish in order to have a high probability to receive a 
                                 medal or at least a certificate."),
                               p("To summarise, for each year the red dot is the result 
                                 of the selected athlete in a specific class. On the same 
                                 vertical line there is the blue band. The upper bound of this 
                                 band is the best result of a person that had finished the competition
                                 in the 8th place, while the lower bound is the best result (considering 
                                 data from 1990) of a person that had won.")),
                           width = 7),
                       box(
                         selectInput("athlete400mWomen", "Athlete: ", 
                                     c("Chelsea Zoller", "Veronica Vancardo", "Giulia Senn")), 
                         tableOutput("athlete400mW_table"),
                         width = 5
                       )),
              fluidRow(box(plotOutput("spidernet400mWomen"), 
                           div(h3("HOW TO READ IT"),
                               p("In this representation we want to understand how 
                             the selected athlete is close to the world record and 
                             from the swiss record in a specific class. The 0% is 
                             represented by a fictious value that could be considered 
                             as the worst possible result in a 400m run with professional 
                             athletes. In this case we decided that the baseline had to be 
                             58.57s. The three light blue dots represent it. The 100% is the 
                             best result in each class from 1990 and it is represented by the 
                             three blue vertices. Inside the figure, it is possible to spot a 
                             dashed line in brown. These vertices of the brown triangle are the 
                             Swiss top result. Lastly, the red line is the performance of the selected athlete. "),
                               p("It is important to understand that, sometimes, athletes with
                             good final results have 0% in one or two classes. This is
                             not due to the fact that they were extremely bad at one 
                             point of their career. In fact, we can explain this problem 
                             with the lack of records. It could be that the athlete gained 
                             the chance to participate in one of the major competitions 
                             in a Senior age or that the athlete is still in Youth/Junior 
                             age and he has no records in the Senior class. ")),
                           width = 6),
                       box(plotlyOutput("difference_histogram_400mW"), 
                           div(h3("HOW TO READ IT"),
                               p("The graph is representing the frequency of top swiss athletes from 
                           the best result of a runner that finished his competition in the 8th place 
                           in a specific class. The black bars are the frequency of the selected athlete in a certain difference. 
                              This graph has two purposes: first of all to make clear how far 
                             is the selected athlete from the minimum result to have a high 
                             probability of receiving a certificate. In second place, it lets the 
                             reader see how far are on average the Swiss athletes from the best 8th 
                             place runner, and, as a consequence, from the certificate."),
                               p("In this case the more the black bars are on the left, the better the result of the athlete.")),
                           width = 6
                       ))
      ),
      tabItem("athleteLongJumpM",
              fluidRow(box(titlePanel(h1("Men Long Jump")),
                       div(h4("The discipline"),
                           p("The long jump is a track and field event in which athletes
                           combine speed, strength and agility. This event has a history 
                           in the ancient Olympic Games and has been a modern Olympic event 
                           for men since the first Olympics in 1896 and for women since 1948.
                        We are considering only runners that have participated at least three 
                             times in the Olympic games or in the World Championships."),
                           p("We are interested in Swiss athletes, comparing them with the rest of the world.
                          In addition, they are divided into classes, in particular, we are
                             analysing “Seniors”, “Juniors” (not a class in the Olympics) and “Youth” classes."),
                           p("The purpose is to make the Federation aware of the current 
                             situation in this discipline, providing tools, the graphs, that 
                             can be used to make strategic decisions.")), 
                       width = 12)
              ),
              fluidRow(box(plotlyOutput("difference_plot_LJM"), 
                           div(h3("HOW TO READ IT"),
                               p("The red line is the conjunction of all the results 
                               of the selected athlete (it is possible to select it in the box on the right). On the x-axis there are the years,
                                 on the y-axis there is the result time. For each measurement 
                                 it is possible to see the result directly on the graph. "),
                               p("The blue band is the range in which an athlete has to 
                                 finish in order to have a high probability to receive a 
                                 medal or at least a certificate."),
                               p("To summarise, for each year the red dot is the result 
                                 of the selected athlete in a specific class. On the same 
                                 vertical line there is the blue band. The upper bound of this 
                                 band is the best result of a person that had finished the competition
                                 in the 8th place, while the lower bound is the best result (considering 
                                 data from 1990) of a person that had won.")),
                           width = 7),
                       box(
                         selectInput("athleteLJM", "Athlete: ", 
                                     c("Benjamin Gföhler", "Julien Fivaz", "Yves Zellweger",
                                       "Carlos Kouassi", "Jarod Biya", "Cyrill Kernbach",
                                       "Simon Ehammer")), 
                         tableOutput("athleteLJM_table"),
                         width = 5
                       )),
              fluidRow(box(plotOutput("spidernetLJMen"), 
                           div(h3("HOW TO READ IT"),
                               p("In this representation we want to understand how 
                             the selected athlete is close to the world record and 
                             from the swiss record in a specific class. The 0% is 
                             represented by a fictious value that could be considered 
                             as the worst possible result in a Long Jump competition with professional 
                             athletes. In this case we decided that the baseline had to be 
                             0m. The three light blue dots represent it. The 100% is the 
                             best result in each class from 1990 and it is represented by the 
                             three blue vertices. Inside the figure, it is possible to spot a 
                             dashed line in brown. These vertices of the brown triangle are the 
                             Swiss top result. Lastly, the red line is the performance of the selected athlete. "),
                               p("It is important to understand that, sometimes, athletes with
                             good final results have 0% in one or two classes. This is
                             not due to the fact that they were extremely bad at one 
                             point of their career. In fact, we can explain this problem 
                             with the lack of records. It could be that the athlete gained 
                             the chance to participate in one of the major competitions 
                             in a Senior age or that the athlete is still in Youth/Junior 
                             age and he has no records in the Senior class. ")),
                           width = 6),
                       box(plotlyOutput("difference_histogram_LJM"), 
                           div(h3("HOW TO READ IT"),
                               p("The graph is representing the frequency of top swiss athletes from 
                           the best result of a runner that finished his competition in the 8th place 
                           in a specific class. The black bars are the frequency of the selected athlete in a certain difference. 
                              This graph has two purposes: first of all to make clear how far 
                             is the selected athlete from the minimum result to have a high 
                             probability of receiving a certificate. In second place, it lets the 
                             reader see how far are on average the Swiss athletes from the best 8th 
                             place runner, and, as a consequence, from the certificate."),
                               p("In this case the more the black bars are on the right, the better the result of the athlete.")),
                           width = 6
                       ))),
      tabItem("athleteLongJumpW",
              fluidRow(box(titlePanel(h1("Women Long Jump")),
                           div(h4("The discipline"),
                               p("The long jump is a track and field event in which athletes
                           combine speed, strength and agility. This event has a history 
                           in the ancient Olympic Games and has been a modern Olympic event 
                           for men since the first Olympics in 1896 and for women since 1948.
                        We are considering only runners that have participated at least three 
                             times in the Olympic games or in the World Championships."),
                               p("We are interested in Swiss athletes, comparing them with the rest of the world.
                          In addition, they are divided into classes, in particular, we are
                             analysing “Seniors”, “Juniors” (not a class in the Olympics) and “Youth” classes."),
                               p("The purpose is to make the Federation aware of the current 
                             situation in this discipline, providing tools, the graphs, that 
                             can be used to make strategic decisions.")), 
                           width = 12)),
              fluidRow(box(plotlyOutput("difference_plot_LJW"), 
                           div(h3("HOW TO READ IT"),
                               p("The red line is the conjunction of all the results 
                               of the selected athlete (it is possible to select it in the box on the right). On the x-axis there are the years,
                                 on the y-axis there is the result time. For each measurement 
                                 it is possible to see the result directly on the graph. "),
                               p("The blue band is the range in which an athlete has to 
                                 finish in order to have a high probability to receive a 
                                 medal or at least a certificate."),
                               p("To summarise, for each year the red dot is the result 
                                 of the selected athlete in a specific class. On the same 
                                 vertical line there is the blue band. The upper bound of this 
                                 band is the best result of a person that had finished the competition
                                 in the 8th place, while the lower bound is the best result (considering 
                                 data from 1990) of a person that had won.")),
                           width = 7),
                       box(
                         selectInput("athleteLJW", "Athlete: ", 
                                     c("Irene Pusterla", "Clélia Rard-Reuse", "Annik Kälin",
                                       "Emma Piffaretti", "Elena Debelic")), 
                         tableOutput("athleteLJW_table"),
                         width = 5
                       )),
              fluidRow(box(plotOutput("spidernetLJWomen"), 
                           div(h3("HOW TO READ IT"),
                               p("In this representation we want to understand how 
                             the selected athlete is close to the world record and 
                             from the swiss record in a specific class. The 0% is 
                             represented by a fictious value that could be considered 
                             as the worst possible result in a Long Jump competition with professional 
                             athletes. In this case we decided that the baseline had to be 
                             0m. The three light blue dots represent it. The 100% is the 
                             best result in each class from 1990 and it is represented by the 
                             three blue vertices. Inside the figure, it is possible to spot a 
                             dashed line in brown. These vertices of the brown triangle are the 
                             Swiss top result. Lastly, the red line is the performance of the selected athlete. "),
                               p("It is important to understand that, sometimes, athletes with
                             good final results have 0% in one or two classes. This is
                             not due to the fact that they were extremely bad at one 
                             point of their career. In fact, we can explain this problem 
                             with the lack of records. It could be that the athlete gained 
                             the chance to participate in one of the major competitions 
                             in a Senior age or that the athlete is still in Youth/Junior 
                             age and he has no records in the Senior class. ")),
                           width = 6),
                       box(plotlyOutput("difference_histogram_LJW"), 
                           div(h3("HOW TO READ IT"),
                               p("The graph is representing the frequency of top swiss athletes from 
                           the best result of a runner that finished his competition in the 8th place 
                           in a specific class. The black bars are the frequency of the selected athlete in a certain difference. 
                              This graph has two purposes: first of all to make clear how far 
                             is the selected athlete from the minimum result to have a high 
                             probability of receiving a certificate. In second place, it lets the 
                             reader see how far are on average the Swiss athletes from the best 8th 
                             place runner, and, as a consequence, from the certificate."),
                               p("In this case the more the black bars are on the right, the better the result of the athlete.")),
                           width = 6
                       )))
    )
  )
)

server <- function(input, output) {
  
  ##################### 100 m men ##############################################################
  
  output$difference_plot_M <- renderPlotly({
    p <- ggplot(data = athlete100mM[which(athlete100mM$Name == input$athlete100mMen), ],
                aes(x = Year, y = Result)) +
      geom_line(color = "red", size = 1) +
      geom_point(color = "red", size = 2.5) +
      geom_ribbon(aes(x = Year, ymax = Top_8, ymin = Top_1), fill = "blue", alpha = 0.2) +
      geom_text(
        aes(label = Result),
        nudge_x = 0.01,
        nudge_y = 0.1,
        check_overlap = TRUE,
        size = 4
      )  + 
      xlab("Year") + 
      ylab("Time") 
    
    ggplotly(p)

  })
  
  output$spidernet100mMen <- renderPlot({
    radarchart(radar_frame[which(radar_frame$Name == input$athlete100mMen | radar_frame$Name == "World Record in the class"
                                 | radar_frame$Name == "Fictious baseline" | radar_frame$Name == "Swiss Record in the class"), c("Seniors", "Juniors", "Youth") ],
               maxmin = FALSE,
               axistype = 1,
               vlabels = c("Seniors", "Juniors", "Youth"),
               vlcex = 1.1,
               pcol = c("red", "gold", "lightblue", "darkred"),
               plwd = c(3,1,1,2),
               plty = c("solid", "dashed", "dashed", "dashed"),
               caxislabels = c("0%", "25%", "50%", "75%", "100%"),
               axislabcol = 'grey', 
               cglwd = 0.6
    )
    legend(1,1, legend=unique(radar_frame[which(radar_frame$Name == input$athlete100mMen | radar_frame$Name == "World Record in the class"
                                                | radar_frame$Name == "Fictious baseline" | radar_frame$Name == "Swiss Record in the class"), "Name"])
           , seg.len=0.5, pch=1, border = "black",
           bty="n" ,lwd=3, y.intersp=1, horiz=FALSE, col =c("red", "gold", "lightblue", "darkred")
    )
  })
  
  output$athlete100mM_table <- renderTable(athlete100mM[which(athlete100mM$Name == input$athlete100mMen), ])
  
  output$difference_histogram_M <- renderPlotly({
    p <- ggplot(data = athlete100mM)+
      geom_histogram(aes(x = Difference_8, fill = Class), binwidth = .05) +
      geom_histogram(data = athlete100mM[which(athlete100mM$Name == input$athlete100mMen), ],
                     aes(x = Difference_8), binwidth = 0.05) +
      scale_fill_manual(values = heat.colors(4)) + 
      xlab("Difference in time from Top 8 performers") +
      ylab("Number of occurrences") +
      theme(
        legend.title = element_text(
          family = "Calibri",
          face = "bold",
          size = 12),
        legend.background = element_rect(fill = "white", colour = "black"),
      )
    
    ggplotly(p)
  })
  
  ##################### 100 m women ##############################################################
  
  output$difference_plot_W <- renderPlotly({
    p <- ggplot(data = athlete100mW[which(athlete100mW$Name == input$athlete100mWomen), ],
                aes(x = Year, y = Result)) +
      geom_line(color = "red", size = 1) +
      geom_point(color = "red", size = 2.5) +
      geom_ribbon(aes(x = Year, ymax = Top_8, ymin = Top_1), fill = "blue", alpha = 0.2) +
      geom_text(
        aes(label = Result),
        nudge_x = 0.01,
        nudge_y = 0.1,
        check_overlap = TRUE,
        size = 4
      )  + 
      xlab("Year") + 
      ylab("Time") 
    
    ggplotly(p)
    
  })
  
  output$spidernet100mWomen <- renderPlot({
    radarchart(radar_frameW[which(radar_frameW$Name == input$athlete100mWomen | radar_frameW$Name == "World Record in the class"
                                  | radar_frameW$Name == "Fictious baseline" | radar_frameW$Name == "Swiss Record in the class"), c("Seniors", "Juniors", "Youth") ],
               maxmin = FALSE,
               axistype = 1,
               vlabels = c("Seniors", "Juniors", "Youth"),
               vlcex = 1.1,
               pcol = c("red", "gold", "lightblue", "darkred"),
               plwd = c(3,1,1,2),
               plty = c("solid", "dashed", "dashed", "dashed"),
               caxislabels = c("0%", "25%", "50%", "75%", "100%"),
               axislabcol = 'grey', 
               cglwd = 0.6
    )
    legend(1,1, legend=unique(radar_frameW[which(radar_frameW$Name == input$athlete100mWomen | radar_frameW$Name == "World Record in the class"
                                                 | radar_frameW$Name == "Fictious baseline" | radar_frameW$Name == "Swiss Record in the class"), "Name"])
           , seg.len=0.5, pch=1, border = "black",
           bty="n" ,lwd=3, y.intersp=1, horiz=FALSE, col =c("red", "gold", "lightblue", "darkred")
    )
  })
  
  output$athlete100mW_table <- renderTable(athlete100mW[which(athlete100mW$Name == input$athlete100mWomen), ])
  
  output$difference_histogram_W <- renderPlotly({
    p <- ggplot(data = athlete100mW)+
      geom_histogram(aes(x = Difference_8, fill = Class), binwidth = .05) +
      geom_histogram(data = athlete100mW[which(athlete100mW$Name == input$athlete100mWomen), ],
                     aes(x = Difference_8), binwidth = 0.05) +
      scale_fill_manual(values = heat.colors(4)) + 
      xlab("Difference in time from Top 8 performers") +
      ylab("Number of occurrences") +
      theme(
        legend.title = element_text(
          family = "Calibri",
          face = "bold",
          size = 12),
        legend.background = element_rect(fill = "white", colour = "black"),
      )
    
    ggplotly(p)
  })
  
  ##################### 200 m men ##############################################################
  
  output$difference_plot_200M <- renderPlotly({
    p <- ggplot(data = athlete200mM[which(athlete200mM$Name == input$athlete200mMen), ],
                aes(x = Year, y = Result)) +
      geom_line(color = "red", size = 1) +
      geom_point(color = "red", size = 2.5) +
      geom_ribbon(aes(x = Year, ymax = Top_8, ymin = Top_1), fill = "blue", alpha = 0.2) +
      geom_text(
        aes(label = Result),
        nudge_x = 0.01,
        nudge_y = 0.1,
        check_overlap = TRUE,
        size = 4
      )  + 
      ggtitle("Difference of Suisse athletes in comparison with top 8 range") +
      xlab("Year") + 
      ylab("Time") 
    
    ggplotly(p)
  })
  
  output$spidernet200mMen <- renderPlot({
    radarchart(radar_frame200[which(radar_frame200$Name == input$athlete200mMen | radar_frame200$Name == "World Record in the class"
                                    | radar_frame200$Name == "Fictious baseline" | radar_frame200$Name == "Swiss Record in the class"), c("Seniors", "Juniors", "Youth") ],
               maxmin = FALSE,
               axistype = 1,
               vlabels = c("Seniors", "Juniors", "Youth"),
               vlcex = 1.1,
               pcol = c("red", "gold", "lightblue", "darkred"),
               plwd = c(3,1,1,2),
               plty = c("solid", "dashed", "dashed", "dashed"),
               caxislabels = c("0%", "25%", "50%", "75%", "100%"),
               axislabcol = 'grey', 
               cglwd = 0.6
    )
    legend(1,1, legend=unique(radar_frame200[which(radar_frame200$Name == input$athlete200mMen | radar_frame200$Name == "World Record in the class"
                                                   | radar_frame200$Name == "Fictious baseline" | radar_frame200$Name == "Swiss Record in the class"), "Name"])
           , seg.len=0.5, pch=1, border = "black",
           bty="n" ,lwd=3, y.intersp=1, horiz=FALSE, col =c("red", "gold", "lightblue", "darkred")
    )
  })
  
  output$athlete200mM_table <- renderTable(athlete200mM[which(athlete200mM$Name == input$athlete200mMen), ])
  
  output$difference_histogram_200mM <- renderPlotly({
    p <- ggplot(data = athlete200mM)+
      geom_histogram(aes(x = Difference_8, fill = Class), binwidth = .05) +
      geom_histogram(data = athlete200mM[which(athlete200mM$Name == input$athlete200mMen), ],
                     aes(x = Difference_8), binwidth = 0.05) +
      scale_fill_manual(values = heat.colors(4)) +
      theme(
        legend.title = element_text(
          family = "Calibri",
          face = "bold",
          size = 12),
        legend.background = element_rect(fill = "white", colour = "black"),
      )
    
    ggplotly(p)
  })
  
  ##################### 200 m women ##############################################################
  
  output$difference_plot_200W <- renderPlotly({
    p <- ggplot(data = athlete200mW[which(athlete200mW$Name == input$athlete200mWomen), ],
                aes(x = Year, y = Result)) +
      geom_line(color = "red", size = 1) +
      geom_point(color = "red", size = 2.5) +
      geom_ribbon(aes(x = Year, ymax = Top_8, ymin = Top_1), fill = "blue", alpha = 0.2) +
      geom_text(
        aes(label = Result),
        nudge_x = 0.01,
        nudge_y = 0.1,
        check_overlap = TRUE,
        size = 4
      )  + 
      ggtitle("Difference of Suisse athletes in comparison with top 8 range") +
      xlab("Year") + 
      ylab("Time") 
    
    ggplotly(p)
  })
  
  output$spidernet200mWomen <- renderPlot({
    radarchart(radar_frame200W[which(radar_frame200W$Name == input$athlete200mWomen | radar_frame200W$Name == "World Record in the class"
                                     | radar_frame200W$Name == "Fictious baseline" | radar_frame200W$Name == "Swiss Record in the class"), c("Seniors", "Juniors", "Youth") ],
               maxmin = FALSE,
               axistype = 1,
               vlabels = c("Seniors", "Juniors", "Youth"),
               vlcex = 1.1,
               pcol = c("red", "gold", "lightblue", "darkred"),
               plwd = c(3,1,1,2),
               plty = c("solid", "dashed", "dashed", "dashed"),
               caxislabels = c("0%", "25%", "50%", "75%", "100%"),
               axislabcol = 'grey', 
               cglwd = 0.6
    )
    legend(1,1, legend=unique(radar_frame200W[which(radar_frame200W$Name == input$athlete200mWomen | radar_frame200W$Name == "World Record in the class"
                                                    | radar_frame200W$Name == "Fictious baseline" | radar_frame200W$Name == "Swiss Record in the class"), "Name"])
           , seg.len=0.5, pch=1, border = "black",
           bty="n" ,lwd=3, y.intersp=1, horiz=FALSE, col =c("red", "gold", "lightblue", "darkred")
    )
  })
  
  output$athlete200mW_table <- renderTable(athlete200mW[which(athlete200mW$Name == input$athlete200mWomen), ])
  
  output$difference_histogram_200mW <- renderPlotly({
    p <- ggplot(data = athlete200mW)+
      geom_histogram(aes(x = Difference_8, fill = Class), binwidth = .05) +
      geom_histogram(data = athlete200mM[which(athlete200mW$Name == input$athlete200mWomen), ],
                     aes(x = Difference_8), binwidth = 0.05) +
      scale_fill_manual(values = heat.colors(4)) +
      theme(
        legend.title = element_text(
          family = "Calibri",
          face = "bold",
          size = 12),
        legend.background = element_rect(fill = "white", colour = "black"),
      )
    
    ggplotly(p)
  })
  
  ##################### 400 m men ##############################################################
  
  output$difference_plot_400M <- renderPlotly({
    p <- ggplot(data = athlete400mM[which(athlete400mM$Name == input$athlete400mMen), ],
                aes(x = Year, y = Result)) +
      geom_line(color = "red", size = 1) +
      geom_point(color = "red", size = 2.5) +
      geom_ribbon(aes(x = Year, ymax = Top_8, ymin = Top_1), fill = "blue", alpha = 0.2) +
      geom_text(
        aes(label = Result),
        nudge_x = 0.01,
        nudge_y = 0.1,
        check_overlap = TRUE,
        size = 4
      )  + 
      ggtitle("Difference of Suisse athletes in comparison with top 8 range") +
      xlab("Year") + 
      ylab("Time") 
    
    ggplotly(p)
  })
  
  output$spidernet400mMen <- renderPlot({
    radarchart(radar_frame400[which(radar_frame400$Name == input$athlete400mMen | radar_frame400$Name == "World Record in the class"
                                    | radar_frame400$Name == "Fictious baseline" | radar_frame400$Name == "Swiss Record in the class"), c("Seniors", "Juniors", "Youth") ],
               maxmin = FALSE,
               axistype = 1,
               vlabels = c("Seniors", "Juniors", "Youth"),
               vlcex = 1.1,
               pcol = c("red", "gold", "lightblue", "darkred"),
               plwd = c(3,1,1,2),
               plty = c("solid", "dashed", "dashed", "dashed"),
               caxislabels = c("0%", "25%", "50%", "75%", "100%"),
               axislabcol = 'grey', 
               cglwd = 0.6
    )
    legend(1,1, legend=unique(radar_frame400[which(radar_frame400$Name == input$athlete400mMen | radar_frame400$Name == "World Record in the class"
                                                   | radar_frame400$Name == "Fictious baseline" | radar_frame400$Name == "Swiss Record in the class"), "Name"])
           , seg.len=0.5, pch=1, border = "black",
           bty="n" ,lwd=3, y.intersp=1, horiz=FALSE, col =c("red", "gold", "lightblue", "darkred")
    )
  })
  
  output$athlete400mM_table <- renderTable(athlete400mM[which(athlete400mM$Name == input$athlete400mMen), ])
  
  output$difference_histogram_400mM <- renderPlotly({
    p <- ggplot(data = athlete400mM)+
      geom_histogram(aes(x = Difference_8, fill = Class), binwidth = .2) +
      geom_histogram(data = athlete400mM[which(athlete400mM$Name == input$athlete400mMen), ],
                     aes(x = Difference_8), binwidth = 0.2) +
      scale_fill_manual(values = heat.colors(4)) +
      theme(
        legend.title = element_text(
          family = "Calibri",
          face = "bold",
          size = 12),
        legend.background = element_rect(fill = "white", colour = "black"),
      )
    
    ggplotly(p)
  })
  
  ##################### 400 m women ############################################################
  
  output$difference_plot_400W <- renderPlotly({
    p <- ggplot(data = athlete400mW[which(athlete400mW$Name == input$athlete400mWomen), ],
                aes(x = Year, y = Result)) +
      geom_line(color = "red", size = 1) +
      geom_point(color = "red", size = 2.5) +
      geom_ribbon(aes(x = Year, ymax = Top_8, ymin = Top_1), fill = "blue", alpha = 0.2) +
      geom_text(
        aes(label = Result),
        nudge_x = 0.01,
        nudge_y = 0.1,
        check_overlap = TRUE,
        size = 4
      )  + 
      ggtitle("Difference of Suisse athletes in comparison with top 8 range") +
      xlab("Year") + 
      ylab("Time") 
    
    ggplotly(p)
  })
  
  output$spidernet400mWomen <- renderPlot({
    radarchart(radar_frame400W[which(radar_frame400W$Name == input$athlete400mWomen | radar_frame400W$Name == "World Record in the class"
                                     | radar_frame400W$Name == "Fictious baseline" | radar_frame400W$Name == "Swiss Record in the class"), c("Seniors", "Juniors", "Youth") ],
               maxmin = FALSE,
               axistype = 1,
               vlabels = c("Seniors", "Juniors", "Youth"),
               vlcex = 1.1,
               pcol = c("red", "gold", "lightblue", "darkred"),
               plwd = c(3,1,1,2),
               plty = c("solid", "dashed", "dashed", "dashed"),
               caxislabels = c("0%", "25%", "50%", "75%", "100%"),
               axislabcol = 'grey', 
               cglwd = 0.6
    )
    legend(1,1, legend=unique(radar_frame200W[which(radar_frame200W$Name == input$athlete400mWomen | radar_frame200W$Name == "World Record in the class"
                                                    | radar_frame200W$Name == "Fictious baseline" | radar_frame200W$Name == "Swiss Record in the class"), "Name"])
           , seg.len=0.5, pch=1, border = "black",
           bty="n" ,lwd=3, y.intersp=1, horiz=FALSE, col =c("red", "gold", "lightblue", "darkred")
    )
  })
  
  output$athlete400mW_table <- renderTable(athlete400mW[which(athlete400mW$Name == input$athlete400mWomen), ])
  
  output$difference_histogram_400mW <- renderPlotly({
    p <- ggplot(data = athlete400mW)+
      geom_histogram(aes(x = Difference_8, fill = Class), binwidth = .2) +
      geom_histogram(data = athlete400mW[which(athlete400mW$Name == input$athlete400mWomen), ],
                     aes(x = Difference_8), binwidth = 0.2) +
      scale_fill_manual(values = heat.colors(4)) +
      theme(
        legend.title = element_text(
          family = "Calibri",
          face = "bold",
          size = 12),
        legend.background = element_rect(fill = "white", colour = "black"),
      )
    
    ggplotly(p)
  })
  
  ##################### Long Jump Men ############################################################
  
  output$difference_plot_LJM <- renderPlotly({
    p <- ggplot(data = athleteLongJumpM[which(athleteLongJumpM$Name == input$athleteLJM), ],
                aes(x = Year, y = Result)) +
      geom_line(color = "red", size = 1) +
      geom_point(color = "red", size = 2.5) +
      geom_ribbon(aes(x = Year, ymax = Top_8, ymin = Top_1), fill = "blue", alpha = 0.2) +
      geom_text(
        aes(label = Result),
        nudge_x = 0.01,
        nudge_y = 0.1,
        check_overlap = TRUE,
        size = 4
      )  + 
      ggtitle("Difference of Suisse athletes in comparison with top 8 range") +
      xlab("Year") + 
      ylab("Distance") 
    
    ggplotly(p)
  })
  
  output$spidernetLJMen <- renderPlot({
    radarchart(radar_frameLJ[which(radar_frameLJ$Name == input$athleteLJM | radar_frameLJ$Name == "World Record in the class"
                                   | radar_frameLJ$Name == "Fictious baseline" | radar_frameLJ$Name == "Swiss Record in the class"), c("Seniors", "Juniors", "Youth") ],
               maxmin = FALSE,
               axistype = 1,
               vlabels = c("Seniors", "Juniors", "Youth"),
               vlcex = 1.1,
               pcol = c("red", "gold", "lightblue", "darkred"),
               plwd = c(3,1,1,2),
               plty = c("solid", "dashed", "dashed", "dashed"),
               caxislabels = c("0%", "25%", "50%", "75%", "100%"),
               axislabcol = 'grey', 
               cglwd = 0.6
    )
    legend(1,1, legend=unique(radar_frameLJ[which(radar_frameLJ$Name == input$athleteLJM | radar_frameLJ$Name == "World Record in the class"
                                                  | radar_frameLJ$Name == "Fictious baseline" | radar_frameLJ$Name == "Swiss Record in the class"), "Name"])
           , seg.len=0.5, pch=1, border = "black",
           bty="n" ,lwd=3, y.intersp=1, horiz=FALSE, col =c("red", "gold", "lightblue", "darkred")
    )
  })
  
  output$athleteLJM_table <- renderTable(athleteLongJumpM[which(athleteLongJumpM$Name == input$athleteLJM), ])
  
  output$difference_histogram_LJM <- renderPlotly({
    p <- ggplot(data = athleteLongJumpM)+
      geom_histogram(aes(x = Difference_8, fill = Class), binwidth = .2) +
      geom_histogram(data = athleteLongJumpM[which(athleteLongJumpM$Name == input$athleteLJM), ],
                     aes(x = Difference_8), binwidth = 0.2) +
      scale_fill_manual(values = heat.colors(4)) +
      theme(
        legend.title = element_text(
          family = "Calibri",
          face = "bold",
          size = 12),
        legend.background = element_rect(fill = "white", colour = "black"),
      )
    
    ggplotly(p)
  })
  
  ##################### Long Jump Women ############################################################
  
  output$difference_plot_LJW <- renderPlotly({
    p <- ggplot(data = athleteLongJumpW[which(athleteLongJumpW$Name == input$athleteLJW), ],
                aes(x = Year, y = Result)) +
      geom_line(color = "red", size = 1) +
      geom_point(color = "red", size = 2.5) +
      geom_ribbon(aes(x = Year, ymax = Top_8, ymin = Top_1), fill = "blue", alpha = 0.2) +
      geom_text(
        aes(label = Result),
        nudge_x = 0.01,
        nudge_y = 0.1,
        check_overlap = TRUE,
        size = 4
      )  + 
      ggtitle("Difference of Suisse athletes in comparison with top 8 range") +
      xlab("Year") + 
      ylab("Distance") 
    
    ggplotly(p)
  })
  
  output$spidernetLJWomen <- renderPlot({
    radarchart(radar_frameLJW[which(radar_frameLJW$Name == input$athleteLJW | radar_frameLJW$Name == "World Record in the class"
                                    | radar_frameLJW$Name == "Fictious baseline" | radar_frameLJW$Name == "Swiss Record in the class"), c("Seniors", "Juniors", "Youth") ],
               maxmin = FALSE,
               axistype = 1,
               vlabels = c("Seniors", "Juniors", "Youth"),
               vlcex = 1.1,
               pcol = c("red", "gold", "lightblue", "darkred"),
               plwd = c(3,1,1,2),
               plty = c("solid", "dashed", "dashed", "dashed"),
               caxislabels = c("0%", "25%", "50%", "75%", "100%"),
               axislabcol = 'grey', 
               cglwd = 0.6
    )
    legend(1,1, legend=unique(radar_frameLJW[which(radar_frameLJW$Name == input$athleteLJW | radar_frameLJW$Name == "World Record in the class"
                                                   | radar_frameLJW$Name == "Fictious baseline" | radar_frameLJW$Name == "Swiss Record in the class"), "Name"])
           , seg.len=0.5, pch=1, border = "black",
           bty="n" ,lwd=3, y.intersp=1, horiz=FALSE, col =c("red", "gold", "lightblue", "darkred")
    )
  })
  
  output$athleteLJW_table <- renderTable(athleteLongJumpW[which(athleteLongJumpW$Name == input$athleteLJW), ])
  
  output$difference_histogram_LJW <- renderPlotly({
    p <- ggplot(data = athleteLongJumpW)+
      geom_histogram(aes(x = Difference_8, fill = Class), binwidth = .2) +
      geom_histogram(data = athleteLongJumpW[which(athleteLongJumpW$Name == input$athleteLJW), ],
                     aes(x = Difference_8), binwidth = 0.2) +
      scale_fill_manual(values = heat.colors(4)) +
      theme(
        legend.title = element_text(
          family = "Calibri",
          face = "bold",
          size = 12),
        legend.background = element_rect(fill = "white", colour = "black"),
      )
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)