olympics <- read.csv("C:/Users/buson/OneDrive/Documenti/R_universita/sportdataanalytics/Data/athletics.csv")
table(olympics$Discipline)
library(fmsb)
library(ggplot2)
library(plotly)
#######################################################################################################################
###################################################100m Men############################################################
#######################################################################################################################
sprint100mMale <- olympics[which(olympics$Discipline == "100m"), ]
sprint100mMale <- sprint100mMale[which(sprint100mMale$Year>=1990), ]
sprint100mMale <- sprint100mMale[which(sprint100mMale$Competition == "Olympic Games" 
                 | sprint100mMale$Competition == "World Championships"), ]
sprint100mMale <- sprint100mMale[which(sprint100mMale$Gender == "Men"), ]
sprint100mMale <- sprint100mMale[which(sprint100mMale$Class == "Seniors"), ]
sprint100mMale <- sprint100mMale[which(sprint100mMale$Result != ""), ]
# sprint100mMale <- sprint100mMale[which(sprint100mMale$Medal != ""), ]
sprint100mMale[, "Result"] <- as.double(sprint100mMale$Result)
summary(sprint100mMale$Result)

sprint100mMaleJunior <- olympics[which(olympics$Discipline == "100m"), ]
sprint100mMaleJunior <- sprint100mMaleJunior[which(sprint100mMaleJunior$Year>=1990), ]
sprint100mMaleJunior <- sprint100mMaleJunior[which(sprint100mMaleJunior$Competition == "World Junior Championships"), ]
sprint100mMaleJunior <- sprint100mMaleJunior[which(sprint100mMaleJunior$Gender == "Men"), ]
sprint100mMaleJunior <- sprint100mMaleJunior[which(sprint100mMaleJunior$Class == "Juniors"), ]
sprint100mMaleJunior <- sprint100mMaleJunior[which(sprint100mMaleJunior$Result != ""), ]
# sprint100mMaleJunior <- sprint100mMaleJunior[which(sprint100mMaleJunior$Medal != ""), ]
sprint100mMaleJunior[, "Result"] <- as.double(sprint100mMaleJunior$Result)
summary(sprint100mMaleJunior$Result)

sprint100mMaleYouth <- olympics[which(olympics$Discipline == "100m"), ]
sprint100mMaleYouth <- sprint100mMaleYouth[which(sprint100mMaleYouth$Year>=1990), ]
sprint100mMaleYouth <- sprint100mMaleYouth[which(sprint100mMaleYouth$Competition == "World Youth Championships"
                                         | sprint100mMaleYouth$Competition == "Youth Olympic Games"), ]
sprint100mMaleYouth <- sprint100mMaleYouth[which(sprint100mMaleYouth$Gender == "Men"), ]
sprint100mMaleYouth <- sprint100mMaleYouth[which(sprint100mMaleYouth$Class == "Youth"), ]
sprint100mMaleYouth <- sprint100mMaleYouth[which(sprint100mMaleYouth$Result != ""), ]
# sprint100mMaleYouth <- sprint100mMaleYouth[which(sprint100mMaleYouth$Medal != ""), ]
sprint100mMaleYouth[, "Result"] <- as.double(sprint100mMaleYouth$Result)
summary(sprint100mMaleYouth$Result)


i <- 2
j <- 2
k <- 2
vector_senior_year <- c(min(sprint100mMale[which(sprint100mMale$Year == 2000), "Result"]))
vector_junior_year <- c(min(sprint100mMaleJunior[which(sprint100mMaleJunior$Year == 2000), "Result"]))
vector_youth_year <- c(min(sprint100mMaleYouth[which(sprint100mMaleYouth$Year == 2001), "Result"]))
for (year in 2001:2022){
  vector_senior_year[i] <- min(c(min(sprint100mMale[which(sprint100mMale$Year == year), "Result"]), vector_senior_year[i-1]))
  i <- i + 1
  vector_junior_year[j] <- min(c(min(sprint100mMaleJunior[which(sprint100mMaleJunior$Year == year), "Result"]), vector_junior_year[j-1]))
  j <- j + 1
  vector_youth_year[k] <- min(c(min(sprint100mMaleYouth[which(sprint100mMaleYouth$Year == year), "Result"]), vector_youth_year[k-1]))
  k <- k + 1
}

i <- 2
j <- 2
k <- 2
vector_senior_year_8 <- c(sprint100mMale[which(sprint100mMale$Year == 1999 & sprint100mMale$Rank == "8"), "Result"])
vector_junior_year_8 <- c(sprint100mMaleJunior[which(sprint100mMaleJunior$Year == 2000 & sprint100mMaleJunior$Rank == "8"), "Result"])
vector_youth_year_8 <- c(sprint100mMaleYouth[which(sprint100mMaleYouth$Year == 2001 & sprint100mMaleYouth$Rank == "8"), "Result"])
for (year in 2001:2022){
  vector_senior_year_8[i] <- min(c(sprint100mMale[which(sprint100mMale$Year == year & sprint100mMale$Rank == "8"), "Result"], vector_senior_year_8[i-1]))
  i <- i + 1
  vector_junior_year_8[j] <- min(c(sprint100mMaleJunior[which(sprint100mMaleJunior$Year == year & sprint100mMaleJunior$Rank == "8"), "Result"], vector_junior_year_8[j-1]))
  j <- j + 1
  vector_youth_year_8[k] <- min(c(sprint100mMaleYouth[which(sprint100mMaleYouth$Year == year & sprint100mMaleYouth$Rank == "8"), "Result"], vector_youth_year_8[k-1]))
  k <- k + 1
}



vector_senior_year_8
vector_junior_year_8
vector_youth_year_8
vector_best_year <- c(vector_senior_year, vector_junior_year, vector_youth_year)
vector_8_year <- c(vector_senior_year_8, vector_junior_year_8, vector_youth_year_8)
v1 <- 2000:2022
years <- c(v1,v1,v1)
years
vector_class <- c()
vector_class[1:23] <- "Seniors"
vector_class[24:46] <- "Juniors"
vector_class[47:69] <- "Youth"
vector_class

df_result <- data.frame(Year = years,
                        Top_8 = vector_8_year,
                        Top_1 = vector_best_year,
                        Class = vector_class)
df_result

switzerland_athlete_Men_100m <- olympics[which(olympics$Discipline == "100m"), ]
switzerland_athlete_Men_100m <- switzerland_athlete_Men_100m[which(switzerland_athlete_Men_100m$Year>=2000), ]
switzerland_athlete_Men_100m <- switzerland_athlete_Men_100m[which(switzerland_athlete_Men_100m$Gender == "Men"), ]
switzerland_athlete_Men_100m <- switzerland_athlete_Men_100m[which(switzerland_athlete_Men_100m$Nationality == "SUI"), ]
switzerland_athlete_Men_100m <- switzerland_athlete_Men_100m[which(switzerland_athlete_Men_100m$Result != ""), ]
# sprint100mMale <- sprint100mMale[which(sprint100mMale$Medal != ""), ]
switzerland_athlete_Men_100m[, "Result"] <- as.double(switzerland_athlete_Men_100m$Result)
summary(switzerland_athlete_Men_100m$Result)
table(switzerland_athlete_Men_100m$Person)


switzerland_athlete_Men_100m <- data.frame(Name = switzerland_athlete_Men_100m$Person,
                                           Result = switzerland_athlete_Men_100m$Result,
                                           Year = switzerland_athlete_Men_100m$Year,
                                           Class = switzerland_athlete_Men_100m$Class)

switzerland_athlete_Men_100m <- merge(x = switzerland_athlete_Men_100m,
                                      y = df_result, 
                                      by = c("Year", "Class")
                                )
switzerland_athlete_Men_100m


# Da fare in shiny con un input sul nome dell'atleta.

athlete_list_M <- as.list(unique(switzerland_athlete_Men_100m$Name))
athlete_result <- c()
athlete_name <- c()
athlete_year <- c()
athlete_class <- c()
top8 <- c()
top1 <- c()
i = 1
for (name in athlete_list_M){
  print(name)
  for (year in min(switzerland_athlete_Men_100m$Year):max(switzerland_athlete_Men_100m$Year)){
    print(year)
    if(length(switzerland_athlete_Men_100m[which(switzerland_athlete_Men_100m$Name == name 
                                                 & switzerland_athlete_Men_100m$Year == year), "Result"] != 0)){
      athlete_name[i] <- name
      
      athlete_result[i] <- min(switzerland_athlete_Men_100m[which(switzerland_athlete_Men_100m$Name == name
                                                           & switzerland_athlete_Men_100m$Year == year),"Result"])
      athlete_year[i] <- year
      athlete_class[i] <- switzerland_athlete_Men_100m[which(switzerland_athlete_Men_100m$Result == athlete_result[i] &
                                                               switzerland_athlete_Men_100m$Name == name & 
                                                               switzerland_athlete_Men_100m$Year == year),"Class"]
      top1[i] <- switzerland_athlete_Men_100m[which(switzerland_athlete_Men_100m$Result == athlete_result[i] &
                                                       switzerland_athlete_Men_100m$Name == name & 
                                                       switzerland_athlete_Men_100m$Year == year),"Top_1"]
      print(top1[i])
      top8[i] <- switzerland_athlete_Men_100m[which(switzerland_athlete_Men_100m$Result == athlete_result[i] &
                                                       switzerland_athlete_Men_100m$Name == name & 
                                                       switzerland_athlete_Men_100m$Year == year),"Top_8"]
      i <- i + 1
    }
  }
}
athlete100mM <- data.frame(Year = athlete_year,
                      Name = athlete_name,
                      Result = athlete_result,
                      Class = athlete_class,
                      Top_8 = top8,
                      Top_1 = top1)
athlete100mM

WR100mMen <- min(sprint100mMale$Result)
WR100mMenJ <- min(sprint100mMaleJunior$Result)
WR100mMenY <- min(sprint100mMaleYouth$Result)

SR100mMen <- min(athlete100mM$Result[which(athlete100mM$Class == "Seniors")])
SR100mMenJ <- min(athlete100mM$Result[which(athlete100mM$Class == "Juniors")])
SR100mMenY <- min(athlete100mM$Result[which(athlete100mM$Class == "Youth")])

i <- 1
a_name <- c()
result <- c()
class <- c()
wr <- c()
sr <- c()
for (name in athlete_list_M){
  a_name[i] <- name
  if (min(athlete100mM[which(athlete100mM$Name == name & athlete100mM$Class == "Seniors"), "Result"]) == Inf){
    result[i] <- 0
  }
  else{
    result[i] <- (11.19 - min(athlete100mM[which(athlete100mM$Name == name & athlete100mM$Class == "Seniors"), "Result"]))/(11.19 - WR100mMen)
  }
  print(result[i])
  class[i] <- "Seniors"
  wr[i] <- 1
  sr[i] <- (11.19 - SR100mMen)/(11.19-WR100mMen)
  i <- i + 1
}
i <- 1
a_nameJ <- c()
resultJ <- c()
classJ <- c()
wrJ <- c()
srJ <- c()
for (name in athlete_list_M){
  a_nameJ[i] <- name
  if (min(athlete100mM[which(athlete100mM$Name == name & athlete100mM$Class == "Juniors"), "Result"]) == Inf){
    resultJ[i] <- 0
  }
  else{
    resultJ[i] <- (11.19 - min(athlete100mM[which(athlete100mM$Name == name & athlete100mM$Class == "Juniors"), "Result"]))/(11.19 - WR100mMenJ)
  }
  print(result[i])
  classJ[i] <- "Juniors"
  wrJ[i] <- 1
  srJ[i] <- (11.19 - SR100mMenJ)/(11.19-WR100mMenJ)

  i <- i + 1
}
i <- 1
a_nameY <- c()
resultY <- c()
classY <- c()
wrY <- c()
srY <- c()
for (name in athlete_list_M){
  a_nameY[i] <- name
  if (min(athlete100mM[which(athlete100mM$Name == name & athlete100mM$Class == "Youth"), "Result"]) == Inf){
    resultY[i] <- 0
  }
  else{
    resultY[i] <- (11.19 - min(athlete100mM[which(athlete100mM$Name == name & athlete100mM$Class == "Youth"), "Result"]))/(11.19 - WR100mMenY)
  }
  classY[i] <- "Youth"
  wrY[i] <- 1
  srY[i] <- (11.19 - SR100mMenY)/(11.19-WR100mMenY)
  i <- i + 1
}
i <- 1
radar_frameS <- data.frame(Name = a_name,
                          Result = result, 
                          Class = class, 
                          WR = wr,
                          SR = sr)
radar_frameJ <- data.frame(Name = a_nameJ,
                           Result = resultJ, 
                           Class = classJ, 
                           WR = wrJ,
                           SR = srJ)
radar_frameY <- data.frame(Name = a_nameY,
                           Result = resultY, 
                           Class = classY, 
                           WR = wrY,
                           SR = srY)

radar_frame <- merge(x = merge(x = radar_frameS, y = radar_frameJ, by = "Name"), y = radar_frameY, by = "Name")

radar_frame <- subset(radar_frame, select = -c(Class.x, WR.x, SR.x, Class.y, WR.y, SR.y, Class, WR, SR))

colnames(radar_frame) <- c("Name", "Seniors", "Juniors", "Youth")

radar_frame[(length(radar_frame$Name) + 1), ] <- c("World Record in the class", 1,1,1)
radar_frame[(length(radar_frame$Name) + 1), ] <- c("Fictious baseline", 0,0,0)
radar_frame[(length(radar_frame$Name) + 1), ] <- c("Swiss Record in the class", (11.19 - SR100mMen)/(11.19-WR100mMen), (11.19 - SR100mMenJ)/(11.19-WR100mMenJ), (11.19 - SR100mMenY)/(11.19-WR100mMenY))
radar_frame$Seniors <- as.numeric(radar_frame$Seniors)
radar_frame$Juniors <- as.numeric(radar_frame$Juniors)
radar_frame$Youth <- as.numeric(radar_frame$Youth)
radar_frame

radarchart(radar_frame[which(radar_frame$Name == "Silvan Wicki" | radar_frame$Name == "World Record in the class"
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
legend(1,1, legend=unique(radar_frame[which(radar_frame$Name == "Silvan Wicki" | radar_frame$Name == "World Record in the class"
                                              | radar_frame$Name == "Fictious baseline" | radar_frame$Name == "Swiss Record in the class"), "Name"])
       , seg.len=0.5, pch=1, border = "black",
       bty="n" ,lwd=3, y.intersp=0.75, horiz=FALSE, col =c("red", "gold", "lightblue", "darkred")
       )


ggplot() +
  geom_point(data = athlete100mM, aes(x = Year, y = Result, colour = Class))

# Good evidence that we are working on the right direction
fit <- lm(data = athlete100mM[which(athlete100mM$Class == "Seniors"), ], 
          formula = Result ~ Year)
summary(fit)

# We are on the opposite trend, trying to understand why
fit <- lm(data = athlete100mM[which(athlete100mM$Class == "Juniors"), ], 
          formula = Result ~ Year)
summary(fit)

# Interesting result. We are working in the right direction
fit <- lm(data = athlete100mM[which(athlete100mM$Class == "Youth"), ], 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = athlete100mM[which(athlete100mM$Name == athlete_list_M[[5]]), ],
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

athlete100mM[, "Difference_8"] <- athlete100mM$Result - athlete100mM$Top_8

ggplot() +
  geom_point(data = athlete100mM, aes(x = Year, y = Difference_8, colour = Class))

# No evidence to say anything
fit <- lm(data = athlete100mM[which(athlete100mM$Class == "Seniors"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

# We are on the opposite way, every year we do worst then the year before. 
fit <- lm(data = athlete100mM[which(athlete100mM$Class == "Juniors"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

# Interesting result. We are working in the right direction
fit <- lm(data = athlete100mM[which(athlete100mM$Class == "Youth"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

p <- ggplot(data = athlete100mM)+
  geom_histogram(aes(x = Difference_8, fill = Class), binwidth = .05) +
  geom_histogram(data = athlete100mM[which(athlete100mM$Name == athlete_list_M[[5]]), ],
                 aes(x = Difference_8), binwidth = 0.05) +
  scale_fill_manual(values = heat.colors(4))  +
  theme(
    legend.title = element_text(
      family = "Calibri",
      face = "bold",
      size = 12),
    legend.background = element_rect(fill = "white", colour = "black"),
  )

ggplotly(p)

#######################################################################################################################
###################################################100m Women##########################################################
#######################################################################################################################



sprint100mWoman <- olympics[which(olympics$Discipline == "100m"), ]
sprint100mWoman <- sprint100mWoman[which(sprint100mWoman$Year>=1990), ]
sprint100mWoman <- sprint100mWoman[which(sprint100mWoman$Competition == "Olympic Games" 
                                       | sprint100mWoman$Competition == "World Championships"), ]
sprint100mWoman <- sprint100mWoman[which(sprint100mWoman$Gender == "Women"), ]
sprint100mWoman <- sprint100mWoman[which(sprint100mWoman$Class == "Seniors"), ]
sprint100mWoman <- sprint100mWoman[which(sprint100mWoman$Result != ""), ]
# sprint100mWoman <- sprint100mWoman[which(sprint100mWoman$Medal != ""), ]
sprint100mWoman[, "Result"] <- as.double(sprint100mWoman$Result)
summary(sprint100mWoman$Result)

sprint100mWomanJunior <- olympics[which(olympics$Discipline == "100m"), ]
sprint100mWomanJunior <- sprint100mWomanJunior[which(sprint100mWomanJunior$Year>=1990), ]
sprint100mWomanJunior <- sprint100mWomanJunior[which(sprint100mWomanJunior$Competition == "World Junior Championships"), ]
sprint100mWomanJunior <- sprint100mWomanJunior[which(sprint100mWomanJunior$Gender == "Women"), ]
sprint100mWomanJunior <- sprint100mWomanJunior[which(sprint100mWomanJunior$Class == "Juniors"), ]
sprint100mWomanJunior <- sprint100mWomanJunior[which(sprint100mWomanJunior$Result != ""), ]
# sprint100mWomanJunior <- sprint100mWomanJunior[which(sprint100mWomanJunior$Medal != ""), ]
sprint100mWomanJunior[, "Result"] <- as.double(sprint100mWomanJunior$Result)
summary(sprint100mWomanJunior$Result)

sprint100mWomanYouth <- olympics[which(olympics$Discipline == "100m"), ]
sprint100mWomanYouth <- sprint100mWomanYouth[which(sprint100mWomanYouth$Year>=1990), ]
sprint100mWomanYouth <- sprint100mWomanYouth[which(sprint100mWomanYouth$Competition == "World Youth Championships"
                                                 | sprint100mWomanYouth$Competition == "Youth Olympic Games"), ]
sprint100mWomanYouth <- sprint100mWomanYouth[which(sprint100mWomanYouth$Gender == "Women"), ]
sprint100mWomanYouth <- sprint100mWomanYouth[which(sprint100mWomanYouth$Class == "Youth"), ]
sprint100mWomanYouth <- sprint100mWomanYouth[which(sprint100mWomanYouth$Result != ""), ]
# sprint100mWomanYouth <- sprint100mWomanYouth[which(sprint100mWomanYouth$Medal != ""), ]
sprint100mWomanYouth[, "Result"] <- as.double(sprint100mWomanYouth$Result)
summary(sprint100mWomanYouth$Result)


i <- 2
j <- 2
k <- 2
vector_senior_year <- c(min(sprint100mWoman[which(sprint100mWoman$Year == 2000), "Result"]))
vector_junior_year <- c(min(sprint100mWomanJunior[which(sprint100mWomanJunior$Year == 2000), "Result"]))
vector_youth_year <- c(min(sprint100mWomanYouth[which(sprint100mWomanYouth$Year == 2001), "Result"]))
for (year in 2001:2022){
  vector_senior_year[i] <- min(c(min(sprint100mWoman[which(sprint100mWoman$Year == year), "Result"]), vector_senior_year[i-1]))
  i <- i + 1
  vector_junior_year[j] <- min(c(min(sprint100mWomanJunior[which(sprint100mWomanJunior$Year == year), "Result"]), vector_junior_year[j-1]))
  j <- j + 1
  vector_youth_year[k] <- min(c(min(sprint100mWomanYouth[which(sprint100mWomanYouth$Year == year), "Result"]), vector_youth_year[k-1]))
  k <- k + 1
}

i <- 2
j <- 2
k <- 2
vector_senior_year_8 <- c(sprint100mWoman[which(sprint100mWoman$Year == 1999 & sprint100mWoman$Rank == "8"), "Result"])
vector_junior_year_8 <- c(sprint100mWomanJunior[which(sprint100mWomanJunior$Year == 2000 & sprint100mWomanJunior$Rank == "8"), "Result"])
vector_youth_year_8 <- c(sprint100mWomanYouth[which(sprint100mWomanYouth$Year == 2003 & sprint100mWomanYouth$Rank == "8"), "Result"])
for (year in 2001:2022){
  vector_senior_year_8[i] <- min(c(sprint100mWoman[which(sprint100mWoman$Year == year & sprint100mWoman$Rank == "8"), "Result"], vector_senior_year_8[i-1]))
  i <- i + 1
  vector_junior_year_8[j] <- min(c(sprint100mWomanJunior[which(sprint100mWomanJunior$Year == year & sprint100mWomanJunior$Rank == "8"), "Result"], vector_junior_year_8[j-1]))
  j <- j + 1
  vector_youth_year_8[k] <- min(c(sprint100mWomanYouth[which(sprint100mWomanYouth$Year == year & sprint100mWomanYouth$Rank == "8"), "Result"], vector_youth_year_8[k-1]))
  k <- k + 1
}



vector_senior_year_8
vector_junior_year_8
vector_youth_year_8
vector_best_year <- c(vector_senior_year, vector_junior_year, vector_youth_year)
vector_8_year <- c(vector_senior_year_8, vector_junior_year_8, vector_youth_year_8)
v1 <- 2000:2022
years <- c(v1,v1,v1)
years
vector_class <- c()
vector_class[1:23] <- "Seniors"
vector_class[24:46] <- "Juniors"
vector_class[47:69] <- "Youth"
vector_class

df_result <- data.frame(Year = years,
                        Top_8 = vector_8_year,
                        Top_1 = vector_best_year,
                        Class = vector_class)
df_result

switzerland_athlete_Women_100m <- olympics[which(olympics$Discipline == "100m"), ]
switzerland_athlete_Women_100m <- switzerland_athlete_Women_100m[which(switzerland_athlete_Women_100m$Year>=2000), ]
switzerland_athlete_Women_100m <- switzerland_athlete_Women_100m[which(switzerland_athlete_Women_100m$Gender == "Women"), ]
switzerland_athlete_Women_100m <- switzerland_athlete_Women_100m[which(switzerland_athlete_Women_100m$Nationality == "SUI"), ]
switzerland_athlete_Women_100m <- switzerland_athlete_Women_100m[which(switzerland_athlete_Women_100m$Result != ""), ]
# sprint100mWoman <- sprint100mWoman[which(sprint100mWoman$Medal != ""), ]
switzerland_athlete_Women_100m[, "Result"] <- as.double(switzerland_athlete_Women_100m$Result)
summary(switzerland_athlete_Women_100m$Result)
table(switzerland_athlete_Women_100m$Person)


switzerland_athlete_Women_100m <- data.frame(Name = switzerland_athlete_Women_100m$Person,
                                           Result = switzerland_athlete_Women_100m$Result,
                                           Year = switzerland_athlete_Women_100m$Year,
                                           Class = switzerland_athlete_Women_100m$Class)

switzerland_athlete_Women_100m <- merge(x = switzerland_athlete_Women_100m,
                                      y = df_result, 
                                      by = c("Year", "Class")
)
switzerland_athlete_Women_100m


# Da fare in shiny con un input sul nome dell'atleta.

athlete_list_W <- as.list(unique(switzerland_athlete_Women_100m$Name))
athlete_result <- c()
athlete_name <- c()
athlete_year <- c()
athlete_class <- c()
top8 <- c()
top1 <- c()
i = 1
for (name in athlete_list_W){
  print(name)
  for (year in min(switzerland_athlete_Women_100m$Year):max(switzerland_athlete_Women_100m$Year)){
    print(year)
    if(length(switzerland_athlete_Women_100m[which(switzerland_athlete_Women_100m$Name == name 
                                                 & switzerland_athlete_Women_100m$Year == year), "Result"] != 0)){
      athlete_name[i] <- name
      
      athlete_result[i] <- min(switzerland_athlete_Women_100m[which(switzerland_athlete_Women_100m$Name == name
                                                                  & switzerland_athlete_Women_100m$Year == year),"Result"])
      athlete_year[i] <- year
      athlete_class[i] <- switzerland_athlete_Women_100m[which(switzerland_athlete_Women_100m$Result == athlete_result[i] &
                                                               switzerland_athlete_Women_100m$Name == name & 
                                                               switzerland_athlete_Women_100m$Year == year),"Class"]
      top1[i] <- switzerland_athlete_Women_100m[which(switzerland_athlete_Women_100m$Result == athlete_result[i] &
                                                      switzerland_athlete_Women_100m$Name == name & 
                                                      switzerland_athlete_Women_100m$Year == year),"Top_1"]
      print(top1[i])
      top8[i] <- switzerland_athlete_Women_100m[which(switzerland_athlete_Women_100m$Result == athlete_result[i] &
                                                      switzerland_athlete_Women_100m$Name == name & 
                                                      switzerland_athlete_Women_100m$Year == year),"Top_8"]
      i <- i + 1
    }
  }
}
athlete100mW <- data.frame(Year = athlete_year,
                           Name = athlete_name,
                           Result = athlete_result,
                           Class = athlete_class,
                           Top_8 = top8,
                           Top_1 = top1)
athlete100mW

WR100mWomen <- min(sprint100mWoman$Result)
WR100mWomenJ <- min(sprint100mWomanJunior$Result)
WR100mWomenY <- min(sprint100mWomanYouth$Result)

SR100mWomen <- min(athlete100mW$Result[which(athlete100mW$Class == "Seniors")])
SR100mWomenJ <- min(athlete100mW$Result[which(athlete100mW$Class == "Juniors")])
SR100mWomenY <- min(athlete100mW$Result[which(athlete100mW$Class == "Youth")])

i <- 1
a_name <- c()
result <- c()
class <- c()
wr <- c()
sr <- c()
for (name in athlete_list_W){
  a_name[i] <- name
  if (min(athlete100mW[which(athlete100mW$Name == name & athlete100mW$Class == "Seniors"), "Result"]) == Inf){
    result[i] <- 0
  }
  else{
    result[i] <- (12.43 - min(athlete100mW[which(athlete100mW$Name == name & athlete100mW$Class == "Seniors"), "Result"]))/(12.43 - WR100mWomen)
  }
  print(result[i])
  class[i] <- "Seniors"
  wr[i] <- 1
  sr[i] <- (12.43 - SR100mWomen)/(12.43-WR100mWomen)
  i <- i + 1
}
i <- 1
a_nameJ <- c()
resultJ <- c()
classJ <- c()
wrJ <- c()
srJ <- c()
for (name in athlete_list_W){
  a_nameJ[i] <- name
  if (min(athlete100mW[which(athlete100mW$Name == name & athlete100mW$Class == "Juniors"), "Result"]) == Inf){
    resultJ[i] <- 0
  }
  else{
    resultJ[i] <- (12.43 - min(athlete100mW[which(athlete100mW$Name == name & athlete100mW$Class == "Juniors"), "Result"]))/(12.43 - WR100mWomenJ)
  }
  print(result[i])
  classJ[i] <- "Juniors"
  wrJ[i] <- 1
  srJ[i] <- (12.43 - SR100mWomenJ)/(12.43-WR100mWomenJ)
  
  i <- i + 1
}
i <- 1
a_nameY <- c()
resultY <- c()
classY <- c()
wrY <- c()
srY <- c()
for (name in athlete_list_W){
  a_nameY[i] <- name
  if (min(athlete100mW[which(athlete100mW$Name == name & athlete100mW$Class == "Youth"), "Result"]) == Inf){
    resultY[i] <- 0
  }
  else{
    resultY[i] <- (12.43 - min(athlete100mW[which(athlete100mW$Name == name & athlete100mW$Class == "Youth"), "Result"]))/(12.43 - WR100mWomenY)
  }
  classY[i] <- "Youth"
  wrY[i] <- 1
  srY[i] <- (12.43 - SR100mWomenY)/(12.43-WR100mWomenY)
  i <- i + 1
}
i <- 1
radar_frameS <- data.frame(Name = a_name,
                          Result = result, 
                          Class = class, 
                          WR = wr,
                          SR = sr)
radar_frameJ <- data.frame(Name = a_nameJ,
                           Result = resultJ, 
                           Class = classJ, 
                           WR = wrJ,
                           SR = srJ)
radar_frameY <- data.frame(Name = a_nameY,
                           Result = resultY, 
                           Class = classY, 
                           WR = wrY,
                           SR = srY)

radar_frameW <- merge(x = merge(x = radar_frameS, y = radar_frameJ, by = "Name"), y = radar_frameY, by = "Name")
radar_frameW

radar_frameW <- subset(radar_frameW, select = -c(Class.x, WR.x, SR.x, Class.y, WR.y, SR.y, Class, WR, SR))

colnames(radar_frameW) <- c("Name", "Seniors", "Juniors", "Youth")

radar_frameW[(length(radar_frameW$Name) + 1), ] <- c("World Record in the class", 1,1,1)
radar_frameW[(length(radar_frameW$Name) + 1), ] <- c("Fictious baseline", 0,0,0)
radar_frameW[(length(radar_frameW$Name) + 1), ] <- c("Swiss Record in the class", (12.43 - SR100mWomen)/(12.43-WR100mWomen), (12.43 - SR100mWomenJ)/(12.43-WR100mWomenJ), (12.43 - SR100mWomenY)/(12.43-WR100mWomenY))
radar_frameW$Seniors <- as.numeric(radar_frameW$Seniors)
radar_frameW$Juniors <- as.numeric(radar_frameW$Juniors)
radar_frameW$Youth <- as.numeric(radar_frameW$Youth)
radar_frameW

radarchart(radar_frameW[which(radar_frameW$Name == "Mujinga Kambundji" | radar_frameW$Name == "World Record in the class"
                             | radar_frameW$Name == "Fictious baseline" | radar_frameW$Name == "Swiss Record in the class"), c("Seniors", "Juniors", "Youth") ],
           maxmin = FALSE,
           axistype = 1,
           vlabels = c("Seniors", "Juniors", "Youth"),
           vlcex = 1.1,
           pcol = c("red", "gold", "lightblue", "darkred"),
           plwd = c(3,2,1,2),
           plty = c("solid", "dashed", "dashed"),
           caxislabels = c("0%", "25%", "50%", "75%", "100%"),
           axislabcol = 'grey', 
           cglwd = 0.6
)
legend(1,1, legend=unique(radar_frameW[which(radar_frameW$Name == "Mujinga Kambundji" | radar_frameW$Name == "World Record in the class"
                                            | radar_frameW$Name == "Fictious baseline" | radar_frameW$Name == "Swiss Record in the class"), "Name"])
       , seg.len=0.5, pch=1, border = "black",
       bty="n" ,lwd=3, y.intersp=0.75, horiz=FALSE, col =c("red", "gold", "lightblue", "darkred")
)


ggplot() +
  geom_point(data = athlete100mW, aes(x = Year, y = Result, colour = Class))

# We are doing better, but it is not that significant 1%
fit <- lm(data = athlete100mW[which(athlete100mW$Class == "Seniors"), ], 
          formula = Result ~ Year)
summary(fit)

# No significance to state anything
fit <- lm(data = athlete100mW[which(athlete100mW$Class == "Juniors"), ], 
          formula = Result ~ Year)
summary(fit)

# No significance to state anything
fit <- lm(data = athlete100mW[which(athlete100mW$Class == "Youth"), ], 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = athlete100mW[which(athlete100mW$Name == athlete_list_W[[12]]), ],
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

athlete100mW[, "Difference_8"] <- athlete100mW$Result - athlete100mW$Top_8

ggplot() +
  geom_point(data = athlete100mW, aes(x = Year, y = Difference_8, colour = Class))

# very low evidence, but still something 
fit <- lm(data = athlete100mW[which(athlete100mW$Class == "Seniors"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

# no significance to state anything 
fit <- lm(data = athlete100mW[which(athlete100mW$Class == "Juniors"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

# No significance to state anything 
fit <- lm(data = athlete100mW[which(athlete100mW$Class == "Youth"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

p <- ggplot(data = athlete100mW)+
  geom_histogram(aes(x = Difference_8, fill = Class), binwidth = .05) +
  geom_histogram(data = athlete100mW[which(athlete100mW$Name == athlete_list_W[[12]]), ],
                 aes(x = Difference_8), binwidth = 0.05) +
  scale_fill_manual(values = heat.colors(4)) 

ggplotly(p)

################################################################################
################################ 200m Male #####################################
################################################################################

sprint200mMale <- olympics[which(olympics$Discipline == "200m"), ]
sprint200mMale <- sprint200mMale[which(sprint200mMale$Year>=1990), ]
sprint200mMale <- sprint200mMale[which(sprint200mMale$Competition == "Olympic Games" 
                                       | sprint200mMale$Competition == "World Championships"), ]
sprint200mMale <- sprint200mMale[which(sprint200mMale$Gender == "Men"), ]
sprint200mMale <- sprint200mMale[which(sprint200mMale$Class == "Seniors"), ]
sprint200mMale <- sprint200mMale[which(sprint200mMale$Result != ""), ]
# sprint200mMale <- sprint200mMale[which(sprint200mMale$Medal != ""), ]
sprint200mMale[, "Result"] <- as.double(sprint200mMale$Result)
sprint200mMale <- sprint200mMale[which(!is.na(sprint200mMale$Result)), ]
summary(sprint200mMale$Result[which(!is.na(sprint200mMale$Result))])

sprint200mMaleJunior <- olympics[which(olympics$Discipline == "200m"), ]
sprint200mMaleJunior <- sprint200mMaleJunior[which(sprint200mMaleJunior$Year>=1990), ]
sprint200mMaleJunior <- sprint200mMaleJunior[which(sprint200mMaleJunior$Competition == "World Junior Championships"), ]
sprint200mMaleJunior <- sprint200mMaleJunior[which(sprint200mMaleJunior$Gender == "Men"), ]
sprint200mMaleJunior <- sprint200mMaleJunior[which(sprint200mMaleJunior$Class == "Juniors"), ]
sprint200mMaleJunior <- sprint200mMaleJunior[which(sprint200mMaleJunior$Result != ""), ]
# sprint200mMaleJunior <- sprint200mMaleJunior[which(sprint200mMaleJunior$Medal != ""), ]
sprint200mMaleJunior[, "Result"] <- as.double(sprint200mMaleJunior$Result)
sprint200mMaleJunior <- sprint200mMaleJunior[which(!is.na(sprint200mMaleJunior$Result)), ]
summary(sprint200mMaleJunior$Result)

sprint200mMaleYouth <- olympics[which(olympics$Discipline == "200m"), ]
sprint200mMaleYouth <- sprint200mMaleYouth[which(sprint200mMaleYouth$Year>=1990), ]
sprint200mMaleYouth <- sprint200mMaleYouth[which(sprint200mMaleYouth$Competition == "World Youth Championships"
                                                 | sprint200mMaleYouth$Competition == "Youth Olympic Games"), ]
sprint200mMaleYouth <- sprint200mMaleYouth[which(sprint200mMaleYouth$Gender == "Men"), ]
sprint200mMaleYouth <- sprint200mMaleYouth[which(sprint200mMaleYouth$Class == "Youth"), ]
sprint200mMaleYouth <- sprint200mMaleYouth[which(sprint200mMaleYouth$Result != ""), ]
# sprint200mMaleYouth <- sprint200mMaleYouth[which(sprint200mMaleYouth$Medal != ""), ]
sprint200mMaleYouth[, "Result"] <- as.double(sprint200mMaleYouth$Result)
summary(sprint200mMaleYouth$Result)


i <- 2
j <- 2
k <- 2
vector_senior_year <- c(min(sprint200mMale[which(sprint200mMale$Year == 2000), "Result"]))
vector_junior_year <- c(min(sprint200mMaleJunior[which(sprint200mMaleJunior$Year == 2000), "Result"]))
vector_youth_year <- c(min(sprint200mMaleYouth[which(sprint200mMaleYouth$Year == 2001), "Result"]))
for (year in 2001:2022){
  vector_senior_year[i] <- min(c(min(sprint200mMale[which(sprint200mMale$Year == year), "Result"]), vector_senior_year[i-1]))
  i <- i + 1
  vector_junior_year[j] <- min(c(min(sprint200mMaleJunior[which(sprint200mMaleJunior$Year == year), "Result"]), vector_junior_year[j-1]))
  j <- j + 1
  vector_youth_year[k] <- min(c(min(sprint200mMaleYouth[which(sprint200mMaleYouth$Year == year), "Result"]), vector_youth_year[k-1]))
  k <- k + 1
}

i <- 2
j <- 2
k <- 2
vector_senior_year_8 <- c(sprint200mMale[which(sprint200mMale$Year == 2000 & sprint200mMale$Rank == "8"), "Result"])
vector_junior_year_8 <- c(sprint200mMaleJunior[which(sprint200mMaleJunior$Year == 2000 & sprint200mMaleJunior$Rank == "8"), "Result"])
vector_youth_year_8 <- c(sprint200mMaleYouth[which(sprint200mMaleYouth$Year == 2009 & sprint200mMaleYouth$Rank == "8"), "Result"])
for (year in 2001:2022){
  vector_senior_year_8[i] <- min(c(sprint200mMale[which(sprint200mMale$Year == year & sprint200mMale$Rank == "8"), "Result"], vector_senior_year_8[i-1]))
  i <- i + 1
  vector_junior_year_8[j] <- min(c(sprint200mMaleJunior[which(sprint200mMaleJunior$Year == year & sprint200mMaleJunior$Rank == "8"), "Result"], vector_junior_year_8[j-1]))
  j <- j + 1
  vector_youth_year_8[k] <- min(c(sprint200mMaleYouth[which(sprint200mMaleYouth$Year == year & sprint200mMaleYouth$Rank == "8"), "Result"], vector_youth_year_8[k-1]))
  k <- k + 1
}


vector_best_year <- c(vector_senior_year, vector_junior_year, vector_youth_year)
vector_8_year <- c(vector_senior_year_8, vector_junior_year_8, vector_youth_year_8)
v1 <- 2000:2022
years <- c(v1,v1,v1)
years
vector_class <- c()
vector_class[1:23] <- "Seniors"
vector_class[24:46] <- "Juniors"
vector_class[47:69] <- "Youth"
vector_class

df_result <- data.frame(Year = years,
                        Top_8 = vector_8_year,
                        Top_1 = vector_best_year,
                        Class = vector_class)
df_result

switzerland_athlete_Men_200m <- olympics[which(olympics$Discipline == "200m"), ]
switzerland_athlete_Men_200m <- switzerland_athlete_Men_200m[which(switzerland_athlete_Men_200m$Year>=2000), ]
switzerland_athlete_Men_200m <- switzerland_athlete_Men_200m[which(switzerland_athlete_Men_200m$Gender == "Men"), ]
switzerland_athlete_Men_200m <- switzerland_athlete_Men_200m[which(switzerland_athlete_Men_200m$Nationality == "SUI"), ]
switzerland_athlete_Men_200m <- switzerland_athlete_Men_200m[which(switzerland_athlete_Men_200m$Result != ""), ]
# sprint200mMale <- sprint200mMale[which(sprint200mMale$Medal != ""), ]
switzerland_athlete_Men_200m[, "Result"] <- as.double(switzerland_athlete_Men_200m$Result)
summary(switzerland_athlete_Men_200m$Result)
table(switzerland_athlete_Men_200m$Person)


switzerland_athlete_Men_200m <- data.frame(Name = switzerland_athlete_Men_200m$Person,
                                           Result = switzerland_athlete_Men_200m$Result,
                                           Year = switzerland_athlete_Men_200m$Year,
                                           Class = switzerland_athlete_Men_200m$Class)

switzerland_athlete_Men_200m <- merge(x = switzerland_athlete_Men_200m,
                                      y = df_result, 
                                      by = c("Year", "Class")
)
switzerland_athlete_Men_200m


# Da fare in shiny con un input sul nome dell'atleta.

athlete_list_M_200m <- as.list(unique(switzerland_athlete_Men_200m$Name))
athlete_result <- c()
athlete_name <- c()
athlete_year <- c()
athlete_class <- c()
top8 <- c()
top1 <- c()
i = 1
for (name in athlete_list_M_200m){
  print(name)
  for (year in min(switzerland_athlete_Men_200m$Year):max(switzerland_athlete_Men_200m$Year)){
    print(year)
    if(length(switzerland_athlete_Men_200m[which(switzerland_athlete_Men_200m$Name == name 
                                                 & switzerland_athlete_Men_200m$Year == year), "Result"] != 0)){
      athlete_name[i] <- name
      
      athlete_result[i] <- min(switzerland_athlete_Men_200m[which(switzerland_athlete_Men_200m$Name == name
                                                                  & switzerland_athlete_Men_200m$Year == year),"Result"])
      athlete_year[i] <- year
      athlete_class[i] <- switzerland_athlete_Men_200m[which(switzerland_athlete_Men_200m$Result == athlete_result[i] &
                                                               switzerland_athlete_Men_200m$Name == name & 
                                                               switzerland_athlete_Men_200m$Year == year),"Class"]
      top1[i] <- switzerland_athlete_Men_200m[which(switzerland_athlete_Men_200m$Result == athlete_result[i] &
                                                      switzerland_athlete_Men_200m$Name == name & 
                                                      switzerland_athlete_Men_200m$Year == year),"Top_1"]
      print(top1[i])
      top8[i] <- switzerland_athlete_Men_200m[which(switzerland_athlete_Men_200m$Result == athlete_result[i] &
                                                      switzerland_athlete_Men_200m$Name == name & 
                                                      switzerland_athlete_Men_200m$Year == year),"Top_8"]
      i <- i + 1
    }
  }
}
athlete200mM <- data.frame(Year = athlete_year,
                           Name = athlete_name,
                           Result = athlete_result,
                           Class = athlete_class,
                           Top_8 = top8,
                           Top_1 = top1)
athlete200mM

WR200mMen <- min(sprint200mMale$Result)
WR200mMenJ <- min(sprint200mMaleJunior$Result)
WR200mMenY <- min(sprint200mMaleYouth$Result)

SR200mMen <- min(athlete200mM$Result[which(athlete200mM$Class == "Seniors")])
SR200mMenJ <- min(athlete200mM$Result[which(athlete200mM$Class == "Juniors")])
SR200mMenY <- min(athlete200mM$Result[which(athlete200mM$Class == "Youth")])

i <- 1
a_name <- c()
result <- c()
class <- c()
wr <- c()
sr <- c()
for (name in athlete_list_M_200m){
  a_name[i] <- name
  if (min(athlete200mM[which(athlete200mM$Name == name & athlete200mM$Class == "Seniors"), "Result"]) == Inf){
    result[i] <- 0
  }
  else{
    result[i] <- (22.97 - min(athlete200mM[which(athlete200mM$Name == name & athlete200mM$Class == "Seniors"), "Result"]))/(22.97 - WR200mMen)
  }
  print(result[i])
  class[i] <- "Seniors"
  wr[i] <- 1
  sr[i] <- (22.97 - SR200mMen)/(22.97-WR200mMen)
  i <- i + 1
}
i <- 1
a_nameJ <- c()
resultJ <- c()
classJ <- c()
wrJ <- c()
srJ <- c()
for (name in athlete_list_M_200m){
  a_nameJ[i] <- name
  if (min(athlete200mM[which(athlete200mM$Name == name & athlete200mM$Class == "Juniors"), "Result"]) == Inf){
    resultJ[i] <- 0
  }
  else{
    resultJ[i] <- (22.97 - min(athlete200mM[which(athlete200mM$Name == name & athlete200mM$Class == "Juniors"), "Result"]))/(22.97 - WR200mMenJ)
  }
  print(result[i])
  classJ[i] <- "Juniors"
  wrJ[i] <- 1
  srJ[i] <- (22.97 - SR200mMenJ)/(22.97-WR200mMenJ)
  
  i <- i + 1
}
i <- 1
a_nameY <- c()
resultY <- c()
classY <- c()
wrY <- c()
srY <- c()
for (name in athlete_list_M_200m){
  a_nameY[i] <- name
  if (min(athlete200mM[which(athlete200mM$Name == name & athlete200mM$Class == "Youth"), "Result"]) == Inf){
    resultY[i] <- 0
  }
  else{
    resultY[i] <- (22.97 - min(athlete200mM[which(athlete200mM$Name == name & athlete200mM$Class == "Youth"), "Result"]))/(22.97 - WR200mMenY)
  }
  classY[i] <- "Youth"
  wrY[i] <- 1
  srY[i] <- (22.97 - SR200mMenY)/(22.97-WR200mMenY)
  i <- i + 1
}
i <- 1
radar_frameS <- data.frame(Name = a_name,
                           Result = result, 
                           Class = class, 
                           WR = wr,
                           SR = sr)
radar_frameJ <- data.frame(Name = a_nameJ,
                           Result = resultJ, 
                           Class = classJ, 
                           WR = wrJ,
                           SR = srJ)
radar_frameY <- data.frame(Name = a_nameY,
                           Result = resultY, 
                           Class = classY, 
                           WR = wrY,
                           SR = srY)

radar_frame200 <- merge(x = merge(x = radar_frameS, y = radar_frameJ, by = "Name"), y = radar_frameY, by = "Name")

radar_frame200 <- subset(radar_frame200, select = -c(Class.x, WR.x, SR.x, Class.y, WR.y, SR.y, Class, WR, SR))

colnames(radar_frame200) <- c("Name", "Seniors", "Juniors", "Youth")

radar_frame200[(length(radar_frame200$Name) + 1), ] <- c("World Record in the class", 1,1,1)
radar_frame200[(length(radar_frame200$Name) + 1), ] <- c("Fictious baseline", 0,0,0)
radar_frame200[(length(radar_frame200$Name) + 1), ] <- c("Swiss Record in the class", (22.97 - SR200mMen)/(22.97-WR200mMen), (22.97 - SR200mMenJ)/(22.97-WR200mMenJ), (22.97 - SR200mMenY)/(22.97-WR200mMenY))
radar_frame200$Seniors <- as.numeric(radar_frame200$Seniors)
radar_frame200$Juniors <- as.numeric(radar_frame200$Juniors)
radar_frame200$Youth <- as.numeric(radar_frame200$Youth)
radar_frame200

radarchart(radar_frame200[which(radar_frame200$Name == "Silvan Wicki" | radar_frame200$Name == "World Record in the class"
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
legend(1,1, legend=unique(radar_frame200[which(radar_frame200$Name == "Silvan Wicki" | radar_frame200$Name == "World Record in the class"
                                            | radar_frame200$Name == "Fictious baseline" | radar_frame200$Name == "Swiss Record in the class"), "Name"])
       , seg.len=0.5, pch=1, border = "black",
       bty="n" ,lwd=3, y.intersp=0.75, horiz=FALSE, col =c("red", "gold", "lightblue", "darkred")
)


ggplot() +
  geom_point(data = athlete200mM, aes(x = Year, y = Result, colour = Class))

# We are doing better, but it is not that significant 3%
fit <- lm(data = athlete200mM[which(athlete200mM$Class == "Seniors"), ], 
          formula = Result ~ Year)
summary(fit)

# No significance to state anything
fit <- lm(data = athlete200mM[which(athlete200mM$Class == "Juniors"), ], 
          formula = Result ~ Year)
summary(fit)

# No significance to state anything
fit <- lm(data = athlete200mM[which(athlete200mM$Class == "Youth"), ], 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = athlete200mM[which(athlete200mM$Name == athlete_list_M_200m[[5]]), ],
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

athlete200mM[, "Difference_8"] <- athlete200mM$Result - athlete200mM$Top_8

ggplot() +
  geom_point(data = athlete200mM, aes(x = Year, y = Difference_8, colour = Class))

# no significance 
fit <- lm(data = athlete200mM[which(athlete200mM$Class == "Seniors"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

# opposite direction but low significance
fit <- lm(data = athlete200mM[which(athlete200mM$Class == "Juniors"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

# No significance to state anything 
fit <- lm(data = athlete200mM[which(athlete200mM$Class == "Youth"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

p <- ggplot(data = athlete200mM)+
  geom_histogram(aes(x = Difference_8, fill = Class), binwidth = .05) +
  geom_histogram(data = athlete200mM[which(athlete200mM$Name == athlete_list_M_200m[[5]]), ],
                 aes(x = Difference_8), binwidth = 0.05) +
  scale_fill_manual(values = heat.colors(4)) 

ggplotly(p)

################################################################################
################################ 200m Female ###################################
################################################################################

sprint200mWoman <- olympics[which(olympics$Discipline == "200m"), ]
sprint200mWoman <- sprint200mWoman[which(sprint200mWoman$Year>=1990), ]
sprint200mWoman <- sprint200mWoman[which(sprint200mWoman$Competition == "Olympic Games" 
                                       | sprint200mWoman$Competition == "World Championships"), ]
sprint200mWoman <- sprint200mWoman[which(sprint200mWoman$Gender == "Women"), ]
sprint200mWoman <- sprint200mWoman[which(sprint200mWoman$Class == "Seniors"), ]
sprint200mWoman <- sprint200mWoman[which(sprint200mWoman$Result != ""), ]
# sprint200mWoman <- sprint200mWoman[which(sprint200mWoman$Medal != ""), ]
sprint200mWoman[, "Result"] <- as.double(sprint200mWoman$Result)
sprint200mWoman <- sprint200mWoman[which(!is.na(sprint200mWoman$Result)), ]
summary(sprint200mWoman$Result[which(!is.na(sprint200mWoman$Result))])

sprint200mWomanJunior <- olympics[which(olympics$Discipline == "200m"), ]
sprint200mWomanJunior <- sprint200mWomanJunior[which(sprint200mWomanJunior$Year>=1990), ]
sprint200mWomanJunior <- sprint200mWomanJunior[which(sprint200mWomanJunior$Competition == "World Junior Championships"), ]
sprint200mWomanJunior <- sprint200mWomanJunior[which(sprint200mWomanJunior$Gender == "Women"), ]
sprint200mWomanJunior <- sprint200mWomanJunior[which(sprint200mWomanJunior$Class == "Juniors"), ]
sprint200mWomanJunior <- sprint200mWomanJunior[which(sprint200mWomanJunior$Result != ""), ]
# sprint200mWomanJunior <- sprint200mWomanJunior[which(sprint200mWomanJunior$Medal != ""), ]
sprint200mWomanJunior[, "Result"] <- as.double(sprint200mWomanJunior$Result)
sprint200mWomanJunior <- sprint200mWomanJunior[which(!is.na(sprint200mWomanJunior$Result)), ]
summary(sprint200mWomanJunior$Result)

sprint200mWomanYouth <- olympics[which(olympics$Discipline == "200m"), ]
sprint200mWomanYouth <- sprint200mWomanYouth[which(sprint200mWomanYouth$Year>=1990), ]
sprint200mWomanYouth <- sprint200mWomanYouth[which(sprint200mWomanYouth$Competition == "World Youth Championships"
                                                 | sprint200mWomanYouth$Competition == "Youth Olympic Games"), ]
sprint200mWomanYouth <- sprint200mWomanYouth[which(sprint200mWomanYouth$Gender == "Women"), ]
sprint200mWomanYouth <- sprint200mWomanYouth[which(sprint200mWomanYouth$Class == "Youth"), ]
sprint200mWomanYouth <- sprint200mWomanYouth[which(sprint200mWomanYouth$Result != ""), ]
# sprint200mWomanYouth <- sprint200mWomanYouth[which(sprint200mWomanYouth$Medal != ""), ]
sprint200mWomanYouth[, "Result"] <- as.double(sprint200mWomanYouth$Result)
summary(sprint200mWomanYouth$Result)


i <- 2
j <- 2
k <- 2
vector_senior_year <- c(min(sprint200mWoman[which(sprint200mWoman$Year == 2000), "Result"]))
vector_junior_year <- c(min(sprint200mWomanJunior[which(sprint200mWomanJunior$Year == 2000), "Result"]))
vector_youth_year <- c(min(sprint200mWomanYouth[which(sprint200mWomanYouth$Year == 2001), "Result"]))
for (year in 2001:2022){
  vector_senior_year[i] <- min(c(min(sprint200mWoman[which(sprint200mWoman$Year == year), "Result"]), vector_senior_year[i-1]))
  i <- i + 1
  vector_junior_year[j] <- min(c(min(sprint200mWomanJunior[which(sprint200mWomanJunior$Year == year), "Result"]), vector_junior_year[j-1]))
  j <- j + 1
  vector_youth_year[k] <- min(c(min(sprint200mWomanYouth[which(sprint200mWomanYouth$Year == year), "Result"]), vector_youth_year[k-1]))
  k <- k + 1
}

i <- 2
j <- 2
k <- 2
vector_senior_year_8 <- c(sprint200mWoman[which(sprint200mWoman$Year == 2000 & sprint200mWoman$Rank == "8"), "Result"])
vector_junior_year_8 <- c(sprint200mWomanJunior[which(sprint200mWomanJunior$Year == 2000 & sprint200mWomanJunior$Rank == "8"), "Result"])
vector_youth_year_8 <- c(sprint200mWomanYouth[which(sprint200mWomanYouth$Year == 2003 & sprint200mWomanYouth$Rank == "8"), "Result"])
for (year in 2001:2022){
  vector_senior_year_8[i] <- min(c(sprint200mWoman[which(sprint200mWoman$Year == year & sprint200mWoman$Rank == "8"), "Result"], vector_senior_year_8[i-1]))
  i <- i + 1
  vector_junior_year_8[j] <- min(c(sprint200mWomanJunior[which(sprint200mWomanJunior$Year == year & sprint200mWomanJunior$Rank == "8"), "Result"], vector_junior_year_8[j-1]))
  j <- j + 1
  vector_youth_year_8[k] <- min(c(sprint200mWomanYouth[which(sprint200mWomanYouth$Year == year & sprint200mWomanYouth$Rank == "8"), "Result"], vector_youth_year_8[k-1]))
  k <- k + 1
}


vector_best_year <- c(vector_senior_year, vector_junior_year, vector_youth_year)
vector_8_year <- c(vector_senior_year_8, vector_junior_year_8, vector_youth_year_8)
v1 <- 2000:2022
years <- c(v1,v1,v1)
years
vector_class <- c()
vector_class[1:23] <- "Seniors"
vector_class[24:46] <- "Juniors"
vector_class[47:69] <- "Youth"
vector_class

df_result <- data.frame(Year = years,
                        Top_8 = vector_8_year,
                        Top_1 = vector_best_year,
                        Class = vector_class)
df_result

switzerland_athlete_Women_200m <- olympics[which(olympics$Discipline == "200m"), ]
switzerland_athlete_Women_200m <- switzerland_athlete_Women_200m[which(switzerland_athlete_Women_200m$Year>=2000), ]
switzerland_athlete_Women_200m <- switzerland_athlete_Women_200m[which(switzerland_athlete_Women_200m$Gender == "Women"), ]
switzerland_athlete_Women_200m <- switzerland_athlete_Women_200m[which(switzerland_athlete_Women_200m$Nationality == "SUI"), ]
switzerland_athlete_Women_200m <- switzerland_athlete_Women_200m[which(switzerland_athlete_Women_200m$Result != ""), ]
# sprint200mWoman <- sprint200mWoman[which(sprint200mWoman$Medal != ""), ]
switzerland_athlete_Women_200m[, "Result"] <- as.double(switzerland_athlete_Women_200m$Result)
summary(switzerland_athlete_Women_200m$Result)
table(switzerland_athlete_Women_200m$Person)


switzerland_athlete_Women_200m <- data.frame(Name = switzerland_athlete_Women_200m$Person,
                                           Result = switzerland_athlete_Women_200m$Result,
                                           Year = switzerland_athlete_Women_200m$Year,
                                           Class = switzerland_athlete_Women_200m$Class)

switzerland_athlete_Women_200m <- merge(x = switzerland_athlete_Women_200m,
                                      y = df_result, 
                                      by = c("Year", "Class")
)
switzerland_athlete_Women_200m


# Da fare in shiny con un input sul nome dell'atleta.

athlete_list_W_200m <- as.list(unique(switzerland_athlete_Women_200m$Name))
athlete_result <- c()
athlete_name <- c()
athlete_year <- c()
athlete_class <- c()
top8 <- c()
top1 <- c()
i = 1
for (name in athlete_list_W_200m){
  print(name)
  for (year in min(switzerland_athlete_Women_200m$Year):max(switzerland_athlete_Women_200m$Year)){
    print(year)
    if(length(switzerland_athlete_Women_200m[which(switzerland_athlete_Women_200m$Name == name 
                                                 & switzerland_athlete_Women_200m$Year == year), "Result"] != 0)){
      athlete_name[i] <- name
      
      athlete_result[i] <- min(switzerland_athlete_Women_200m[which(switzerland_athlete_Women_200m$Name == name
                                                                  & switzerland_athlete_Women_200m$Year == year),"Result"])
      athlete_year[i] <- year
      athlete_class[i] <- switzerland_athlete_Women_200m[which(switzerland_athlete_Women_200m$Result == athlete_result[i] &
                                                               switzerland_athlete_Women_200m$Name == name & 
                                                               switzerland_athlete_Women_200m$Year == year),"Class"]
      top1[i] <- switzerland_athlete_Women_200m[which(switzerland_athlete_Women_200m$Result == athlete_result[i] &
                                                      switzerland_athlete_Women_200m$Name == name & 
                                                      switzerland_athlete_Women_200m$Year == year),"Top_1"]
      print(top1[i])
      top8[i] <- switzerland_athlete_Women_200m[which(switzerland_athlete_Women_200m$Result == athlete_result[i] &
                                                      switzerland_athlete_Women_200m$Name == name & 
                                                      switzerland_athlete_Women_200m$Year == year),"Top_8"]
      i <- i + 1
    }
  }
}
athlete200mW <- data.frame(Year = athlete_year,
                           Name = athlete_name,
                           Result = athlete_result,
                           Class = athlete_class,
                           Top_8 = top8,
                           Top_1 = top1)
athlete200mW

WR200mWomen <- min(sprint200mWoman$Result)
WR200mWomenJ <- min(sprint200mWomanJunior$Result)
WR200mWomenY <- min(sprint200mWomanYouth$Result)

SR200mWomen <- min(athlete200mW$Result[which(athlete200mW$Class == "Seniors")])
SR200mWomenJ <- min(athlete200mW$Result[which(athlete200mW$Class == "Juniors")])
SR200mWomenY <- min(athlete200mW$Result[which(athlete200mW$Class == "Youth")])

i <- 1
a_name <- c()
result <- c()
class <- c()
wr <- c()
sr <- c()
for (name in athlete_list_W_200m){
  a_name[i] <- name
  if (min(athlete200mW[which(athlete200mW$Name == name & athlete200mW$Class == "Seniors"), "Result"]) == Inf){
    result[i] <- 0
  }
  else{
    result[i] <- (25.74 - min(athlete200mW[which(athlete200mW$Name == name & athlete200mW$Class == "Seniors"), "Result"]))/(25.74 - WR200mWomen)
  }
  print(result[i])
  class[i] <- "Seniors"
  wr[i] <- 1
  sr[i] <- (25.74 - SR200mWomen)/(25.74-WR200mWomen)
  i <- i + 1
}
i <- 1
a_nameJ <- c()
resultJ <- c()
classJ <- c()
wrJ <- c()
srJ <- c()
for (name in athlete_list_W_200m){
  a_nameJ[i] <- name
  if (min(athlete200mW[which(athlete200mW$Name == name & athlete200mW$Class == "Juniors"), "Result"]) == Inf){
    resultJ[i] <- 0
  }
  else{
    resultJ[i] <- (25.74 - min(athlete200mW[which(athlete200mW$Name == name & athlete200mW$Class == "Juniors"), "Result"]))/(25.74 - WR200mWomenJ)
  }
  print(result[i])
  classJ[i] <- "Juniors"
  wrJ[i] <- 1
  srJ[i] <- (25.74 - SR200mWomenJ)/(25.74-WR200mWomenJ)
  
  i <- i + 1
}
i <- 1
a_nameY <- c()
resultY <- c()
classY <- c()
wrY <- c()
srY <- c()
for (name in athlete_list_W_200m){
  a_nameY[i] <- name
  if (min(athlete200mW[which(athlete200mW$Name == name & athlete200mW$Class == "Youth"), "Result"]) == Inf){
    resultY[i] <- 0
  }
  else{
    resultY[i] <- (25.74 - min(athlete200mW[which(athlete200mW$Name == name & athlete200mW$Class == "Youth"), "Result"]))/(25.74 - WR200mWomenY)
  }
  classY[i] <- "Youth"
  wrY[i] <- 1
  srY[i] <- (25.74 - SR200mWomenY)/(25.74-WR200mWomenY)
  i <- i + 1
}
i <- 1
radar_frameS <- data.frame(Name = a_name,
                           Result = result, 
                           Class = class, 
                           WR = wr,
                           SR = sr)
radar_frameJ <- data.frame(Name = a_nameJ,
                           Result = resultJ, 
                           Class = classJ, 
                           WR = wrJ,
                           SR = srJ)
radar_frameY <- data.frame(Name = a_nameY,
                           Result = resultY, 
                           Class = classY, 
                           WR = wrY,
                           SR = srY)

radar_frame200W <- merge(x = merge(x = radar_frameS, y = radar_frameJ, by = "Name"), y = radar_frameY, by = "Name")
radar_frame200W

radar_frame200W <- subset(radar_frame200W, select = -c(Class.x, WR.x, SR.x, Class.y, WR.y, SR.y, Class, WR, SR))

colnames(radar_frame200W) <- c("Name", "Seniors", "Juniors", "Youth")

radar_frame200W[(length(radar_frame200W$Name) + 1), ] <- c("World Record in the class", 1,1,1)
radar_frame200W[(length(radar_frame200W$Name) + 1), ] <- c("Fictious baseline", 0,0,0)
radar_frame200W[(length(radar_frame200W$Name) + 1), ] <- c("Swiss Record in the class", (25.74 - SR200mWomen)/(25.74-WR200mWomen), (25.74- SR200mWomenJ)/(25.74-WR200mWomenJ), (25.74- SR200mWomenY)/(25.74-WR200mWomenY))
radar_frame200W$Seniors <- as.numeric(radar_frame200W$Seniors)
radar_frame200W$Juniors <- as.numeric(radar_frame200W$Juniors)
radar_frame200W$Youth <- as.numeric(radar_frame200W$Youth)
radar_frame200W

radarchart(radar_frame200W[which(radar_frame200W$Name == "Mujinga Kambundji" | radar_frame200W$Name == "World Record in the class"
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
legend(1,1, legend=unique(radar_frame200W[which(radar_frame200W$Name == "Mujinga Kambundji" | radar_frame200W$Name == "World Record in the class"
                                             | radar_frame200W$Name == "Fictious baseline" | radar_frame200W$Name == "Swiss Record in the class"), "Name"])
       , seg.len=0.5, pch=1, border = "black",
       bty="n" ,lwd=3, y.intersp=1, horiz=FALSE, col =c("red", "gold", "lightblue", "darkred")
)


ggplot() +
  geom_point(data = athlete200mW, aes(x = Year, y = Result, colour = Class))

# No significance to state anything 
fit <- lm(data = athlete200mW[which(athlete200mW$Class == "Seniors"), ], 
          formula = Result ~ Year)
summary(fit)

# No significance to state anything
fit <- lm(data = athlete200mW[which(athlete200mW$Class == "Juniors"), ], 
          formula = Result ~ Year)
summary(fit)

# No significance to state anything
fit <- lm(data = athlete200mW[which(athlete200mW$Class == "Youth"), ], 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = athlete200mW[which(athlete200mW$Name == athlete_list_W_200m[[7]]), ],
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

athlete200mW[, "Difference_8"] <- athlete200mW$Result - athlete200mW$Top_8

ggplot() +
  geom_point(data = athlete200mW, aes(x = Year, y = Difference_8, colour = Class))

# no significance 
fit <- lm(data = athlete200mW[which(athlete200mW$Class == "Seniors"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

# No significance 
fit <- lm(data = athlete200mW[which(athlete200mW$Class == "Juniors"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

# No significance to state anything 
fit <- lm(data = athlete200mW[which(athlete200mW$Class == "Youth"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

p <- ggplot(data = athlete200mW)+
  geom_histogram(aes(x = Difference_8, fill = Class), binwidth = .05) +
  geom_histogram(data = athlete200mW[which(athlete200mW$Name == athlete_list_W_200m[[7]]), ],
                 aes(x = Difference_8), binwidth = 0.05) +
  scale_fill_manual(values = heat.colors(4)) 

ggplotly(p)


################################################################################
################################ 400m Male #####################################
################################################################################

sprint400mMan <- olympics[which(olympics$Discipline == "400m"), ]
sprint400mMan <- sprint400mMan[which(sprint400mMan$Year>=1990), ]
sprint400mMan <- sprint400mMan[which(sprint400mMan$Competition == "Olympic Games" 
                                         | sprint400mMan$Competition == "World Championships"), ]
sprint400mMan <- sprint400mMan[which(sprint400mMan$Gender == "Men"), ]
sprint400mMan <- sprint400mMan[which(sprint400mMan$Class == "Seniors"), ]
sprint400mMan <- sprint400mMan[which(sprint400mMan$Result != ""), ]
# sprint400mMan <- sprint400mMan[which(sprint400mMan$Medal != ""), ]
sprint400mMan[, "Result"] <- as.double(sprint400mMan$Result)
sprint400mMan <- sprint400mMan[which(!is.na(sprint400mMan$Result)), ]
summary(sprint400mMan$Result[which(!is.na(sprint400mMan$Result))])

sprint400mManJunior <- olympics[which(olympics$Discipline == "400m"), ]
sprint400mManJunior <- sprint400mManJunior[which(sprint400mManJunior$Year>=1990), ]
sprint400mManJunior <- sprint400mManJunior[which(sprint400mManJunior$Competition == "World Junior Championships"), ]
sprint400mManJunior <- sprint400mManJunior[which(sprint400mManJunior$Gender == "Men"), ]
sprint400mManJunior <- sprint400mManJunior[which(sprint400mManJunior$Class == "Juniors"), ]
sprint400mManJunior <- sprint400mManJunior[which(sprint400mManJunior$Result != ""), ]
# sprint400mManJunior <- sprint400mManJunior[which(sprint400mManJunior$Medal != ""), ]
sprint400mManJunior[, "Result"] <- as.double(sprint400mManJunior$Result)
sprint400mManJunior <- sprint400mManJunior[which(!is.na(sprint400mManJunior$Result)), ]
summary(sprint400mManJunior$Result)

sprint400mManYouth <- olympics[which(olympics$Discipline == "400m"), ]
sprint400mManYouth <- sprint400mManYouth[which(sprint400mManYouth$Year>=1990), ]
sprint400mManYouth <- sprint400mManYouth[which(sprint400mManYouth$Competition == "World Youth Championships"
                                                   | sprint400mManYouth$Competition == "Youth Olympic Games"), ]
sprint400mManYouth <- sprint400mManYouth[which(sprint400mManYouth$Gender == "Men"), ]
sprint400mManYouth <- sprint400mManYouth[which(sprint400mManYouth$Class == "Youth"), ]
sprint400mManYouth <- sprint400mManYouth[which(sprint400mManYouth$Result != ""), ]
# sprint400mManYouth <- sprint400mManYouth[which(sprint400mManYouth$Medal != ""), ]
sprint400mManYouth[, "Result"] <- as.double(sprint400mManYouth$Result)
summary(sprint400mManYouth$Result)


i <- 2
j <- 2
k <- 2
vector_senior_year <- c(min(sprint400mMan[which(sprint400mMan$Year == 2000), "Result"]))
vector_junior_year <- c(min(sprint400mManJunior[which(sprint400mManJunior$Year == 2000), "Result"]))
vector_youth_year <- c(min(sprint400mManYouth[which(sprint400mManYouth$Year == 2001), "Result"]))
for (year in 2001:2022){
  vector_senior_year[i] <- min(c(min(sprint400mMan[which(sprint400mMan$Year == year), "Result"]), vector_senior_year[i-1]))
  i <- i + 1
  vector_junior_year[j] <- min(c(min(sprint400mManJunior[which(sprint400mManJunior$Year == year), "Result"]), vector_junior_year[j-1]))
  j <- j + 1
  vector_youth_year[k] <- min(c(min(sprint400mManYouth[which(sprint400mManYouth$Year == year), "Result"]), vector_youth_year[k-1]))
  k <- k + 1
}

i <- 2
j <- 2
k <- 2
vector_senior_year_8 <- c(sprint400mMan[which(sprint400mMan$Year == 2000 & sprint400mMan$Rank == "8"), "Result"])
vector_junior_year_8 <- c(sprint400mManJunior[which(sprint400mManJunior$Year == 2000 & sprint400mManJunior$Rank == "8"), "Result"])
vector_youth_year_8 <- c(sprint400mManYouth[which(sprint400mManYouth$Year == 2003 & sprint400mManYouth$Rank == "8"), "Result"])
for (year in 2001:2022){
  vector_senior_year_8[i] <- min(c(sprint400mMan[which(sprint400mMan$Year == year & sprint400mMan$Rank == "8"), "Result"], vector_senior_year_8[i-1]))
  i <- i + 1
  vector_junior_year_8[j] <- min(c(sprint400mManJunior[which(sprint400mManJunior$Year == year & sprint400mManJunior$Rank == "8"), "Result"], vector_junior_year_8[j-1]))
  j <- j + 1
  vector_youth_year_8[k] <- min(c(sprint400mManYouth[which(sprint400mManYouth$Year == year & sprint400mManYouth$Rank == "8"), "Result"], vector_youth_year_8[k-1]))
  k <- k + 1
}

vector_best_year <- c(vector_senior_year, vector_junior_year, vector_youth_year)
vector_8_year <- c(vector_senior_year_8, vector_junior_year_8, vector_youth_year_8)
v1 <- 2000:2022
years <- c(v1,v1,v1)
years
vector_class <- c()
vector_class[1:23] <- "Seniors"
vector_class[24:46] <- "Juniors"
vector_class[47:69] <- "Youth"
vector_class

df_result <- data.frame(Year = years,
                        Top_8 = vector_8_year,
                        Top_1 = vector_best_year,
                        Class = vector_class)
df_result

switzerland_athlete_Men_400m <- olympics[which(olympics$Discipline == "400m"), ]
switzerland_athlete_Men_400m <- switzerland_athlete_Men_400m[which(switzerland_athlete_Men_400m$Year>=2000), ]
switzerland_athlete_Men_400m <- switzerland_athlete_Men_400m[which(switzerland_athlete_Men_400m$Gender == "Men"), ]
switzerland_athlete_Men_400m <- switzerland_athlete_Men_400m[which(switzerland_athlete_Men_400m$Nationality == "SUI"), ]
switzerland_athlete_Men_400m <- switzerland_athlete_Men_400m[which(switzerland_athlete_Men_400m$Result != ""), ]
# sprint400mMan <- sprint400mMan[which(sprint400mMan$Medal != ""), ]
switzerland_athlete_Men_400m[, "Result"] <- as.double(switzerland_athlete_Men_400m$Result)
summary(switzerland_athlete_Men_400m$Result)
table(switzerland_athlete_Men_400m$Person)


switzerland_athlete_Men_400m <- data.frame(Name = switzerland_athlete_Men_400m$Person,
                                             Result = switzerland_athlete_Men_400m$Result,
                                             Year = switzerland_athlete_Men_400m$Year,
                                             Class = switzerland_athlete_Men_400m$Class)

switzerland_athlete_Men_400m <- merge(x = switzerland_athlete_Men_400m,
                                        y = df_result, 
                                        by = c("Year", "Class")
)
switzerland_athlete_Men_400m


# Da fare in shiny con un input sul nome dell'atleta.

athlete_list_M_400m <- as.list(unique(switzerland_athlete_Men_400m$Name))
athlete_result <- c()
athlete_name <- c()
athlete_year <- c()
athlete_class <- c()
top8 <- c()
top1 <- c()
i = 1
for (name in athlete_list_M_400m){
  print(name)
  for (year in min(switzerland_athlete_Men_400m$Year):max(switzerland_athlete_Men_400m$Year)){
    print(year)
    if(length(switzerland_athlete_Men_400m[which(switzerland_athlete_Men_400m$Name == name 
                                                   & switzerland_athlete_Men_400m$Year == year), "Result"] != 0)){
      athlete_name[i] <- name
      
      athlete_result[i] <- min(switzerland_athlete_Men_400m[which(switzerland_athlete_Men_400m$Name == name
                                                                    & switzerland_athlete_Men_400m$Year == year),"Result"])
      athlete_year[i] <- year
      athlete_class[i] <- switzerland_athlete_Men_400m[which(switzerland_athlete_Men_400m$Result == athlete_result[i] &
                                                                 switzerland_athlete_Men_400m$Name == name & 
                                                                 switzerland_athlete_Men_400m$Year == year),"Class"]
      top1[i] <- switzerland_athlete_Men_400m[which(switzerland_athlete_Men_400m$Result == athlete_result[i] &
                                                        switzerland_athlete_Men_400m$Name == name & 
                                                        switzerland_athlete_Men_400m$Year == year),"Top_1"]
      print(top1[i])
      top8[i] <- switzerland_athlete_Men_400m[which(switzerland_athlete_Men_400m$Result == athlete_result[i] &
                                                        switzerland_athlete_Men_400m$Name == name & 
                                                        switzerland_athlete_Men_400m$Year == year),"Top_8"]
      i <- i + 1
    }
  }
}
athlete400mM <- data.frame(Year = athlete_year,
                           Name = athlete_name,
                           Result = athlete_result,
                           Class = athlete_class,
                           Top_8 = top8,
                           Top_1 = top1)
athlete400mM

WR400mMen <- min(sprint400mMan$Result)
WR400mMenJ <- min(sprint400mManJunior$Result)
WR400mMenY <- min(sprint400mManYouth$Result)

SR400mMen <- min(athlete400mM$Result[which(athlete400mM$Class == "Seniors")])
SR400mMenJ <- min(athlete400mM$Result[which(athlete400mM$Class == "Juniors")])
SR400mMenY <- min(athlete400mM$Result[which(athlete400mM$Class == "Youth")])

i <- 1
a_name <- c()
result <- c()
class <- c()
wr <- c()
sr <- c()
for (name in athlete_list_M_400m){
  a_name[i] <- name
  if (min(athlete400mM[which(athlete400mM$Name == name & athlete400mM$Class == "Seniors"), "Result"]) == Inf){
    result[i] <- 0
  }
  else{
    result[i] <- (50.97 - min(athlete400mM[which(athlete400mM$Name == name & athlete400mM$Class == "Seniors"), "Result"]))/(50.97 - WR400mMen)
  }
  print(result[i])
  class[i] <- "Seniors"
  wr[i] <- 1
  sr[i] <- (50.97 - SR400mMen)/(50.97-WR400mMen)
  i <- i + 1
}
i <- 1
a_nameJ <- c()
resultJ <- c()
classJ <- c()
wrJ <- c()
srJ <- c()
for (name in athlete_list_M_400m){
  a_nameJ[i] <- name
  if (min(athlete400mM[which(athlete400mM$Name == name & athlete400mM$Class == "Juniors"), "Result"]) == Inf){
    resultJ[i] <- 0
  }
  else{
    resultJ[i] <- (50.97 - min(athlete400mM[which(athlete400mM$Name == name & athlete400mM$Class == "Juniors"), "Result"]))/(50.97 - WR400mMenJ)
  }
  print(result[i])
  classJ[i] <- "Juniors"
  wrJ[i] <- 1
  srJ[i] <- (50.97 - SR400mMenJ)/(50.97-WR400mMenJ)
  
  i <- i + 1
}
i <- 1
a_nameY <- c()
resultY <- c()
classY <- c()
wrY <- c()
srY <- c()
for (name in athlete_list_M_400m){
  a_nameY[i] <- name
  if (min(athlete400mM[which(athlete400mM$Name == name & athlete400mM$Class == "Youth"), "Result"]) == Inf){
    resultY[i] <- 0
  }
  else{
    resultY[i] <- (50.97 - min(athlete400mM[which(athlete400mM$Name == name & athlete400mM$Class == "Youth"), "Result"]))/(50.97 - WR400mMenY)
  }
  classY[i] <- "Youth"
  wrY[i] <- 1
  srY[i] <- (50.97 - SR400mMenY)/(50.97-WR400mMenY)
  i <- i + 1
}
i <- 1
radar_frameS <- data.frame(Name = a_name,
                           Result = result, 
                           Class = class, 
                           WR = wr,
                           SR = sr)
radar_frameJ <- data.frame(Name = a_nameJ,
                           Result = resultJ, 
                           Class = classJ, 
                           WR = wrJ,
                           SR = srJ)
radar_frameY <- data.frame(Name = a_nameY,
                           Result = resultY, 
                           Class = classY, 
                           WR = wrY,
                           SR = srY)

radar_frame400 <- merge(x = merge(x = radar_frameS, y = radar_frameJ, by = "Name"), y = radar_frameY, by = "Name")

radar_frame400 <- subset(radar_frame400, select = -c(Class.x, WR.x, SR.x, Class.y, WR.y, SR.y, Class, WR, SR))

colnames(radar_frame400) <- c("Name", "Seniors", "Juniors", "Youth")

radar_frame400[(length(radar_frame400$Name) + 1), ] <- c("World Record in the class", 1,1,1)
radar_frame400[(length(radar_frame400$Name) + 1), ] <- c("Fictious baseline", 0,0,0)
radar_frame400[(length(radar_frame400$Name) + 1), ] <- c("Swiss Record in the class", (50.97 - SR400mMen)/(50.97-WR400mMen), (50.97 - SR400mMenJ)/(50.97-WR400mMenJ), (50.97 - SR400mMenY)/(50.97-WR400mMenY))
radar_frame400$Seniors <- as.numeric(radar_frame400$Seniors)
radar_frame400$Juniors <- as.numeric(radar_frame400$Juniors)
radar_frame400$Youth <- as.numeric(radar_frame400$Youth)
radar_frame400

radarchart(radar_frame400[which(radar_frame400$Name == "Ricky Petrucciani" | radar_frame400$Name == "World Record in the class"
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
legend(1,1, legend=unique(radar_frame400[which(radar_frame400$Name == "Ricky Petrucciani" | radar_frame400$Name == "World Record in the class"
                                               | radar_frame400$Name == "Fictious baseline" | radar_frame400$Name == "Swiss Record in the class"), "Name"])
       , seg.len=0.5, pch=1, border = "black",
       bty="n" ,lwd=3, y.intersp=0.75, horiz=FALSE, col =c("red", "gold", "lightblue", "darkred")
)

ggplot() +
  geom_point(data = athlete400mM, aes(x = Year, y = Result, colour = Class))

# No significance to state anything 
fit <- lm(data = athlete400mM[which(athlete400mM$Class == "Seniors"), ], 
          formula = Result ~ Year)
summary(fit)

# No significance to state anything
fit <- lm(data = athlete400mM[which(athlete400mM$Class == "Juniors"), ], 
          formula = Result ~ Year)
summary(fit)

# No significance to state anything
fit <- lm(data = athlete400mM[which(athlete400mM$Class == "Youth"), ], 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = athlete400mM[which(athlete400mM$Name == athlete_list_M_400m[[10]]), ],
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

athlete400mM[, "Difference_8"] <- athlete400mM$Result - athlete400mM$Top_8

ggplot() +
  geom_point(data = athlete400mM, aes(x = Year, y = Difference_8, colour = Class))

# no significance 
fit <- lm(data = athlete400mM[which(athlete400mM$Class == "Seniors"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

# No significance 
fit <- lm(data = athlete400mM[which(athlete400mM$Class == "Juniors"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

# No significance to state anything 
fit <- lm(data = athlete400mM[which(athlete400mM$Class == "Youth"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

p <- ggplot(data = athlete400mM)+
  geom_histogram(aes(x = Difference_8, fill = Class), binwidth = .05) +
  geom_histogram(data = athlete400mM[which(athlete400mM$Name == athlete_list_M_400m[[10]]), ],
                 aes(x = Difference_8), binwidth = 0.05) +
  scale_fill_manual(values = heat.colors(4)) 

ggplotly(p)

################################################################################
################################ 400m Female ###################################
################################################################################

sprint400mwoman <- olympics[which(olympics$Discipline == "400m"), ]
sprint400mwoman <- sprint400mwoman[which(sprint400mwoman$Year>=1990), ]
sprint400mwoman <- sprint400mwoman[which(sprint400mwoman$Competition == "Olympic Games" 
                                     | sprint400mwoman$Competition == "World Championships"), ]
sprint400mwoman <- sprint400mwoman[which(sprint400mwoman$Gender == "Women"), ]
sprint400mwoman <- sprint400mwoman[which(sprint400mwoman$Class == "Seniors"), ]
sprint400mwoman <- sprint400mwoman[which(sprint400mwoman$Result != ""), ]
# sprint400mwoman <- sprint400mwoman[which(sprint400mwoman$Medal != ""), ]
sprint400mwoman[, "Result"] <- as.double(sprint400mwoman$Result)
sprint400mwoman <- sprint400mwoman[which(!is.na(sprint400mwoman$Result)), ]
summary(sprint400mwoman$Result[which(!is.na(sprint400mwoman$Result))])

sprint400mWomanJunior <- olympics[which(olympics$Discipline == "400m"), ]
sprint400mWomanJunior <- sprint400mWomanJunior[which(sprint400mWomanJunior$Year>=1990), ]
sprint400mWomanJunior <- sprint400mWomanJunior[which(sprint400mWomanJunior$Competition == "World Junior Championships"), ]
sprint400mWomanJunior <- sprint400mWomanJunior[which(sprint400mWomanJunior$Gender == "Women"), ]
sprint400mWomanJunior <- sprint400mWomanJunior[which(sprint400mWomanJunior$Class == "Juniors"), ]
sprint400mWomanJunior <- sprint400mWomanJunior[which(sprint400mWomanJunior$Result != ""), ]
# sprint400mWomanJunior <- sprint400mWomanJunior[which(sprint400mWomanJunior$Medal != ""), ]
sprint400mWomanJunior[, "Result"] <- as.double(sprint400mWomanJunior$Result)
sprint400mWomanJunior <- sprint400mWomanJunior[which(!is.na(sprint400mWomanJunior$Result)), ]
summary(sprint400mWomanJunior$Result)

sprint400mWomanYouth <- olympics[which(olympics$Discipline == "400m"), ]
sprint400mWomanYouth <- sprint400mWomanYouth[which(sprint400mWomanYouth$Year>=1990), ]
sprint400mWomanYouth <- sprint400mWomanYouth[which(sprint400mWomanYouth$Competition == "World Youth Championships"
                                               | sprint400mWomanYouth$Competition == "Youth Olympic Games"), ]
sprint400mWomanYouth <- sprint400mWomanYouth[which(sprint400mWomanYouth$Gender == "Women"), ]
sprint400mWomanYouth <- sprint400mWomanYouth[which(sprint400mWomanYouth$Class == "Youth"), ]
sprint400mWomanYouth <- sprint400mWomanYouth[which(sprint400mWomanYouth$Result != ""), ]
# sprint400mManYouth <- sprint400mManYouth[which(sprint400mManYouth$Medal != ""), ]
sprint400mWomanYouth[, "Result"] <- as.double(sprint400mWomanYouth$Result)
sprint400mWomanYouth <- sprint400mWomanYouth[which(!is.na(sprint400mWomanYouth$Result)), ]
summary(sprint400mWomanYouth$Result)


i <- 2
j <- 2
k <- 2
vector_senior_year <- c(min(sprint400mwoman[which(sprint400mwoman$Year == 2000), "Result"]))
vector_junior_year <- c(min(sprint400mWomanJunior[which(sprint400mWomanJunior$Year == 2000), "Result"]))
vector_youth_year <- c(min(sprint400mWomanYouth[which(sprint400mWomanYouth$Year == 2001), "Result"]))
for (year in 2001:2022){
  vector_senior_year[i] <- min(c(min(sprint400mwoman[which(sprint400mwoman$Year == year), "Result"]), vector_senior_year[i-1]))
  i <- i + 1
  vector_junior_year[j] <- min(c(min(sprint400mWomanJunior[which(sprint400mWomanJunior$Year == year), "Result"]), vector_junior_year[j-1]))
  j <- j + 1
  vector_youth_year[k] <- min(c(min(sprint400mWomanYouth[which(sprint400mWomanYouth$Year == year), "Result"]), vector_youth_year[k-1]))
  k <- k + 1
}

i <- 2
j <- 2
k <- 2
vector_senior_year_8 <- c(sprint400mwoman[which(sprint400mwoman$Year == 2000 & sprint400mwoman$Rank == "8"), "Result"])
vector_junior_year_8 <- c(sprint400mWomanJunior[which(sprint400mWomanJunior$Year == 2000 & sprint400mWomanJunior$Rank == "8"), "Result"])
vector_youth_year_8 <- c(sprint400mWomanYouth[which(sprint400mWomanYouth$Year == 2003 & sprint400mWomanYouth$Rank == "8"), "Result"])
for (year in 2001:2022){
  vector_senior_year_8[i] <- min(c(sprint400mwoman[which(sprint400mwoman$Year == year & sprint400mwoman$Rank == "8"), "Result"], vector_senior_year_8[i-1]))
  i <- i + 1
  vector_junior_year_8[j] <- min(c(sprint400mWomanJunior[which(sprint400mWomanJunior$Year == year & sprint400mWomanJunior$Rank == "8"), "Result"], vector_junior_year_8[j-1]))
  j <- j + 1
  vector_youth_year_8[k] <- min(c(sprint400mWomanYouth[which(sprint400mWomanYouth$Year == year & sprint400mWomanYouth$Rank == "8"), "Result"], vector_youth_year_8[k-1]))
  k <- k + 1
}

vector_best_year <- c(vector_senior_year, vector_junior_year, vector_youth_year)
vector_8_year <- c(vector_senior_year_8, vector_junior_year_8, vector_youth_year_8)
v1 <- 2000:2022
years <- c(v1,v1,v1)
years
vector_class <- c()
vector_class[1:23] <- "Seniors"
vector_class[24:46] <- "Juniors"
vector_class[47:69] <- "Youth"
vector_class

df_result <- data.frame(Year = years,
                        Top_8 = vector_8_year,
                        Top_1 = vector_best_year,
                        Class = vector_class)
df_result

switzerland_athlete_Women_400m <- olympics[which(olympics$Discipline == "400m"), ]
switzerland_athlete_Women_400m <- switzerland_athlete_Women_400m[which(switzerland_athlete_Women_400m$Year>=2000), ]
switzerland_athlete_Women_400m <- switzerland_athlete_Women_400m[which(switzerland_athlete_Women_400m$Gender == "Women"), ]
switzerland_athlete_Women_400m <- switzerland_athlete_Women_400m[which(switzerland_athlete_Women_400m$Nationality == "SUI"), ]
switzerland_athlete_Women_400m <- switzerland_athlete_Women_400m[which(switzerland_athlete_Women_400m$Result != ""), ]
# sprint400mMan <- sprint400mMan[which(sprint400mMan$Medal != ""), ]
switzerland_athlete_Women_400m[, "Result"] <- as.double(switzerland_athlete_Women_400m$Result)
summary(switzerland_athlete_Women_400m$Result)
table(switzerland_athlete_Women_400m$Person)


switzerland_athlete_Women_400m <- data.frame(Name = switzerland_athlete_Women_400m$Person,
                                           Result = switzerland_athlete_Women_400m$Result,
                                           Year = switzerland_athlete_Women_400m$Year,
                                           Class = switzerland_athlete_Women_400m$Class)

switzerland_athlete_Women_400m <- merge(x = switzerland_athlete_Women_400m,
                                      y = df_result, 
                                      by = c("Year", "Class")
)
switzerland_athlete_Women_400m


# Da fare in shiny con un input sul nome dell'atleta.

athlete_list_W_400m <- as.list(unique(switzerland_athlete_Women_400m$Name))
athlete_result <- c()
athlete_name <- c()
athlete_year <- c()
athlete_class <- c()
top8 <- c()
top1 <- c()
i = 1
for (name in athlete_list_W_400m){
  print(name)
  for (year in min(switzerland_athlete_Women_400m$Year):max(switzerland_athlete_Women_400m$Year)){
    print(year)
    if(length(switzerland_athlete_Women_400m[which(switzerland_athlete_Women_400m$Name == name 
                                                 & switzerland_athlete_Women_400m$Year == year), "Result"] != 0)){
      athlete_name[i] <- name
      
      athlete_result[i] <- min(switzerland_athlete_Women_400m[which(switzerland_athlete_Women_400m$Name == name
                                                                  & switzerland_athlete_Women_400m$Year == year),"Result"])
      athlete_year[i] <- year
      athlete_class[i] <- switzerland_athlete_Women_400m[which(switzerland_athlete_Women_400m$Result == athlete_result[i] &
                                                               switzerland_athlete_Women_400m$Name == name & 
                                                               switzerland_athlete_Women_400m$Year == year),"Class"]
      top1[i] <- switzerland_athlete_Women_400m[which(switzerland_athlete_Women_400m$Result == athlete_result[i] &
                                                      switzerland_athlete_Women_400m$Name == name & 
                                                      switzerland_athlete_Women_400m$Year == year),"Top_1"]
      print(top1[i])
      top8[i] <- switzerland_athlete_Women_400m[which(switzerland_athlete_Women_400m$Result == athlete_result[i] &
                                                      switzerland_athlete_Women_400m$Name == name & 
                                                      switzerland_athlete_Women_400m$Year == year),"Top_8"]
      i <- i + 1
    }
  }
}
athlete400mW <- data.frame(Year = athlete_year,
                           Name = athlete_name,
                           Result = athlete_result,
                           Class = athlete_class,
                           Top_8 = top8,
                           Top_1 = top1)
athlete400mW

WR400mWomen <- min(sprint400mwoman$Result)
WR400mWomenJ <- min(sprint400mWomanJunior$Result)
WR400mWomenY <- min(sprint400mWomanYouth$Result)

SR400mWomen <- min(athlete400mW$Result[which(athlete400mW$Class == "Seniors")])
SR400mWomenJ <- min(athlete400mW$Result[which(athlete400mW$Class == "Juniors")])
SR400mWomenY <- min(athlete400mW$Result[which(athlete400mW$Class == "Youth")])

i <- 1
a_name <- c()
result <- c()
class <- c()
wr <- c()
sr <- c()
for (name in athlete_list_W_400m){
  a_name[i] <- name
  if (min(athlete400mW[which(athlete400mW$Name == name & athlete400mW$Class == "Seniors"), "Result"]) == Inf){
    result[i] <- 0
  }
  else{
    result[i] <- (58.57 - min(athlete400mW[which(athlete400mW$Name == name & athlete400mW$Class == "Seniors"), "Result"]))/(58.57 - WR400mWomen)
  }
  print(result[i])
  class[i] <- "Seniors"
  wr[i] <- 1
  sr[i] <- (58.57 - SR400mWomen)/(58.57-WR400mWomen)
  i <- i + 1
}
i <- 1
a_nameJ <- c()
resultJ <- c()
classJ <- c()
wrJ <- c()
srJ <- c()
for (name in athlete_list_W_400m){
  a_nameJ[i] <- name
  if (min(athlete400mW[which(athlete400mW$Name == name & athlete400mW$Class == "Juniors"), "Result"]) == Inf){
    resultJ[i] <- 0
  }
  else{
    resultJ[i] <- (58.57 - min(athlete400mW[which(athlete400mW$Name == name & athlete400mW$Class == "Juniors"), "Result"]))/(58.57 - WR400mWomenJ)
  }
  print(result[i])
  classJ[i] <- "Juniors"
  wrJ[i] <- 1
  srJ[i] <- (58.57 - SR400mWomenJ)/(58.57-WR400mWomenJ)
  
  i <- i + 1
}
i <- 1
a_nameY <- c()
resultY <- c()
classY <- c()
wrY <- c()
srY <- c()
for (name in athlete_list_W_400m){
  a_nameY[i] <- name
  if (min(athlete400mW[which(athlete400mW$Name == name & athlete400mW$Class == "Youth"), "Result"]) == Inf){
    resultY[i] <- 0
  }
  else{
    resultY[i] <- (58.57 - min(athlete400mW[which(athlete400mW$Name == name & athlete400mW$Class == "Youth"), "Result"]))/(58.57 - WR400mWomenY)
  }
  classY[i] <- "Youth"
  wrY[i] <- 1
  srY[i] <- (58.57 - SR400mWomenY)/(58.57-WR400mWomenY)
  i <- i + 1
}
i <- 1
radar_frameS <- data.frame(Name = a_name,
                           Result = result, 
                           Class = class, 
                           WR = wr,
                           SR = sr)
radar_frameJ <- data.frame(Name = a_nameJ,
                           Result = resultJ, 
                           Class = classJ, 
                           WR = wrJ,
                           SR = srJ)
radar_frameY <- data.frame(Name = a_nameY,
                           Result = resultY, 
                           Class = classY, 
                           WR = wrY,
                           SR = srY)

radar_frame400W <- merge(x = merge(x = radar_frameS, y = radar_frameJ, by = "Name"), y = radar_frameY, by = "Name")
radar_frame400W

radar_frame400W <- subset(radar_frame400W, select = -c(Class.x, WR.x, SR.x, Class.y, WR.y, SR.y, Class, WR, SR))

colnames(radar_frame400W) <- c("Name", "Seniors", "Juniors", "Youth")

radar_frame400W[(length(radar_frame400W$Name) + 1), ] <- c("World Record in the class", 1,1,1)
radar_frame400W[(length(radar_frame400W$Name) + 1), ] <- c("Fictious baseline", 0,0,0)
radar_frame400W[(length(radar_frame400W$Name) + 1), ] <- c("Swiss Record in the class", (58.57 - SR400mWomen)/(58.57-WR400mWomen),
                                                           (58.57- SR400mWomenJ)/(58.57-WR400mWomenJ), 
                                                           (58.57- SR400mWomenY)/(58.57-WR400mWomenY))
radar_frame400W$Seniors <- as.numeric(radar_frame400W$Seniors)
radar_frame400W$Juniors <- as.numeric(radar_frame400W$Juniors)
radar_frame400W$Youth <- as.numeric(radar_frame400W$Youth)
radar_frame400W

radarchart(radar_frame400W[which(radar_frame400W$Name == "Veronica Vancardo" | radar_frame400W$Name == "World Record in the class"
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
legend(1,1, legend=unique(radar_frame400W[which(radar_frame200W$Name == "Veronica Vancardo" | radar_frame200W$Name == "World Record in the class"
                                                | radar_frame200W$Name == "Fictious baseline" | radar_frame200W$Name == "Swiss Record in the class"), "Name"])
       , seg.len=0.5, pch=1, border = "black",
       bty="n" ,lwd=3, y.intersp=1, horiz=FALSE, col =c("red", "gold", "lightblue", "darkred")
)

ggplot() +
  geom_point(data = athlete400mW, aes(x = Year, y = Result, colour = Class))

# No significance to state anything 
fit <- lm(data = athlete400mW[which(athlete400mW$Class == "Seniors"), ], 
          formula = Result ~ Year)
summary(fit)

# low level of significance in the model seems to be in the right direction 
fit <- lm(data = athlete400mW[which(athlete400mW$Class == "Juniors"), ], 
          formula = Result ~ Year)
summary(fit)

# No significance to state anything
fit <- lm(data = athlete400mW[which(athlete400mW$Class == "Youth"), ], 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = athlete400mW[which(athlete400mW$Name == athlete_list_W_400m[[2]]), ],
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

athlete400mW[, "Difference_8"] <- athlete400mW$Result - athlete400mW$Top_8

ggplot() +
  geom_point(data = athlete400mW, aes(x = Year, y = Difference_8, colour = Class))

# no significance 
fit <- lm(data = athlete400mW[which(athlete400mW$Class == "Seniors"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

# little significance 3% 
fit <- lm(data = athlete400mW[which(athlete400mW$Class == "Juniors"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

# No significance to state anything 
fit <- lm(data = athlete400mW[which(athlete400mW$Class == "Youth"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

p <- ggplot(data = athlete400mW)+
  geom_histogram(aes(x = Difference_8, fill = Class), binwidth = .25) +
  geom_histogram(data = athlete400mW[which(athlete400mW$Name == athlete_list_W_400m[[2]]), ],
                 aes(x = Difference_8), binwidth = 0.25) +
  scale_fill_manual(values = heat.colors(4)) 

ggplotly(p)

################################################################################
################################ Long Male ###################################
################################################################################

longJumpMan <- olympics[which(olympics$Discipline == "Long Jump"), ]
longJumpMan <- longJumpMan[which(longJumpMan$Year>=1990), ]
longJumpMan <- longJumpMan[which(longJumpMan$Competition == "Olympic Games" 
                                         | longJumpMan$Competition == "World Championships"), ]
longJumpMan <- longJumpMan[which(longJumpMan$Gender == "Men"), ]
longJumpMan <- longJumpMan[which(longJumpMan$Class == "Seniors"), ]
longJumpMan <- longJumpMan[which(longJumpMan$Result != ""), ]
# longJumpMan <- longJumpMan[which(longJumpMan$Medal != ""), ]
longJumpMan[, "Result"] <- as.double(longJumpMan$Result)
longJumpMan <- longJumpMan[which(!is.na(longJumpMan$Result)), ]
summary(longJumpMan$Result[which(!is.na(longJumpMan$Result))])

longJumpManJunior <- olympics[which(olympics$Discipline == "Long Jump"), ]
longJumpManJunior <- longJumpManJunior[which(longJumpManJunior$Year>=1990), ]
longJumpManJunior <- longJumpManJunior[which(longJumpManJunior$Competition == "World Junior Championships"), ]
longJumpManJunior <- longJumpManJunior[which(longJumpManJunior$Gender == "Men"), ]
longJumpManJunior <- longJumpManJunior[which(longJumpManJunior$Class == "Juniors"), ]
longJumpManJunior <- longJumpManJunior[which(longJumpManJunior$Result != ""), ]
# sprint400mWomanJunior <- longJumpManJunior[which(longJumpManJunior$Medal != ""), ]
longJumpManJunior[, "Result"] <- as.double(longJumpManJunior$Result)
longJumpManJunior <- longJumpManJunior[which(!is.na(longJumpManJunior$Result)), ]
summary(longJumpManJunior$Result)

longJumpManYouth <- olympics[which(olympics$Discipline == "Long Jump"), ]
longJumpManYouth <- longJumpManYouth[which(longJumpManYouth$Year>=1990), ]
longJumpManYouth <- longJumpManYouth[which(longJumpManYouth$Competition == "World Youth Championships"
                                                   | longJumpManYouth$Competition == "Youth Olympic Games"), ]
longJumpManYouth <- longJumpManYouth[which(longJumpManYouth$Gender == "Men"), ]
longJumpManYouth <- longJumpManYouth[which(longJumpManYouth$Class == "Youth"), ]
longJumpManYouth <- longJumpManYouth[which(longJumpManYouth$Result != ""), ]
# longJumpManYouth <- longJumpManYouth[which(longJumpManYouth$Medal != ""), ]
longJumpManYouth[, "Result"] <- as.double(longJumpManYouth$Result)
longJumpManYouth <- longJumpManYouth[which(!is.na(longJumpManYouth$Result)), ]
summary(longJumpManYouth$Result)


i <- 2
j <- 2
k <- 2
vector_senior_year <- c(max(longJumpMan[which(longJumpMan$Year == 2000), "Result"]))
vector_junior_year <- c(max(longJumpManJunior[which(longJumpManJunior$Year == 2000), "Result"]))
vector_youth_year <- c(max(longJumpManYouth[which(longJumpManYouth$Year == 2001), "Result"]))
for (year in 2001:2022){
  vector_senior_year[i] <- max(c(max(longJumpMan[which(longJumpMan$Year == year), "Result"]), vector_senior_year[i-1]))
  i <- i + 1
  vector_junior_year[j] <- max(c(max(longJumpManJunior[which(longJumpManJunior$Year == year), "Result"]), vector_junior_year[j-1]))
  j <- j + 1
  vector_youth_year[k] <- max(c(max(longJumpManYouth[which(longJumpManYouth$Year == year), "Result"]), vector_youth_year[k-1]))
  k <- k + 1
}

i <- 2
j <- 2
k <- 2
vector_senior_year_8 <- c(longJumpMan[which(longJumpMan$Year == 2000 & longJumpMan$Rank == "8"), "Result"])
vector_junior_year_8 <- c(longJumpManJunior[which(longJumpManJunior$Year == 2000 & longJumpManJunior$Rank == "8"), "Result"])
vector_youth_year_8 <- c(longJumpManYouth[which(longJumpManYouth$Year == 2001 & longJumpManYouth$Rank == "8"), "Result"])
for (year in 2001:2022){
  vector_senior_year_8[i] <- max(c(longJumpMan[which(longJumpMan$Year == year & longJumpMan$Rank == "8"), "Result"], vector_senior_year_8[i-1]))
  i <- i + 1
  vector_junior_year_8[j] <- max(c(longJumpManJunior[which(longJumpManJunior$Year == year & longJumpManJunior$Rank == "8"), "Result"], vector_junior_year_8[j-1]))
  j <- j + 1
  vector_youth_year_8[k] <- max(c(longJumpManYouth[which(longJumpManYouth$Year == year & longJumpManYouth$Rank == "8"), "Result"], vector_youth_year_8[k-1]))
  k <- k + 1
}

vector_best_year <- c(vector_senior_year, vector_junior_year, vector_youth_year)
vector_8_year <- c(vector_senior_year_8, vector_junior_year_8, vector_youth_year_8)
v1 <- 2000:2022
years <- c(v1,v1,v1)
years
vector_class <- c()
vector_class[1:23] <- "Seniors"
vector_class[24:46] <- "Juniors"
vector_class[47:69] <- "Youth"
vector_class

df_result <- data.frame(Year = years,
                        Top_8 = vector_8_year,
                        Top_1 = vector_best_year,
                        Class = vector_class)
df_result

switzerland_athlete_Men_LongJump <- olympics[which(olympics$Discipline == "Long Jump"), ]
switzerland_athlete_Men_LongJump <- switzerland_athlete_Men_LongJump[which(switzerland_athlete_Men_LongJump$Year>=2000), ]
switzerland_athlete_Men_LongJump <- switzerland_athlete_Men_LongJump[which(switzerland_athlete_Men_LongJump$Gender == "Men"), ]
switzerland_athlete_Men_LongJump <- switzerland_athlete_Men_LongJump[which(switzerland_athlete_Men_LongJump$Nationality == "SUI"), ]
switzerland_athlete_Men_LongJump <- switzerland_athlete_Men_LongJump[which(switzerland_athlete_Men_LongJump$Result != ""), ]
# sprint400mMan <- sprint400mMan[which(sprint400mMan$Medal != ""), ]
switzerland_athlete_Men_LongJump[, "Result"] <- as.double(switzerland_athlete_Men_LongJump$Result)
summary(switzerland_athlete_Men_LongJump$Result)
table(switzerland_athlete_Men_LongJump$Person)


switzerland_athlete_Men_LongJump <- data.frame(Name = switzerland_athlete_Men_LongJump$Person,
                                             Result = switzerland_athlete_Men_LongJump$Result,
                                             Year = switzerland_athlete_Men_LongJump$Year,
                                             Class = switzerland_athlete_Men_LongJump$Class)

switzerland_athlete_Men_LongJump <- merge(x = switzerland_athlete_Men_LongJump,
                                        y = df_result, 
                                        by = c("Year", "Class")
)
switzerland_athlete_Men_LongJump


# Da fare in shiny con un input sul nome dell'atleta.

athlete_list_M_LongJump <- as.list(unique(switzerland_athlete_Men_LongJump$Name))
athlete_result <- c()
athlete_name <- c()
athlete_year <- c()
athlete_class <- c()
top8 <- c()
top1 <- c()
i = 1
for (name in athlete_list_M_LongJump){
  print(name)
  for (year in min(switzerland_athlete_Men_LongJump$Year):max(switzerland_athlete_Men_LongJump$Year)){
    if(length(switzerland_athlete_Men_LongJump[which(switzerland_athlete_Men_LongJump$Name == name 
                                                   & switzerland_athlete_Men_LongJump$Year == year), "Result"] != 0)){
      athlete_name[i] <- name
      
      athlete_result[i] <- max(switzerland_athlete_Men_LongJump[which(switzerland_athlete_Men_LongJump$Name == name
                                                                    & switzerland_athlete_Men_LongJump$Year == year),"Result"])
      athlete_year[i] <- year
      athlete_class[i] <- switzerland_athlete_Men_LongJump[which(switzerland_athlete_Men_LongJump$Result == athlete_result[i] &
                                                                 switzerland_athlete_Men_LongJump$Name == name & 
                                                                 switzerland_athlete_Men_LongJump$Year == year),"Class"]
      top1[i] <- switzerland_athlete_Men_LongJump[which(switzerland_athlete_Men_LongJump$Result == athlete_result[i] &
                                                          switzerland_athlete_Men_LongJump$Name == name & 
                                                          switzerland_athlete_Men_LongJump$Year == year),"Top_1"]
      top8[i] <- switzerland_athlete_Men_LongJump[which(switzerland_athlete_Men_LongJump$Result == athlete_result[i] &
                                                          switzerland_athlete_Men_LongJump$Name == name & 
                                                          switzerland_athlete_Men_LongJump$Year == year),"Top_8"]
      i <- i + 1
    }
  }
}
athleteLongJumpM <- data.frame(Year = athlete_year,
                           Name = athlete_name,
                           Result = athlete_result,
                           Class = athlete_class,
                           Top_8 = top8,
                           Top_1 = top1)
athleteLongJumpM

WRLJMen <- max(longJumpMan$Result)
WRLJMenJ <- max(longJumpManJunior$Result)
WRLJMenY <- max(longJumpManYouth$Result)

SRLJMen <- max(athleteLongJumpM$Result[which(athleteLongJumpM$Class == "Seniors")])
SRLJMenJ <- max(athleteLongJumpM$Result[which(athleteLongJumpM$Class == "Juniors")])
SRLJMenY <- max(athleteLongJumpM$Result[which(athleteLongJumpM$Class == "Youth")])

i <- 1
a_name <- c()
result <- c()
class <- c()
wr <- c()
sr <- c()
for (name in athlete_list_M_LongJump){
  a_name[i] <- name
  if (max(athleteLongJumpM[which(athleteLongJumpM$Name == name & athleteLongJumpM$Class == "Seniors"), "Result"]) == -Inf){
    result[i] <- 0
  }
  else{
    result[i] <- (max(athleteLongJumpM[which(athleteLongJumpM$Name == name & athleteLongJumpM$Class == "Seniors"), "Result"]))/(WRLJMen)
  }
  print(result[i])
  class[i] <- "Seniors"
  wr[i] <- 1
  sr[i] <- (SRLJMen)/(WRLJMen)
  i <- i + 1
}
i <- 1
a_nameJ <- c()
resultJ <- c()
classJ <- c()
wrJ <- c()
srJ <- c()
for (name in athlete_list_M_LongJump){
  a_nameJ[i] <- name
  if (max(athleteLongJumpM[which(athleteLongJumpM$Name == name & athleteLongJumpM$Class == "Juniors"), "Result"]) == -Inf){
    resultJ[i] <- 0
  }
  else{
    resultJ[i] <- (max(athleteLongJumpM[which(athleteLongJumpM$Name == name & athleteLongJumpM$Class == "Juniors"), "Result"]))/(WRLJMenJ)
  }
  print(result[i])
  classJ[i] <- "Juniors"
  wrJ[i] <- 1
  srJ[i] <- (SRLJMenJ)/(WRLJMenJ)
  
  i <- i + 1
}
i <- 1
a_nameY <- c()
resultY <- c()
classY <- c()
wrY <- c()
srY <- c()
for (name in athlete_list_M_LongJump){
  a_nameY[i] <- name
  if (max(athleteLongJumpM[which(athleteLongJumpM$Name == name & athleteLongJumpM$Class == "Youth"), "Result"]) == -Inf){
    resultY[i] <- 0
  }
  else{
    resultY[i] <- (max(athleteLongJumpM[which(athleteLongJumpM$Name == name & athleteLongJumpM$Class == "Youth"), "Result"]))/(WRLJMenY)
  }
  classY[i] <- "Youth"
  wrY[i] <- 1
  srY[i] <- (SRLJMenY)/(WRLJMenY)
  i <- i + 1
}
i <- 1
radar_frameS <- data.frame(Name = a_name,
                           Result = result, 
                           Class = class, 
                           WR = wr,
                           SR = sr)
radar_frameJ <- data.frame(Name = a_nameJ,
                           Result = resultJ, 
                           Class = classJ, 
                           WR = wrJ,
                           SR = srJ)
radar_frameY <- data.frame(Name = a_nameY,
                           Result = resultY, 
                           Class = classY, 
                           WR = wrY,
                           SR = srY)

radar_frameLJ <- merge(x = merge(x = radar_frameS, y = radar_frameJ, by = "Name"), y = radar_frameY, by = "Name")
radar_frameLJ

radar_frameLJ <- subset(radar_frameLJ, select = -c(Class.x, WR.x, SR.x, Class.y, WR.y, SR.y, Class, WR, SR))

colnames(radar_frameLJ) <- c("Name", "Seniors", "Juniors", "Youth")

radar_frameLJ[(length(radar_frameLJ$Name) + 1), ] <- c("World Record in the class", 1,1,1)
radar_frameLJ[(length(radar_frameLJ$Name) + 1), ] <- c("Fictious baseline", 0,0,0)
radar_frameLJ[(length(radar_frameLJ$Name) + 1), ] <- c("Swiss Record in the class", (SRLJMen)/(WRLJMen),
                                                           (SRLJMenJ)/(WRLJMenJ), 
                                                           (SRLJMenY)/(WRLJMenY))
radar_frameLJ$Seniors <- as.numeric(radar_frameLJ$Seniors)
radar_frameLJ$Juniors <- as.numeric(radar_frameLJ$Juniors)
radar_frameLJ$Youth <- as.numeric(radar_frameLJ$Youth)
radar_frameLJ

radarchart(radar_frameLJ[which(radar_frameLJ$Name == "Jarod Biya" | radar_frameLJ$Name == "World Record in the class"
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
legend(0.5, 1.2, legend=unique(radar_frameLJ[which(radar_frameLJ$Name == "Jarod Biya" | radar_frameLJ$Name == "World Record in the class"
                                                | radar_frameLJ$Name == "Fictious baseline" | radar_frameLJ$Name == "Swiss Record in the class"), "Name"])
       , seg.len=0.5, pch=1, border = "black",
       bty="n" ,lwd=3, y.intersp=1, horiz=FALSE, col =c("red", "gold", "lightblue", "darkred")
)


ggplot() +
  geom_point(data = athleteLongJumpM, aes(x = Year, y = Result, colour = Class))

# great significance good direction 
fit <- lm(data = athleteLongJumpM[which(athleteLongJumpM$Class == "Seniors"), ], 
          formula = Result ~ Year)
summary(fit)

# no significance
fit <- lm(data = athleteLongJumpM[which(athleteLongJumpM$Class == "Juniors"), ], 
          formula = Result ~ Year)
summary(fit)

# No significance to state anything
fit <- lm(data = athleteLongJumpM[which(athleteLongJumpM$Class == "Youth"), ], 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = athleteLongJumpM[which(athleteLongJumpM$Name == athlete_list_M_LongJump[[5]]), ],
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

athleteLongJumpM[, "Difference_8"] <- athleteLongJumpM$Result - athleteLongJumpM$Top_8

ggplot() +
  geom_point(data = athleteLongJumpM, aes(x = Year, y = Difference_8, colour = Class))

# high significance !!!!
fit <- lm(data = athleteLongJumpM[which(athleteLongJumpM$Class == "Seniors"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

# no significance 
fit <- lm(data = athleteLongJumpM[which(athleteLongJumpM$Class == "Juniors"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

# No significance to state anything 
fit <- lm(data = athleteLongJumpM[which(athleteLongJumpM$Class == "Youth"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

p <- ggplot(data = athleteLongJumpM)+
  geom_histogram(aes(x = Difference_8, fill = Class), binwidth = .2) +
  geom_histogram(data = athleteLongJumpM[which(athleteLongJumpM$Name == athlete_list_M_LongJump[[5]]), ],
                 aes(x = Difference_8), binwidth = 0.2) +
  scale_fill_manual(values = heat.colors(4)) 

ggplotly(p)

################################################################################
################################ Long Female ###################################
################################################################################

longJumpWoman <- olympics[which(olympics$Discipline == "Long Jump"), ]
longJumpWoman <- longJumpWoman[which(longJumpWoman$Year>=1990), ]
longJumpWoman <- longJumpWoman[which(longJumpWoman$Competition == "Olympic Games" 
                                 | longJumpWoman$Competition == "World Championships"), ]
longJumpWoman <- longJumpWoman[which(longJumpWoman$Gender == "Women"), ]
longJumpWoman <- longJumpWoman[which(longJumpWoman$Class == "Seniors"), ]
longJumpWoman <- longJumpWoman[which(longJumpWoman$Result != ""), ]
# longJumpWoman <- longJumpWoman[which(longJumpWoman$Medal != ""), ]
longJumpWoman[, "Result"] <- as.double(longJumpWoman$Result)
longJumpWoman <- longJumpWoman[which(!is.na(longJumpWoman$Result)), ]
summary(longJumpWoman$Result[which(!is.na(longJumpWoman$Result))])

longJumpWomanJunior <- olympics[which(olympics$Discipline == "Long Jump"), ]
longJumpWomanJunior <- longJumpWomanJunior[which(longJumpWomanJunior$Year>=1990), ]
longJumpWomanJunior <- longJumpWomanJunior[which(longJumpWomanJunior$Competition == "World Junior Championships"), ]
longJumpWomanJunior <- longJumpWomanJunior[which(longJumpWomanJunior$Gender == "Women"), ]
longJumpWomanJunior <- longJumpWomanJunior[which(longJumpWomanJunior$Class == "Juniors"), ]
longJumpWomanJunior <- longJumpWomanJunior[which(longJumpWomanJunior$Result != ""), ]
# longJumpWomanJunior <- longJumpWomanJunior[which(longJumpWomanJunior$Medal != ""), ]
longJumpWomanJunior[, "Result"] <- as.double(longJumpWomanJunior$Result)
longJumpWomanJunior <- longJumpWomanJunior[which(!is.na(longJumpWomanJunior$Result)), ]
summary(longJumpWomanJunior$Result)

longJumpWomanYouth <- olympics[which(olympics$Discipline == "Long Jump"), ]
longJumpWomanYouth <- longJumpWomanYouth[which(longJumpWomanYouth$Year>=1990), ]
longJumpWomanYouth <- longJumpWomanYouth[which(longJumpWomanYouth$Competition == "World Youth Championships"
                                           | longJumpWomanYouth$Competition == "Youth Olympic Games"), ]
longJumpWomanYouth <- longJumpWomanYouth[which(longJumpWomanYouth$Gender == "Women"), ]
longJumpWomanYouth <- longJumpWomanYouth[which(longJumpWomanYouth$Class == "Youth"), ]
longJumpWomanYouth <- longJumpWomanYouth[which(longJumpWomanYouth$Result != ""), ]
# longJumpWomanYouth <- longJumpWomanYouth[which(longJumpWomanYouth$Medal != ""), ]
longJumpWomanYouth[, "Result"] <- as.double(longJumpWomanYouth$Result)
longJumpWomanYouth <- longJumpWomanYouth[which(!is.na(longJumpWomanYouth$Result)), ]
summary(longJumpWomanYouth$Result)


i <- 2
j <- 2
k <- 2
vector_senior_year <- c(max(longJumpWoman[which(longJumpWoman$Year == 2000), "Result"]))
vector_junior_year <- c(max(longJumpWomanJunior[which(longJumpWomanJunior$Year == 2000), "Result"]))
vector_youth_year <- c(max(longJumpWomanYouth[which(longJumpWomanYouth$Year == 2001), "Result"]))
for (year in 2001:2022){
  vector_senior_year[i] <- max(c(max(longJumpWoman[which(longJumpWoman$Year == year), "Result"]), vector_senior_year[i-1]))
  i <- i + 1
  vector_junior_year[j] <- max(c(max(longJumpWomanJunior[which(longJumpWomanJunior$Year == year), "Result"]), vector_junior_year[j-1]))
  j <- j + 1
  vector_youth_year[k] <- max(c(max(longJumpWomanYouth[which(longJumpWomanYouth$Year == year), "Result"]), vector_youth_year[k-1]))
  k <- k + 1
}

i <- 2
j <- 2
k <- 2
vector_senior_year_8 <- c(longJumpWoman[which(longJumpWoman$Year == 2000 & longJumpWoman$Rank == "8"), "Result"])
vector_junior_year_8 <- c(longJumpWomanJunior[which(longJumpWomanJunior$Year == 2000 & longJumpWomanJunior$Rank == "8"), "Result"])
vector_youth_year_8 <- c(longJumpWomanYouth[which(longJumpWomanYouth$Year == 2001 & longJumpWomanYouth$Rank == "8"), "Result"])
for (year in 2001:2022){
  vector_senior_year_8[i] <- max(c(longJumpWoman[which(longJumpWoman$Year == year & longJumpWoman$Rank == "8"), "Result"], vector_senior_year_8[i-1]))
  i <- i + 1
  vector_junior_year_8[j] <- max(c(longJumpWomanJunior[which(longJumpWomanJunior$Year == year & longJumpWomanJunior$Rank == "8"), "Result"], vector_junior_year_8[j-1]))
  j <- j + 1
  vector_youth_year_8[k] <- max(c(longJumpWomanYouth[which(longJumpWomanYouth$Year == year & longJumpWomanYouth$Rank == "8"), "Result"], vector_youth_year_8[k-1]))
  k <- k + 1
}

vector_best_year <- c(vector_senior_year, vector_junior_year, vector_youth_year)
vector_8_year <- c(vector_senior_year_8, vector_junior_year_8, vector_youth_year_8)
v1 <- 2000:2022
years <- c(v1,v1,v1)
years
vector_class <- c()
vector_class[1:23] <- "Seniors"
vector_class[24:46] <- "Juniors"
vector_class[47:69] <- "Youth"
vector_class

df_result <- data.frame(Year = years,
                        Top_8 = vector_8_year,
                        Top_1 = vector_best_year,
                        Class = vector_class)
df_result

switzerland_athlete_Women_LongJump <- olympics[which(olympics$Discipline == "Long Jump"), ]
switzerland_athlete_Women_LongJump <- switzerland_athlete_Women_LongJump[which(switzerland_athlete_Women_LongJump$Year>=2000), ]
switzerland_athlete_Women_LongJump <- switzerland_athlete_Women_LongJump[which(switzerland_athlete_Women_LongJump$Gender == "Women"), ]
switzerland_athlete_Women_LongJump <- switzerland_athlete_Women_LongJump[which(switzerland_athlete_Women_LongJump$Nationality == "SUI"), ]
switzerland_athlete_Women_LongJump <- switzerland_athlete_Women_LongJump[which(switzerland_athlete_Women_LongJump$Result != ""), ]
# sprint400mMan <- sprint400mMan[which(sprint400mMan$Medal != ""), ]
switzerland_athlete_Women_LongJump[, "Result"] <- as.double(switzerland_athlete_Women_LongJump$Result)
summary(switzerland_athlete_Women_LongJump$Result)
table(switzerland_athlete_Women_LongJump$Person)


switzerland_athlete_Women_LongJump <- data.frame(Name = switzerland_athlete_Women_LongJump$Person,
                                               Result = switzerland_athlete_Women_LongJump$Result,
                                               Year = switzerland_athlete_Women_LongJump$Year,
                                               Class = switzerland_athlete_Women_LongJump$Class)

switzerland_athlete_Women_LongJump <- merge(x = switzerland_athlete_Women_LongJump,
                                          y = df_result, 
                                          by = c("Year", "Class")
)
switzerland_athlete_Women_LongJump


# Da fare in shiny con un input sul nome dell'atleta.

athlete_list_W_LongJump <- as.list(unique(switzerland_athlete_Women_LongJump$Name))
athlete_result <- c()
athlete_name <- c()
athlete_year <- c()
athlete_class <- c()
top8 <- c()
top1 <- c()
i = 1
for (name in athlete_list_W_LongJump){
  print(name)
  for (year in min(switzerland_athlete_Women_LongJump$Year):max(switzerland_athlete_Women_LongJump$Year)){
    print(year)
    if(length(switzerland_athlete_Women_LongJump[which(switzerland_athlete_Women_LongJump$Name == name 
                                                     & switzerland_athlete_Women_LongJump$Year == year), "Result"] != 0)){
      athlete_name[i] <- name
      
      athlete_result[i] <- max(switzerland_athlete_Women_LongJump[which(switzerland_athlete_Women_LongJump$Name == name
                                                                      & switzerland_athlete_Women_LongJump$Year == year),"Result"])
      athlete_year[i] <- year
      athlete_class[i] <- switzerland_athlete_Women_LongJump[which(switzerland_athlete_Women_LongJump$Result == athlete_result[i] &
                                                                     switzerland_athlete_Women_LongJump$Name == name & 
                                                                     switzerland_athlete_Women_LongJump$Year == year),"Class"]
      top1[i] <- switzerland_athlete_Women_LongJump[which(switzerland_athlete_Women_LongJump$Result == athlete_result[i] &
                                                          switzerland_athlete_Women_LongJump$Name == name & 
                                                          switzerland_athlete_Women_LongJump$Year == year),"Top_1"]
      print(top1[i])
      top8[i] <- switzerland_athlete_Women_LongJump[which(switzerland_athlete_Women_LongJump$Result == athlete_result[i] &
                                                            switzerland_athlete_Women_LongJump$Name == name & 
                                                            switzerland_athlete_Women_LongJump$Year == year),"Top_8"]
      i <- i + 1
    }
  }
}
athleteLongJumpW <- data.frame(Year = athlete_year,
                               Name = athlete_name,
                               Result = athlete_result,
                               Class = athlete_class,
                               Top_8 = top8,
                               Top_1 = top1)
athleteLongJumpW

WRLJWomen <- max(longJumpWoman$Result)
WRLJWomenJ <- max(longJumpWomanJunior$Result)
WRLJWomenY <- max(longJumpWomanYouth$Result)

SRLJWomen <- max(athleteLongJumpW$Result[which(athleteLongJumpW$Class == "Seniors")])
SRLJWomenJ <- max(athleteLongJumpW$Result[which(athleteLongJumpW$Class == "Juniors")])
SRLJWomenY <- max(athleteLongJumpW$Result[which(athleteLongJumpW$Class == "Youth")])

i <- 1
a_name <- c()
result <- c()
class <- c()
wr <- c()
sr <- c()
for (name in athlete_list_W_LongJump){
  a_name[i] <- name
  if (max(athleteLongJumpW[which(athleteLongJumpW$Name == name & athleteLongJumpW$Class == "Seniors"), "Result"]) == -Inf){
    result[i] <- 0
  }
  else{
    result[i] <- (max(athleteLongJumpW[which(athleteLongJumpW$Name == name & athleteLongJumpW$Class == "Seniors"), "Result"]))/(WRLJWomen)
  }
  print(result[i])
  class[i] <- "Seniors"
  wr[i] <- 1
  sr[i] <- (SRLJWomen)/(WRLJWomen)
  i <- i + 1
}
i <- 1
a_nameJ <- c()
resultJ <- c()
classJ <- c()
wrJ <- c()
srJ <- c()
for (name in athlete_list_W_LongJump){
  a_nameJ[i] <- name
  if (max(athleteLongJumpW[which(athleteLongJumpW$Name == name & athleteLongJumpW$Class == "Juniors"), "Result"]) == -Inf){
    resultJ[i] <- 0
  }
  else{
    resultJ[i] <- (max(athleteLongJumpW[which(athleteLongJumpW$Name == name & athleteLongJumpW$Class == "Juniors"), "Result"]))/(WRLJWomenJ)
  }
  print(result[i])
  classJ[i] <- "Juniors"
  wrJ[i] <- 1
  srJ[i] <- (SRLJWomenJ)/(WRLJWomenJ)
  
  i <- i + 1
}
i <- 1
a_nameY <- c()
resultY <- c()
classY <- c()
wrY <- c()
srY <- c()
for (name in athlete_list_W_LongJump){
  a_nameY[i] <- name
  if (max(athleteLongJumpW[which(athleteLongJumpW$Name == name & athleteLongJumpW$Class == "Youth"), "Result"]) == -Inf){
    resultY[i] <- 0
  }
  else{
    resultY[i] <- (max(athleteLongJumpW[which(athleteLongJumpW$Name == name & athleteLongJumpW$Class == "Youth"), "Result"]))/(WRLJMenY)
  }
  classY[i] <- "Youth"
  wrY[i] <- 1
  srY[i] <- (SRLJWomenY)/(WRLJWomenY)
  i <- i + 1
}
i <- 1
radar_frameS <- data.frame(Name = a_name,
                           Result = result, 
                           Class = class, 
                           WR = wr,
                           SR = sr)
radar_frameJ <- data.frame(Name = a_nameJ,
                           Result = resultJ, 
                           Class = classJ, 
                           WR = wrJ,
                           SR = srJ)
radar_frameY <- data.frame(Name = a_nameY,
                           Result = resultY, 
                           Class = classY, 
                           WR = wrY,
                           SR = srY)

radar_frameLJW <- merge(x = merge(x = radar_frameS, y = radar_frameJ, by = "Name"), y = radar_frameY, by = "Name")
radar_frameLJW

radar_frameLJW <- subset(radar_frameLJW, select = -c(Class.x, WR.x, SR.x, Class.y, WR.y, SR.y, Class, WR, SR))

colnames(radar_frameLJW) <- c("Name", "Seniors", "Juniors", "Youth")

radar_frameLJW[(length(radar_frameLJW$Name) + 1), ] <- c("World Record in the class", 1,1,1)
radar_frameLJW[(length(radar_frameLJW$Name) + 1), ] <- c("Fictious baseline", 0,0,0)
radar_frameLJW[(length(radar_frameLJW$Name) + 1), ] <- c("Swiss Record in the class", (SRLJWomen)/(WRLJWomen),
                                                       (SRLJWomenJ)/(WRLJWomenJ), 
                                                       (SRLJWomenY)/(WRLJWomenY))
radar_frameLJW$Seniors <- as.numeric(radar_frameLJW$Seniors)
radar_frameLJW$Juniors <- as.numeric(radar_frameLJW$Juniors)
radar_frameLJW$Youth <- as.numeric(radar_frameLJW$Youth)
radar_frameLJW

radarchart(radar_frameLJW[which(radar_frameLJW$Name == "Irene Pusterla" | radar_frameLJW$Name == "World Record in the class"
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
legend(1,1, legend=unique(radar_frameLJW[which(radar_frameLJW$Name == "Irene Pusterla" | radar_frameLJW$Name == "World Record in the class"
                                              | radar_frameLJW$Name == "Fictious baseline" | radar_frameLJW$Name == "Swiss Record in the class"), "Name"])
       , seg.len=0.5, pch=1, border = "black",
       bty="n" ,lwd=3, y.intersp=1, horiz=FALSE, col =c("red", "gold", "lightblue", "darkred")
)


ggplot() +
  geom_point(data = athleteLongJumpW, aes(x = Year, y = Result, colour = Class))

# no significance 
fit <- lm(data = athleteLongJumpW[which(athleteLongJumpW$Class == "Seniors"), ], 
          formula = Result ~ Year)
summary(fit)

# no significance
fit <- lm(data = athleteLongJumpW[which(athleteLongJumpW$Class == "Juniors"), ], 
          formula = Result ~ Year)
summary(fit)

# No significance to state anything
fit <- lm(data = athleteLongJumpW[which(athleteLongJumpW$Class == "Youth"), ], 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = athleteLongJumpW[which(athleteLongJumpW$Name == athlete_list_W_LongJump[[3]]), ],
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

athleteLongJumpW[, "Difference_8"] <- athleteLongJumpW$Result - athleteLongJumpW$Top_8

ggplot() +
  geom_point(data = athleteLongJumpW, aes(x = Year, y = Difference_8, colour = Class))

# no significance 
fit <- lm(data = athleteLongJumpW[which(athleteLongJumpW$Class == "Seniors"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

# no significance 
fit <- lm(data = athleteLongJumpW[which(athleteLongJumpW$Class == "Juniors"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

# No significance to state anything 
fit <- lm(data = athleteLongJumpW[which(athleteLongJumpW$Class == "Youth"), ], 
          formula = Difference_8 ~ Year)
summary(fit)

p <- ggplot(data = athleteLongJumpW)+
  geom_histogram(aes(x = Difference_8, fill = Class), binwidth = .2) +
  geom_histogram(data = athleteLongJumpW[which(athleteLongJumpW$Name == athlete_list_W_LongJump[[3]]), ],
                 aes(x = Difference_8), binwidth = 0.2) +
  scale_fill_manual(values = heat.colors(4)) 

ggplotly(p)

################################################################################
################################################################################
################################################################################



