olympics <- read.csv("C:/Users/buson/OneDrive/Documenti/R_universita/sportdataanalytics/Data/athletics.csv")
table(olympics$Discipline)

df_momentaneo <- olympics[which(olympics$Discipline == "100m"), ]
df_momentaneo <- df_momentaneo[which(df_momentaneo$Gender == "Women"), ]

#######################################################################################################################
###################################################100m Men############################################################
#######################################################################################################################
suimale100m <- olympics[which(olympics$Discipline == "100m"), ]
suimale100m <- suimale100m[which(suimale100m$Gender == "Men"), ]
suimale100m <- suimale100m[which(suimale100m$Class != "Under 23"), ]
suimale100m <- suimale100m[which(suimale100m$Nationality == "SUI"), ]
suimale100m <- suimale100m[which(suimale100m$Result != ""), ]
suimale100m <- suimale100m[which(suimale100m$Result <= 12), ]
# suimale100m <- male100m[which(male100m$Medal != ""), ]
suimale100m[, "Result"] <- as.double(suimale100m$Result)
summary(suimale100m$Result)

fit <- lm(data = suimale100m[which(suimale100m$Class == "Seniors"), ], 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suimale100m,
       aes(x = Year, y = Result, color = Class)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)


suimale100mJunior <- olympics[which(olympics$Discipline == "100m"), ]
suimale100mJunior <- suimale100mJunior[which(suimale100mJunior$Gender == "Men"), ]
suimale100mJunior <- suimale100mJunior[which(suimale100mJunior$Class == "Juniors"), ]
suimale100mJunior <- suimale100mJunior[which(suimale100mJunior$Nationality == "SUI"), ]
suimale100mJunior <- suimale100mJunior[which(suimale100mJunior$Result != ""), ]
# suimale100mJunior <- suimale100mJunior[which(suimale100mJunior$Result <= 12), ]
# suimale100mJunior <- suimale100mJunior[which(suimale100mJunior$Medal != ""), ]
suimale100mJunior[, "Result"] <- as.double(suimale100mJunior$Result)
summary(suimale100mJunior$Result)

fit <- lm(data = suimale100mJunior, 
          formula = Result ~ Year)
summary(fit)


suimale100mYouth <- olympics[which(olympics$Discipline == "100m"), ]
suimale100mYouth <- suimale100mYouth[which(suimale100mYouth$Gender == "Men"), ]
suimale100mYouth <- suimale100mYouth[which(suimale100mYouth$Class == "Youth"), ]
suimale100mYouth <- suimale100mYouth[which(suimale100mYouth$Nationality == "SUI"), ]
suimale100mYouth <- suimale100mYouth[which(suimale100mYouth$Result != ""), ]
# suimale100mYouth <- suimale100mYouth[which(suimale100mYouth$Result <= 12), ]
# suimale100mYouth <- suimale100mYouth[which(suimale100mYouth$Medal != ""), ]
suimale100mYouth[, "Result"] <- as.double(suimale100mYouth$Result)
summary(suimale100mYouth$Result)

fit <- lm(data = suimale100mYouth, 
          formula = Result ~ Year)
summary(fit)

#######################################################################################################################
################################################# 100m Women###########################################################
#######################################################################################################################
suifemale100m <- olympics[which(olympics$Discipline == "100m"), ]
suifemale100m <- suifemale100m[which(suifemale100m$Gender == "Women"), ]
suifemale100m <- suifemale100m[which(suifemale100m$Class != "Under 23"), ]
suifemale100m <- suifemale100m[which(suifemale100m$Nationality == "SUI"), ]
suifemale100m <- suifemale100m[which(suifemale100m$Result != ""), ]
suifemale100m <- suifemale100m[which(suifemale100m$Result <= 12), ]
# suifemale100m <- suifemale100m[which(suifemale100m$Medal != ""), ]
suifemale100m[, "Result"] <- as.double(suifemale100m$Result)
summary(suifemale100m$Result)

# you can try to use year >= 2002 to better understand. 
fit <- lm(data = suifemale100m[which(suifemale100m$Class == "Seniors"), ], 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suifemale100m,
            aes(x = Year, y = Result, color = Class)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)


suifemale100mJunior <- olympics[which(olympics$Discipline == "100m"), ]
suifemale100mJunior <- suifemale100mJunior[which(suifemale100mJunior$Gender == "Women"), ]
suifemale100mJunior <- suifemale100mJunior[which(suifemale100mJunior$Class == "Juniors"), ]
suifemale100mJunior <- suifemale100mJunior[which(suifemale100mJunior$Nationality == "SUI"), ]
suifemale100mJunior <- suifemale100mJunior[which(suifemale100mJunior$Result != ""), ]
suifemale100mJunior[, "Result"] <- as.double(suifemale100mJunior$Result)
summary(suifemale100mJunior$Result)

fit <- lm(data = suifemale100mJunior, 
          formula = Result ~ Year)
summary(fit)

suifemale100mYouth <- olympics[which(olympics$Discipline == "100m"), ]
suifemale100mYouth <- suifemale100mYouth[which(suifemale100mYouth$Gender == "Women"), ]
suifemale100mYouth <- suifemale100mYouth[which(suifemale100mYouth$Class == "Youth"), ]
suifemale100mYouth <- suifemale100mYouth[which(suifemale100mYouth$Nationality == "SUI"), ]
suifemale100mYouth <- suifemale100mYouth[which(suifemale100mYouth$Result != ""), ]
suifemale100mYouth[, "Result"] <- as.double(suifemale100mYouth$Result)
summary(suifemale100mYouth$Result)

fit <- lm(data = suifemale100mYouth, 
          formula = Result ~ Year)
summary(fit)

#######################################################################################################################
################################################# 200m Men ############################################################
#######################################################################################################################
suimale200m <- olympics[which(olympics$Discipline == "200m"), ]
suimale200m <- suimale200m[which(suimale200m$Class!="Under 23"), ]
suimale200m <- suimale200m[which(suimale200m$Gender == "Men"), ]
suimale200m <- suimale200m[which(suimale200m$Nationality == "SUI"), ]
suimale200m <- suimale200m[which(suimale200m$Result != ""), ]
suimale200m[, "Result"] <- as.double(suimale200m$Result)
summary(suimale200m$Result)


fit <- lm(data = suimale200m, 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suimale200m,
            aes(x = Year, y = Result, colour = Class)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)


suimale200mJunior <- olympics[which(olympics$Discipline == "200m"), ]
suimale200mJunior <- suimale200mJunior[which(suimale200mJunior$Gender == "Men"), ]
suimale200mJunior <- suimale200mJunior[which(suimale200mJunior$Class == "Juniors"), ]
suimale200mJunior <- suimale200mJunior[which(suimale200mJunior$Nationality == "SUI"), ]
suimale200mJunior <- suimale200mJunior[which(suimale200mJunior$Result != ""), ]
suimale200mJunior[, "Result"] <- as.double(suimale200mJunior$Result)
summary(suimale200mJunior$Result)

# just a few values, not possible...
fit <- lm(data = suimale200mJunior, 
          formula = Result ~ Year)
summary(fit)


suimale200mYouth <- olympics[which(olympics$Discipline == "200m"), ]
suimale200mYouth <- suimale200mYouth[which(suimale200mYouth$Gender == "Men"), ]
suimale200mYouth <- suimale200mYouth[which(suimale200mYouth$Class == "Youth"), ]
suimale200mYouth <- suimale200mYouth[which(suimale200mYouth$Nationality == "SUI"), ]
suimale200mYouth <- suimale200mYouth[which(suimale200mYouth$Result != ""), ]
suimale200mYouth[, "Result"] <- as.double(suimale200mYouth$Result)
summary(suimale200mYouth$Result)

# just a few values, not possible to infer anything...
fit <- lm(data = suimale200mYouth, 
          formula = Result ~ Year)
summary(fit)

#######################################################################################################################
################################################### 200m Women ########################################################
#######################################################################################################################
suifemale200m <- olympics[which(olympics$Discipline == "200m"), ]
suifemale200m <- suifemale200m[which(suifemale200m$Year>=1990), ]
suifemale200m <- suifemale200m[which(suifemale200m$Gender == "Women"), ]
suifemale200m <- suifemale200m[which(suifemale200m$Class == "Seniors"), ]
suifemale200m <- suifemale200m[which(suifemale200m$Nationality == "SUI"), ]
suifemale200m <- suifemale200m[which(suifemale200m$Result != ""), ]
suifemale200m[, "Result"] <- as.double(suifemale200m$Result)
summary(suifemale200m$Result)


fit <- lm(data = suifemale200m, 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suifemale200m,
            aes(x = Year, y = Result)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)


suifemale200mJunior <- olympics[which(olympics$Discipline == "200m"), ]
suifemale200mJunior <- suifemale200mJunior[which(suifemale200mJunior$Year>=1990), ]
suifemale200mJunior <- suifemale200mJunior[which(suifemale200mJunior$Gender == "Women"), ]
suifemale200mJunior <- suifemale200mJunior[which(suifemale200mJunior$Class == "Juniors"), ]
suifemale200mJunior <- suifemale200mJunior[which(suifemale200mJunior$Nationality == "SUI"), ]
suifemale200mJunior <- suifemale200mJunior[which(suifemale200mJunior$Result != ""), ]
suifemale200mJunior[, "Result"] <- as.double(suifemale200mJunior$Result)
summary(suifemale200mJunior$Result)

fit <- lm(data = suifemale200mJunior, 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suifemale200mJunior,
            aes(x = Year, y = Result)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)


suifemale200mYouth <- olympics[which(olympics$Discipline == "200m"), ]
suifemale200mYouth <- suifemale200mYouth[which(suifemale200mYouth$Year>=1990), ]
suifemale200mYouth <- suifemale200mYouth[which(suifemale200mYouth$Gender == "Women"), ]
suifemale200mYouth <- suifemale200mYouth[which(suifemale200mYouth$Class == "Youth"), ]
suifemale200mYouth <- suifemale200mYouth[which(suifemale200mYouth$Nationality == "SUI"), ]
suifemale200mYouth <- suifemale200mYouth[which(suifemale200mYouth$Result != ""), ]
suifemale200mYouth[, "Result"] <- as.double(suifemale200mYouth$Result)
summary(suifemale200mYouth$Result)

# just a few values, not possible to infer anything...
fit <- lm(data = suifemale200mYouth, 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suifemale200mYouth,
            aes(x = Year, y = Result)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)

#######################################################################################################################
################################################# 400m Men ############################################################
#######################################################################################################################
suimale400m <- olympics[which(olympics$Discipline == "400m"), ]
suimale400m <- suimale400m[which(suimale400m$Year>=1990), ]
suimale400m <- suimale400m[which(suimale400m$Gender == "Men"), ]
suimale400m <- suimale400m[which(suimale400m$Class == "Seniors"), ]
suimale400m <- suimale400m[which(suimale400m$Nationality == "SUI"), ]
suimale400m <- suimale400m[which(suimale400m$Result != ""), ]
suimale400m[, "Result"] <- as.double(suimale400m$Result)
summary(suimale400m$Result)


fit <- lm(data = suimale400m, 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suimale400m,
            aes(x = Year, y = Result)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)


suimale400mJunior <- olympics[which(olympics$Discipline == "400m"), ]
suimale400mJunior <- suimale400mJunior[which(suimale400mJunior$Year>=1990), ]
suimale400mJunior <- suimale400mJunior[which(suimale400mJunior$Gender == "Men"), ]
suimale400mJunior <- suimale400mJunior[which(suimale200mJunior$Class == "Juniors"), ]
suimale400mJunior <- suimale400mJunior[which(suimale400mJunior$Nationality == "SUI"), ]
suimale400mJunior <- suimale400mJunior[which(suimale400mJunior$Result != ""), ]
suimale400mJunior[, "Result"] <- as.double(suimale400mJunior$Result)
summary(suimale400mJunior$Result)

fit <- lm(data = suimale400mJunior, 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suimale400mJunior,
            aes(x = Year, y = Result)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)


suimale400mYouth <- olympics[which(olympics$Discipline == "400m"), ]
suimale400mYouth <- suimale400mYouth[which(suimale400mYouth$Year>=1990), ]
suimale400mYouth <- suimale400mYouth[which(suimale400mYouth$Gender == "Men"), ]
suimale400mYouth <- suimale400mYouth[which(suimale400mYouth$Class == "Youth"), ]
suimale400mYouth <- suimale400mYouth[which(suimale400mYouth$Nationality == "SUI"), ]
suimale400mYouth <- suimale400mYouth[which(suimale400mYouth$Result != ""), ]
suimale400mYouth[, "Result"] <- as.double(suimale400mYouth$Result)
summary(suimale400mYouth$Result)

# just a few values, not possible to infer anything...
fit <- lm(data = suimale400mYouth, 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suimale400mYouth,
            aes(x = Year, y = Result)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)

#######################################################################################################################
################################################# 400m Women ##########################################################
#######################################################################################################################
suifemale400m <- olympics[which(olympics$Discipline == "400m"), ]
suifemale400m <- suifemale400m[which(suifemale400m$Year>=1990), ]
suifemale400m <- suifemale400m[which(suifemale400m$Gender == "Women"), ]
suifemale400m <- suifemale400m[which(suifemale400m$Class == "Seniors"), ]
suifemale400m <- suifemale400m[which(suifemale400m$Nationality == "SUI"), ]
suifemale400m <- suifemale400m[which(suifemale400m$Result != ""), ]
suifemale400m[, "Result"] <- as.double(suifemale400m$Result)
summary(suifemale400m$Result)

#not enough records
fit <- lm(data = suifemale400m, 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suifemale400m,
            aes(x = Year, y = Result)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)


suifemale400mJunior <- olympics[which(olympics$Discipline == "400m"), ]
suifemale400mJunior <- suifemale400mJunior[which(suifemale400mJunior$Year>=1990), ]
suifemale400mJunior <- suifemale400mJunior[which(suifemale400mJunior$Gender == "Women"), ]
suifemale400mJunior <- suifemale400mJunior[which(suifemale400mJunior$Class == "Juniors"), ]
suifemale400mJunior <- suifemale400mJunior[which(suifemale400mJunior$Nationality == "SUI"), ]
suifemale400mJunior <- suifemale400mJunior[which(suifemale400mJunior$Result != ""), ]
suifemale400mJunior[, "Result"] <- as.double(suifemale400mJunior$Result)
summary(suifemale400mJunior$Result)

# not enough record, but the fit seems to be okay at 5%
fit <- lm(data = suifemale400mJunior, 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suifemale400mJunior,
            aes(x = Year, y = Result)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)


suifemale400mYouth <- olympics[which(olympics$Discipline == "400m"), ]
suifemale400mYouth <- suifemale400mYouth[which(suifemale400mYouth$Year>=1990), ]
suifemale400mYouth <- suifemale400mYouth[which(suifemale400mYouth$Gender == "Women"), ]
suifemale400mYouth <- suifemale400mYouth[which(suifemale400mYouth$Class == "Youth"), ]
suifemale400mYouth <- suifemale400mYouth[which(suifemale400mYouth$Nationality == "SUI"), ]
suifemale400mYouth <- suifemale400mYouth[which(suifemale400mYouth$Result != ""), ]
suifemale400mYouth[, "Result"] <- as.double(suifemale400mYouth$Result)
summary(suifemale400mYouth$Result)

# not enough records and no significance
fit <- lm(data = suifemale400mYouth, 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suifemale400mYouth,
            aes(x = Year, y = Result)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)

#######################################################################################################################
################################################# Long Jump Men #######################################################
#######################################################################################################################
suimaleLJ <- olympics[which(olympics$Discipline == "Long Jump"), ]
suimaleLJ <- suimaleLJ[which(suimaleLJ$Year>=1990), ]
suimaleLJ <- suimaleLJ[which(suimaleLJ$Gender == "Men"), ]
suimaleLJ <- suimaleLJ[which(suimaleLJ$Class == "Seniors"), ]
suimaleLJ <- suimaleLJ[which(suimaleLJ$Nationality == "SUI"), ]
suimaleLJ <- suimaleLJ[which(suimaleLJ$Result != ""), ]
suimaleLJ[, "Result"] <- as.double(suimaleLJ$Result)
summary(suimaleLJ$Result)

# no fit
fit <- lm(data = suimaleLJ, 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suimaleLJ,
            aes(x = Year, y = Result)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)


suimaleLJJunior <- olympics[which(olympics$Discipline == "Long Jump"), ]
suimaleLJJunior <- suimaleLJJunior[which(suimaleLJJunior$Year>=1990), ]
suimaleLJJunior <- suimaleLJJunior[which(suimaleLJJunior$Gender == "Men"), ]
suimaleLJJunior <- suimaleLJJunior[which(suimaleLJJunior$Class == "Juniors"), ]
suimaleLJJunior <- suimaleLJJunior[which(suimaleLJJunior$Nationality == "SUI"), ]
suimaleLJJunior <- suimaleLJJunior[which(suimaleLJJunior$Result != ""), ]
suimaleLJJunior[, "Result"] <- as.double(suimaleLJJunior$Result)
summary(suimaleLJJunior$Result)

fit <- lm(data = suimaleLJJunior, 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suimaleLJJunior,
            aes(x = Year, y = Result)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)


suimaleLJYouth <- olympics[which(olympics$Discipline == "Long Jump"), ]
suimaleLJYouth <- suimaleLJYouth[which(suimaleLJYouth$Year>=1990), ]
suimaleLJYouth <- suimaleLJYouth[which(suimaleLJYouth$Gender == "Men"), ]
suimaleLJYouth <- suimaleLJYouth[which(suimaleLJYouth$Class == "Youth"), ]
suimaleLJYouth <- suimaleLJYouth[which(suimaleLJYouth$Nationality == "SUI"), ]
suimaleLJYouth <- suimaleLJYouth[which(suimaleLJYouth$Result != ""), ]
suimaleLJYouth[, "Result"] <- as.double(suimaleLJYouth$Result)
summary(suimaleLJYouth$Result)

# just a few values, not possible to infer anything +
fit <- lm(data = suimaleLJYouth, 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suimaleLJYouth,
            aes(x = Year, y = Result)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)

#######################################################################################################################
############################################### Long Jump Women #######################################################
#######################################################################################################################
suifemaleLJ <- olympics[which(olympics$Discipline == "Long Jump"), ]
suifemaleLJ <- suifemaleLJ[which(suifemaleLJ$Gender == "Women"), ]
suifemaleLJ <- suifemaleLJ[which(suifemaleLJ$Class != "Under 23"), ]
suifemaleLJ <- suifemaleLJ[which(suifemaleLJ$Nationality == "SUI"), ]
suifemaleLJ <- suifemaleLJ[which(suifemaleLJ$Result != ""), ]
suifemaleLJ[, "Result"] <- as.double(suifemaleLJ$Result)
suifemaleLJ <- suifemaleLJ[which(suifemaleLJ$Result>2.1),]
summary(suimaleLJ$Result)

# no fit
fit <- lm(data = suifemaleLJ, 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suifemaleLJ,
            aes(x = Year, y = Result, colour = Class)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)


suifemaleLJJunior <- olympics[which(olympics$Discipline == "Long Jump"), ]
suifemaleLJJunior <- suifemaleLJJunior[which(suifemaleLJJunior$Year>=1990), ]
suifemaleLJJunior <- suifemaleLJJunior[which(suifemaleLJJunior$Gender == "Men"), ]
suifemaleLJJunior <- suifemaleLJJunior[which(suifemaleLJJunior$Class == "Juniors"), ]
suifemaleLJJunior <- suifemaleLJJunior[which(suifemaleLJJunior$Nationality == "SUI"), ]
suifemaleLJJunior <- suifemaleLJJunior[which(suifemaleLJJunior$Result != ""), ]
suifemaleLJJunior[, "Result"] <- as.double(suifemaleLJJunior$Result)
summary(suifemaleLJJunior$Result)

# not enough data
fit <- lm(data = suifemaleLJJunior, 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suifemaleLJJunior,
            aes(x = Year, y = Result)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)


suifemaleLJYouth <- olympics[which(olympics$Discipline == "Long Jump"), ]
suifemaleLJYouth <- suifemaleLJYouth[which(suifemaleLJYouth$Year>=1990), ]
suifemaleLJYouth <- suifemaleLJYouth[which(suifemaleLJYouth$Gender == "Men"), ]
suifemaleLJYouth <- suifemaleLJYouth[which(suifemaleLJYouth$Class == "Youth"), ]
suifemaleLJYouth <- suifemaleLJYouth[which(suifemaleLJYouth$Nationality == "SUI"), ]
suifemaleLJYouth <- suifemaleLJYouth[which(suifemaleLJYouth$Result != ""), ]
suifemaleLJYouth[, "Result"] <- as.double(suifemaleLJYouth$Result)
summary(suifemaleLJYouth$Result)

# just a few values, not possible to infer anything
fit <- lm(data = suifemaleLJYouth, 
          formula = Result ~ Year)
summary(fit)

p <- ggplot(data = suifemaleLJYouth,
            aes(x = Year, y = Result)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplotly(p)