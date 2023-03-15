#########################
# helper
########################

# setup----------------------
# load packages
library(pacman)
pacman::p_load(janitor, dplyr, tidyr, ggplot2, plotly, fmsb)

# load data
athletics_master <- read.csv("C:/Users/buson/OneDrive/Documenti/R_universita/sportdataanalytics/Data/athletics.csv")

# 100m----------------------
getplot100m <- function(gender){
  # data cleaning and selection 
  athletics_selection <- athletics_master %>% 
    mutate(Rank = as.numeric(Rank), Result = as.numeric(Result)) %>% 
    filter(Year %in% c(2000:2023), !is.na(Rank), !is.na(Result), !Competition == "Diamond League", Discipline == "100m", Gender == gender) %>% 
    select(Year, Competition, Discipline, Gender, Class, Person.Team, Person, Rank, Age, Nationality, Result)
  
  if(gender == "Men"){
    # results of finalists in Wch and OG who came in last
    rank8_wch_og <- athletics_selection %>% 
      # filter results so that outliers are not being taken into consideration
      filter(Rank %in% c(1:8), Result < 10.5, Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
      group_by(Year) %>% 
      slice_max(Rank) %>% 
      ungroup()%>% 
      mutate(reference_type = "Certificate_WCh_and_OG")
  } else{
    # results of finalists in Wch and OG who came in last
    rank8_wch_og <- athletics_selection %>% 
      # filter results so that outliers are not being taken into consideration
      filter(Rank %in% c(1:8), Result < 11.5, Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
      group_by(Year) %>% 
      slice_max(Rank) %>% 
      ungroup()%>% 
      mutate(reference_type = "Certificate_WCh_and_OG")
  }
  
  # results of finalists in Wch and OG who came in first
  rank1_wch_og <- athletics_selection %>% 
    filter(Rank %in% c(1:8), Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
    group_by(Year) %>% 
    slice_min(Rank) %>% 
    ungroup() %>% 
    group_by(Year, Rank) %>% 
    slice_min(Result) %>% 
    ungroup() %>% 
    mutate(reference_type = "Rank1_WCh_and_OG")
  
  # best result of each season from swiss athletes
  swissathletes <- athletics_selection %>% 
    filter(Nationality == "SUI", Class == "Seniors") %>%
    group_by(Year) %>% 
    slice_min(Result) %>%
    ungroup() %>% 
    mutate(reference_type = "Swiss_Season_Best") 
  
  # combining dataframes to one whole
  referencetimes <- bind_rows(rank8_wch_og, rank1_wch_og, swissathletes) 
  
  
  # create ggplot
  g_reference <- ggplot(data = referencetimes, mapping = aes(Year, Result))+
    geom_line(mapping = aes(colour = reference_type))+
    theme_minimal()+
    theme(
      legend.position = "none"
    )
  
  ggplotly(g_reference)
}

gettable100m <- function(gender){
  # data cleaning and selection 
  athletics_selection <- athletics_master %>% 
    mutate(Rank = as.numeric(Rank), Result = as.numeric(Result)) %>% 
    filter(Year %in% c(2000:2023), !is.na(Rank), !is.na(Result), !Competition == "Diamond League", Discipline == "100m", Gender == gender) %>% 
    select(Year, Competition, Discipline, Gender, Class, Person.Team, Person, Rank, Age, Nationality, Result)
  # best result of each season from swiss athletes
  swissathletes <- athletics_selection %>% 
    filter(Nationality == "SUI", Class == "Seniors") %>%
    group_by(Year) %>% 
    slice_min(Result) %>%
    ungroup() %>% 
    mutate(reference_type = "Swiss_Season_Best") %>% 
    mutate(Rank = as.character(Rank))
}

# 200m------------------------
getplot200m <- function(gender){
  # data cleaning and selection 
  athletics_selection <- athletics_master %>% 
    mutate(Rank = as.numeric(Rank), Result = as.numeric(Result)) %>% 
    filter(Year %in% c(2000:2023), !is.na(Rank), !is.na(Result), !Competition == "Diamond League", 
           Discipline == "200m", Gender == gender) %>% 
    select(Year, Competition, Discipline, Gender, Class, Person.Team, Person, Rank, Age, Nationality, Result)
  
  if(gender == "Men"){
    # results of finalists in Wch and OG who came in last
    rank8_wch_og <- athletics_selection %>% 
      # filter results so that outliers are not being taken into consideration
      filter(Rank %in% c(1:8), Result < 21.0, Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
      group_by(Year) %>% 
      slice_max(Rank) %>% 
      ungroup()%>% 
      mutate(reference_type = "Certificate_WCh_and_OG")
  } else{
    # results of finalists in Wch and OG who came in last
    rank8_wch_og <- athletics_selection %>% 
      # filter results so that outliers are not being taken into consideration
      filter(Rank %in% c(1:8), Result < 23.5, Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
      group_by(Year) %>% 
      slice_max(Rank) %>% 
      ungroup()%>% 
      mutate(reference_type = "Certificate_WCh_and_OG")
  }
  
  # results of finalists in Wch and OG who came in first
  rank1_wch_og <- athletics_selection %>% 
    filter(Rank %in% c(1:8), Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
    group_by(Year) %>% 
    slice_min(Rank) %>% 
    ungroup() %>% 
    group_by(Year, Rank) %>% 
    slice_min(Result) %>% 
    ungroup() %>% 
    mutate(reference_type = "Rank1_WCh_and_OG")
  
  # best result of each season from swiss athletes
  swissathletes <- athletics_selection %>% 
    filter(Nationality == "SUI", Class == "Seniors") %>%
    group_by(Year) %>% 
    slice_min(Result) %>%
    ungroup() %>% 
    mutate(reference_type = "Swiss_Season_Best") 
  
  # combining dataframes to one whole
  referencetimes <- bind_rows(rank8_wch_og, rank1_wch_og, swissathletes) 
  
  
  # create ggplot
  g_reference <- ggplot(data = referencetimes, mapping = aes(Year, Result))+
    geom_line(mapping = aes(colour = reference_type))+
    theme_minimal()+
    theme(
      legend.position = "none"
    )
  
  ggplotly(g_reference)
}

gettable200m <- function(gender){
  # data cleaning and selection 
  athletics_selection <- athletics_master %>% 
    mutate(Rank = as.numeric(Rank), Result = as.numeric(Result)) %>% 
    filter(Year %in% c(2000:2023), !is.na(Rank), !is.na(Result), !Competition == "Diamond League", 
           Discipline == "200m", Gender == gender) %>% 
    select(Year, Competition, Discipline, Gender, Class, Person.Team, Person, Rank, Age, Nationality, Result)
  # best result of each season from swiss athletes
  swissathletes <- athletics_selection %>% 
    filter(Nationality == "SUI", Class == "Seniors") %>%
    group_by(Year) %>% 
    slice_min(Result) %>%
    ungroup() %>% 
    mutate(reference_type = "Swiss_Season_Best") %>% 
    mutate(Rank = as.character(Rank))
}

# 400m------------------------
getplot400m <- function(gender){
  # data cleaning and selection 
  athletics_selection <- athletics_master %>% 
    mutate(Rank = as.numeric(Rank), Result = as.numeric(Result)) %>% 
    filter(Year %in% c(2000:2023), !is.na(Rank), !is.na(Result), 
           !Competition == "Diamond League", Discipline == "400m", Gender == gender) %>% 
    select(Year, Competition, Discipline, Gender, Class, Person.Team, Person, Rank, Age, Nationality, Result)
  
  # results of finalists in Wch and OG who came in last
  rank8_wch_og <- athletics_selection %>% 
    filter(Rank %in% c(1:8), Class == "Seniors", 
           Competition %in% c("World Championships", "Olympic Games")) %>%
    group_by(Year) %>% 
    slice_max(Rank) %>% 
    ungroup()%>% 
    mutate(reference_type = "Certificate_WCh_and_OG")
  
  # results of finalists in Wch and OG who came in first
  rank1_wch_og <- athletics_selection %>% 
    filter(Rank %in% c(1:8), Class == "Seniors", 
           Competition %in% c("World Championships", "Olympic Games")) %>%
    group_by(Year) %>% 
    slice_min(Rank) %>% 
    ungroup() %>% 
    group_by(Year, Rank) %>% 
    slice_min(Result) %>% 
    ungroup() %>% 
    mutate(reference_type = "Rank1_WCh_and_OG")
  
  # best result of each season from swiss athletes
  swissathletes <- athletics_selection %>% 
    filter(Nationality == "SUI") %>%
    group_by(Year) %>% 
    slice_min(Result) %>%
    ungroup() %>% 
    mutate(reference_type = "Swiss_Season_Best")
  
  # combining dataframes to one whole
  referencetimes <- bind_rows(rank8_wch_og, rank1_wch_og, swissathletes)
  
  # create ggplot
  g_reference <- ggplot(data = referencetimes, mapping = aes(Year, Result))+
    geom_line(mapping = aes(colour = reference_type))+
    theme_minimal()+
    theme(
      legend.position = "none"
    )
  
  ggplotly(g_reference)
}

gettable400m <- function(gender){
  # data cleaning and selection 
  athletics_selection <- athletics_master %>% 
    mutate(Rank = as.numeric(Rank), Result = as.numeric(Result)) %>% 
    filter(Year %in% c(2000:2023), !is.na(Rank), !is.na(Result), 
           !Competition == "Diamond League", Discipline == "400m", Gender == gender) %>% 
    select(Year, Competition, Discipline, Gender, Class, Person.Team, Person, Rank, Age, Nationality, Result)
  
  # best result of each season from swiss athletes
  swissathletes <- athletics_selection %>% 
    filter(Nationality == "SUI") %>%
    group_by(Year) %>% 
    slice_min(Result) %>%
    ungroup() %>%  
    mutate(reference_type = "Swiss_Season_Best") %>% 
    mutate(Rank = as.character(Rank))
}

# Long Jump------------------------
getplotlongjump <- function(gender){
  # data cleaning and selection 
  athletics_selection <- athletics_master %>% 
    mutate(Rank = as.numeric(Rank), Result = as.numeric(Result)) %>% 
    filter(Year %in% c(2000:2023), !is.na(Rank), !is.na(Result), !Competition == "Diamond League", 
           Discipline == "Long Jump", Gender == gender) %>% 
    select(Year, Competition, Discipline, Gender, Class, Person.Team, Person, Rank, Age, Nationality, Result)
  
  # results of finalists in Wch and OG who came in last
  rank8_wch_og <- athletics_selection %>% 
    # filter results so that outliers are not being taken into consideration
    filter(Rank %in% c(1:8), Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
    group_by(Year) %>% 
    slice_max(Rank) %>% 
    ungroup()%>% 
    mutate(reference_type = "Certificate_WCh_and_OG")
  
  
  # results of finalists in Wch and OG who came in first
  rank1_wch_og <- athletics_selection %>% 
    filter(Rank %in% c(1:8), Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
    group_by(Year) %>% 
    slice_min(Rank) %>% 
    ungroup() %>% 
    group_by(Year, Rank) %>% 
    slice_max(Result) %>% 
    ungroup() %>% 
    mutate(reference_type = "Rank1_WCh_and_OG")
  
  # best result of each season from swiss athletes
  swissathletes <- athletics_selection %>% 
    filter(Nationality == "SUI") %>%
    group_by(Year) %>% 
    slice_max(Result) %>%
    ungroup() %>% 
    mutate(reference_type = "Swiss_Season_Best") 
  
  # combining dataframes to one whole
  referencetimes <- bind_rows(rank8_wch_og, rank1_wch_og, swissathletes) 
  
  # create ggplot
  g_reference <- ggplot(data = referencetimes, mapping = aes(Year, Result))+
    geom_line(mapping = aes(colour = reference_type))+
    theme_minimal()+
    theme(
      legend.position = "none"
    )
  
  ggplotly(g_reference)
}

gettablelongjump <- function(gender){
  # data cleaning and selection 
  athletics_selection <- athletics_master %>% 
    mutate(Rank = as.numeric(Rank), Result = as.numeric(Result)) %>% 
    filter(Year %in% c(2000:2023), !is.na(Rank), !is.na(Result), !Competition == "Diamond League", 
           Discipline == "Long Jump", Gender == gender) %>% 
    select(Year, Competition, Discipline, Gender, Class, Person.Team, Person, Rank, Age, Nationality, Result)
  # best result of each season from swiss athletes
  swissathletes <- athletics_selection %>% 
    filter(Nationality == "SUI") %>%
    group_by(Year) %>% 
    slice_max(Result) %>%
    ungroup() %>% 
    mutate(reference_type = "Swiss_Season_Best") %>% 
    mutate(Rank = as.character(Rank))
}

# overview ------------------
## 100m men---------------------
# data cleaning and selection 
athletics_selection_100m_men <- athletics_master %>% 
  mutate(Rank = as.numeric(Rank), Result = as.numeric(Result)) %>% 
  filter(Year %in% c(2000:2023), !is.na(Rank), !is.na(Result), !Competition == "Diamond League", Discipline == "100m", Gender == "Men") %>% 
  select(Year, Competition, Discipline, Gender, Class, Person.Team, Person, Rank, Age, Nationality, Result)

# results of finalists in Wch and OG who came in last
rank8_wch_og_100m_men <- athletics_selection_100m_men %>% 
  # filter results so that outliers are not being taken into consideration
  filter(Rank %in% c(1:8), Result < 10.5, Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
  group_by(Year) %>% 
  slice_max(Rank) %>% 
  ungroup()%>% 
  mutate(reference_type = "Certificate_WCh_and_OG")

# results of finalists in Wch and OG who came in first
rank1_wch_og_100m_men <- athletics_selection_100m_men %>% 
  filter(Rank %in% c(1:8), Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
  group_by(Year) %>% 
  slice_min(Rank) %>% 
  ungroup() %>% 
  group_by(Year, Rank) %>% 
  slice_min(Result) %>% 
  ungroup() %>% 
  mutate(reference_type = "Rank1_WCh_and_OG")

# best result of each season from swiss athletes
swissathletes_100m_men <- athletics_selection_100m_men %>% 
  filter(Nationality == "SUI", Class == "Seniors") %>%
  group_by(Year) %>% 
  slice_min(Result) %>%
  ungroup() %>% 
  mutate(reference_type = "Swiss_Season_Best") 

# combining dataframes to one whole
referencetimes_100m_men <- bind_rows(rank8_wch_og_100m_men, rank1_wch_og_100m_men, swissathletes_100m_men)

## 200m men------------------------
# data cleaning and selection 
athletics_selection_200m_men <- athletics_master %>% 
  mutate(Rank = as.numeric(Rank), Result = as.numeric(Result)) %>% 
  filter(Year %in% c(2000:2023), !is.na(Rank), !is.na(Result), !Competition == "Diamond League", Discipline == "200m", Gender == "Men") %>% 
  select(Year, Competition, Discipline, Gender, Class, Person.Team, Person, Rank, Age, Nationality, Result)

# results of finalists in Wch and OG who came in last
rank8_wch_og_200m_men <- athletics_selection_200m_men %>% 
  # filter results so that outliers are not being taken into consideration
  filter(Rank %in% c(1:8), Result < 21.00, Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
  group_by(Year) %>% 
  slice_max(Rank) %>% 
  ungroup()%>% 
  mutate(reference_type = "Certificate_WCh_and_OG")

# results of finalists in Wch and OG who came in first
rank1_wch_og_200m_men <- athletics_selection_200m_men %>% 
  filter(Rank %in% c(1:8), Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
  group_by(Year) %>% 
  slice_min(Rank) %>% 
  ungroup() %>% 
  group_by(Year, Rank) %>% 
  slice_min(Result) %>% 
  ungroup() %>% 
  mutate(reference_type = "Rank1_WCh_and_OG")

# best result of each season from swiss athletes
swissathletes_200m_men <- athletics_selection_200m_men %>% 
  filter(Nationality == "SUI", Class == "Seniors") %>%
  group_by(Year) %>% 
  slice_min(Result) %>%
  ungroup() %>% 
  mutate(reference_type = "Swiss_Season_Best")

# combining dataframes to one whole
referencetimes_200m_men <- bind_rows(rank8_wch_og_200m_men, rank1_wch_og_200m_men, swissathletes_200m_men)

## 400m men--------------------------------
# data cleaning and selection 
athletics_selection_400m_men <- athletics_master %>% 
  mutate(Rank = as.numeric(Rank), Result = as.numeric(Result)) %>% 
  filter(Year %in% c(2000:2023), !is.na(Rank), !is.na(Result), !Competition == "Diamond League", Discipline == "400m", Gender == "Men") %>% 
  select(Year, Competition, Discipline, Gender, Class, Person.Team, Person, Rank, Age, Nationality, Result)

# results of finalists in Wch and OG who came in last
rank8_wch_og_400m_men <- athletics_selection_400m_men %>% 
  filter(Rank %in% c(1:8), Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
  group_by(Year) %>% 
  slice_max(Rank) %>% 
  ungroup()%>% 
  mutate(reference_type = "Certificate_WCh_and_OG")

# results of finalists in Wch and OG who came in first
rank1_wch_og_400m_men <- athletics_selection_400m_men %>% 
  filter(Rank %in% c(1:8), Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
  group_by(Year) %>% 
  slice_min(Rank) %>% 
  ungroup() %>% 
  group_by(Year, Rank) %>% 
  slice_min(Result) %>% 
  ungroup() %>% 
  mutate(reference_type = "Rank1_WCh_and_OG")

# best result of each season from swiss athletes
swissathletes_400m_men <- athletics_selection_400m_men %>% 
  filter(Nationality == "SUI") %>%
  group_by(Year) %>% 
  slice_min(Result) %>%
  ungroup() %>% 
  mutate(reference_type = "Swiss_Season_Best")

# combining dataframes to one whole
referencetimes_400m_men <- bind_rows(rank8_wch_og_400m_men, rank1_wch_og_400m_men, swissathletes_400m_men)

## Long Jump Men-------------------------------------
# data cleaning and selection 
athletics_selection_longjump_men <- athletics_master %>% 
  mutate(Rank = as.numeric(Rank), Result = as.numeric(Result)) %>% 
  filter(Year %in% c(2000:2023), !is.na(Rank), !is.na(Result), !Competition %in% c("Diamond League", "World Indoor Tour"), Discipline == "Long Jump", Gender == "Men") %>% 
  select(Year, Competition, Discipline, Gender, Class, Person.Team, Person, Rank, Age, Nationality, Result)

# results of finalists in Wch and OG who came in last
rank8_wch_og_longjump_men <- athletics_selection_longjump_men %>% 
  filter(Rank %in% c(1:8), Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
  group_by(Year) %>% 
  slice_max(Rank) %>% 
  ungroup()%>% 
  mutate(reference_type = "Certificate_WCh_and_OG")

# results of finalists in Wch and OG who came in first
rank1_wch_og_longjump_men <- athletics_selection_longjump_men %>% 
  filter(Rank %in% c(1:8), Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
  group_by(Year) %>% 
  slice_min(Rank) %>% 
  ungroup() %>% 
  group_by(Year, Rank) %>% 
  slice_max(Result) %>% 
  ungroup() %>% 
  mutate(reference_type = "Rank1_WCh_and_OG")

# best result of each season from swiss athletes
swissathletes_longjump_men <- athletics_selection_longjump_men %>% 
  filter(Nationality == "SUI") %>%
  group_by(Year) %>% 
  slice_max(Result) %>%
  ungroup() %>% 
  mutate(reference_type = "Swiss_Season_Best")

# combining dataframes to one whole
reference_longjump_men <- bind_rows(rank8_wch_og_longjump_men, rank1_wch_og_longjump_men, swissathletes_longjump_men)

## 100m women--------------------------------
# data cleaning and selection 
athletics_selection_100m_women <- athletics_master %>% 
  mutate(Rank = as.numeric(Rank), Result = as.numeric(Result)) %>% 
  filter(Year %in% c(2000:2023), !is.na(Rank), !is.na(Result), !Competition == "Diamond League", Discipline == "100m", Gender == "Women") %>% 
  select(Year, Competition, Discipline, Gender, Class, Person.Team, Person, Rank, Age, Nationality, Result)

# results of finalists in Wch and OG who came in last
rank8_wch_og_100m_women <- athletics_selection_100m_women %>% 
  # filter results so that outliers are not being taken into consideration
  filter(Rank %in% c(1:8), Result < 11.5, Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
  group_by(Year) %>% 
  slice_max(Rank) %>% 
  ungroup()%>% 
  mutate(reference_type = "Certificate_WCh_and_OG")

# results of finalists in Wch and OG who came in first
rank1_wch_og_100m_women <- athletics_selection_100m_women %>% 
  filter(Rank %in% c(1:8), Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
  group_by(Year) %>% 
  slice_min(Rank) %>% 
  ungroup() %>% 
  group_by(Year, Rank) %>% 
  slice_min(Result) %>% 
  ungroup() %>% 
  mutate(reference_type = "Rank1_WCh_and_OG")

# best result of each season from swiss athletes
swissathletes_100m_women <- athletics_selection_100m_women %>% 
  filter(Nationality == "SUI") %>%
  group_by(Year) %>% 
  slice_min(Result) %>%
  ungroup() %>% 
  mutate(reference_type = "Swiss_Season_Best") 

# combining dataframes to one whole
referencetimes_100m_women <- bind_rows(rank8_wch_og_100m_women, rank1_wch_og_100m_women, swissathletes_100m_women) 

##200m women-------------------------------
# data cleaning and selection 
athletics_selection_200m_women <- athletics_master %>% 
  mutate(Rank = as.numeric(Rank), Result = as.numeric(Result)) %>% 
  filter(Year %in% c(2000:2023), !is.na(Rank), !is.na(Result), !Competition == "Diamond League", Discipline == "200m", Gender == "Women") %>% 
  select(Year, Competition, Discipline, Gender, Class, Person.Team, Person, Rank, Age, Nationality, Result)

# results of finalists in Wch and OG who came in last
rank8_wch_og_200m_women <- athletics_selection_200m_women %>% 
  # filter results so that outliers are not being taken into consideration
  filter(Rank %in% c(1:8), Result < 23.5, Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
  group_by(Year) %>% 
  slice_max(Rank) %>% 
  ungroup()%>% 
  mutate(reference_type = "Certificate_WCh_and_OG")

# results of finalists in Wch and OG who came in first
rank1_wch_og_200m_women <- athletics_selection_200m_women %>% 
  filter(Rank %in% c(1:8), Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
  group_by(Year) %>% 
  slice_min(Rank) %>% 
  ungroup() %>% 
  group_by(Year, Rank) %>% 
  slice_min(Result) %>% 
  ungroup() %>% 
  mutate(reference_type = "Rank1_WCh_and_OG")

# best result of each season from swiss athletes
swissathletes_200m_women <- athletics_selection_200m_women %>% 
  filter(Nationality == "SUI", Class == "Seniors") %>%
  group_by(Year) %>% 
  slice_min(Result) %>%
  ungroup() %>% 
  mutate(reference_type = "Swiss_Season_Best")

# combining dataframes to one whole
referencetimes_200m_women <- bind_rows(rank8_wch_og_200m_women, rank1_wch_og_200m_women, swissathletes_200m_women)

## 400m women-----------------------
# data cleaning and selection 
athletics_selection_400m_women <- athletics_master %>% 
  mutate(Rank = as.numeric(Rank), Result = as.numeric(Result)) %>% 
  filter(Year %in% c(2000:2023), !is.na(Rank), !is.na(Result), !Competition == "Diamond League", Discipline == "400m", Gender == "Women") %>% 
  select(Year, Competition, Discipline, Gender, Class, Person.Team, Person, Rank, Age, Nationality, Result)

# results of finalists in Wch and OG who came in last
rank8_wch_og_400m_women <- athletics_selection_400m_women %>% 
  filter(Rank %in% c(1:8), Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
  group_by(Year) %>% 
  slice_max(Rank) %>% 
  ungroup()%>% 
  mutate(reference_type = "Certificate_WCh_and_OG")

# results of finalists in Wch and OG who came in first
rank1_wch_og_400m_women <- athletics_selection_400m_women %>% 
  filter(Rank %in% c(1:8), Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
  group_by(Year) %>% 
  slice_min(Rank) %>% 
  ungroup() %>% 
  group_by(Year, Rank) %>% 
  slice_min(Result) %>% 
  ungroup() %>% 
  mutate(reference_type = "Rank1_WCh_and_OG")

# best result of each season from swiss athletes
swissathletes_400m_women <- athletics_selection_400m_women %>% 
  filter(Nationality == "SUI") %>%
  group_by(Year) %>% 
  slice_min(Result) %>%
  ungroup() %>% 
  mutate(reference_type = "Swiss_Season_Best")

# combining dataframes to one whole
referencetimes_400m_women <- bind_rows(rank8_wch_og_400m_women, rank1_wch_og_400m_women, swissathletes_400m_women)

## Long Jump Women-----------------------------
# data cleaning and selection 
athletics_selection_longjump_women <- athletics_master %>% 
  mutate(Rank = as.numeric(Rank), Result = as.numeric(Result)) %>% 
  filter(Year %in% c(2000:2023), !is.na(Rank), !is.na(Result), !Competition %in% c("Diamond League", "World Indoor Tour"), Discipline == "Long Jump", Gender == "Women") %>% 
  select(Year, Competition, Discipline, Gender, Class, Person.Team, Person, Rank, Age, Nationality, Result)

# results of finalists in Wch and OG who came in last
rank8_wch_og_longjump_women <- athletics_selection_longjump_women %>% 
  filter(Rank %in% c(1:8), Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
  group_by(Year) %>% 
  slice_max(Rank) %>% 
  ungroup()%>% 
  mutate(reference_type = "Certificate_WCh_and_OG")

# results of finalists in Wch and OG who came in first
rank1_wch_og_longjump_women <- athletics_selection_longjump_women %>% 
  filter(Rank %in% c(1:8), Class == "Seniors", Competition %in% c("World Championships", "Olympic Games")) %>%
  group_by(Year) %>% 
  slice_min(Rank) %>% 
  ungroup() %>% 
  group_by(Year, Rank) %>% 
  slice_max(Result) %>% 
  ungroup() %>% 
  mutate(reference_type = "Rank1_WCh_and_OG")

# best result of each season from swiss athletes
swissathletes_longjump_women <- athletics_selection_longjump_women %>% 
  filter(Nationality == "SUI") %>%
  group_by(Year) %>% 
  slice_max(Result) %>%
  ungroup() %>% 
  mutate(reference_type = "Swiss_Season_Best")

# combining dataframes to one whole
reference_longjump_women <- bind_rows(rank8_wch_og_longjump_women, rank1_wch_og_longjump_women, swissathletes_longjump_women)

## radarplot men----------------------
# World Records
seasonsworldbest_100m_men <- athletics_selection_100m_men %>% 
  filter(Class == "Seniors") %>%
  slice_min(Result, with_ties = F) %>% 
  ungroup() %>% 
  mutate(reference_type = "Seasons_World_Best")

seasonsworldbest_200m_men <- athletics_selection_200m_men %>% 
  filter(Class == "Seniors") %>%
  slice_min(Result, with_ties = F) %>%
  mutate(reference_type = "Seasons_World_Best")

seasonsworldbest_400m_men <- athletics_selection_400m_men %>% 
  filter(Class == "Seniors") %>%
  slice_min(Result, with_ties = F) %>% 
  mutate(reference_type = "Seasons_World_Best")

seasonsworldbest_longjump_men <- athletics_selection_longjump_men %>% 
  filter(Class == "Seniors") %>%
  slice_max(Result, with_ties = F) %>% 
  mutate(reference_type = "Seasons_World_Best")

# Mittelwert über die letzten 6 Jahre bilden (2016 - 2022)
mean_referencetimes_100m_men <- referencetimes_100m_men %>% 
  filter(Year %in% 2016:2022) %>% 
  group_by(reference_type) %>% 
  summarize(mean = mean(Result)) %>% 
  ungroup() %>%
  # Weltrekord hinzufügen 
  bind_rows(seasonsworldbest_100m_men %>% summarize(mean = min(Result), reference_type = "Seasons_World_Best")) %>% 
  mutate(discipline = "100m_men") %>% 
  pivot_wider(names_from = reference_type, values_from = mean) %>% 
  # Vergleichen mit Weltrekord (normalisieren)  
  mutate(Certificate_WCh_and_OG = (11.19-Certificate_WCh_and_OG)/(11.19-Seasons_World_Best),
         Rank1_WCh_and_OG = (11.19-Rank1_WCh_and_OG)/(11.19-Seasons_World_Best),
         Swiss_Season_Best = (11.19-Swiss_Season_Best)/(11.19-Seasons_World_Best),
         Seasons_World_Best = 1) %>% 
  rename(
    "Certificate WCh and OG" = Certificate_WCh_and_OG,
    "Gold Medal WCh and OG" = Rank1_WCh_and_OG,
    "Swiss Season Best" = Swiss_Season_Best,
    "World Record" = Seasons_World_Best
  )

# Mittelwert über die letzten 6 Jahre bilden 
mean_referencetimes_200m_men <- referencetimes_200m_men %>% 
  filter(Year %in% 2016:2022) %>% 
  group_by(reference_type) %>% 
  summarize(mean = mean(Result)) %>% 
  ungroup() %>% 
  # Weltrekord hinzufügen 
  bind_rows(seasonsworldbest_200m_men %>% summarize(mean = min(Result), reference_type = "Seasons_World_Best")) %>%
  mutate(discipline = "200m_men") %>% 
  pivot_wider(names_from = reference_type, values_from = mean) %>% 
  # Vergleichen mit Weltrekord (normalisieren)  
  mutate(Certificate_WCh_and_OG = (22.97-Certificate_WCh_and_OG)/(22.97-Seasons_World_Best),
         Rank1_WCh_and_OG = (22.97-Rank1_WCh_and_OG)/(22.97-Seasons_World_Best),
         Swiss_Season_Best = (22.97-Swiss_Season_Best)/(22.97-Seasons_World_Best),
         Seasons_World_Best = 1) %>% 
  rename(
    "Certificate WCh and OG" = Certificate_WCh_and_OG,
    "Gold Medal WCh and OG" = Rank1_WCh_and_OG,
    "Swiss Season Best" = Swiss_Season_Best,
    "World Record" = Seasons_World_Best
  )

# Mittelwert über die letzten 6 Jahre bilden 
mean_referencetimes_400m_men <- referencetimes_400m_men %>% 
  filter(Year %in% 2016:2022) %>% 
  group_by(reference_type) %>% 
  summarize(mean = mean(Result)) %>% 
  ungroup() %>% 
  # Weltrekord hinzufügen 
  bind_rows(seasonsworldbest_400m_men %>% summarize(mean = min(Result), reference_type = "Seasons_World_Best")) %>%
  mutate(discipline = "400m_men") %>% 
  pivot_wider(names_from = reference_type, values_from = mean) %>% 
  # Vergleichen mit Weltrekord (normalisieren)  
  mutate(Certificate_WCh_and_OG = (50.97-Certificate_WCh_and_OG)/(50.97-Seasons_World_Best),
         Rank1_WCh_and_OG = (50.97-Rank1_WCh_and_OG)/(50.97-Seasons_World_Best),
         Swiss_Season_Best = (50.97-Swiss_Season_Best)/(50.97-Seasons_World_Best),
         Seasons_World_Best = 1) %>% 
  rename(
    "Certificate WCh and OG" = Certificate_WCh_and_OG,
    "Gold Medal WCh and OG" = Rank1_WCh_and_OG,
    "Swiss Season Best" = Swiss_Season_Best,
    "World Record" = Seasons_World_Best
  )

# Mittelwert über die letzten 6 Jahre bilden 
mean_reference_longjump_men <- reference_longjump_men %>% 
  filter(Year %in% 2016:2022) %>% 
  group_by(reference_type) %>% 
  summarize(mean = mean(Result)) %>% 
  ungroup() %>% 
  # Weltrekord hinzufügen  
  bind_rows(seasonsworldbest_longjump_men %>% summarize(mean = max(Result), reference_type = "Seasons_World_Best")) %>%
  mutate(discipline = "longjump_men") %>% 
  pivot_wider(names_from = reference_type, values_from = mean) %>% 
  # Vergleichen mit Weltrekord (normalisieren)  
  mutate(Certificate_WCh_and_OG = (6.80-Certificate_WCh_and_OG)/(6.80-Seasons_World_Best),
         Rank1_WCh_and_OG = (6.80-Rank1_WCh_and_OG)/(6.80-Seasons_World_Best),
         Swiss_Season_Best = (6.80-Swiss_Season_Best)/(6.80-Seasons_World_Best),
         Seasons_World_Best = 1) %>% 
  rename(
    "Certificate WCh and OG" = Certificate_WCh_and_OG,
    "Gold Medal WCh and OG" = Rank1_WCh_and_OG,
    "Swiss Season Best" = Swiss_Season_Best,
    "World Record" = Seasons_World_Best
  )

# max und min Werte für normalisierte Daten definieren (Voraussetzung für radar plot)
max_min_men <- data.frame(reference_type = c("min", "max"),'100m_men' = c(0, 1), '200m_men' = c(0, 1), '400m_men' = c(0, 1), 'longjump_men' = c(0, 1)) %>% 
  rename("100m_men" = X100m_men,
         "200m_men" = X200m_men,
         "400m_men" = X400m_men,
  )

# Daten der verschiedenen Disziplinen zusammenführen
mean_reference_men <- bind_rows(mean_referencetimes_100m_men, mean_referencetimes_200m_men, mean_referencetimes_400m_men, mean_reference_longjump_men) %>% 
  pivot_longer(cols = c(2:5), names_to = "reference_type", values_to = "results") %>%
  filter(!results == 1) %>% 
  pivot_wider(names_from = discipline, values_from = results) %>% 
  bind_rows(max_min_men) %>% 
  mutate(reference_type = factor(reference_type, levels = c("max", "min", "Gold Medal WCh and OG", "Certificate WCh and OG", "Swiss Season Best"))) %>% 
  arrange(reference_type)

## radarplot women----------------------
# World Records
seasonsworldbest_100m_women <- athletics_selection_100m_women %>% 
  filter(Class == "Seniors") %>%
  slice_min(Result, with_ties = F) %>% 
  ungroup() %>% 
  mutate(reference_type = "Seasons_World_Best")

seasonsworldbest_200m_women <- athletics_selection_200m_women %>% 
  filter(Class == "Seniors") %>%
  slice_min(Result, with_ties = F) %>%
  mutate(reference_type = "Seasons_World_Best")

seasonsworldbest_400m_women <- athletics_selection_400m_women %>% 
  filter(Class == "Seniors") %>%
  slice_min(Result, with_ties = F) %>% 
  mutate(reference_type = "Seasons_World_Best")

seasonsworldbest_longjump_women <- athletics_selection_longjump_women %>% 
  filter(Class == "Seniors") %>%
  slice_max(Result, with_ties = F) %>% 
  mutate(reference_type = "Seasons_World_Best")

# Mittelwert über die letzten 6 Jahre bilden (2016 - 2022)
mean_referencetimes_100m_women <- referencetimes_100m_women %>% 
  filter(Year %in% 2016:2022) %>% 
  group_by(reference_type) %>% 
  summarize(mean = mean(Result)) %>% 
  ungroup() %>%
  # Weltrekord hinzufügen 
  bind_rows(seasonsworldbest_100m_women %>% summarize(mean = min(Result), reference_type = "Seasons_World_Best")) %>% 
  mutate(discipline = "100m_women") %>% 
  pivot_wider(names_from = reference_type, values_from = mean) %>% 
  # Vergleichen mit Weltrekord (normalisieren)  
  mutate(Certificate_WCh_and_OG = (12.43-Certificate_WCh_and_OG)/(12.43-Seasons_World_Best),
         Rank1_WCh_and_OG = (12.43-Rank1_WCh_and_OG)/ (12.43-Seasons_World_Best),
         Swiss_Season_Best = (12.43-Swiss_Season_Best)/(12.42-Seasons_World_Best),
         Seasons_World_Best = 1) %>% 
  rename(
    "Certificate WCh and OG" = Certificate_WCh_and_OG,
    "Gold Medal WCh and OG" = Rank1_WCh_and_OG,
    "Swiss Season Best" = Swiss_Season_Best,
    "World Record" = Seasons_World_Best
  )

# Mittelwert über die letzten 6 Jahre bilden 
mean_referencetimes_200m_women <- referencetimes_200m_women %>% 
  filter(Year %in% 2016:2022) %>% 
  group_by(reference_type) %>% 
  summarize(mean = mean(Result)) %>% 
  ungroup() %>% 
  # Weltrekord hinzufügen 
  bind_rows(seasonsworldbest_200m_women %>% summarize(mean = min(Result), reference_type = "Seasons_World_Best")) %>%
  mutate(discipline = "200m_women") %>% 
  pivot_wider(names_from = reference_type, values_from = mean) %>% 
  # Vergleichen mit Weltrekord (normalisieren)  
  mutate(Certificate_WCh_and_OG = (25.74-Certificate_WCh_and_OG)/(25.74-Seasons_World_Best),
         Rank1_WCh_and_OG = (25.74-Rank1_WCh_and_OG)/ (25.74-Seasons_World_Best),
         Swiss_Season_Best = (25.74-Swiss_Season_Best)/(25.74-Seasons_World_Best),
         Seasons_World_Best = 1) %>% 
  rename(
    "Certificate WCh and OG" = Certificate_WCh_and_OG,
    "Gold Medal WCh and OG" = Rank1_WCh_and_OG,
    "Swiss Season Best" = Swiss_Season_Best,
    "World Record" = Seasons_World_Best
  )

# Mittelwert über die letzten 6 Jahre bilden 
mean_referencetimes_400m_women <- referencetimes_400m_women %>% 
  filter(Year %in% 2016:2022) %>% 
  group_by(reference_type) %>% 
  summarize(mean = mean(Result)) %>% 
  ungroup() %>% 
  # Weltrekord hinzufügen 
  bind_rows(seasonsworldbest_400m_women %>% summarize(mean = min(Result), reference_type = "Seasons_World_Best")) %>%
  mutate(discipline = "400m_women") %>% 
  pivot_wider(names_from = reference_type, values_from = mean) %>% 
  # Vergleichen mit Weltrekord (normalisieren)  
  mutate(Certificate_WCh_and_OG = (58.57-Certificate_WCh_and_OG)/(58.57-Seasons_World_Best),
         Rank1_WCh_and_OG = (58.57-Rank1_WCh_and_OG)/ (58.57-Seasons_World_Best),
         Swiss_Season_Best = (58.57-Swiss_Season_Best)/(58.57-Seasons_World_Best),
         Seasons_World_Best = 1) %>% 
  rename(
    "Certificate WCh and OG" = Certificate_WCh_and_OG,
    "Gold Medal WCh and OG" = Rank1_WCh_and_OG,
    "Swiss Season Best" = Swiss_Season_Best,
    "World Record" = Seasons_World_Best
  )

# Mittelwert über die letzten 6 Jahre bilden 
mean_reference_longjump_women <- reference_longjump_women %>% 
  filter(Year %in% 2016:2022) %>% 
  group_by(reference_type) %>% 
  summarize(mean = mean(Result)) %>% 
  ungroup() %>% 
  # Weltrekord hinzufügen  
  bind_rows(seasonsworldbest_longjump_women %>% summarize(mean = max(Result), reference_type = "Seasons_World_Best")) %>%
  mutate(discipline = "longjump_women") %>% 
  pivot_wider(names_from = reference_type, values_from = mean) %>% 
  # Vergleichen mit Weltrekord (normalisieren)  
  mutate(Certificate_WCh_and_OG = (5.72-Certificate_WCh_and_OG)/ (5.72-Seasons_World_Best),
         Rank1_WCh_and_OG = (5.72-Rank1_WCh_and_OG)/(5.72-Seasons_World_Best),
         Swiss_Season_Best = (5.72-Swiss_Season_Best)/ (5.72-Seasons_World_Best),
         Seasons_World_Best = 1) %>% 
  rename(
    "Certificate WCh and OG" = Certificate_WCh_and_OG,
    "Gold Medal WCh and OG" = Rank1_WCh_and_OG,
    "Swiss Season Best" = Swiss_Season_Best,
    "World Record" = Seasons_World_Best
  )

# max und min Werte für normalisierte Daten definieren (Voraussetzung für radar plot)
max_min_women <- data.frame(reference_type = c("min", "max"),'100m_women' = c(0, 1), '200m_women' = c(0, 1), '400m_women' = c(0, 1), 'longjump_women' = c(0, 1)) %>% 
  rename("100m_women" = X100m_women,
         "200m_women" = X200m_women,
         "400m_women" = X400m_women,
  )

# Daten der verschiedenen Disziplinen zusammenführen
mean_reference_women <- bind_rows(mean_referencetimes_100m_women, mean_referencetimes_200m_women, mean_referencetimes_400m_women, mean_reference_longjump_women) %>% 
  pivot_longer(cols = c(2:5), names_to = "reference_type", values_to = "results") %>%
  filter(!results == 1) %>% 
  pivot_wider(names_from = discipline, values_from = results) %>% 
  bind_rows(max_min_women) %>% 
  mutate(reference_type = factor(reference_type, levels = c("max", "min", "Gold Medal WCh and OG", "Certificate WCh and OG", "Swiss Season Best"))) %>% 
  arrange(reference_type)


