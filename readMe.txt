1. Explanation of the folders. 

folder: sportdataanalytics
content: 
	folder: Data
	folder: Rmarkdown
	folder: Script
	folder: Shiny
	readMe.txt (file that you are reading)
	SportDataAnalyticsReport.pdf (summised pdf, report without graphical analysis)

folder: data
content: 
	athletics.csv 
	
folder: Rmarkdown
content: 
	sport_data_analytics_report.html (complete report, with graphical analysis)
	sport_data_analytics_report.Rmd
	
folder: Script
content: 
	.RData
	.Rhistory
	helper.R (script used in a shiny app)
	sport_analysis.R (script used in the analysis process)
	sport_data_analytics.R (script used in analysis process)

folder: Shiny
content: 
	.RData
	.Rhistory
	app_athletics_trends.R (shiny app that covers the second part of the rmd file)
	development_athletes.R (shiny app that covers the first part of the rmd file)
	sport_data_shiny.R (shiny app that covers the third part of the rmd file)

2. Suggestions. 
	1. You need to fix the path, ours are made in windows. Here is a list of lines of code that must be changed if you want to run the code locally.
		- in Rmarkdown/sport_data_analytics_report.Rmd: 
			* line 12 -> olympics <- read.csv("YOUR PATH/Data/athletics.csv")
			
		- in Script/helper.R:
			* line 11 -> olympics <- read.csv("YOUR PATH/Data/athletics.csv")
			
		- in Script/sport_analysis.R: 
			* line 1 -> olympics <- read.csv("YOUR PATH/Data/athletics.csv")
			
		- in Script/sport_data_analytics.R: 
			* line 1 -> olympics <- read.csv("YOUR PATH/Data/athletics.csv")
			
		- in Shiny/app_athletics_trends.R:
			* line 12 -> source("YOUR PATH/Script/helper.R")
			
		- in Shiny/development_athletes.R: 
			* line 20 -> olympics <- read.csv("YOUR PATH/Data/athletics.csv")
			
		- in Shiny/sport_data_shiny.R:
			* line 6 -> source("YOUR PATH/Script/sport_data_analytics.R")
	
	2. please install the following libraries: 
		- "ggplot2", "plotly", "fmsb", "shiny", "shinydashboard", "dplyr", "tidyverse", "readr", "shinythemes",
			"janitor", "pacman"
	