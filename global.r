library(shiny)
library(wikipediatrend)
library(shinydashboard)
library(GuardianR)
library(dplyr)
library(ggvis)
library(rvest) # prob already there
library(DT)
library(stringr)
library(XML)
library(markdown)
library(httr)
library(lattice)
library(readr)

library(plotly)
library(crosstalk)
library(googleVis)

# show lie if no articles
blankdf <- data.frame(link = "No articles on selected day")

### import data
repData <- read_csv("repCandidatesJanJun2015.csv")


# import DT data

DT_data <- readRDS("data/DT.rds")