library(shiny)
library(wikipediatrend)
library(shiny)
library(GuardianR)
library(dplyr)
library(ggvis)
library(rvest) # prob already there
library(DT)
library(stringr)
library(XML)


# show lie if no articles
blankdf <- data.frame(link="No articles on selected day")

