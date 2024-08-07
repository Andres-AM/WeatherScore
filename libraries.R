
## All packages for Shiny App
library(lubridate)
library(modelr)
library(plotly)
library(DT)
library(shiny)
library(shinydashboard)
library(tidyverse)

## Specific
library(openmeteo)

## Other settings 
theme_set(theme_bw())
options(digits=4,dplyr.summarise.inform=F,"lubridate.week.start" = 1)
rm(list = ls())
cat("\014")
 