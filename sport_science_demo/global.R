library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(reactable)
library(plotly)
library(htmltools)
library(ggthemes)

load(file = "./data/acwr_wide.rda")
load(file = "./data/acwr_long.rda")
load(file = "./data/combined_data.rda")
load(file = "./data/sleep_data.rda")
load(file = "./data/wellness_data.rda")

mytheme <-  create_theme(
  adminlte_color(
    light_blue = "grey"
  )
)