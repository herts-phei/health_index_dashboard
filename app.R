# install.packages("sf")
# install.packages("remotes")
# remotes::install_github("deepanshu88/summaryBox")
# library(remotes)
# install_github("r-spatial/sf")

library(shiny)
library(shinydashboard)
library(bs4Dash)
library(reactable)
library(tidyverse)
library(tippy)
library(readxl)
library(reactablefmtr)
library(pins)
library(plotly)
library(summaryBox)
library(leaflet.extras)

ui <- dashboardPage(
  dark = NULL,
  
  #header 
  header = dashboardHeader(
    title = "Health Index 2020 ",
    tags$img(src = 'team_grey_png.png',
             title = "Health Index", height = "30px")
    
  ),
  #sidebar
  sidebar = bs4DashSidebar(
    status = "gray-dark",
    elevation = 3,
    skin = "light",
    width = "220px", 
    minified = F, 
    bs4SidebarMenu(id = "tabs",
                   menuItem("Overview", tabName = "overview", icon = icon("home")),
                   menuItem("Indicators", tabName = "indicator", icon = icon("bars")),
                   menuItem("Trend", tabName = "trend", icon = icon("line-chart")))
  ),
  
  body = dashboardBody(
    
    tabItems(
      tab_overview_mod("overview"),
      tab_indicators_mod("indicators"),
      tab_trend_mod("trend")
      
    )
  )
)

server <- function(input, output) {
  
  rv <- reactiveValues()
  rv$data <- get_data()
  rv$upper_indicators_data <- get_indicators_data("upper")
  rv$lower_indicators_data <- get_indicators_data("lower")
  
  tab_overview_server("overview", 
                      map_data = reactive(rv$data))
  
  tab_indicators_server("indicators")
  
  tab_trend_server("trend",
                   upper_data = reactive(rv$upper_indicators_data),
                   lower_data = reactive(rv$lower_indicators_data))
}

shinyApp(ui, server)

