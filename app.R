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
                   menuItem("Trend", tabName = "trend", icon = icon("line-chart")))#,
    # selectInput("district_selector", "Select district", choices = c("Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire",
    #                                                                 "Stevenage", "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers", "Uttlesford",
    #                                                                 "Epping Forest", "Harlow"), selected = "Broxbourne"),
    # selectInput("comparator_selector", "Select comparator", choices = c("No Comparator", "Hertfordshire", "Essex", "England"), selected = "No Comparator"),
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
  
  # observe({
  # 
  #   if (input$comparator_selector == "No Comparator") {
  #     rv$create_comp_data <- create_comp_data(data = rv$data,
  #                                             area = input$district_selector)
  # 
  #     rv$create_comp_data2 <- data.frame()
  #     rv$mode <- "Gradient"
  # 
  #   } else {
  #     # if "No comparator" is not the selection it will trigger a second dataset used for comparison.
  #     rv$create_comp_data <- create_comp_data(data = rv$data,
  #                                             area = input$district_selector)
  # 
  #     rv$create_comp_data2 <- create_comp_data(data = rv$data,
  #                                              area = input$comparator_selector)
  #     rv$mode <- "Categorical"
  #   }
  # 
  # })

  
  tab_overview_server("overview", 
                      map_data = reactive(rv$data)#, 
                      # comparator = reactive(input$comparator_selector),
                      # mode = reactive(rv$mode)
                      )
  
  tab_indicators_server("indicators"#, 
                        # comp_data = reactive(rv$create_comp_data), 
                        # comp_data2 = reactive(rv$create_comp_data2), 
                        # mode = reactive(rv$mode),
                        # ltla = reactive(input$district_selector),
                        # comparator = reactive(input$comparator_selector)
                        )
  
  tab_trend_server("trend",
                   upper_data = reactive(rv$upper_indicators_data),
                   lower_data = reactive(rv$lower_indicators_data))
}

shinyApp(ui, server)

