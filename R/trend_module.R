
tab_trend_mod <- function(id, label = "trend"){
  
  ns <- NS(id)
  
  tabItem(tabName = "trend",
          fluidRow(width = 12,
                   box(title = "Please select an indicator to display",
                       width = 4, align = "center", offset = 0,
                       htmlOutput(ns("intro_text")),
                       radioButtons(ns("type_selector"), "Indicator type:",
                                    c("overall", "domain", "subdomain"), selected = "overall",
                                    inline = FALSE),
                       uiOutput(ns("domain_selector")),
                       uiOutput(ns("ind_selector")),
                       actionButton(ns("button"), "Go"),
                       htmlOutput(ns("trend_text"))),
                   box(title = "Data Visualisation",
                       width = 8, align = "center", offset = 0,
                       radioButtons(ns("geography_selector"), "Geography:",
                                    c("County and wider area", "Districts"), 
                                    selected = "County and wider area",
                                    inline = TRUE),
                       plotlyOutput(ns("graph")))
          ))
}

# https://stackoverflow.com/questions/50726365/how-to-plot-selected-input-from-selectinput-function-in-shiny
#create go button as trigger for selecting values for graph

tab_trend_server <- function(id){
  
  moduleServer(
    
    id,
    
    function(input, output, session) {
      
      ns <- NS(id)
      
      output$intro_text<-renderUI({
        
        p(paste0("The tab is an interactive plotting tool that allows you to visualise your selected index by various geographies over time. Please select an indicator type below and the 'Go' button to start plotting.")
          , style = "font-family:Bahnschrift,garamond,serif;font-size:18px;font-weight:lighter;")
        
      })
      
      observe({
        relayout <- event_data("plotly_relayout")
        hidden_labels <- relayout$hiddenlabels
        print(hidden_labels)
      })
      
      output$trend_text<-renderUI({
        #https://stackoverflow.com/questions/52335837/event-when-clicking-a-name-in-the-legend-of-a-plotlys-graph-in-r-shiny/54505531#54505531
        #https://stackoverflow.com/questions/47405422/r-shiny-and-plotly-getting-legend-click-events
        
        trends <- sig_diff_df() %>% 
          filter(ind == values$selected,
                 AreaName == "Hertfordshire")
          
        p(paste0("There is a ", trends$sig_diff," ", trends$direction, " in Hertfordshire over time.")
          , style = "font-family:Bahnschrift,garamond,serif;font-size:18px;font-weight:lighter;")
        
      })
      
      output$domain_selector <- renderUI({
        
        if(input$type_selector %in% c("domain", "subdomain")){
          
          selectInput(ns("domain_selector"), "Select domain:",
                      c("Healthy People Domain", "Healthy Lives Domain", "Healthy Places Domain"),  selected = "Healthy People Domain")
          
        }
        
      })
      
      output$ind_selector <- renderUI({
        
        
        if(input$type_selector == "subdomain"){
          
          if(input$domain_selector == "Healthy People Domain"){
            
            list <- read.csv("data/domain_lookup.csv") %>%
              filter(domain == "Healthy People Domain") %>%
              distinct(subdomain) %>%
              pull(subdomain)
            
          }else if(input$domain_selector == "Healthy Lives Domain"){
            
            list <- read.csv("data/domain_lookup.csv") %>%
              filter(domain == "Healthy Lives Domain") %>%
              distinct(subdomain) %>%
              pull(subdomain)
            
          }else if(input$domain_selector == "Healthy Places Domain"){
            
            list <- read.csv("data/domain_lookup.csv") %>%
              filter(domain == "Healthy Places Domain") %>%
              distinct(subdomain) %>%
              pull(subdomain)
            
          }
          
          selectInput(ns("ind_selector"), "Select subdomain", choices = list, selected = list[1])
          
        }
      })
      
      values <- reactiveValues(indi_type = "overall", domain = NULL, subdomain = NULL, selected = "Health Index")
      
      observeEvent(input$button, {
        
        values$indi_type <- input$type_selector
        values$domain <- input$domain_selector
        values$subdomain <- input$ind_selector
        
        selected_list <- c(values$indi_type, values$domain, values$subdomain)
        
        if(selected_list[1] == "overall"){
          
          values$selected <- "Health Index"
          
        } else if (selected_list[1] == "domain"){
          
          values$selected <- input$domain_selector
          
        } else if (selected_list[1] == "subdomain"){
          
          values$selected <- input$ind_selector
        }
        
      })
      
      myPlotlyProxy <- plotlyProxy("graph")
      
      legendClickEvents <- reactive({
        event_data(source = "A", "plotly_legendclick")
      })
      
      output$clickedLegendItem <- renderPrint({
        clickedItem <- legendClickEvents()$name
        if (is.null(clickedItem)){"Clicked item appears here"} else {clickedItem}
      })
      
      output$graph <- renderPlotly({
        
        
        if(input$geography_selector == "County and wider area"){
          
          data <- get_indicators_data("upper")
          
          data %>%
            filter(ind == values$selected) %>%
            plot_ly(source = "A", x = ~year, y = ~value, type = "scatter", mode = "", color = ~AreaName) %>% 
            layout(title = paste0("Score for ", tolower(as.character(values$selected))," over time"), 
                   xaxis = list(title = 'Year'), 
                   yaxis = list(title = 'Score', range = c(50, 145))) %>% 
            event_register('plotly_legendclick') 
          
        }else if(input$geography_selector == "Districts"){
          
          data <- get_indicators_data("lower") %>% 
            filter(ind == values$selected)
          
          plot_ly(source = "A", data = data %>% filter(AreaName %in% c("Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire",
                                                         "Stevenage", "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers")),
                  x = ~year, y = ~value, type = "scatter", mode = "", color = ~AreaName) %>% 
            add_trace(data = data %>% filter(AreaName %in% c("Uttlesford", "Epping Forest", "Harlow")),
                      x = ~year, y = ~value, type = "scatter", mode = "", color = ~AreaName, visible = "legendonly") %>%
            layout(title = paste0("Score for ", tolower(as.character(values$selected))," over time"), 
                   xaxis = list(title = 'Year'), 
                   yaxis = list(title = 'Score', range = c(50, 145))) %>% 
            event_register('plotly_legendclick') 
          
         }
      })
      
    })}
        
    
    