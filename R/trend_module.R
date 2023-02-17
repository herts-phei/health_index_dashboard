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
                       actionButton(ns("button"), "Go")),
                   box(title = "Data Visualisation",
                       width = 8, align = "center", offset = 0,
                       htmlOutput(ns("graph_intro_text")),
                       radioButtons(ns("geography_selector"), "Geography:",
                                    c("County and wider area", "Districts"), 
                                    selected = "County and wider area",
                                    inline = TRUE),
                       plotlyOutput(ns("graph")),
                       htmlOutput(ns("trend_text")))
          ))
}

tab_trend_server <- function(id, upper_data, lower_data){
  
  moduleServer(
    
    id,
    
    function(input, output, session) {
      
      ns <- NS(id)
      
      output$intro_text<-renderUI({
        
        p(paste0("The tab is an interactive plotting tool that allows you to visualise your selected index by various geographies over time. Please select an indicator type below and the 'Go' button to start plotting.")
          , style = "font-family:Bahnschrift,garamond,serif;font-size:18px;font-weight:lighter;")
        
      })
      
      
      output$graph_intro_text<-renderUI({
        
        p(paste0("Please select an area in the legend on the right to change area for the summary below.")
          , style = "font-family:Bahnschrift,garamond,serif;font-size:18px;font-weight:lighter;")
        
      })
      
      observe({
        
        relayout <- event_data("plotly_relayout")
        hidden_labels <- relayout$hiddenlabels

      })
      
      output$trend_text<-renderUI({
      
        clickedItem <- "Hertfordshire"
        
        if(!is.null(legendClickEvents()$name)){
          
          clickedItem <- legendClickEvents()$name
          
          }

        sv <- summary_vals(indicator = values$selected,
                           area = clickedItem)
        
            text <- paste0("In ", sv$current_year, ", ", clickedItem, "’s score in ", values$selected, " was ", sv$current_year_val, ", this is a ", sv$select_area_last_year_diff_sig, " ", sv$select_area_last_year_change, " from ", sv$last_year, ". ")
      
            if(input$geography_selector == "Districts"){
            
            text <- paste0("For ", values$selected, ", the district that performed the best in the latest year was ", sv$best_ltla_name, "with a score of ", sv$best_ltla_value, ". The district with the lowest score in this domain was ", sv$worst_ltla_name, " with a score of ", sv$worst_ltla_value, ".
               Since last year, the district that showed the biggest improvement in performance for ", values$selected, " was ", sv$most_improved_name, ", which a ", sv$most_improved_sig_change, " increase can be observed. Oppositely, the district that had the largest decrease in score was ", sv$most_worsened_name, " which the decrease was ", sv$most_worsened_sig_change, ".")
            
          }
          
          p(text, style = "font-family:Bahnschrift,garamond,serif;font-size:18px;font-weight:lighter;")
          
      })
      
      output$domain_selector <- renderUI({
        
        if(input$type_selector %in% c("domain", "subdomain")){
          
          selectInput(ns("domain_selector"), "Select domain:",
                      c("Healthy People Domain", "Healthy Lives Domain", "Healthy Places Domain"),  selected = "Healthy People Domain")
          
        }
        
      })
      
      output$ind_selector <- renderUI({
        
        if(input$type_selector == "subdomain"){
          
          list <- read.csv("data/domain_lookup.csv") %>%
            filter(domain == "Healthy People Domain") %>%
            distinct(subdomain) %>%
            pull(subdomain)
          
            # list <-  c("Difficulties in daily life", "Disability", "Frailty", "Mental health", 
            #            "Children's social, emotional and mental health", "Mental health conditions",
            #            "Self-harm", "Suicides", "Mortality", "Avoidable mortality", "Infant mortality",
            #            "Life expectancy", "Mortality from all causes", "Personal well-being",
            #            "Activities in life are worthwhile", "Feelings of anxiety", "Happiness",
            #            "Life satisfaction", "Physical health conditions", "Dementia", "Diabetes", 
            #            "Kidney and liver disease", "Musculoskeletal conditions", "Respiratory conditions")
            
          if(input$domain_selector == "Healthy Lives Domain"){
            
            list <- read.csv("data/domain_lookup.csv") %>%
              filter(domain == "Healthy Lives Domain") %>%
              distinct(subdomain) %>%
              pull(subdomain)
            
            # list <- c("Behavioural risk factors", "Alcohol misuse", "Drug misuse", "Healthy eating", 
            #           "Physical activity", "Sedentary behaviour", "Sexually transmitted infections",
            #           "Smoking", "Children and young people", "Early years development", "Pupil absences",
            #           "Pupil attainment", "Teenage pregnancy", "Young people in education, employment and apprenticeships",
            #           "Physiological risk factors", "High blood pressure", "Low birth weight", "Overweight and obesity in adults",
            #           "Overweight and obesity in children", "Protective measures", "Cancer screening attendance", "Child vaccination coverage")
            
          }else if(input$domain_selector == "Healthy Places Domain"){
            
            list <- read.csv("data/domain_lookup.csv") %>%
              filter(domain == "Healthy Places Domain") %>%
              distinct(subdomain) %>%
              pull(subdomain)
            
            # list <- c("Access to green space", "Private outdoor space", "Access to services", "Distance to GP services",
            #          "Distance to pharmacies", "Distance to sports or leisure facilities", "Internet access", 
            #          "Patients offered acceptable GP practice appointments", "Crime", "Low-level crime",
            #          "Personal crime", "Economic and working conditions", "Child poverty", "Job-related training",
            #          "Unemployment", "Workplace safety", "Living conditions", "Air pollution", 
            #          "Household overcrowding", "Noise complaints", "Road safety", "Rough sleeping")
          }
          
          selectInput(ns("ind_selector"), "Select subdomain", choices = list, selected = list[1])
          
        }
      })
      
      values <- reactiveValues(indi_type = "overall", domain = "Healthy People Domain", subdomain = "Difficulties in daily life", selected = "Health Index")
      
      observeEvent(input$button, {
        
        values$indi_type <- input$type_selector
        values$domain <- input$domain_selector
        values$subdomain <- input$ind_selector
        
        selected_list <- c(values$indi_type, values$domain, values$subdomain)
        
          values$selected <- "Health Index"
          
        if(selected_list[1] == "domain"){
          
          values$selected <- input$domain_selector
          
        } else if (selected_list[1] == "subdomain"){
          
          values$selected <- input$ind_selector
        }
        
      })
      
         output$graph <- renderPlotly({
        
          graph <- upper_data() %>%
            filter(ind == values$selected) %>%
            plot_ly(source = "A", x = ~year, y = ~value, type = "scatter", mode = "", color = ~AreaName, colours = "Set2") %>% 
            layout(title = paste0("Score for ", tolower(as.character(values$selected))," over time"), 
                   xaxis = list(title = 'Year'), 
                   yaxis = list(title = 'Score', range = c(50, 145))) %>% 
            event_register('plotly_legendclick')
          
          if(input$geography_selector == "Districts"){
          
          data <- lower_data() %>% 
            filter(ind == values$selected)
          
          graph <- plot_ly(source = "A", data = data %>% filter(AreaName %in% c("Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire",
                                                         "Stevenage", "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers")),
                  x = ~year, y = ~value, type = "scatter", mode = "", color = ~AreaName, colours = "Set2") %>% 
            add_trace(data = data %>% filter(AreaName %in% c("Uttlesford", "Epping Forest", "Harlow")),
                      x = ~year, y = ~value, type = "scatter", mode = "", color = ~AreaName, visible = "legendonly") %>%
            layout(title = paste0("Score for ", tolower(as.character(values$selected))," over time"), 
                   xaxis = list(title = 'Year'), 
                   yaxis = list(title = 'Score', range = c(50, 145))) #%>% 
            # event_register('plotly_legendclick')
          
          }
          
          graph
      })
         
         myPlotlyProxy <- plotlyProxy("graph")
         
         legendClickEvents <- reactive({
           event_data(event =  "plotly_legendclick", source = "A")
         })
         
         
    })}
        
    
    