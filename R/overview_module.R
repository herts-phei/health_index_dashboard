# UI ----------------------------------------------------------------------

tab_overview_mod <- function(id, label = "overview"){
  
  ns <- NS(id)
  
  tabItem(tabName = "overview",
          uiOutput(ns("sub_summarybox")),
          fluidRow(
            tags$head(tags$style(HTML(".leaflet-container { background: #ffffff; }"))),
            column(width = 4,
                   align = "left",
                   box(
                     title = "Background",
                     width = 12,
                     align = "left",
                     htmlOutput(ns("intro_text")),
                     htmlOutput(ns("instruction_text"))),
                   box(
                     title = "Summary",
                     width = 12,
                     align = "left",
                     htmlOutput(ns("summary_text")))),
            box(
              title = "Map",
              width = 8,
              align = "right",
              column(offset = 1, width = 11, align = "left", selectInput(ns("comparator_selector"), "Select comparator", choices = c("No Comparator", "Hertfordshire", "Essex", "England"), selected = "No Comparator")),
              leafletOutput(ns("map"), width = "100%", height = 660),
              fluidRow(column(width = 6, align = "left",imageOutput(ns("legend"), height = 85)),
                       column(width = 6, align = "right", actionButton(ns("reset"), "Reset to Hertfordshire")))
              
            )))
  
}

# Server ------------------------------------------------------------------

tab_overview_server <- function(id, map_data){
  
  moduleServer(
    
    id,
    
    function(input, output, session) {
      
      ns <- NS(id)
      
      map_df <- isolate(map_data()$df_hioverall) %>%
        filter(`Area Name` %in% c("Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire",
                                  "Stevenage", "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers", "Uttlesford",
                                  "Epping Forest", "Harlow")) %>% 
        mutate(`Area Name` =  factor(`Area Name`, levels = c("Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire",
                                                             "Stevenage", "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers", "Uttlesford",
                                                             "Epping Forest", "Harlow"))) %>% 
        select(-`Area Code`) %>% 
        arrange(`Area Name`) 
      
      output$intro_text<-renderUI({
        
        p("This application provides local insights to the ONS Health Index dataset in a Hertfordshire context. The Health Index is comprised of 56 indicators, summarised into 14 subdomains, 3 domains and then the overall score for each geographical area. A score of 100 in the Health Index and its components represents health in England in 2015, which serves as a baseline figure for subsequent years. A higher number means better health and a lower number means worse health."
          , style = "font-family:Bahnschrift,garamond,serif;font-size:18px;font-weight:lighter;")
      })
      
      output$instruction_text<-renderUI({
        
        p("On the right is a map of districts in Hertfordshire and West Essex, please select a district to display scores for the overall health index and the three domains."
          , style = "font-family:Bahnschrift,garamond,serif;font-size:18px;font-weight:lighter;")
        
      })
      
      action_value <- reactiveValues(map_shape_click = 0)
      
      observeEvent(input$map_shape_click, {

                action_value$map_shape_click <- domain_scores(input$map_shape_click$id[1])
        
      })
      
      observeEvent(input$reset, {
        
        action_value$map_shape_click <-  list(104.6, 106.8, 107.2, 99.7, "0000000000000000", "Healthy Lives Domain", "smoking, obesity and sedentary behaviours", "Hertfordshire")
        
      })
      
      area_select <- eventReactive(action_value, {
        
        return(action_value)
        
      })
      
      
      trigger <- reactiveValues(reset = 9, map_click = 9)
      
      observeEvent(input$reset, {trigger$reset <- (-1*(trigger$map_click))})
      observeEvent(input$map_click, {trigger$reset <- trigger$map_click})
      
      trigger_status <- eventReactive(trigger, {
        
        return(trigger)
        
      })
      
      output$summary_text<-renderUI({
        
        # trigger_reset <- trigger_status()$reset
        # trigger_map <- trigger_status()$map_click
        # 
        # print(paste0("reset: ", trigger_reset, " | map_click: ", trigger_map))
        
        reset_trigger <- trigger_status()$reset
        
        if(reset_trigger > 0 & !is.null(input$map_shape_click$id[1])){
          
          summary <- area_select()$map_shape_click
          
          p(paste0("The overall health index score for ", summary[8], " is ", summary[1] , ". This is ", summary[5], " than the Hertfordshire figure. Of all the three main domains, ", summary[8], " scored best in the ", summary[6], ", which takes various topics such as ", summary[7], " into account. ")
            , style = "font-family:Bahnschrift,garamond,serif;font-size:18px;font-weight:bold;")
          
        }else if(reset_trigger < 0 | is.null(input$map_shape_click$id[1])){
          
          p(paste0("The overall health index score for Hertfordshire is 104.6. Of all the three main domains, Hertfordshire scored best in the Healthy Lives Domain, which takes various topics such as smoking, obesity and sedentary behaviours into account."),
            style = "font-family:Bahnschrift,garamond,serif;font-size:18px;font-weight:bold;")
          
        }
      })
      
      output$legend <- renderImage({
        
        legend <- list(src = "www/health_index_gradient.png", contentType = 'image/png')
        
        if(!input$comparator_selector == "No Comparator"){
          
          legend <- list(src = "www/map_legend.png", contentType = 'image/png')
          
        }
        
        legend
        
        }, deleteFile = FALSE)
      
      #pre-processing for map
      
      # render leaflet
      output$map <- renderLeaflet({

        ltla_shp <- readRDS("data/ltla_shp.rds") %>%
          arrange(LAD20NM)
        
        map_df <- map_df %>%
          left_join(ltla_shp, by = c("Area Name" = "LAD20NM")) %>%
          arrange(`Area Name`)

        hi_pal <- colorBin(colorRamp(rev(c("#133959", "#206095", "#8fafca", "#bccfdf"))), domain = map_df$`2019`, bins = 5, right = FALSE, na.color = "transparent")
        
        map <- map_df %>%
          leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15
                                           ,scrollWheelZoom =  FALSE)) %>% 
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          setView(lng = -0.082395, lat = 51.816811, zoom = 10) %>%
          addPolygons(data = ltla_shp, layerId = map_df$`Area Name`,
                      weight = 3, opacity = 0.8,  fillOpacity = 0.8, color = "grey",
                      fillColor = ~hi_pal(map_df$`2019`),
                      label = paste(map_df$`Area Name` , "| ",map_df$`2019`)) %>% 
          mapOptions(zoomToLimits =  "first") 
        
        if(!input$comparator_selector == "No Comparator"){
          
        comp_df <- map_comp_data(get_data(), input$comparator_selector) %>%
            filter(`Area Name` %in% c("Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire",
                                        "Stevenage", "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers", "Uttlesford",
                                        "Epping Forest", "Harlow")) %>% 
            mutate(color = case_when(comp_diff == 0 ~ "#FFC9A5",
                                     comp_diff < 0 & abs(comp_diff) < 20 ~ "#FE781F",
                                     comp_diff < 0 & abs(comp_diff) >= 20 ~ "#D0021B",
                                     comp_diff > 0 & abs(comp_diff) < 20 ~ "#8FAFCA",
                                     comp_diff > 0 & abs(comp_diff) >= 20 ~ "#206095")) %>% 
            left_join(ltla_shp, by = c("Area Name" = "LAD20NM")) %>%
            arrange(`Area Name`)
          
          map <- comp_df %>%
            leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15
                                             ,scrollWheelZoom =  FALSE)) %>% 
            addProviderTiles(providers$Esri.WorldTopoMap) %>%
            setView(lng = -0.082395, lat = 51.816811, zoom = 10) %>%
            addPolygons(data = ltla_shp, layerId = comp_df$`Area Name`,
                        weight = 3, opacity = 0.8,  fillOpacity = 0.8, color = "grey",
                        fillColor = ~comp_df$color,
                        label = paste(comp_df$`Area Name` , ": ",comp_df$`value`, " | ", comp_df$comp, ": ", comp_df$comp_value)) %>% 
            mapOptions(zoomToLimits =  "first")
        }

        map
        
      })
      
      output$sub_summarybox <- renderUI({
        
        herts_value <-  list(104.6, 106.8, 107.2, 99.7, "0000000000000000", "Healthy Lives Domain", "smoking, obesity and sedentary behaviours", "Hertfordshire")
        
        summarycards <- fluidRow(width = 6, align = "center",
                 column(width = 4, summaryBox3("Selected Area", "Hertfordshire", width = 12, icon = "fas fa-map-marked-alt", style = "info")),
                 column(width = 2, summaryBox3("Overall Health Index", herts_value[1], width = 12, icon = "fas fa-chart-bar", style = "info")),
                 column(width = 2, summaryBox3("Health People Index", herts_value[2], width = 12, icon = "fas fa-users", style = "secondary")),
                 column(width = 2, summaryBox3("Health Lives Index", herts_value[3], width = 12, icon = "fas fa-heartbeat", style = "secondary")),
                 column(width = 2, summaryBox3("Health Places Index", herts_value[4], width = 12, icon = "fas fa-map-marker-alt", style = "secondary")))
        
        if(!is.null(input$map_shape_click$id[1])){
          
          score <- area_select()$map_shape_click
          
          summarycards <- fluidRow(width = 6, align = "center",
                   column(width = 4, summaryBox3("Selected Area", score[8], width = 12, icon = "fas fa-map-marked-alt", style = "info")),
                   column(width = 2, summaryBox3("Overall Health Index", score[1], width = 12, icon = "fas fa-chart-bar", style = "info")),
                   column(width = 2, summaryBox3("Healthy People Index", score[2], width = 12, icon = "fas fa-users", style = "secondary")),
                   column(width = 2, summaryBox3("Healthy Lives Index", score[3], width = 12, icon = "fas fa-heartbeat", style = "secondary")),
                   column(width = 2, summaryBox3("Healthy Places Index", score[4], width = 12, icon = "fas fa-map-marker-alt", style = "secondary")))
          
        }
        
        summarycards
        
      })
      
    })
}

