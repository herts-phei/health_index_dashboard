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
                     htmlOutput(ns("instruction_text"))
                     ),
                   box(
                     title = "Summary",
                     width = 12,
                     align = "left",
                     htmlOutput(ns("summary_text")))),
            box(
              title = "Map",
              width = 8,
              align = "right",
              fluidRow(column(width = 6, align = "center",
                              uiOutput(outputId = ns("legend"))
                              ),
                       column(width = 3, align = "left", selectInput(ns("comparator_selector"), "Select comparator", 
                              choices = c("No Comparator", "Hertfordshire", "Essex", "Hertfordshire and West Essex", "England"), selected = "No Comparator")),
                       column(width = 2, align = "center", actionButton(ns("reset"), "Reset to Hertfordshire")),
                       column(width = 1, align = "right", actionButton(ns("info_msg"), "", icon = icon("circle-question")))),
              leafletOutput(ns("map"), width = "100%", height = 660)
              
              ))
  )
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
      
      latest_year_col <- colnames(map_df)[ncol(map_df)]
      
      herts_values <- get_herts_data()
      
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

        action_value$map_shape_click <- domain_scores(input$map_shape_click$id[1], input$comparator_selector)
        
      })
      
      observeEvent(input$reset, {
        
        action_value$map_shape_click <-  list(herts_values$overall, herts_values$healthy_people, herts_values$healthy_lives, herts_values$healthy_places, "0000000000000000", "Healthy Lives Domain", "smoking, obesity and sedentary behaviours", "Hertfordshire")
        
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
      
      observeEvent(input$info_msg, 
                   {showModal(
                     ui = modalDialog(title = "Information", 
                                      size = "xl", 
                                      easyClose = T,
                                      HTML("<B>User Guidance</B><br><br>",
                                           
                                           "The app can be navigated by using the collapsable side menu bar, where you can access the different tabs.<br><br>",
                                           
                                           "<ul><li>The <B>overview tab</B> shows a map of districts in Hertfordshire and West Essex and provides a brief summary of the overall health index in the areas. </li><br>",
                                           
                                           "<li>The <B>indicators tab</B> allows user to investigate the the details of the domains, subdomains and indicators that make up the overall health index. </li><br>",
                                           
                                           "<li>The <B>trends tab</B> enables user examine how the health indexes have changed over time since 2015. </li><br>",
                                           
                                           "<li>The <B>information tab</B> provides background information on the health index and the app. </li></ul><br><br>",
                                           
                                           "<B>Interpretting the index</B><br><br>",  
                                           
                                           "Every level from the overall Health Index right through to the individual indicators is indexed around 100. A score of 100 means health is equal to England’s health in 2015. A score higher than 100 means health is better than England in 2015; a score lower than 100 means health is worse. ",
                                           
                                           "When comparing index scores, a score of 10 denotes one standard deviation. Differences are labelled significantly different when they are two standard deviations (20 scores) apart. More information on the health index can be found <a href='https://blog.ons.gov.uk/2022/11/09/the-health-index-2020-measuring-the-nations-health/#:~:text=The%20ONS%E2%80%99%20Health%20Index%20is%20a%20rich%20data,area%20and%20what%20local%20factors%20are%20in%20play./'>here</a>. <br><br>"
                                      )
                                           ))})
      
      output$summary_text<-renderUI({
        
        # trigger_reset <- trigger_status()$reset
        # trigger_map <- trigger_status()$map_click
        # 
        # print(paste0("reset: ", trigger_reset, " | map_click: ", trigger_map))
        
        reset_trigger <- trigger_status()$reset
        
        if(reset_trigger > 0 & !is.null(input$map_shape_click$id[1])){

          summary <- area_select()$map_shape_click
          comparator <- input$comparator_selector 
          
          if(input$comparator_selector == "No Comparator"){comparator <- "Hertfordshire"}
          
          p(paste0("The overall health index score for ", summary[8], " is ", summary[1] , ". This is ", summary[5], " than the ", comparator, " figure. Of all the three main domains, ", summary[8], " scored best in the ", summary[6], ", which takes various topics such as ", summary[7], " into account. ")
            , style = "font-family:Bahnschrift,garamond,serif;font-size:18px;font-weight:normal;")
          
        }else if(reset_trigger < 0 | is.null(input$map_shape_click$id[1])){
          
          p(paste0("The overall health index score for Hertfordshire is ", herts_values$overall, ". Of all the three main domains, Hertfordshire scored best in the Healthy Lives Domain, which takes various topics such as smoking, obesity and sedentary behaviours into account."),
            style = "font-family:Bahnschrift,garamond,serif;font-size:18px;font-weight:normal;")
          
        }
      })
    
      
        output$legend<-renderUI({
          
          if(input$comparator_selector == "No Comparator"){
            
            img(src='health_index_gradient.png')
          
          }else{
            
            img(src='map_legend.png')
            
          }
        })
        
      #pre-processing for map
      
      # render leaflet
      output$map <- renderLeaflet({
        
        ltla_shp <- readRDS("data/ltla_shp.rds") %>%
          arrange(LAD20NM)
        
        map_df <- map_df %>%
          left_join(ltla_shp, by = c("Area Name" = "LAD20NM")) %>%
          arrange(`Area Name`)
        
        hi_pal <- colorBin(colorRamp(rev(c("#133959", "#206095", "#8fafca", "#bccfdf"))), domain = as.numeric(unlist(map_df[{{latest_year_col}}])), bins = 5, right = FALSE, na.color = "transparent")

        map <- map_df %>%
          leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15
                                           ,scrollWheelZoom =  FALSE)) %>% 
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          setView(lng = -0.082395, lat = 51.816811, zoom = 10) %>%
          addPolygons(data = ltla_shp, layerId = map_df$`Area Name`,
                      weight = 3, opacity = 0.8,  fillOpacity = 0.8, color = "grey",
                      fillColor = ~hi_pal(as.numeric(unlist(map_df[{{latest_year_col}}]))),
                      label = paste(map_df$`Area Name` , "| ",as.numeric(unlist(map_df[{{latest_year_col}}])))) %>% 
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
        
        herts_value <-  list(herts_values$overall, herts_values$healthy_people, herts_values$healthy_lives, herts_values$healthy_places, "0000000000000000", "Healthy Lives Domain", "smoking, obesity and sedentary behaviours", "Hertfordshire")
        
        summarycards <- fluidRow(align = "center",
                                 column(width = 4, summaryBox3("Selected Area", "Hertfordshire", width = 12, icon = "fas fa-map-marked-alt", style = "info")),
                                 column(width = 2, summaryBox3(paste0("Overall Health\n Index", sep = "\n"), herts_value[1], width = 12, icon = "fas fa-chart-bar", style = "info")),
                                 column(width = 2, summaryBox3(paste0("Healthy People\n Index", sep = "\n"), herts_value[2], width = 12, icon = "fas fa-users", style = "secondary")),
                                 column(width = 2, summaryBox3(paste0("Healthy Lives\n  Index", sep = "\n"), herts_value[3], width = 12, icon = "fas fa-heartbeat", style = "secondary")),
                                 column(width = 2, summaryBox3(paste0("Healthy Places\n Index", sep = "\n"), herts_value[4], width = 12, icon = "fas fa-map-marker-alt", style = "secondary")))
        
        if(!is.null(input$map_shape_click$id[1])){
          
          score <- area_select()$map_shape_click
          
          summarycards <- fluidRow(align = "center",
                                   column(width = 4, summaryBox3("Selected Area", score[8], width = 12, icon = "fas fa-map-marked-alt", style = "info")),
                                   column(width = 2, summaryBox3(paste0("Overall Health\n Index", sep = "\n"), score[1], width = 12, icon = "fas fa-chart-bar", style = "info")),
                                   column(width = 2, summaryBox3(paste0("Healthy People\n Index", sep = "\n"), score[2], width = 12, icon = "fas fa-users", style = "secondary")),
                                   column(width = 2, summaryBox3(paste0("Healthy Lives\n  Index", sep = "\n"), score[3], width = 12, icon = "fas fa-heartbeat", style = "secondary")),
                                   column(width = 2, summaryBox3(paste0("Healthy Places\n Index", sep = "\n"), score[4], width = 12, icon = "fas fa-map-marker-alt", style = "secondary")))
          
        }
        
        
        summarycards
        
      })
      
    })
}
