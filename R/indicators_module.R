# UI ----------------------------------------------------------------------


tab_indicators_mod <- function(id, label = "indicators"){
  
  ns <- NS(id)
  
  tabItem(tabName = "indicator",
          
          fluidRow(
            column(width = 8,
                   box(width = 12, align = "center", offset = 0, 
                       column(width = 12, align = "left", 
                              htmlOutput(ns("text"))),
                       fluidRow(column(width = 6, align = "left", 
                                       selectInput(ns("district_selector"), "Select district", choices = c("Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire",
                                                                                                           "Stevenage", "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers", "Uttlesford",
                                                                                                           "Epping Forest", "Harlow"), selected = "Broxbourne")),
                                column(width = 6, align = "left", selectInput(ns("comparator_selector"), "Select comparator", choices = c("No Comparator", "Hertfordshire", "Essex", "Hertfordshire and West Essex", "England"), selected = "No Comparator"))),
                       fluidRow(column(width = 11, 
                                       uiOutput(outputId = ns("legend"))),
                                column(width = 1, actionButton(ns("info_msg"), "", icon = icon("circle-question")))),
                       reactableOutput(ns("table")))),
            column(width = 4, 
                   box(width = 12, title = "Healthy People Domain", align = "left", collapsed = FALSE,
                       htmlOutput(ns("bar_text1")),
                       selectInput(ns("subdomain1_selector"), "Select a subdomain", choices = c("Difficulties in daily life", "Mental health", "Mortality", "Personal well-being", "Physical health conditions"), selected = "Difficulties in daily life"),
                       uiOutput(ns("subdomain1"))),
                   box(width = 12, title = "Healthy Lives Domain", align = "left", collapsed = TRUE,
                       htmlOutput(ns("bar_text2")),
                       selectInput(ns("subdomain2_selector"), "Select a subdomain", choices = c("Behavioural risk factors", "Children and young people", "Physiological risk factors", "Protective measures"), selected = "Behavioural risk factors"),
                       uiOutput(ns("subdomain2"))),
                   box(width = 12, title = "Healthy Places Domain", align = "left", collapsed = TRUE,
                       htmlOutput(ns("bar_text3")),
                       selectInput(ns("subdomain3_selector"), "Select a subdomain", choices = c("Access to green space", "Access to services", "Crime", "Economic and working conditions", "Living conditions"), selected = "Access to green space"),
                       uiOutput(ns("subdomain3"))))
            
          ))}


# Server ------------------------------------------------------------------

tab_indicators_server <- function(id){
  
  
  moduleServer(
    
    id,
    
    function(input, output, session) {
      
      ns <- NS(id)
      
      backing_data <- reactiveValues()
      
      observe({
        
        if (input$comparator_selector == "No Comparator") {
          backing_data$comp_data <- create_comp_data(data = get_data(), 
                                                     area = input$district_selector) 
          
          backing_data$comp_data2 <- data.frame()
          backing_data$mode <- "Gradient" 
          
        } else { 
          # if "No comparator" is not the selection it will trigger a second dataset used for comparison. 
          backing_data$comp_data <- create_comp_data(data = get_data(), 
                                                     area = input$district_selector) 
          
          backing_data$comp_data2 <- create_comp_data(data = get_data(), 
                                                      area = input$comparator_selector) 
          backing_data$mode <- "Categorical"
        }
      })
      
      #Info button
      
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
      
      #Intro text 
      
      output$text<-renderUI({
        
        p("This tab visualises the breakdown of health index scores into (sub)domains and indicators. The colours provide an indication of whether the selected district is performing better or worse than the selected comparator. By hovering the mouse over the colour blocks, a pop-up label will show which indicator it represents and its score. 
          When “no comparator” is selected, the colours are indication of how the indicator or (sub)domain performs when compared to other indicators or (sub)domains in the district.
          ", style = "font-family:Bahnschrift,garamond,serif;font-size:18px;font-weight:lighter;")
      })
      
      output$bar_text1<-renderUI({
        
        p("The bar chart below shows scores of indicators in the selected subdomain for the chosen district and comparator.
          ", style = "font-family:Bahnschrift,garamond,serif;font-size:18px;font-weight:lighter;")
      })
      
      output$bar_text2<-renderUI({
        
        p("The bar chart below shows scores of indicators in the selected subdomain for the chosen district and comparator.
          ", style = "font-family:Bahnschrift,garamond,serif;font-size:18px;font-weight:lighter;")
      })
      
      output$bar_text3<-renderUI({
        
        p("The bar chart below shows scores of indicators in the selected subdomain for the chosen district and comparator.
          ", style = "font-family:Bahnschrift,garamond,serif;font-size:18px;font-weight:lighter;")
      })
      

      #Side plots
      
      output$subdomain1 <- renderUI({
        
        if(backing_data$mode == "Categorical"){
          
          plot_func(ltla = input$district_selector, comparator = input$comparator_selector, subdomain = input$subdomain1_selector)
          
        }else{
          
          plot_func(ltla = input$district_selector, subdomain = input$subdomain1_selector)
          
        }
      })
      
      output$subdomain2 <- renderUI({
        
        if(backing_data$mode == "Categorical"){
          
          plot_func(ltla = input$district_selector, comparator = input$comparator_selector, subdomain = input$subdomain2_selector)
          
        }else{
          
          plot_func(ltla = input$district_selector, subdomain = input$subdomain2_selector)
          
        }
      })
      
      output$subdomain3 <- renderUI({
        
        if(backing_data$mode == "Categorical"){
          
          plot_func(ltla = input$district_selector, comparator = input$comparator_selector, subdomain = input$subdomain3_selector)
          
        }else{
          
          plot_func(ltla = input$district_selector, subdomain = input$subdomain3_selector)
          
        }
      })
      
      #Legend

      output$legend<-renderUI({
        
        if(input$comparator_selector == "No Comparator"){
          
          img(src='health_index_gradient.png', width = '452px', height = '85px')
          
        }else{
          
          img(src='map_legend.png', width = '452px', height = '85px')
          
        }
      })
      
      #Main table
      
      output$table <- reactable::renderReactable({
        
        backing_data$comp_data %>%
          reactable(pagination = FALSE,
                    sortable = FALSE,
                    bordered = FALSE,
                    wrap = FALSE, 
                    theme = reactableTheme(
                      style = list(fontSize = 19, fontFamily = "Bahnschrift", fontWeight = "lighter")),
                    class = "index-table",
                    defaultColDef = colDef(
                      class = "cell",
                      headerClass = "header",
                      footerStyle = list(fontSize = 17)
                    ),
                    rowStyle = function(index) {
                      if (backing_data$comp_data[index, "ind"] %in% c(" Healthy People Domain", " Healthy Lives Domain", " Healthy Places Domain")) {
                        list(fontWeight = "bold", fontSize = 20, font = "Bahnschrift")
                      } else if (backing_data$comp_data[index, "ind"] == " Health Index") {
                        list(fontWeight = "bold", backgroundColor = "#f7f7f7", fontSize = 24, font = "Bahnschrift")
                      }
                    },
                    columns = list(
                      ind = colDef(maxWidth = 400, name = "",
                                   
                                   # Cell function to add icons
                                   cell = function(value) {
                                     
                                     # Style for each domain
                                     if(value %in% c(" Healthy People Domain", " Healthy Lives Domain", " Healthy Places Domain")){
                                       
                                       img_src <- knitr::image_uri(paste0("www/", gsub(" ", "-", stringr::str_trim(value)), ".png"))
                                       image <- img(src = img_src, style = "height: 20px;", alt = value)
                                       tagList(
                                         div(style = "display: inline-block; height: 20px; width: 55px;", image),
                                         value)
                                     } else (return(value))
                                     
                                   },
                                   footer = function() "Source: ONS Health Index Scores 2021",
                                   align = "left"),
                      row1 = colDef(show = F),
                      row2 = colDef(show = F),
                      row3 = colDef(show = F),
                      row4 = colDef(show = F),
                      row5 = colDef(show = F),
                      row6 = colDef(show = F),
                      row7 = colDef(show = F),
                      index_value = colDef(name = "",
                                           maxWidth = 80,
                                           
                                           cell = function(value, index) {
                                             figure( backing_data$comp_data, index = index, col = "index_value")
                                           },
                                           
                                           style = function(value, index, table_df, col, comparator, mode){
                                             
                                             #underline colours
                                             color <- sig_diff_colour(table_df = backing_data$comp_data,
                                                                      col = "index_value",
                                                                      value = backing_data$comp_data$index_value[index],
                                                                      index = index,
                                                                      comparator = backing_data$comp_data2,
                                                                      mode = backing_data$mode)
                                             
                                             if(value == " Health Index") {
                                               bar_style(fill = color, length = "100%", colour = "#fff")
                                             } else if (value %in% c(" Healthy People Domain", " Healthy Lives Domain", " Healthy Places Domain")) {
                                               bar_style(fill = color, length = "100%")
                                             } else {
                                               bar_style(fill = color, length = "100%", bg_colour = "#f7f7f7") 
                                             }
                                           }
                      ),
                      # #cell block 
                      value1 = colDef(name = "",
                                      maxWidth = 70,
                                      html = T,
                                      cell = function(value, index, name) {cell_colouring(backing_data$comp_data, value, index, "row1", comparator = backing_data$comp_data2, mode = backing_data$mode)}),
                      value2 = colDef(name = "",
                                      maxWidth = 70,
                                      cell = function(value, index, name) {cell_colouring(backing_data$comp_data, value, index, "row2", comparator = backing_data$comp_data2, mode = backing_data$mode)}),
                      value3 = colDef(name = "",
                                      maxWidth = 70,
                                      cell = function(value, index, name) {cell_colouring(backing_data$comp_data, value, index, "row3", comparator = backing_data$comp_data2, mode = backing_data$mode)}),
                      value4 = colDef(name = "",
                                      maxWidth = 70,
                                      cell = function(value, index, name) {cell_colouring(backing_data$comp_data, value, index, "row4", comparator = backing_data$comp_data2, mode = backing_data$mode)}),
                      value5 = colDef(name = "",
                                      maxWidth = 70,
                                      cell = function(value, index, name) {cell_colouring(backing_data$comp_data, value, index, "row5", comparator = backing_data$comp_data2, mode = backing_data$mode)}),
                      value6 = colDef(name = "",
                                      maxWidth = 70,
                                      cell = function(value, index, name) {cell_colouring(backing_data$comp_data, value, index, "row6", comparator = backing_data$comp_data2, mode = backing_data$mode)}),
                      value7 = colDef(name = "",
                                      maxWidth = 70,
                                      cell = function(value, index, name) {cell_colouring(backing_data$comp_data, value, index, "row7", comparator = backing_data$comp_data2, mode = backing_data$mode)}),
                      comp = colDef(show = FALSE),
                      row_id = colDef(show = FALSE),
                      value = colDef(show = FALSE),
                      AreaName = colDef(show = FALSE),
                      order_ind = colDef(show = FALSE)
                    ))
        
      })})}