tab_information_mod <- function(id, label = "trend"){
  
  ns <- NS(id)
  
  tabItem(tabName = "information",
          box(title = "Information", width = 12,
             htmlOutput(ns("info")))
          
          )
}


tab_information_server <- function(id){
  
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- shiny::NS(id)
      
      output$info <- shiny::renderText({
        
        paste("The <a href='https://blog.ons.gov.uk/2022/11/09/the-health-index-2020-measuring-the-nations-health/#:~:text=The%20ONS%E2%80%99%20Health%20Index%20is%20a%20rich%20data,area%20and%20what%20local%20factors%20are%20in%20play./'>ONS Health Index</a> provides a single value for health that can show how health changes over time. It uses a broad definition of health, including health outcomes, health-related behaviours and personal circumstances, and wider drivers of health. ",
               
              "The Health Index is organised around three broad areas:<br><br>",
               
              "<ul><li><B>Healthy People</B>: covers health outcomes such as life expectancy, physical health conditions like dementia, cancer and kidney disease, disability, personal well-being and mental health.</li><br>",
               
              "<li><B>Healthy Lives</B>: covers health-related behaviours and personal circumstances including obesity, hypertension, drug misuse, smoking and cancer screening.</li><br>",
               
              "<li><B>Healthy Places</B>: covers the wider social, economic and environmental drivers of health such as crime, unemployment, child poverty, pollution, noise and road traffic.</li></ul>",
               
              "<B>Interpretting the index</B><br><br>",  
               
              "Every level from the overall Health Index right through to the individual indicators is indexed around 100. A score of 100 means health is equal to England’s health in 2015. A score higher than 100 means health is better than England in 2015; a score lower than 100 means health is worse. ",
               
              "When comparing index scores, a score of 10 denotes one standard deviation. Differences are considered significantly different when they are two standard deviations (20 scores) apart. <br><br>",
              
              "<B>User Guidance</B><br><br>",  
              
              "The app can be navigated by using the collapsable side menu bar, where you can access the different tabs.<br><br>",
              
              "<ul><li>The <B>overview tab</B> shows a map of districts in Hertfordshire and West Essex and provides a brief summary of the overall health index in the areas. </li><br>",
              
              "<li>The <B>indicators tab</B> allows user to investigate the the details of the domains, subdomains and indicators that make up the overall health index. </li><br>",
              
              "<li>The <B>trends tab</B> enables user examine how the health indexes have changed over time since 2015. </li><br>",
              
              "<li>The <B>information tab</B> provides background information on the health index and the app. </li></ul>"
              
          )
        
      })
      
    }
  )
}

    
  
      