library(sf)
library(leaflet)
library(leaflet.extras)

map_data <- function(){
  
  areas <- get_params()
  
  df <- data$df_hioverall %>% 
    filter(`Area Name` %in% c(areas$comp_area, areas$districts_lt))
  
  return(df)
  
}

domain_scores <- function(area_select, comparator){
  
  # map_df <- isolate(map_data()$df_hioverall)
  # area_select = "Hertfordshire"
  
  if(comparator == "No Comparator"){comparator <- "Hertfordshire"}
  
  data <- get_data()
  
  info <- list()
  
  main_df <- data$df_hioverall 
  subdomain_df <- data$df 
  
  latest_yr_col <- colnames(main_df)[ncol(main_df)]
  
  subdomain_scores <- subdomain_df %>% 
    filter(`Area Name` %in% area_select) %>% 
    select(c("Area Name","Healthy People Domain", "Healthy Lives Domain", "Healthy Places Domain")) 
  
  info[1] <- main_df %>% 
    filter(`Area Name` == area_select) %>% 
    pull({{latest_yr_col}})
  
  info[2] <- subdomain_scores %>% 
    pull(`Healthy People Domain`)
  
  info[3] <- subdomain_scores %>% 
    pull(`Healthy Lives Domain`)
  
  info[4] <- subdomain_scores %>% 
    pull(`Healthy Places Domain`)
  
  #new part
  
  herts_hi <- main_df %>% 
    filter(`Area Name` %in% comparator) %>% 
    pull({{latest_yr_col}})
  
  # [5] sig diff with comparator 
  
  if((abs(info[[1]][1] - herts_hi)) >= 20 & (info[[1]][1] - herts_hi) > 0){
    
    info[5] <- "significantly higher"
    
  }else if((abs(info[[1]][1] - herts_hi)) >= 20 & (info[[1]][1] - herts_hi) < 0){
    
    info[5] <- "significantly lower"
    
  }else if((abs(info[[1]][1] - herts_hi)) < 20 & (info[[1]][1] - herts_hi) < 0){
    
    info[5] <- "not significantly lower"
    
  }else if((abs(info[[1]][1] - herts_hi)) < 20 & (info[[1]][1] - herts_hi) > 0){
    
    info[5] <- "not significantly lower"
    
  }
  
  # [6] highest/best performing domain
  
  info[6] <- subdomain_scores %>%
    select(where(~ any(. ==  max(unlist(info)[2:4])))) %>% 
    colnames()
  
  # [3] indicator examples
  
  if(info[6] == "Healthy People Domain"){
    
    info[7] <- "life expectancy, life satisfaction and various physical health conditions"
    
  }else if(info[6] == "Healthy Lives Domain"){
    
    info[7] <- "smoking, obesity and sedentary behaviours"
    
  }else if(info[6] == "Healthy Places Domain"){
    
    info[7] <- "public green space access, crime and air pollution"
    
  }
  
  info[8] <- area_select
  
  return(info)
}

map_comp_data <- function(map_data, comp){

  
  df <- isolate(map_data$df_hioverall)
  
  latest_yr_col <- colnames(df)[ncol(df)]
  
  df <- df %>% 
    select(c(`Area Code`, `Area Name`, "value" = {{latest_yr_col}}))
  
  comp_df <- df %>% 
    mutate(comp = comp,
           comp_value = df$`value`[which(df$`Area Name`==comp)],
           comp_diff = value - comp_value)
  
  return(comp_df)
}