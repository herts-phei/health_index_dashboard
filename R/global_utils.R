
get_params <- function(){
  
  comp_area <- c("Hertfordshire", "Essex", "Hertfordshire and West Essex", "England")
  
  districts_lt <- c("Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire",
                    "Stevenage", "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers", "Uttlesford",
                    "Epping Forest", "Harlow")
  
  return(list(comp_area = comp_area,
              districts_lt = districts_lt))
}

get_data <- function(){
  
  data <- list()
  
  ics_overall <- read_excel("data/healthindexscoresintegratedcaresystemsengland.xlsx", sheet = 5, skip = 2) %>% 
    filter(`Area Name` %in%  "NHS Hertfordshire and West Essex Integrated Care Board") %>% 
    mutate(`Area Name` = "Hertfordshire and West Essex")
  
  data$df_hioverall <- read_excel("data/healthindexscoresengland.xlsx", 
                                  sheet = 5, skip = 2) %>% 
    select(-`Area Type [Note 3]`) %>% 
    filter(`Area Name` %in% c("Hertfordshire", "Essex", "ENGLAND" , "Broxbourne", "Dacorum", 
                              "East Hertfordshire", "Hertsmere", "North Hertfordshire", "Stevenage",
                              "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers", "Uttlesford",
                              "Epping Forest", "Harlow")) %>% 
    mutate(`Area Name` = gsub("ENGLAND", "England", `Area Name`)) %>% 
    rbind(ics_overall)
  
  latest_year_col <- max(colnames(data$df_hioverall)[-c(1:2)])
  
  ics_breakdown <- read_excel("data/healthindexscoresintegratedcaresystemsengland.xlsx", sheet = 6, skip = 4) %>% 
    filter(`Area Name` %in%  "NHS Hertfordshire and West Essex Integrated Care Board") %>% 
    mutate(`Area Name` = "Hertfordshire and West Essex")
  
  data$df <- read_excel("data/healthindexscoresengland.xlsx", 
                        sheet = 6, skip = 4) %>% 
    select(-`Area Type [Note 3]`) %>% 
    mutate(`Area Name` = gsub("ENGLAND", "England", `Area Name`)) %>% 
    filter(`Area Name` %in% c("Hertfordshire", "Essex", "ENGLAND" , "Broxbourne", "Dacorum",
                              "East Hertfordshire", "Hertsmere", "North Hertfordshire", "Stevenage",
                              "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers", "Uttlesford",
                              "Epping Forest", "Harlow")) %>%
    rbind(ics_breakdown) %>% 
    left_join(select(data$df_hioverall, {{latest_year_col}}, `Area Name`), by = "Area Name") %>% 
    rename(`Health Index` = {{latest_year_col}})
  
  return(data)
}


get_herts_data <- function(){
  
  herts_values <- list()
  
  overall_val <- read_excel("data/healthindexscoresengland.xlsx", 
                                  sheet = 5, skip = 2) %>% 
    select(-`Area Type [Note 3]`) %>% 
    filter(`Area Name` == "Hertfordshire")
  
  herts_values$overall <- overall_val %>% 
    select(ncol(overall_val)) %>% 
    pull()
  
  domains_val <- read_excel("data/healthindexscoresengland.xlsx", 
                        sheet = 6, skip = 4) %>% 
    select(-`Area Type [Note 3]`) %>% 
    filter(`Area Name` == "Hertfordshire") %>% 
    select(contains("Domain"))
  
  herts_values$healthy_people <- domains_val$`Healthy People Domain`
  herts_values$healthy_lives <- domains_val$`Healthy Lives Domain`
  herts_values$healthy_places <- domains_val$`Healthy Places Domain`
  
  return(herts_values)
}


wrapper <- function(x, ...) {
  
  paste(strwrap(x, ...), collapse = "\n")
  
}

get_indicators_data <- function(geog_level){
  
  ics_overall <- read_excel("data/healthindexscoresintegratedcaresystemsengland.xlsx", sheet = 5, skip = 2) %>% 
    filter(`Area Name` %in%  "NHS Hertfordshire and West Essex Integrated Care Board") %>% 
    mutate(`Area Name` = "Hertfordshire and West Essex") %>% 
    select(-1)
  
    hi_df <- read_excel("data/healthindexscoresengland.xlsx",
                      sheet = 5, skip = 2) %>%
    select(-c(1,3))%>%
    filter(`Area Name` %in% c("Hertfordshire", "Essex", "ENGLAND" , "Broxbourne", "Dacorum",
                              "East Hertfordshire", "Hertsmere", "North Hertfordshire", "Stevenage",
                              "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers", "Uttlesford",
                              "Epping Forest", "Harlow")) %>%
    rbind(ics_overall) %>% 
    mutate(`Area Name` = gsub("ENGLAND", "England", `Area Name`),
           ind = "Health Index") %>%
    pivot_longer(cols = c(2:(ncol(.)-1)), names_to = "year") %>%
    rename("AreaName" = "Area Name")
  
  sheets_num <- length(excel_sheets(path = "data/healthindexscoresengland.xlsx"))
  
  data <- list()
  
  sheet_num <- 6:(sheets_num-1)
  
  data_year <- rev(unique(hi_df$year))
  
  for( i in 1:length(data_year)){
    
    ics_df <- read_excel("data/healthindexscoresintegratedcaresystemsengland.xlsx",
                         sheet = sheet_num[i], skip = 4) %>%
      filter(`Area Name` %in%  "NHS Hertfordshire and West Essex Integrated Care Board") %>% 
      mutate(`Area Name` = "Hertfordshire and West Essex") %>%
      rename("AreaName" = "Area Name") %>%
      select(-1) %>% 
      pivot_longer(cols = colnames(.[-1]), names_to = "ind") %>%
      mutate(ind = gsub(" \\[.*\\]", "", ind),
             year = data_year[i])
    
    df <- read_excel("data/healthindexscoresengland.xlsx",
                     sheet = sheet_num[i], skip = 4) %>%
      filter(`Area Name` %in% c("Hertfordshire", "Essex", "ENGLAND" , "Broxbourne", "Dacorum",
                                "East Hertfordshire", "Hertsmere", "North Hertfordshire", "Stevenage",
                                "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers", "Uttlesford",
                                "Epping Forest", "Harlow")) %>%
      mutate(`Area Name` = gsub("ENGLAND", "England", `Area Name`)) %>%
      rename("AreaName" = "Area Name") %>%
      select(-c(1, 3)) %>%
      pivot_longer(cols = colnames(.[-1]), names_to = "ind") %>%
      mutate(ind = gsub(" \\[.*\\]", "", ind),
             year = data_year[i]) %>% 
      rbind(ics_df)
    
    data[[paste0(data_year[i])]] <- df
    
  }
  
  all_df <- bind_rows(data) %>%
    rbind(hi_df)
  
  if(geog_level == "upper"){
    
    all_df <- all_df %>%
      filter(AreaName %in% c("Hertfordshire", "Essex", "Hertfordshire and West Essex", "England"))
    
  }else if(geog_level == "lower"){
    
    all_df <- all_df %>%
      filter(!AreaName %in% c("Hertfordshire", "Essex", "Hertfordshire and West Essex", "England"))
  }else{
    
    print(paste0("Please check you have entered a geography level"))
  }
  
  return(all_df)
}