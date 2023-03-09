
get_params <- function(){
  
  comp_area <- c("Hertfordshire", "Essex", "England")
  
  districts_lt <- c("Broxbourne", "Dacorum", "East Hertfordshire", "Hertsmere", "North Hertfordshire",
                    "Stevenage", "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers", "Uttlesford",
                    "Epping Forest", "Harlow")
  
  return(list(comp_area = comp_area,
              districts_lt = districts_lt))
}

get_data <- function(){
  
  data <- list()
  
  data$df_hioverall <- read_excel("data/healthindexscoresengland.xlsx", 
                                  sheet = 5, skip = 2) %>% 
    select(-`Area Type [Note 3]`) %>% 
    filter(`Area Name` %in% c("Hertfordshire", "Essex", "ENGLAND" , "Broxbourne", "Dacorum", 
                              "East Hertfordshire", "Hertsmere", "North Hertfordshire", "Stevenage",
                              "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers", "Uttlesford",
                              "Epping Forest", "Harlow")) %>% 
    mutate(`Area Name` = gsub("ENGLAND", "England", `Area Name`))
  
  latest_year_col <- max(colnames(data$df_hioverall)[-c(1:2)])
  
  data$df <- read_excel("data/healthindexscoresengland.xlsx", 
                        sheet = 6, skip = 4) %>% 
    select(-`Area Type [Note 3]`) %>% 
    mutate(`Area Name` = gsub("ENGLAND", "England", `Area Name`)) %>% 
    left_join(select(data$df_hioverall, {{latest_year_col}}, `Area Name`), by = "Area Name") %>% 
    rename(`Health Index` = {{latest_year_col}})
  
  return(data)
}

wrapper <- function(x, ...) {
  
  paste(strwrap(x, ...), collapse = "\n")
  
}

get_indicators_data <- function(geog_level){
  
  hi_df <- read_excel("data/healthindexscoresengland.xlsx",
                      sheet = 5, skip = 2) %>%
    select(-c(1,3))%>%
    filter(`Area Name` %in% c("Hertfordshire", "Essex", "ENGLAND" , "Broxbourne", "Dacorum",
                              "East Hertfordshire", "Hertsmere", "North Hertfordshire", "Stevenage",
                              "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers", "Uttlesford",
                              "Epping Forest", "Harlow")) %>%
    mutate(`Area Name` = gsub("ENGLAND", "England", `Area Name`),
           ind = "Health Index") %>%
    pivot_longer(cols = c(2:(ncol(.)-1)), names_to = "year") %>%
    rename("AreaName" = "Area Name")
  
  sheets_num <- length(excel_sheets(path = "data/healthindexscoresengland.xlsx"))
  
  data <- list()
  
  sheet_num <- 6:(sheets_num-1)
  
  data_year <- rev(unique(hi_df$year))
  
  for( i in 1:length(data_year)){
    
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
             year = data_year[i])
    
    data[[paste0(data_year[i])]] <- df
    
  }
  
  all_df <- bind_rows(data) %>%
    rbind(hi_df)
  
  if(geog_level == "upper"){
    
    all_df <- all_df %>%
      filter(AreaName %in% c("Hertfordshire", "Essex", "England"))
    
  }else if(geog_level == "lower"){
    
    all_df <- all_df %>%
      filter(!AreaName %in% c("Hertfordshire", "Essex", "England"))
  }else{
    
    print(paste0("Please check you have entered a geography level"))
  }
  
  return(all_df)
}
