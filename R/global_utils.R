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
  
  data$df_hioverall <- read_excel("data/healthindex.xlsx", 
                             sheet = 5, skip = 2) %>% 
    filter(`Area Name` %in% c("Hertfordshire", "Essex", "ENGLAND" , "Broxbourne", "Dacorum", 
                              "East Hertfordshire", "Hertsmere", "North Hertfordshire", "Stevenage",
                              "St Albans", "Watford", "Welwyn Hatfield", "Three Rivers", "Uttlesford",
                              "Epping Forest", "Harlow")) %>% 
    mutate(`Area Name` = gsub("ENGLAND", "England", `Area Name`))
  
  data$df <- read_excel("data/healthindex.xlsx", 
                   sheet = 10, skip = 4) %>% 
    mutate(`Area Name` = gsub("ENGLAND", "England", `Area Name`)) %>% 
    left_join(select(data$df_hioverall, `2019`, `Area Name`), by = "Area Name") %>% 
    rename(`Health Index` = `2019`)
  
  
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
    pivot_longer(cols = c(2:7), names_to = "year") %>%
    rename("AreaName" = "Area Name")
  
  data <- list()
  
  sheet_num <- 6:11
  
  data_year <- c(2015, 2016, 2017, 2018, 2019, 2020)
  
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
