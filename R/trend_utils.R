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


sig_dff <- function(indicator, area){

data <- get_indicators_data("lower")%>%
  rbind(get_indicators_data("upper")) %>% 
  filter(ind == indicator) %>%
  filter(AreaName == area) %>% 
  mutate(year = as.numeric(year)) 

p_value <- summary(lm(value~ year, data))$coefficient[,"Pr(>|t|)"]["(Intercept)"]

if(p_value >= 0.05){
  
  sig_diff <- T
  
}else{
  
  sig_diff <- F
  
}

return(sig_diff)}

sig_diff_df <- function(){
  
  data <- get_indicators_data("lower") %>%
    rbind(get_indicators_data("upper")) %>% 
    mutate(year = as.numeric(year)) %>% 
    group_by(ind, AreaName) %>% 
    summarise(p_value = summary(lm(value~ year))$coefficient[,"Pr(>|t|)"]["year"],
           coefficient  = summary(lm(value~ year))$coefficient[,"Estimate"]["year"]) %>% 
    ungroup() %>% 
    mutate(sig_diff = case_when(p_value >= 0.05 ~ "not significantly different", TRUE ~ "significantly different"),
           direction = case_when(coefficient > 0 ~ "increase", TRUE ~ "decrease")) #%>% 
    # select(-c(p_value, coefficient))
  
}


# p_value <- summary(lm(value~ year_num, data))$coefficient[,"Pr(>|t|)"]
# p_value <- summary(lm(value~ year, data))$coefficient[,"Pr(>|t|)"]["year"]


# abs_diff <- max(abs(combn(data$value, 2, FUN = diff)))
