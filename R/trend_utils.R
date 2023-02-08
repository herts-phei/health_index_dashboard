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
    mutate(sig_diff = case_when(p_value >= 0.05 ~ "not statistically significant", TRUE ~ "statistically significant"),
           direction = case_when(coefficient > 0 ~ "increasing", TRUE ~ "decreasing"))

}

summary_vals <- function(indicator, area = "Hertfordshire"){

  data <- get_indicators_data("lower") %>%
    rbind(get_indicators_data("upper")) %>%
    filter(ind %in% indicator)

  years <- sort(unique(data$year), decreasing = T)

  worst_ltla <- data %>%
    filter(year == max(year)) %>%
    filter(value == min(value))

  best_ltla <- data %>%
    filter(year == max(year)) %>%
    filter(value == max(value))

  prev_yr <- data %>%
    filter(year == years[2])

  colnames(prev_yr) <- paste0(colnames(prev_yr), "_prev")

  latest_yr <- data %>%
    filter(year == years[1])

  colnames(latest_yr) <- paste0(colnames(latest_yr), "_latest")

  all_df <- cbind(prev_yr, latest_yr) %>%
    mutate(change = case_when(value_latest > value_prev ~ "improved",
                            value_latest < value_prev ~ "worsened",
                            TRUE ~"not changed")) %>%
    mutate(abs_diff = abs(value_latest - value_prev)) %>%
    group_by(change) %>%
    filter(abs_diff %in% max(abs_diff)) %>%
    ungroup() %>%
    mutate(sig_change =  case_when(abs_diff >= 20 ~ "significant",
                                   abs_diff < 20 ~ "non-significant"))

  improved_ltla <- all_df %>%
    filter(change == "improved")

  worsened_ltla <- all_df %>%
    filter(change == "worsened")

  trends <- data %>%
    mutate(year = as.numeric(year)) %>%
    summarise(p_value = summary(lm(value~ year))$coefficient[,"Pr(>|t|)"]["year"],
              coefficient  = summary(lm(value~ year))$coefficient[,"Estimate"]["year"]) %>%
    mutate(sig_diff = case_when(p_value >= 0.05 ~ "not statistically significant", TRUE ~ "statistically significant"),
           direction = case_when(coefficient > 0 ~ "increasing", TRUE ~ "decreasing"))

  select_area_latest_change <- data %>% 
    filter(AreaName == area)
  
  select_area_prev_yr <- select_area_latest_change %>%
    filter(year == years[2])
  
  colnames(select_area_prev_yr) <- paste0(colnames(select_area_prev_yr), "_prev")
  
  select_area_latest_yr <- select_area_latest_change %>%
    filter(year == years[1])
  
  colnames(select_area_latest_yr) <- paste0(colnames(select_area_latest_yr), "_latest")
  
  select_area_all_df <- cbind(select_area_prev_yr, select_area_latest_yr) %>%
    mutate(change = case_when(value_latest > value_prev ~ "increase",
                              value_latest < value_prev ~ "decrease",
                              TRUE ~"not changed")) %>%
    mutate(abs_diff = abs(value_latest - value_prev)) %>%
    group_by(change) %>%
    filter(abs_diff %in% max(abs_diff)) %>%
    ungroup() %>%
    mutate(sig_change =  case_when(abs_diff >= 20 ~ "significant",
                                   abs_diff < 20 ~ "non-significant"))
  
  summary_list <- list(worst_ltla_name = worst_ltla$AreaName,
                       worst_ltla_value = worst_ltla$value,
                       best_ltla_name = best_ltla$AreaName,
                       best_ltla_value = best_ltla$value,
                       most_improved_name = improved_ltla$AreaName_latest,
                       most_improved_sig_change = improved_ltla$sig_change ,
                       most_worsened_name = worsened_ltla$AreaName_latest,
                       most_worsened_sig_change = worsened_ltla$sig_change,
                       trend_sig = trends$sig_diff,
                       trend_direction = trends$direction,
                       current_year_val = select_area_all_df$value_latest,
                       last_year_val = select_area_all_df$value_prev, 
                       select_area_last_year_abs_diff = select_area_all_df$abs_diff,
                       select_area_last_year_change = select_area_all_df$change,
                       select_area_last_year_diff_sig = select_area_all_df$sig_change,
                       last_year = years[2],
                       current_year = years[1])

  return(summary_list)

  }

# p_value <- summary(lm(value~ year_num, data))$coefficient[,"Pr(>|t|)"]
# p_value <- summary(lm(value~ year, data))$coefficient[,"Pr(>|t|)"]["year"]

# abs_diff <- max(abs(combn(data$value, 2, FUN = diff)))
