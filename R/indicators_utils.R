
# Processing --------------------------------------------------------------

#Process data for comparison table

# data <- get_data()
# area <- "Broxbourne"
# create_comp_data( get_data(), "Broxbourne")

create_comp_data <- function(data, area){
  
  # breakdown of health index
  df <- data$df %>% 
    filter(`Area Name` == area)
  
  # lookups for later 
  df_pivot <- df %>%
    rename("AreaName" = "Area Name") %>%
    select(-1) %>%
    pivot_longer(cols = colnames(.[-1]), names_to = "ind") %>%
    mutate(ind = gsub(" \\[.*\\]", "", ind))
  
  # Df to order the indicators for the table
  df1 <- df %>% 
    filter(`Area Name` == area) %>% 
    # select(3:19, `Health Index`) %>% 
    select( "Health Index", "Healthy People Domain", "Difficulties in daily life [Pe]", "Mental health [Pe]", "Mortality [Pe]",
            "Personal well-being [Pe]", "Physical health conditions [Pe]", "Healthy Lives Domain",
            "Behavioural risk factors [L]", "Children and young people [L]", "Physiological risk factors [L]", "Protective measures [L]",
            "Healthy Places Domain", "Access to green space [Pl]", "Access to services [Pl]", "Crime [Pl]",
            "Economic and working conditions [Pl]", "Living conditions [Pl]") %>%
    pivot_longer(cols = colnames(.), names_to = "ind") %>% 
    mutate(ind = case_when(ind %in% c("Healthy People Domain", "Healthy Lives Domain", "Healthy Places Domain") ~ 
                             paste0(" ", ind), 
                           ind == "Health Index" ~ paste0(" ", ind),
                           TRUE ~ ind),
           order_ind = case_when(ind == " Health Index" ~ 0,
                                 grepl("\\[Pe\\]", ind) | ind == " Healthy People Domain" ~ 1, 
                                 grepl("\\[L\\]", ind) | ind == " Healthy Lives Domain" ~ 2, 
                                 grepl("\\[Pl\\]", ind) | ind == " Healthy Places Domain" ~ 3),
           `Area Name` = area) %>% 
    arrange(area, order_ind, ind) %>% 
    select(`Area Name`, ind, value, everything()) %>% 
    rename("index_value" = "value") 
  
  # df to set order and names of colored tiles for each indicator of each domain
  df2 <-  read.csv("data/indicators_table.csv")
  
  df_pivot_temp <- filter(df_pivot, AreaName == area) %>% 
    select(-AreaName)
  
  df3 <- df2 %>%
    left_join(rename(df_pivot_temp, value1 = value), by = c("row1" = "ind")) %>% 
    left_join(rename(df_pivot_temp, value2 = value), by = c("row2" = "ind")) %>% 
    left_join(rename(df_pivot_temp, value3 = value), by = c("row3" = "ind")) %>% 
    left_join(rename(df_pivot_temp, value4 = value), by = c("row4" = "ind")) %>% 
    left_join(rename(df_pivot_temp, value5 = value), by = c("row5" = "ind")) %>% 
    left_join(rename(df_pivot_temp, value6 = value), by = c("row6" = "ind")) %>% 
    left_join(rename(df_pivot_temp, value7 = value), by = c("row7" = "ind")) %>% 
    select(8:14) %>% 
    mutate(`Area Name` = area)
  
  # Combine
  table_df <- df1 %>% 
    rename("AreaName" = "Area Name") %>% 
    mutate(ind = factor(ind, levels = c(" Health Index", " Healthy People Domain", "Difficulties in daily life [Pe]", "Mental health [Pe]", "Mortality [Pe]", 
                                        "Personal well-being [Pe]", "Physical health conditions [Pe]", " Healthy Lives Domain",
                                        "Behavioural risk factors [L]", "Children and young people [L]", "Physiological risk factors [L]", "Protective measures [L]",
                                        " Healthy Places Domain", "Access to green space [Pl]", "Access to services [Pl]", "Crime [Pl]",
                                        "Economic and working conditions [Pl]", "Living conditions [Pl]"))) %>% 
    group_by(AreaName) %>% 
    arrange(ind, .by_group = T) %>% 
    bind_cols(df2) %>% 
    ungroup() %>% 
    mutate(row_id = row_number()) %>% 
    bind_cols(select(df3, -`Area Name`))
  
  table_df$ind<- gsub(" \\[.*\\]", "", table_df$ind)
  
  return(table_df)
  
}


# Reactable functions -----------------------------------------------------

# For the palette (continuous)
index_pal <- function(x) {
  if(!is.na(x) | !is.nan(x) ){
    rgb(colorRamp(rev(c("#133959", "#206095", "#8fafca", "#bccfdf")))(x), maxColorValue = 255)
  }
  else return("#ffffff")
}


sig_diff_colour <- function(table_df, col, value, index, comparator, mode){
  
  if (mode == "Gradient") {
    
    # Gradient
    district_name <- table_df$AreaName[index]
    
    value_vector <- as.vector(
      stats::na.omit(
        unlist(table_df[, c(3, 13:19)])
      )
    )
    
    normalised <- ((value - min(value_vector, na.rm = T)) + 10**-100) / (max(value_vector, na.rm = T) - min(value_vector, na.rm = T))
    color <- index_pal(normalised)
    
  } else if (mode == "Categorical") {
    
    # Categorical
    district_name <- table_df$AreaName[index]
    
    original <- as.vector(
      stats::na.omit(
        unlist(table_df[, c(3, 13:19)])
      )
    )
    
    comp <- as.vector(
      stats::na.omit(
        unlist(comparator[, c(3, 13:19)])
      )
    )
    
    value_vector <- original - comp
    value <- table_df[[col]][index] - comparator[[col]][index]
    
    if(value == 0 | is.na(value) == T){ #if zero, it's exactly the same -> amber. 
      
      color <- "#FFC9A5"
        
    } else if(value < 0 & abs(value) < 20){ #if negative and smaller than 20 (SD for 95% CI), it means health is not significantly worse 
      
      color <- "#FE781F"
        
    }else if(value < 0 & abs(value) >= 20){ #if negative and bigger or equal to 20 (SD for 95% CI), it means health is significantly worse 
      
      color <- "#D0021B"
        
    }else if(value > 0 & abs(value) < 20){ #if positive and smaller than 20 (SD for 95% CI), it means health is better 
      
      color <- "#8FAFCA"
        
    }else if(value > 0 & abs(value) >= 20){ #if positive and bigger or equal to 20 (SD for 95% CI), it means health is significantly better 
      
      color <- "#206095"
        
    } 
    
  }
  
  
}

# Custom function for rendering colored cells with tooltips

# value = 102.1
# index = 3
# col_name = "row1"

cell_colouring <- function(table_df, value, index, col_name, comparator, mode) {
  
  district_name <- table_df$AreaName[index]
  num <- substr(col_name, 4, nchar(col_name))
  
  if(is.na(value)) { return(NULL) }
  
  if (mode == "Gradient") {
    
    color <- sig_diff_colour(table_df = table_df, 
                             col = paste0("value", num),
                             value = table_df[[paste0("value", num)]][index], 
                             index = index, 
                             comparator = comparator, 
                             mode = mode)
    
    tippy(div(
      style = paste0("cursor: info; white-space: nowrap; overflow: hidden; height: 100%; background: ", color, ";")
    ), 
    tooltip = paste0("<span style='font-size:15px;'>", table_df[index,][[col_name]], ": ", value, "<span>"),
    theme = "light")
    
  } else if (mode == "Categorical") {
    
    var <- table_df[[col_name]][index]
    
    
    color <- sig_diff_colour(table_df = table_df, 
                             col = paste0("value", num),
                             value = table_df[[paste0("value", num)]][index], 
                             index = index, 
                             comparator = comparator, 
                             mode = mode)
    
    comp_a <- comparator$AreaName[index]
    
    #district value
    comp_value <- table_df[[paste0("value", num)]][table_df[[paste0("row", num)]] == var] 
    
    #comparator value
    diff <- comparator[[paste0("value", num)]][table_df[[col_name]] == var] 
    
    # difference
    comp_value2 <- comp_value - diff
    
    tippy(div(
      style = paste0("cursor: info; white-space: nowrap; overflow: hidden; height: 100%; background: ", color, ";")
    ), 
    tooltip = paste0("<span style='font-size:15px;'>", table_df[index,][[col_name]], " in ", district_name, " was <b>", comp_value,  
                     "</b> compared to <b>", diff, "</b> in ", comp_a, "<span>"), #TO SORT OUT COMPARATOR NAME
    
    theme = "light")
    
  }
  
}

# Custom function for making the coloured indicators at the start of each row
bar_style <- function(fill, length = "2.5%", colour = NULL, bg_colour = NULL) {
  image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, length)
  
  list(
    backgroundImage = image,
    backgroundSize = "100% 10%",
    backgroundRepeat = "no-repeat",
    backgroundPosition = "bottom",
    backgroundColor = bg_colour,
    color = colour
  )
}


# Custom function for Health Index row bg 
bar_bg <- function(label, fill) {
  bar <- div(style = list(background = fill, width = "100%", height = "100%"))
  chart <- div(style = list(flexGrow = 1), bar)
  div(style = list(display = "flex", alignItems = "left"), chart, label)
}

# Custom function for presenting the area figure instead of the comparison figure 
figure <- function(table_df, index, col) {
  
  district_name <- table_df$AreaName[index]
  
  table_df %>% 
    filter(ind == table_df$ind[index]) %>% 
    pull(col)
}


# ltla <- "Broxbourne"
# comparator <- "Hertfordshire"
# subdomain <- "Access to green space"

plot_func <- function(ltla, comparator, subdomain){
  
  if(comparator == "No Comparator"){
    
    return(paste0(" "))
    
  }else{
    
  ltla_df <- create_comp_data(data = get_data(), area = ltla) %>% 
    filter(ind %in% subdomain)
  
  utla_df <-  create_comp_data(data = get_data(), area = comparator) %>% 
    filter(ind %in% subdomain)
  
  combined_df <- rbind(ltla_df, utla_df) %>% 
    discard(~all(is.na(.) | . == " " |  . == "")) %>% 
    select(-c(row_id, order_ind, index_value)) 
  
  n_indicators <- 1:ncol(combined_df %>% select(starts_with("row")))
  
  for(i in 1:length(n_indicators)){
    
    new_cols <- paste0("new", n_indicators[i])
    
    combined_df <- combined_df %>% 
      mutate(!!new_cols  := paste0(get(paste0("row",i)), ";", get(paste0("value",i)))) %>% 
      select(-c(paste0("row",i), paste0("value",i)))
  }
  
  plot_df <- combined_df %>% 
    pivot_longer(cols = starts_with("new"), names_to = "del", values_to = "new") %>% 
    select(-del) %>% 
    mutate(indicator = sub("\\;.*", "", new),
           value = as.numeric(sub(".*\\;", "", new))) %>% 
    select(-new)
  
  graph <- plot_ly(plot_df, x = ~indicator, y = ~value, type = "bar", 
                   legendgroup = ~ AreaName, color = ~AreaName, colors = "Blues") %>% 
    layout(yaxis = list(tickfont = list(size = 12), title = 'Indicator score'),
           xaxis = list(title = ""),
           title = "") %>% 
    config(displayModeBar = FALSE) %>% 
    layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y= 1.09)) 
  
  return(graph)
  }
}


