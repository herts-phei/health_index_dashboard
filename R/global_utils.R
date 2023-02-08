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
