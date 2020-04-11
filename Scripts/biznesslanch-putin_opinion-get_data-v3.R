## File:        biznesslanch-putin_opinion-get_data.R
## Version:     3
## Description: Pulls most recent data on Putin's approval rating from Levada, FOM and VTsIOM. 

## Set up -----------------
Sys.setlocale("LC_CTYPE", "Russian")
Sys.setlocale("LC_TIME", "Russian")

library(tidyverse)
library(stringr)
library(rvest)
library(lubridate)
library(jsonlite)

## set defaults 
ru_months <- c("января","февраля","марта","апреля","мая","июня","июля","августа","сентября","октября","ноября","декабря")
ru_abbrev <- c("Янв","Фев","Мар","Апр","Май","Июнь","Июль","Авг","Сент","Окт","Ноя","Дек")

setwd(gitdatadir)
### Update Levada Data -----------------------
get_levadadata <- function(url="https://www.levada.ru/indikatory/") {
  # get webpage status - TRUE means there's an http error. This stops the function. If False, then it pulls the data
  url_down <- httr::http_error(url) 
  if(url_down=="TRUE") {
    NULL
  }
  else{
    lvurl <- url
    lvpg <- read_html(lvurl)
    lvtab <- html_table(lvpg, fill=TRUE)
    
    levada_new <- lvtab[[2]]
    
    levada_new <- levada_new %>% rownames_to_column() %>%
      pivot_longer(-rowname) %>% pivot_wider(names_from = rowname, values_from = value) %>% 
      select(-name) %>% 
      rename(Date=1, Approve=2, Disapprove=3, No.Answer=4) %>%
      mutate(source = "Levada") 
    
    # reformulate dates  
    levada_new <- levada_new %>% 
      mutate(Date = as.character(Date)) %>%
      mutate(Date = paste0(Date, ".15")) %>%
      mutate(Date = str_replace_all(Date, "\\.2(?=\\.)", ".2000")) %>%
      mutate(Date = str_replace_all(Date, "\\.201(?=\\.)", ".2010")) %>%
      mutate(Date = str_replace_all(Date, "\\.202(?=\\.)", ".2020")) %>%
      mutate(Date = myd(Date))
    
    ## Append existing levada data
    levada_main <- read_csv("https://raw.githubusercontent.com/biznesslanch/Russian-Public-Opinion-Tracker/master/Data/putin-approval-levada-1999.csv",
                            col_names = TRUE, 
                            col_types = list(col_date(), col_double(), col_double(), col_double(), col_character()))
    
    ## Check for differences,  join data, and save
    levada_check <- dplyr::setdiff(levada_new, levada_main)
    levada_data <- dplyr::union(levada_main, levada_new)  
    levada_list <- list(levada_data, levada_check)
  }
}

levada_list <- get_levadadata()

if(is.null(levada_list)) {
  levada_down <- TRUE
  levada_data <- read_csv("https://raw.githubusercontent.com/biznesslanch/Russian-Public-Opinion-Tracker/master/Data/putin-approval-levada-1999.csv",
                          col_names = TRUE, 
                          col_types = list(col_date(), col_double(), col_double(), col_double(), col_character()))
} else {
  levada_data <- levada_list[[1]]
  levada_check <- levada_list[[2]]
  
  write.csv(levada_data, file="putin-approval-levada-1999.csv", row.names = FALSE)
  rm(levada_list)
}

### Update VTSiOM data ------------------
get_vtsiomdata <- function(url="https://wciom.ru/news/ratings/odobrenie_deyatelnosti_gosudarstvennyx_institutov/") {
  url_down <- httr::http_error(url)
  if (url_down=="TRUE"){
    NULL
  }
  else{
    vtsiom_url <- url
    vtsiom_pg  <- read_html(vtsiom_url)  
    vtsiom_tab <- html_table(vtsiom_pg)
    
    vtsiom_latest_apprv <- vtsiom_tab[[1]] 
    vtsiom_latest_apprv <- vtsiom_latest_apprv %>% pivot_longer(-Одобрение) %>%
      pivot_wider(names_from = Одобрение, values_from = value) %>%
      select(Approve="Президент России", "Date"=name)
    
    vtsiom_latest_dsprv <- vtsiom_tab[[2]] 
    vtsiom_latest_dsprv <- vtsiom_latest_dsprv %>% pivot_longer(-Неодобрение) %>%
      pivot_wider(names_from = Неодобрение, values_from = value) %>%
      select(Disapprove="Президент России", "Date"=name)
    
    # join approval ratings 
    vtsiom_latest <- right_join(vtsiom_latest_apprv, vtsiom_latest_dsprv, by="Date") 
    # Reformulate dates - this has to be change manually
    vtsiom_latest$Date <- vtsiom_latest$Date %>% mgsub::mgsub(.,c(ru_abbrev), c(month.abb))
    vtsiom_latest <- vtsiom_latest %>% mutate(Date = mdy(Date)) %>% select(Date, Approve, Disapprove) %>% mutate(source = "VTsIOM")
    
    ## Bring in existing dataset
    vtsiom_main <- read_csv("https://raw.githubusercontent.com/biznesslanch/Russian-Public-Opinion-Tracker/master/Data/vciom-presidential-approval-2006.csv",
                            col_names = TRUE, 
                            col_types = list(col_date(), col_double(), col_double(), col_character()))
    
    ## Check for differences,  join data, and save 
    vtsiom_check <- dplyr::setdiff(vtsiom_latest, vtsiom_main)
    vtsiom_data <- dplyr::union(vtsiom_main, vtsiom_latest)
    vtsiom_list <- list(vtsiom_data, vtsiom_check)
  }
}

vtsiom_list <- get_vtsiomdata()

if(is.null(vtsiom_list)) {
  vtsiom_down <- TRUE
  vtsiom_data <- read_csv("https://raw.githubusercontent.com/biznesslanch/Russian-Public-Opinion-Tracker/master/Data/vciom-presidential-approval-2006.csv",
                          col_names = TRUE, 
                          col_types = list(col_date(), col_double(), col_double(), col_character()))
} else {
  vtsiom_data <- vtsiom_list[[1]]
  vtsiom_check <- vtsiom_list[[2]]
  
  write.csv(vtsiom_data, file="vciom-presidential-approval-2006.csv", row.names = FALSE)
  rm(vtsiom_list)
}

### Update FOM data ---------------
get_fomdata <- function(url="https://fom.ru/Politika/10946") {
  url_down <- httr::http_error(url)
  if (url_down=="TRUE"){
    NULL
  }
  else {
    fom_url <- "https://fom.ru/Politika/10946"
    fom_pg  <- read_html(fom_url)  
    fom_tab <- html_table(fom_pg)
    
    # Extract text from scraped javascript highcharts graph  
    fom_text <- fom_pg %>% html_nodes('.tab.tab-v3.tab-active') %>% html_nodes('.tab.tab-v3.tab-active') %>% html_nodes('script')
    test <- html_text(fom_text) %>% stringi::stri_split_lines() %>% flatten_chr()
    test <- test[4]
    # Pull out dates and values using regex and then parse the json 
    variables <- regmatches(test, gregexpr("\\[.+?\\]", test)) 
    hard_to_say <- str_remove(variables[[1]][2], "\\[")
    Dates <- fromJSON(variables[[1]][1])
    hard_to_say <- fromJSON(hard_to_say)
    Approve <- fromJSON(variables[[1]][4])
    Disapprove <- fromJSON(variables[[1]][3])
    # Bind vectors created from json and convert to dates and numerics  
    fom_combined <- cbind(Dates, Approve, Disapprove, hard_to_say)
    fom_latest <- as_tibble(fom_combined) %>% mutate(Date = mgsub::mgsub(Dates,c(ru_months), c(month.name))) %>%
      mutate(Date= dmy(Date),
             source="FOM",
             Approve = as.double(Approve),
             Disapprove = as.double(Disapprove),
             hard_to_say = as.double(hard_to_say)) %>% 
      select(-Dates)
    
    ## bring in existing data
    fom_main <- read_csv("https://raw.githubusercontent.com/biznesslanch/Russian-Public-Opinion-Tracker/master/Data/putin-approval-fom.csv",
                         col_names  = TRUE, 
                         col_types = list(col_date(), col_number(), col_number(), col_number(), col_character())) 
    
    ## Check for differences,  join data, and save
    fom_check <- dplyr::setdiff(fom_latest, fom_main) 
    fom_data  <- dplyr::union(fom_main, fom_latest)
    fom_list  <- list(fom_data, fom_check)
  }
}
fom_list <- get_fomdata()

if(is.null(fom_list)) {
  fom_down <- TRUE
  fom_data <- read_csv("https://raw.githubusercontent.com/biznesslanch/Russian-Public-Opinion-Tracker/master/Data/putin-approval-fom.csv",
                       col_names  = TRUE, 
                       col_types = list(col_date(), col_number(), col_number(), col_number(), col_character())) 
} else {
  fom_data <- fom_list[[1]]
  fom_check <- fom_list[[2]]
  
  write.csv(fom_data, file="putin-approval-fom.csv", row.names = FALSE)
  rm(fom_list)
}
