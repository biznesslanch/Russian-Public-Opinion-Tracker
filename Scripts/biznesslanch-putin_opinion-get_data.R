## File:        biznesslanch-putin_opinion-get_data.R
## Version:     1.0
## Description: Pulls most recent data on Putin's approval rating from Levada, FOM and VTsIOM. 

## Set up -----------------
Sys.setlocale("LC_CTYPE", "russian")

library(tidyverse)
library(stringr)
library(rvest)
library(lubridate)

## create objects for Russian months - in correct case for dates used here
ru_months <- c("января","февраля","марта","апреля","мая","июня","июля","августа","сентября","октября","ноября","декабря")
ru_abbrev <- c("Янв","Февр","Март","Апр","Май","Июнь","Июль","Авг","Сент","Окт","Ноя","Дек")


setwd(gitdatadir)
### Update Levada Data -----------------------
lvurl <- "https://www.levada.ru/indikatory/"
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
  mutate(Date = myd(Date))

## Append existing levada data
levada_main <- read_csv("https://raw.githubusercontent.com/biznesslanch/Russian-Public-Opinion-Tracker/master/Data/putin-approval-levada-1999.csv",
                        col_names = TRUE, 
                        col_types = list(col_date(), col_double(), col_double(), col_double(), col_character()))

## check for differences in records
levada_check <- dplyr::setdiff(levada_new, levada_main)

## Append new value to existing dataset
levada_data <- dplyr::union(levada_main, levada_new)

## save scraped data 
write.csv(levada_data, file="putin-approval-levada-1999.csv", row.names = FALSE)

rm(lvtab, lvpg, levada_main, lvurl, levada_new)

### Update VTSiOM data ------------------
vtsiom_url <- "https://wciom.ru/news/ratings/odobrenie_deyatelnosti_gosudarstvennyx_institutov/"
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

## Check for duplicates 
## Check for duplicates 
vtsiom_check <- dplyr::setdiff(vtsiom_latest, vtsiom_main)

## Append and save
vtsiom_data <- dplyr::union(vtsiom_main, vtsiom_latest)

write.csv(vtsiom_data, file="vciom-presidential-approval-2006.csv", row.names = FALSE)

rm(vtsiom_approval,vtsiom_date, vtsiom_main, vtsiom_latest_apprv, vtsiom_latest_dsprv, vtsiom_main, 
   vtsiom_url, vtsiom_pg, vtsiom_tab, vtsiom_disapproval, vtsiom_latest)

### Update FOM data ---------------
fom_url <- "https://fom.ru/Politika/10946"
fom_pg  <- read_html(fom_url)  
fom_tab <- html_table(fom_pg)

## Regex for getting date as formatted in FOM block of text (not robust to other format than dd Month_Name YYYY)
date_reg2 <- "([0-2]?[0-9]|3[01]) (января|февраля|марта|апреля|мая|июня|июля|августа|сентября|октября|ноября|декабря) [0-9]{4}"

## get date
date_text <- fom_pg %>% html_nodes(".chart-remark") %>% html_text 
date_text <- date_text[5]
fom_date <- regmatches(date_text, regexpr(date_reg2, date_text))
# Translate Russian month to English
fom_date <- mgsub::mgsub(fom_date,c(ru_months), c(month.name))

## Get table of latest approval rating
fom_latest <- fom_tab[[6]]
fom_latest <- fom_latest %>% select(X1,X2) %>%
  pivot_longer(-X1) %>% filter(X1!="") %>% 
  pivot_wider(names_from = X1, values_from = value) %>% select(-name) %>%
  rename(Approve = Хорошо, Disapprove = Плохо, hard_to_say = "Затрудняюсь ответить") %>%
  mutate(Date = dmy(fom_date), source = "FOM",
         Approve = as.double(Approve),
         Disapprove = as.double(Disapprove),
         hard_to_say = as.double(hard_to_say),
         source = "FOM")

## bring in existing data
fom_main <- read_csv("https://raw.githubusercontent.com/biznesslanch/Russian-Public-Opinion-Tracker/master/Data/putin-approval-fom.csv",
                     col_names  = TRUE, 
                     col_types = list(col_date(), col_number(), col_number(), col_number(), col_character())) 

## Check for differences and join data
fom_check <- dplyr::setdiff(fom_latest, fom_main) 
fom_data  <- dplyr::union(fom_main, fom_latest)

## Save dataset
write.csv(fom_data, file="putin-approval-fom.csv", row.names = FALSE)

rm(fom_latest, fom_main, fom_tab, fom_pg, fom_url, date_text, fom_date)