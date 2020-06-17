library(dplyr)

## Test suite for cleaning temporal data

#######################################
# Set path and global variables
#######################################

setwd("~/Documents/covid_nyc_map")
source('./src/data_paths.R')

tests <- read.csv(paste(nyc_data, 'tests.csv', sep = ''),
                  stringsAsFactors = F)

tests <- tests %>% 
  mutate(Date = as.Date(DATE, format='%m/%d/%Y'),
         `Percent positive tests` = as.integer(PERCENT_POSITIVE*100),
         `Total tests` = as.double(TOTAL_TESTS),
         `Positive tests` = as.double(POSITIVE_TESTS)) %>% 
  select(-c(PERCENT_POSITIVE_3DAYS_AGG, POSITIVE_TESTS,
            TOTAL_TESTS, PERCENT_POSITIVE, DATE))

deaths <- read.csv(paste(nyc_data, 'deaths/probable-confirmed-dod.csv', sep = ''),
                   stringsAsFactors = F)
deaths <- deaths %>% 
  mutate(Date = as.Date(DATE_OF_DEATH, format='%m/%d/%Y'),
         `Total deaths` = as.double(CONFIRMED_COUNT + PROBABLE_COUNT),
         `Probable deaths` = as.double(PROBABLE_COUNT),
         `Confirmed deaths` = as.double(CONFIRMED_COUNT)) %>% 
  select(-c(DATE_OF_DEATH, CONFIRMED_COUNT, PROBABLE_COUNT))

daily <- dplyr::left_join(tests, deaths, by = "Date")

library(data.table)
tests_melted <- melt(data.table(daily %>% 
                                  mutate(`Percent positive tests` = as.character(`Percent positive tests`),
                                         `Percent positive tests` = paste(`Percent positive tests`, 
                                                                          '%', sep = '')) %>% 
                                  select(Date, `Total tests`, `Positive tests`,
                                         `Percent positive tests`, `Total deaths`,
                                         `Confirmed deaths`, `Probable deaths`)), 
                     id.vars = c('Date', 'Percent positive tests', 'Total deaths'))




