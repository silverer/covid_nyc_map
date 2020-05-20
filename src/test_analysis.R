library(stats)
library(dplyr)
library(sjPlot)

setwd("~/Documents/explore_covid_nyc")
nyc_data = "data/coronavirus-data/"
geo_data = paste(nyc_data, "Geography-resources/", sep = '')
new_data = 'data/'
figs = 'figures/'
SAVE_FIGS = TRUE
vars = read.csv(paste(new_data, 'census_variables.csv', sep = ''),
                stringsAsFactors = F)
merged = read.csv(paste(new_data, 'covid_data_w_census.csv', sep= ''),
                  stringsAsFactors = F)

merged$ZCTA = as.character(merged$ZCTA)

merged$log_case_rate = log(merged$COVID_CASE_RATE)
tm = glm(COVID_CASE_RATE ~ poverty_rate + percent_public_health_insurance + 
           percent_receiving_public_assistance + percent_uninsured +
           percent_black + crowding + percent_hispanic_latino, data = merged)
summary(tm)
plot_model(tm)



