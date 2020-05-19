library(dplyr)
library(tidycensus)
library(ggplot2)
library(sf)

#######################################
# Set path and global variables
#######################################

setwd("~/Documents/explore_covid_nyc")
nyc_data = "data/coronavirus-data/"
geo_data = paste(nyc_data, "Geography-resources/", sep = '')
SAVE_OUTS = FALSE
GET_ACS = FALSE

acs_wrapper <- function(vars){
  acs_data = tidycensus::get_acs(geography = "zip code tabulation area", 
                                 variables = vars,
                                 keep_geo_vars = TRUE,
                                 output = "wide") 
  
  return(acs_data)
}

#######################################
# Read in datasets
#######################################

#Specifies desired ACS variables
vars = read.csv('data/census_variables.csv', stringsAsFactors = FALSE)
vars = vars %>% 
  filter(is.na(pretty_label)==FALSE & pretty_label != '')

#Load COVID data
cov_dat = read.csv(paste(nyc_data,'data-by-modzcta.csv', sep =''), 
                   stringsAsFactors = FALSE)

#Even though modified ZCTA's aren't perfect matches to ZCTA, we'll treat them as if they were
cov_dat = cov_dat %>% 
  filter(is.na(MODIFIED_ZCTA) == FALSE & MODIFIED_ZCTA != '') %>% 
  rename(MODZCTA = MODIFIED_ZCTA) %>% 
  mutate(MODZCTA = as.character(MODZCTA),
         ZCTA = MODZCTA) 


if(GET_ACS == TRUE){
  #Get census data
  all_acs_data = acs_wrapper(as.vector(vars$variable_call))
  var_list = as.vector(vars$variable_call)
  var_list = append(var_list, c('GEOID', 'NAME'))
  
  #Clean census data
  new_names = as.vector(vars$variable_call)
  names(new_names) = as.vector(vars$variable_label)
  acs_vars = all_acs_data %>% 
    select(all_of(var_list)) %>% 
    rename('ZCTA' = GEOID) %>% 
    rename(all_of(new_names)) %>% 
    mutate(crowding = percent_1_1.5_per_room + more_1.5_per_room) %>% 
    select(-c(percent_1_1.5_per_room, more_1.5_per_room, NAME))
  acs_vars = as.data.frame(acs_vars)
  acs_ny_only = acs_vars %>% 
    filter(ZCTA %in% cov_dat$ZCTA)
  if(SAVE_OUTS==TRUE){
    write.csv(acs_ny_only, 'data/acs_data_nyc.csv')
  }
}else{
  acs_vars = read.csv('data/acs_data_nyc.csv', stringsAsFactors = FALSE)
  acs_vars$ZCTA = as.character(acs_vars$ZCTA)
}

#######################################
# Merge Census and Covid data
#######################################

merged = dplyr::left_join(cov_dat, acs_vars, by = 'ZCTA')

#Rename columns
merged = merged %>% 
  rename(Neighborhood = NEIGHBORHOOD_NAME,
         Borough = BOROUGH_GROUP)

if(SAVE_OUTS==TRUE){
  write.csv(merged, 'data/covid_data_w_census.csv')
}




