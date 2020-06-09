library(dplyr)
library(ggplot2)
library(plotly)

setwd("~/Documents/covid_nyc_map")
source('./data_paths.R')

acs_vars <- read.csv(paste(new_data, 'acs_data_nyc.csv', sep = ''), 
                    stringsAsFactors = F)
acs_vars$ZCTA <- as.character(acs_vars$ZCTA)
acs_vars <- acs_vars %>% 
  select(-c(X))

all_time <- read.csv(paste(new_data, 'all_time_covid_data.csv', sep = ''),
                     stringsAsFactors = F)
all_time1 <- read.csv(paste(new_data, 'TESTall_time_covid_data.csv', sep = ''),
                     stringsAsFactors = F)
all_time <- all_time %>% 
  filter(!is.na(MODIFIED_ZCTA) & MODIFIED_ZCTA != '') %>% 
  mutate(ZCTA = as.character(MODIFIED_ZCTA),
         COVID_TEST_RATE = (COVID_TEST_COUNT/POP_DENOMINATOR)*100000) %>% 
  select(-c(MODIFIED_ZCTA, MODZCTA, commit_message, Positive, Total))

merged_all <- dplyr::left_join(all_time, acs_vars, by = 'ZCTA')

assign_quantile_labels <- function(acs, var, quant_lev = 0.25){
  
  quants = quantile(acs_vars[[var]], probs = seq(0, 1, quant_lev))
  
  labs = rep('', length(quants)-1)
  for(i in 1:length(quants)-1){
    first_val = quants[i]
    second_val = quants[i+1]
    l1 = format(round(first_val, 1), nsmall = 1)
    l2 = format(round(second_val, 1), nsmall = 1)
    if(grepl('percent', var)|grepl('rate', var)){
      l1 = paste(l1, '% - ', sep = '')
      l2 = paste(l2, '%', sep = '')
    }else{
      l1 = paste(l1, ' - ', sep = '')
    }
    
    labs[i] = paste(l1, l2, sep = '')
  }
  return(labs)
}

income_quants <- quantile(acs_vars$median_income, probs = seq(0, 1, 0.2))
income_labs <- assign_quantile_labels(acs_vars, 'median_income', quant_lev = 0.2)

city_race_quants <- quantile(acs_vars$percent_black, probs = seq(0, 1, 0.25))
city_race_labs <- assign_quantile_labels(acs_vars, 'percent_black', quant_lev = 0.25)
# Poverty categories:
# - Low: <10% of residents in ZCTA living below the FPT  
# - Medium: 10% to <20%  
# - High: 20% to <30%   
# - Very high: ≥30% residents living below the FPT
merged_cats <- merged_all %>% 
  mutate(
    poverty_category = case_when(
      poverty_rate < 10 ~'Low',
      poverty_rate >= 10 & poverty_rate < 20 ~ 'Medium',
      poverty_rate >= 20 & poverty_rate < 30 ~ 'High',
      poverty_rate >= 30 ~'Very high'
    ),
    poverty_category = as.factor(poverty_category),
    black_residents = case_when(
      percent_black <= city_race_quants[1] ~ city_race_labs[1],
      percent_black > city_race_quants[1] & percent_black <= city_race_quants[2] ~ city_race_labs[2],
      percent_black > city_race_quants[2] & percent_black <= city_race_quants[3] ~ city_race_labs[3],
      percent_black > city_race_quants[3] ~ city_race_labs[4]
    ),
    black_residents = as.factor(black_residents),
    income_category = case_when(
      median_income <= income_quants[1] ~ income_labs[1],
      median_income > income_quants[1] & median_income <= income_quants[2] ~ income_labs[2],
      median_income > income_quants[2] & median_income <= income_quants[3] ~ income_labs[3],
      median_income > income_quants[3] & median_income <= income_quants[4] ~ income_labs[4],
      median_income > income_quants[4] ~ income_labs[5]
    )) %>% 
  mutate(commit_date = as.Date(commit_date))

mean_by_poverty <- merged_cats %>% 
  group_by(commit_date, poverty_category) %>% 
  summarise_at(vars('PERCENT_POSITIVE', 'COVID_DEATH_RATE',
                    'COVID_CASE_RATE'), mean)
  

ggplot(as.data.frame(mean_by_poverty), 
       aes(commit_date, PERCENT_POSITIVE, color = poverty_category))+
  geom_point()

ggplot(as.data.frame(mean_by_poverty %>% filter(!is.na(COVID_DEATH_RATE))), 
       aes(commit_date, COVID_DEATH_RATE, color = poverty_category))+
  geom_point()

ggplot(as.data.frame(mean_by_poverty %>% filter(!is.na(COVID_CASE_RATE))), 
       aes(commit_date, COVID_CASE_RATE, color = poverty_category))+
  geom_point()

mean_by_race <- merged_cats %>% 
  group_by(commit_date, black_residents) %>% 
  summarise_at(vars('PERCENT_POSITIVE', 'COVID_DEATH_RATE',
                    'COVID_CASE_RATE'), mean)

p = ggplot(as.data.frame(mean_by_race %>% filter(!is.na(COVID_CASE_RATE))), 
       aes(commit_date, COVID_CASE_RATE, color = black_residents))+
  geom_point()
ggplotly(p)

mean_by_inc <- merged_cats %>% 
  group_by(commit_date, income_category) %>% 
  summarise_at(vars('PERCENT_POSITIVE', 'COVID_DEATH_RATE',
                    'COVID_CASE_RATE'), mean)

p = ggplot(as.data.frame(mean_by_inc %>% filter(!is.na(COVID_CASE_RATE))), 
           aes(commit_date, COVID_CASE_RATE, color = income_category))+
  geom_point()
ggplotly(p)

