library(dplyr)
library(ggplot2)
library(plotly)
library(stats)

setwd("~/Documents/covid_nyc_map")
source('./data_paths.R')

acs <- read.csv(paste(new_data, 'acs_data_nyc.csv', sep = ''), 
                    stringsAsFactors = F)
acs$ZCTA <- as.character(acs$ZCTA)
acs <- acs %>% 
  select(-c(X)) %>% 
  mutate(percent_non_white = 100 - percent_white)

all_time <- read.csv(paste(new_data, 'all_time_covid_data.csv', sep = ''),
                     stringsAsFactors = F)
all_time <- all_time %>% 
  filter(!is.na(MODIFIED_ZCTA) & MODIFIED_ZCTA != '') %>% 
  mutate(ZCTA = as.character(MODIFIED_ZCTA),
         COVID_TEST_RATE = (COVID_TEST_COUNT/POP_DENOMINATOR)*100000) %>% 
  select(-c(MODIFIED_ZCTA, MODZCTA, Positive, Total))

merged_all <- dplyr::left_join(all_time, acs, by = 'ZCTA')

assign_quantile_labels <- function(acs, var, quant_lev = 0.25){
  
  quants = quantile(acs[[var]], probs = seq(0, 1, quant_lev))
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

income_quants <- quantile(acs$median_income, probs = seq(0, 1, 0.2))
income_labs <- assign_quantile_labels(acs, 'median_income', quant_lev = 0.2)

percent_black_quants <- quantile(acs$percent_black, probs = seq(0, 1, 0.25))
percent_black_labs <- assign_quantile_labels(acs, 'percent_black', quant_lev = 0.25)

non_white_quants <- quantile(acs$percent_non_white, probs = seq(0, 1, 0.2))
non_white_labs <- assign_quantile_labels(acs, 'percent_non_white', quant_lev = 0.2)
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
      percent_black <= percent_black_quants[1] ~ percent_black_labs[1],
      percent_black > percent_black_quants[1] & percent_black <= percent_black_quants[2] ~ percent_black_labs[2],
      percent_black > percent_black_quants[2] & percent_black <= percent_black_quants[3] ~ percent_black_labs[3],
      percent_black > percent_black_quants[3] ~ percent_black_labs[4]
    ),
    black_residents = as.factor(black_residents),
    percent_not_white = case_when(
      percent_non_white <= non_white_quants[1] ~ non_white_labs[1],
      percent_non_white > non_white_quants[1] & percent_non_white <= non_white_quants[2] ~ non_white_labs[2],
      percent_non_white > non_white_quants[2] & percent_non_white <= non_white_quants[3] ~ non_white_labs[3],
      percent_non_white > non_white_quants[3] & percent_non_white <= non_white_quants[4] ~ non_white_labs[4],
      percent_non_white > non_white_quants[4] ~ non_white_labs[5]
    ),
    income_category = case_when(
      median_income <= income_quants[1] ~ income_labs[1],
      median_income > income_quants[1] & median_income <= income_quants[2] ~ income_labs[2],
      median_income > income_quants[2] & median_income <= income_quants[3] ~ income_labs[3],
      median_income > income_quants[3] & median_income <= income_quants[4] ~ income_labs[4],
      median_income > income_quants[4] ~ income_labs[5]
    )) %>% 
  mutate(commit_date = as.Date(commit_date),
         Date = as.Date(actual_date))

mean_by_poverty <- merged_cats %>% 
  group_by(Date, poverty_category) %>% 
  summarise_at(vars('PERCENT_POSITIVE', 'COVID_DEATH_RATE',
                    'COVID_CASE_RATE'), mean)
  
ggplot(as.data.frame(mean_by_poverty), 
       aes(Date, PERCENT_POSITIVE, color = poverty_category))+
  geom_point()

ggplot(as.data.frame(mean_by_poverty %>% filter(!is.na(COVID_DEATH_RATE))), 
       aes(Date, COVID_DEATH_RATE, color = poverty_category))+
  geom_point()

ggplot(as.data.frame(mean_by_poverty %>% filter(!is.na(COVID_CASE_RATE))), 
       aes(Date, COVID_CASE_RATE, color = poverty_category))+
  geom_point()

mean_by_race <- merged_cats %>% 
  group_by(Date, black_residents) %>% 
  summarise_at(vars('PERCENT_POSITIVE', 'COVID_DEATH_RATE',
                    'COVID_CASE_RATE'), mean)

p = ggplot(as.data.frame(mean_by_race %>% filter(!is.na(COVID_CASE_RATE))), 
       aes(Date, COVID_CASE_RATE, color = black_residents))+
  geom_point()
ggplotly(p)

p = ggplot(as.data.frame(mean_by_race %>% filter(!is.na(COVID_DEATH_RATE))), 
           aes(Date, COVID_DEATH_RATE, color = black_residents))+
  geom_point()
ggplotly(p)

mean_by_inc <- merged_cats %>% 
  group_by(Date, income_category) %>% 
  summarise_at(vars('PERCENT_POSITIVE', 'COVID_DEATH_RATE',
                    'COVID_CASE_RATE'), mean)

p = ggplot(as.data.frame(mean_by_inc %>% filter(!is.na(COVID_CASE_RATE))), 
           aes(Date, COVID_CASE_RATE, color = income_category))+
  geom_point()
ggplotly(p)

p = ggplot(as.data.frame(mean_by_inc %>% filter(!is.na(COVID_DEATH_RATE))), 
           aes(Date, COVID_DEATH_RATE, color = income_category))+
  geom_point()
ggplotly(p)

mean_by_percent_white <- merged_cats %>% 
  group_by(Date, percent_not_white) %>% 
  summarise_at(vars('PERCENT_POSITIVE', 'COVID_DEATH_RATE',
                    'COVID_CASE_RATE'), mean)

p = ggplot(as.data.frame(mean_by_percent_white %>% filter(!is.na(COVID_DEATH_RATE))), 
           aes(Date, COVID_DEATH_RATE, color = percent_not_white))+
  geom_point()
ggplotly(p)




  
  
  

