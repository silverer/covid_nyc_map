library(dplyr)
library(ggplot2)
library(plotly)
library(stats)
library(gtools)

setwd("~/Documents/covid_nyc_map")
source('./data_paths.R')

acs <- read.csv(paste(new_data, 'acs_data_nyc.csv', sep = ''), 
                    stringsAsFactors = F)
acs$ZCTA <- as.character(acs$ZCTA)
acs <- acs %>% 
  select(-c(X)) %>% 
  mutate(percent_non_white = 100 - percent_white)

all_time <- read.csv(paste(all_time_data, 'all_time_covid_data.csv', sep = ''),
                     stringsAsFactors = F)
all_time <- all_time %>% 
  filter(!is.na(MODIFIED_ZCTA) & MODIFIED_ZCTA != '') %>% 
  mutate(ZCTA = as.character(MODIFIED_ZCTA),
         COVID_TEST_RATE = (COVID_TEST_COUNT/POP_DENOMINATOR)*100000) %>% 
  select(-c(MODIFIED_ZCTA, MODZCTA, Positive, Total))

merged_all <- dplyr::left_join(all_time, acs, by = 'ZCTA')

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
    poverty_category = factor(poverty_category, levels = rev(c('Low', 'Medium',
                                                           'High', 'Very high'))),
    
    percent_black = quantcut(percent_black),
    percent_black = factor(percent_black, levels = rev(levels(percent_black))),
    
    percent_non_white = quantcut(percent_non_white, q = 5),
    percent_non_white = factor(percent_non_white, 
                               levels = rev(levels(percent_non_white))),
    
    median_income = quantcut(median_income, q = 5),
    median_income = factor(median_income, 
                           levels = rev(levels(median_income))),
    
    percent_hispanic = quantcut(percent_hispanic_latino, q = 4),
    percent_hispanic = factor(percent_hispanic, levels = rev(levels(percent_hispanic))),
    
    public_assistance = quantcut(percent_receiving_public_assistance, q = 5),
    public_assistance = factor(public_assistance, 
                               levels = rev(levels(public_assistance)))) %>% 
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
  group_by(Date, percent_black) %>% 
  summarise_at(vars('PERCENT_POSITIVE', 'COVID_DEATH_RATE',
                    'COVID_CASE_RATE'), mean)

p = ggplot(as.data.frame(mean_by_race %>% filter(!is.na(COVID_CASE_RATE))), 
       aes(Date, COVID_CASE_RATE, color = percent_black))+
  geom_point()
ggplotly(p)

p = ggplot(as.data.frame(mean_by_race %>% filter(!is.na(COVID_DEATH_RATE))), 
           aes(Date, COVID_DEATH_RATE, color = percent_black))+
  geom_point()
ggplotly(p)

mean_by_inc <- merged_cats %>% 
  group_by(Date, income_category) %>% 
  summarise_at(vars('PERCENT_POSITIVE', 'COVID_DEATH_RATE',
                    'COVID_CASE_RATE', 'COVID_TEST_RATE'), mean)

p = ggplot(as.data.frame(mean_by_inc %>% filter(!is.na(COVID_TEST_RATE))), 
           aes(Date, COVID_TEST_RATE, color = income_category))+
  geom_point()
ggplotly(p)

p = ggplot(as.data.frame(mean_by_inc %>% filter(!is.na(COVID_DEATH_RATE))), 
           aes(Date, COVID_DEATH_RATE, color = income_category))+
  geom_point()
ggplotly(p)

mean_by_percent_nonwhite <- merged_cats %>% 
  group_by(Date, percent_non_white) %>% 
  summarise_at(vars('PERCENT_POSITIVE', 'COVID_DEATH_RATE',
                    'COVID_CASE_RATE'), mean)

p = ggplot(as.data.frame(mean_by_percent_nonwhite %>% filter(!is.na(COVID_DEATH_RATE))), 
           aes(Date, COVID_DEATH_RATE, color = percent_non_white))+
  geom_point()
ggplotly(p)




  
  
  

