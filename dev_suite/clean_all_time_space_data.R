library(dplyr)
library(ggplot2)
library(plotly)
library(stats)
library(gtools)
library(stringr)

setwd("~/Documents/covid_nyc_map")
source('./src/data_paths.R')
source('./dev_suite/clean_temporal_data.R')

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
  select(-c(MODIFIED_ZCTA, MODZCTA, Positive, Total)) %>% 
  rename(`Total tests` = COVID_TEST_COUNT,
         `Percent positive tests` = PERCENT_POSITIVE,
         `Total deaths` = COVID_DEATH_COUNT,
         `Positive tests` = COVID_CASE_COUNT,
         `Death rate` = COVID_DEATH_RATE,
         `Testing rate` = COVID_TEST_RATE,
         `Case rate` = COVID_CASE_RATE)


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
    
    percent_black = quantcut(percent_black), #Generate quartiles and assign as factors
    percent_black = factor(percent_black, 
                           levels = rev(levels(percent_black))),#Reverse levels for readability
    
    percent_non_white = quantcut(percent_non_white, q = 5),
    percent_non_white = factor(percent_non_white, 
                               levels = rev(levels(percent_non_white))),
    
    income_category = quantcut(median_income/1000, q = 4),
    income_category = factor(income_category,
                             levels = rev(levels(income_category))),
    
    percent_hispanic = quantcut(percent_hispanic_latino, q = 4),
    percent_hispanic = factor(percent_hispanic, 
                              levels = rev(levels(percent_hispanic))),
    
    uninsured_category = quantcut(percent_uninsured, q = 4),
    uninsured_category = factor(uninsured_category, 
                                levels = rev(levels(uninsured_category))),
    
    public_assistance = quantcut(percent_receiving_public_assistance, q = 5),
    public_assistance = factor(public_assistance, 
                               levels = rev(levels(public_assistance)))) %>% 
  
  mutate(commit_date = as.Date(commit_date),
         Date = as.Date(actual_date))


plot_disparities_over_time <- function(merged_df, grp_var,
                                       cov_var = 'Death rate'){
  
  mean_df = merged_df %>% 
    filter(!is.na(.data[[cov_var]])) %>% 
    group_by(Date, .data[[grp_var]]) %>% 
    summarise_at(vars('Death rate', 'Case rate', 'Testing rate'), mean)
  
  p = ggplot(as.data.frame(mean_df),
             aes(Date, .data[[cov_var]], color = .data[[grp_var]]))+
    geom_point()
  return(p)
}

test_plot <- plot_disparities_over_time(merged_cats, 'income_category')
ggplotly(test_plot)

test_plot <- plot_disparities_over_time(merged_cats, 'income_category',
                                        cov_var = 'Case rate')
ggplotly(test_plot)

test_plot <- plot_disparities_over_time(merged_cats, 'uninsured_category',
                                        cov_var = 'Case rate')
ggplotly(test_plot)

test_plot <- plot_disparities_over_time(merged_cats, 'percent_black',
                                        cov_var = 'Case rate')
ggplotly(test_plot)

rename_columns <- function(rename_list = NULL){
  pretty_columns = read.csv(paste(new_data, 'pretty_column_names.csv', sep = ''),
                            stringsAsFactors = FALSE)
  pretty_columns = pretty_columns %>% 
    filter(!is.na(split_word))
  pretty_columns$l2[pretty_columns$l2 == ''] <- ' '
  pretty_columns = pretty_columns %>% 
    mutate(formatted_name = paste(l1, l2, sep = "\n"),#build names that need to have a newline
           formatted_name = str_trim(formatted_name, side = 'both'), #remove leading/trailing whitespace
           format_1l = paste(l1, l2, sep = ' '), #build names to go on one line
           format_1l = str_trim(format_1l, side = 'both'))
  
  #plot_names is used to rename the choro_inputs columns
  plot_names = as.vector(pretty_columns$original_name)
  names(plot_names) = as.vector(pretty_columns$format_1l)
  if(!is.null(rename_list)){
    for(i in 1:length(rename_list)){
      plot_names[names(rename_list)[i]] = rename_list[i]
    }
  }
  return(plot_names)
}
  
  
get_daily_counts <- function(count_vec){
  new_vec = rep(NA, length(count_vec))
  for(i in 2:length(count_vec)){
    today = count_vec[i]
    yesterday = count_vec[i-1]
    new_vec[i] = today - yesterday
  }
  return(new_vec)
}

