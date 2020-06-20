library(dplyr)
library(ggplot2)
library(plotly)
library(stats)
library(gtools)
library(stringr)
library(RColorBrewer)

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
    `Poverty rate` = case_when(
      poverty_rate < 10 ~'Low',
      poverty_rate >= 10 & poverty_rate < 20 ~ 'Medium',
      poverty_rate >= 20 & poverty_rate < 30 ~ 'High',
      poverty_rate >= 30 ~'Very high'
    ),
    `Poverty rate` = factor(`Poverty rate`, levels = rev(c('Low', 'Medium',
                                                           'High', 'Very high'))),
    
    `Percent Black` = quantcut(percent_black), #Generate quartiles and assign as factors
    `Percent Black` = factor(`Percent Black`, 
                           levels = rev(levels(`Percent Black`))),#Reverse levels for readability
    
    `Percent non-white` = quantcut(percent_non_white, q = 4),
    `Percent non-white` = factor(`Percent non-white`, 
                               levels = rev(levels(`Percent non-white`))),
    
    `Income bracket (thousands)` = quantcut(median_income/1000, q = 4),
    `Income bracket (thousands)` = factor(`Income bracket (thousands)`,
                             levels = rev(levels(`Income bracket (thousands)`))),
    
    `Percent Hispanic/Latino` = quantcut(percent_hispanic_latino, q = 4),
    `Percent Hispanic/Latino` = factor(`Percent Hispanic/Latino`, 
                              levels = rev(levels(`Percent Hispanic/Latino`))),
    
    `Percent uninsured` = quantcut(percent_uninsured, q = 4),
    `Percent uninsured` = factor(`Percent uninsured`, 
                                levels = rev(levels(`Percent uninsured`))),
    
    `Percent rec. public assistance` = quantcut(percent_receiving_public_assistance, q = 4),
    `Percent rec. public assistance` = factor(`Percent rec. public assistance`, 
                               levels = rev(levels(`Percent rec. public assistance`)))) %>% 
  
  mutate(commit_date = as.Date(commit_date),
         Date = as.Date(actual_date))


plot_disparities_over_time <- function(merged_df, grp_var,
                                       cov_var = 'Death rate'){
  
  mean_df = merged_df %>% 
    filter(!is.na(.data[[cov_var]])) %>% 
    group_by(Date, .data[[grp_var]]) %>% 
    summarise_at(vars('Death rate', 'Case rate', 'Testing rate'), mean)
  ylabel = str_to_lower(cov_var)
  leg_words = unlist(str_split(grp_var, " "))
  if(length(leg_words)<3){
    leg_title = grp_var
  }else if(length(leg_words)==3){
    leg_title = paste(leg_words[1], ' ', leg_words[2],
                      '\n', leg_words[3], sep = '')
  }else{
    leg_title = paste(leg_words[1], ' ', leg_words[2],
                      '\n', leg_words[3], leg_words[4], 
                      sep = '')
  }
  p = ggplot(as.data.frame(mean_df),
             aes(x = Date, y = .data[[cov_var]], 
                 color = .data[[grp_var]]))+
    geom_point()+
    scale_color_brewer(palette="Spectral",name = leg_title)+
    labs(x = '', y = paste('Cumulative', ylabel))+
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          text = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10))
  return(p)
}

test_plot <- plot_disparities_over_time(merged_cats, 'Income bracket (thousands)')
ggplotly(test_plot)

test_plot <- plot_disparities_over_time(merged_cats, 'Income bracket (thousands)',
                                        cov_var = 'Case rate')
ggplotly(test_plot)

test_plot <- plot_disparities_over_time(merged_cats, 'poverty_category',
                                        cov_var = 'Case rate')
ggplotly(test_plot)

test_plot <- plot_disparities_over_time(merged_cats, 'uninsured_category',
                                        cov_var = 'Case rate')
ggplotly(test_plot)

test_plot <- plot_disparities_over_time(merged_cats, 'uninsured_category',
                                        cov_var = 'Testing rate')
ggplotly(test_plot)

test_plot <- plot_disparities_over_time(merged_cats, 'percent_black',
                                        cov_var = 'Case rate')
ggplotly(test_plot)

test_plot <- plot_disparities_over_time(merged_cats, 'percent_black',
                                        cov_var = 'Death rate')
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

