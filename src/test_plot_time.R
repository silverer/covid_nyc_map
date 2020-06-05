library(dplyr)
library(ggplot2)
library(plotly)


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

build_time_plot <- function(df, var_label, tooltip_var){
  temp = ggplot(data = df, aes(x = Date, y = .data[[var_label]],
                               label = .data[[tooltip_var]]))+
    geom_bar(stat = 'identity', fill = 'turquoise', color = 'white')+
    labs(title= paste(var_label, 'over time'),
         x ="Date", y = var_label)+
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          text = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.title = element_blank())
  return(temp)
}

build_combined_time_plot <- function(melted_df, plot_type = 'tests'){
  if(plot_type == 'tests'){
    vars = c('Total tests', 'Positive tests')
    temp_df = melted_df %>% 
      filter(variable %in% vars)
    temp = ggplot(data = as.data.frame(temp_df), aes(x = Date, y = value, 
                                                     label = `Percent positive tests`, fill = variable,
                                                     text = paste(variable, value, sep = ': ')))
    title_text='COVID Testing Over Time'
  }else{
    vars = c('Confirmed deaths', 'Probable deaths')
    temp_df = melted_df %>% 
      filter(variable %in% vars)
    temp_df[is.na(temp_df)] = 0
    temp = ggplot(data = as.data.frame(temp_df), aes(x = Date, y = value, 
                                                     label = `Total deaths`, fill = variable,
                                                     text = paste(variable, value, sep = ': ')))
    title_text = 'COVID Deaths Over Time'
  }
  temp = temp +
    geom_bar(stat = 'identity', position = 'stack')+
    scale_fill_discrete(labels = vars)+
    labs(title=title_text,
         x ="Date", y = "Count")+
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "white"),
          text = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.title = element_blank())
  return(temp)
}

test_plot = build_time_plot(daily, 'Total deaths')


p = build_combined_time_plot(tests_melted)
ggplotly(p, tooltip = c("text", "Date", "label"))

p = build_combined_time_plot(tests_melted, plot_type = 'deaths')
ggplotly(p, tooltip = c("text", "Date", "label"))
temp = ggplot(data = as.data.frame(tests_melted), aes(x = Date, y = value, 
                                label = `Percent positive tests`, fill = variable,
                                text = paste(variable, value, sep = ': ')))+
  geom_bar(stat = 'identity', position = 'stack')+
  scale_fill_discrete(labels = c("Total tests", "Positive tests"))+
  labs(title='COVID Testing Over Time',
       x ="Date", y = "Count")+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.title = element_blank())
  
ggplotly(temp, tooltip = c("text", "Date", "label"))

test_plot = build_time_plot(pretty_tests, 'Percent positive', 'Total tests')
ggplotly(test_plot)

# ,
# week = strftime(DATE, format = "%V"),
# day = strftime(DATE,'%A'),
# is_weekend = day == 'Saturday' | day == 'Sunday')


