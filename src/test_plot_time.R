library(dplyr)
library(ggplot2)
library(plotly)


setwd("~/Documents/covid_nyc_map")
source('./src/data_paths.R')

tests <- read.csv(paste(nyc_data, 'tests.csv', sep = ''),
                  stringsAsFactors = F)

tests <- tests %>% 
  mutate(DATE = as.Date(DATE, format='%m/%d/%Y'),
         day = strftime(DATE,'%A')) 

build_time_plot <- function(df, var_label, tooltip_var){
  temp = ggplot(data = df, aes(x = Date, y = .data[[var_label]],
                               label = .data[[tooltip_var]]))+
    geom_bar(stat = 'identity', fill = 'turquoise', color = 'white')+
    labs(title= paste(var_label, 'over time'),
         x ="Date", y = var_label)+
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          text = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.title = element_blank())
  return(temp)
}




new_names = c("Date", "Total tests", "Positive tests",
              "Percent positive tests", "Percent Positive 3 days ago",
              "Day")
pretty_test_names = colnames(tests)
names(pretty_test_names) = new_names
pretty_tests = tests %>% 
  mutate(PERCENT_POSITIVE = PERCENT_POSITIVE * 100,
         PERCENT_POSITIVE_3DAYS_AGG = PERCENT_POSITIVE_3DAYS_AGG * 100) %>% 
  rename(all_of(pretty_names))

library(data.table)
tests_melted <- melt(data.table(pretty_tests %>% 
                                  mutate(`Total tests` = as.double(`Total tests`),
                                          `Positive tests` = as.double(`Positive tests`),
                                         `Percent positive tests` = as.character(`Percent positive tests`),
                                         `Percent positive tests` = paste(`Percent positive tests`, 
                                                                          '%', sep = '')) %>% 
                                  select(Date, `Total tests`, `Positive tests`,
                                         `Percent positive tests`)), 
                     id.vars = c('Date', 'Percent positive tests'))

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


