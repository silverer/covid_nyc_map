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

temp = ggplot(data = tests, aes(x = DATE, y = TOTAL_TESTS, 
                                label = day))+
  geom_bar(stat = 'identity', fill = 'turquoise', color = 'black') +
  theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          text = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.title = element_blank())
  
ggplotly(temp)

library(data.table)
tests_melted <- melt(data.table(tests %>% 
                                  select(DATE, day, TOTAL_TESTS, POSITIVE_TESTS)), 
                     id.vars = c('DATE', 'day'))

temp = ggplot(data = as.data.frame(tests_melted), aes(x = DATE, y = value, 
                                label = day, fill = variable))+
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_discrete(labels = c("Total tests", "Positive tests"))+
  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.title = element_blank())  +
  labs(title='COVID Testing Over Time',
       x ="Date", y = "Frequency")
ggplotly(temp)



# ,
# week = strftime(DATE, format = "%V"),
# day = strftime(DATE,'%A'),
# is_weekend = day == 'Saturday' | day == 'Sunday')


