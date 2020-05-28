library(dplyr)
library(stringr)
source('./src/data_paths.R')

choro_inputs = read.csv(paste(new_data, 'choropleth_inputs.csv', sep = ''),
                        stringsAsFactors = FALSE)
choro_inputs$ZCTA = as.character(choro_inputs$ZCTA)
df = choro_inputs %>% 
  distinct(ZCTA, .keep_all = TRUE)

#Prep formatting dataframe 
pretty_columns = read.csv(paste(new_data, 'pretty_column_names.csv', sep = ''),
                          stringsAsFactors = FALSE)
pretty_columns = pretty_columns %>% 
  filter(!is.na(split_word))
pretty_columns$l2[pretty_columns$l2 == ''] = ' '
pretty_columns = pretty_columns %>% 
  mutate(formatted_name = paste(l1, l2, sep = "\n"),
         formatted_name = str_trim(formatted_name, side = 'both'),
         format_1l = paste(l1, l2, sep = ' '),
         format_1l = str_trim(format_1l, side = 'both'))
plot_names = as.vector(pretty_columns$original_name)
names(plot_names) = as.vector(pretty_columns$format_1l)
rev_names = as.vector(pretty_columns$format_1l)
names(rev_names) = as.vector(pretty_columns$original_name)

pretty_choro_inputs = choro_inputs %>% 
  rename(all_of(plot_names))
