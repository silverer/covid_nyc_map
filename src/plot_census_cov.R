library(choroplethrZip)
library(choroplethr)
library(dplyr)
library(ggplot2)
library(sf)
library(stats)
library(Hmisc)
library(ggcorrplot)

#######################################
# Set path and global variables
#######################################

setwd("~/Documents/explore_covid_nyc")
nyc_data = "data/coronavirus-data/"
geo_data = paste(nyc_data, "Geography-resources/", sep = '')
new_data = 'data/'
figs = 'figures/'
SAVE_FIGS = TRUE
vars = read.csv(paste(new_data, 'census_variables.csv', sep = ''),
                    stringsAsFactors = F)
merged = read.csv(paste(new_data, 'covid_data_w_census.csv', sep= ''),
                  stringsAsFactors = F)

merged$ZCTA = as.character(merged$ZCTA)

nyc_fips = c('36005', '36047', '36061', '36081', '36085')
temp_acs = merged %>% 
  select(region = ZCTA,
         value = median_income)
#Test zip code choropleths
choro = ZipChoropleth$new(temp_acs)
choro$set_zoom_zip(state_zoom = 'new york',county_zoom=nyc_fips, msa_zoom=NULL, zip_zoom=NULL)
choro$title = "Median income"
choro$prepare_map()
choro$ggplot_scale = scale_fill_brewer(name="Median income", palette=2, drop=TRUE)
choro$render()

temp_cov = merged %>% 
  select(value = PERCENT_POSITIVE,
         region = ZCTA)

choro1 = choroplethrZip::ZipChoropleth$new(temp_cov)
choro1$set_zoom_zip(state_zoom = 'new york',county_zoom=nyc_fips, msa_zoom=NULL, 
                    zip_zoom=NULL)

choro1$title = 'Percent Positive'
choro1$prepare_map()
choro1$ggplot_scale = scale_fill_brewer(name='Percent positive', palette=2, drop=TRUE)
choro1$render()


#Create correlation matrix
keep_covid = c('PERCENT_POSITIVE', 'COVID_CASE_RATE', 'COVID_DEATH_RATE')

vars = vars %>% 
  filter(variable_label %in% as.vector(colnames(merged)))

keep_covid = append(keep_covid, as.vector(vars$variable_label))
keep_covid = append(keep_covid, 'crowding')
corr_df = merged %>% 
  select(all_of(keep_covid)) 

#save the merged data frame
#write.csv(corr_df, 'merged_covid_census_data.csv')
#Get correlation matrix
merged.rcorr = rcorr(as.matrix(corr_df))
merged.coeff = merged.rcorr$r
merged.p = merged.rcorr$P

merged.coeff = as.data.frame(merged.coeff) %>% 
  select(c(PERCENT_POSITIVE))
merged.p = as.data.frame(merged.p) %>% 
  select(c(PERCENT_POSITIVE))
#Make correlation dataframe
all_cors = cbind(merged.coeff, merged.p)
colnames(all_cors) = c('r', 'p')
all_cors['variable'] = rownames(all_cors)
rownames(all_cors) = 1:nrow(all_cors)
#Filter to just significant correlations
#Sort by absolute correlation strength
sig_corrs = all_cors %>% 
  filter(p < 0.05) %>% 
  arrange(desc(abs(r)))
#Add % positive to dataframe
keep_plot = append('PERCENT_POSITIVE', as.vector(sig_corrs$variable))
new_corr_df = corr_df %>% 
  select(all_of(keep_plot))

#create correlation matrix with only significant correlations
new_merged.rcorr = rcorr(as.matrix(new_corr_df))
new_merged.coeff = new_merged.rcorr$r
#plot the new correlation matrix
if(SAVE_FIGS == T){
  png(paste(figs, "cov_correlations.png", sep = ''), width = 13, 
      height = 13, units = "in", res = 250)
  ggcorrplot(new_merged.coeff, type = 'lower')
  dev.off()
}else{
  ggcorrplot(new_merged.coeff, type = 'lower')
}
