# Map COVID-19 outcomes against zip-code-level socioeconomic/demographic data

1. Get American Community Survey (ACS) Census data. You will need to request an API key from the Census bureau to do this. Once you have a key, the function `acs_wrapper()` in `get_census.R` will retrieve the Census variables specified in `data/census_variables.csv`.
2. Merge Census data with COVID data from the NYC Department of Health (https://github.com/nychealth/coronavirus-data)
3. Test geospatial plotting in `get_census.R`.
4. Edit the shiny web app (in `app.R`), if desired. Currently, the web app shows the linear relationships between Census variables and COVID data, and plots zip-code-level choropleths of associated data below.
