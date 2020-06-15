# Map COVID-19 outcomes by zip-code-level socioeconomic/demographic Census data

## Files and Folders
### app.R
Source code to run a Shiny web application demonstrating COVID's impact in NYC over space and time.

### coronavirus-data/
NYC COVID-19 data (see "1. Download NYC COVID-19 data." for details)

### data/
Cleaned COVID-19 data and input/helper files for formatting variable names and getting Census data (see "2. Get American Community Survey Census data" and "3. Prepare the master dataset" for details)

### data_over_time/
Note: this folder and associated scripts (`dev_suite/git_processor.py` and `dev_suite/clean_all_time_space_data.R`) are still being tested. 
Cumulative geospatial data by date of update, aggregated by walking through the commits made to the COVID-19 NYC data repo. Allows for visualizing how geospatial COVID disparities have evolved over time.

### dev_suite/
Scripts that allow for quick (i.e., outside of the app) visualizations of data, including incremental development/testing of various app functionalities. The functions that pass testing here are copied into `src/backend.R` for use in the Shiny app.

This folder also includes files for app development and testing (`dev_suite/app.R` and supporting scripts that contains `dev` in the name) that are not published.

### src/
The main scripts for generating app inputs and supporting app functionality. See below for details.

## Downloading, aggregating, and cleaning data

### 1. Download NYC COVID-19 data. 
To do this, open a command line window and navigate to the project directory. When you're in this directory, type `git clone https://github.com/nychealth/coronavirus-data`. This will create a new directory that can be easily be updated whenever NYC uploads new data. To update your local repo, simply open command line in this new directory and `git pull`.

### 2. Get American Community Survey Census data
You will need to request an API key from the Census bureau to do this (see `https://api.census.gov/data/key_signup.html`). Once you have a key (and set it in `src/census_key.R`--you only need to run this once), the function `acs_wrapper()` in `src/get_merge_census_cov.R` will retrieve the Census variables specified in `data/census_variables.csv`. This function will save a dataset in `.csv` format that contains all the variables you pulled.

### 3. Prepare the master dataset
Also in `src/get_merge_census_cov.R`, you will merge the Census data with COVID-19 data by ZIP code tabulation area. After merging, you will use the choroplethrZip package (see installation instructions here: `https://github.com/arilamstein/choroplethrZip`) to associate ZIP codes with the geographic information necessary to create beautiful choropleth (density) maps. This process is completed by calling the function `generate_choro_df()`.

### 4. Examine plots and visualizations
The file `dev_suite/plot_spatial_data.R` allows for quick visualizations of Census and COVID-19 data. To view plots over time, check out `dev_suite/plot_temporal_data.R`



