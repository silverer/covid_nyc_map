# Map COVID-19 outcomes by zip-code-level socioeconomic/demographic Census data

### 1. Download NYC COVID-19 data. 
To do this, open a command line window and navigate to the `covid_nyc_map/data` directory. When you're in this directory, type `git clone https://github.com/nychealth/coronavirus-data`. This will create a new directory that can be easily be updated whenever NYC uploads new data. To update your local repo, simply open command line in this new directory and `git pull`.

### 2. Get American Community Survey Census data
You will need to request an API key from the Census bureau to do this (see `https://api.census.gov/data/key_signup.html`). Once you have a key (and set it in `src/census_key.R`--you only need to run this once), the function `acs_wrapper()` in `src/get_merge_census_cov.R` will retrieve the Census variables specified in `data/census_variables.csv`. This function will save a dataset in `.csv` format that contains all the variables you pulled.

### 3. Prepare the master dataset
Also in `src/get_merge_census_cov.R`, you will merge the Census data with COVID-19 data by ZIP code tabulation area. After merging, you will use the choroplethrZip package (see installation instructions here: `https://github.com/arilamstein/choroplethrZip`) to associate ZIP codes with the geographic information necessary to create beautiful choropleth (density) maps. This process is completed by calling the function `generate_choro_df()`.

### 4. Examine plots and visualizations
The file `src/plot_census_cov.R` allows for quick visualizations of Census and COVID-19 data.



