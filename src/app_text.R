library(shiny)

#### Data refs ####
github_text <- "The code for this app is available "
github_url <- a("on Github", href="https://github.com/silverer/covid_nyc_map/")
git_ref <- list(github_text, github_url)

demo_text <- "Demographic data are sourced from the US Census "
demo_url <- a("American Community Survey 2014-2018.",
              href="https://www.census.gov/programs-surveys/acs/")

covid_text <- "COVID-19 data are sourced from the "
covid_url <- a("NYC Health Dept.",
              href = "https://www.github.com/nychealth/coronavirus-data")
covid_clar <- " Zip code data are aggregated over all time."

panel_refs <- list(demo_text, demo_url,
                   tags$br(), covid_text, covid_url, 
                   covid_clar)

#### Tab 3 context text ####
intro <- paste('The purpose of this project is to help visualize disparities in COVID-19 outcomes', 
               'by demographic and socioeconomic characteristics of neighborhoods in New York City.',
               sep = ' ')

temp_pre_nyt <- " These disparities have been reported by the "
nyt_url <- a("New York Times",
             href="https://www.nytimes.com/2020/05/18/nyregion/coronavirus-deaths-nyc.html")
temp_pre_harv <- " and a working paper (in other words, not yet reviewed by other scientists) developed by "
harv_paper_url <- a("Dr. Jarvis T. Chen and Dr. Nancy Krieger (2020)",
                    href = "https://cdn1.sph.harvard.edu/wp-content/uploads/sites/1266/2020/04/HCPDS_Volume-19_No_1_20_covid19_RevealingUnequalBurden_HCPDSWorkingPaper_04212020-1.pdf")

hsph_url <- a("Harvard School of Public Health.",
              href = "https://www.hsph.harvard.edu/thegeocodingproject/covid-19-resources/")

temp_pre_nejm <- "A recent Perspectives paper in the "
nejm_url <- a("New England Journal of Medicine",
              href = "https://www.nejm.org/doi/full/10.1056/NEJMp2012910")
temp_post_nejm <- " authored by Dr. Merlin Chowkwanyun and Dr. Adolph L. Reed, Jr. provides context regarding COVID-19 disparities."

ref_tags <- list(intro, tags$br(), tags$br(), 
                temp_pre_nyt, nyt_url, 
                temp_pre_harv, harv_paper_url, 
                " from the ", hsph_url, 
                tags$br(), tags$br(),
                temp_pre_nejm, nejm_url, temp_post_nejm)

#### Tab 3 about me text ####
my_intro <- paste0("Elisabeth Silver developed this application.",
                   " She graduated from the University of Michigan in 2018 with a Bachelor's of Science,",
                   " and worked as a Research Analyst at ")
hire_url <- a("Columbia University Medical Center.",
              href = "https://www.genmed.columbia.edu/research/research-centers-and-programs/columbia-outcomes-research-and-decision-analysis-corda-1")
intro_1 <- paste0("She will be starting a PhD program in Industrial/Organizational Psychology",
                  " at Rice University this Fall.")

res_gate_text <- "To read some of the developer's peer-reviewed research, please visit her profile on "
res_gate_url <- a("ResearchGate",
                  href = "https://www.researchgate.net/profile/Elisabeth_Silver2")

contact <- "For inquiries, please contact Elisabeth via email: silver.elisabeth@gmail.com"
about_me_tags <- list(my_intro, hire_url, intro_1, 
                      tags$br(), tags$br(),
                      res_gate_text, res_gate_url, 
                      tags$br(), tags$br(),
                      contact, tags$br())



