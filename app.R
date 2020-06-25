library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

source('./src/backend.R')

#################################
# Define UI for app
#################################
ui <- fluidPage(
  
  # App title ----
  titlePanel("COVID-19 in NYC: Comparisons with Census Demographic Data"),
  br(),
  br(),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      h4(selectInput(inputId = "outcome_var",
                     label = "COVID-19 outcome:",
                     c('Percent Positive COVID Tests' = 'PERCENT_POSITIVE',
                       "Case rate per 100,000" = 'COVID_CASE_RATE',
                       'Death rate per 100,000' = 'COVID_DEATH_RATE',
                       'COVID testing rate per 100,000' = 'COVID_TEST_RATE'),
                     selected = 'PERCENT_POSITIVE')),
      h4(selectInput(inputId = 'demo_var',
                     label = "Census demographic variable:",
                     c("Median income" = 'median_income',
                       'Median rent' = 'median_rent',
                       'Percent Black' = 'percent_black',
                       'Percent white' = 'percent_white',
                       'Percent Asian' = 'percent_asian',
                       'Percent Hispanic/Latino' = 'percent_hispanic_latino',
                       'Percent uninsured' = 'percent_uninsured',
                       'Percent with disability' = 'percent_with_disability',
                       'Percent receiving public assistance' = 'percent_receiving_public_assistance',
                       'High school completion rate' = 'high_school_completion',
                       'College completion rate' = 'college_graduates',
                       'Percent working in managment, arts, sciences' = 'percent_in_mgmt_art_sci',
                       'Poverty rate' = 'poverty_rate',
                       'Percent spending >=35% of income on rent' = 'percent_spending_35_percent_rent'),
                     selected = 'median_income')),
      h4('Plot options: '),
      checkboxInput(inputId = 'show_by_borough',
                    label = h4('Show color coding for boroughs?'),
                    value = TRUE),
      checkboxInput(inputId = 'show_reg_line',
                    label = h4('Show linear relationship between variables?'),
                    value = TRUE),
      br(),
      h5(uiOutput("acs_ref"),
         align = 'center'),
      h5(uiOutput("covid_github_ref"),
         align = 'center')
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tags$style(type="text/css", "
                           #loadmessage {
                             position: fixed;
                             top: 200px;
                             left: 500px;
                             width: 50%;
                             padding: 5px 0px 5px 0px;
                             text-align: left;
                             font-weight: bold;
                             font-size: 200%;
                             color: #000000;
                             z-index: 105;
                           }
                          "),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div("Loading...",id="loadmessage")),
      tabsetPanel(type = "tabs",
                  tabPanel("Geospatial Plots",
                           br(),
                           fluidRow(
                             column(6,
                                    h4(textOutput(outputId = "cov_pretty")),
                                    plotlyOutput(outputId = "cov_plot")
                             ),
                             column(6,
                                    h4(textOutput(outputId = "demo_pretty")),
                                    plotlyOutput(outputId = "census_plot")
                             ),
                           ),
                           h5("Note: ZCTA stands for Zip Code Tabulation Area. For more detailed data, hover your mouse over the charts."),
                           hr(),
                           h4(textOutput(outputId = 'correlation_plot_head')),
                           h5(uiOutput("stats_ref")),
                           plotlyOutput(outputId = "correlation_plot"),
                           br(),
                           h5('All data are represented at the ZIP code or modified ZIP code level', 
                              align = 'center')
                  ),
                  tabPanel("Temporal Plots",
                           h4('These plots show COVID-19 testing and mortality data for New York City over time.',
                              align = 'center'),
                           h4('Hover your mouse over the plot to see more detailed data.',
                              align = 'center'),
                           br(),
                           plotlyOutput(outputId = "big_time_test_plot"),
                           br(),
                           plotlyOutput(outputId = "big_time_death_plot"),
                           br(),
                           hr(),
                           br(),
                           fluidRow(
                             column(6,
                                    plotlyOutput(outputId = "perc_pos_time")
                             ),
                             column(6,
                                    plotlyOutput(outputId = "num_tests_time")
                             ),
                           ),
                           br()
                  ),
                  tabPanel("About", 
                           br(),
                           h3("About the Project & Other Resources"),
                           br(),
                           h5(uiOutput('context_text')),
                           tags$style("#context_text{font-size: 20px}"),
                           br(),
                           hr(),
                           h3("About the Developer"),
                           br(),
                           h5(uiOutput('about_me')),
                           tags$style("#about_me{font-size: 20px}"),
                           br()
                  )
      ),
      br(),
      h5('Last updated: 25 June 2020', 
         align = 'center'),
      h5(uiOutput('app_github_ref'),
         align = 'center'),
      br(),
      br()
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  ######################
  # Formatting helpers
  ######################
  get_corr_vars <- reactive({
    vars = df %>% 
      select(c(input$demo_var, input$outcome_var,
               Borough))
    colnames(vars) = c('x', 'y', 'Borough')
    vars
  })
  
  get_pretty_labels <- reactive({
    lab1 = pretty_columns %>% filter(original_name == input$demo_var)
    rownames(lab1) = c(1)
    lab1 = lab1$formatted_name[1]
    lab2 = pretty_columns %>% filter(original_name == input$outcome_var)
    rownames(lab2) = c(1)
    lab2 = lab2$formatted_name[1]
    labs = c(lab1, lab2)
    labs
  })
  
  output$demo_pretty <- renderText({
    pretty = get_pretty_labels()
    paste(pretty[1])
  })
  
  output$cov_pretty <- renderText({
    pretty = get_pretty_labels()
    paste(pretty[2])
  })
  
  ######################
  #Plot outputs
  ######################
  #Tab 1 plots (space)
  output$correlation_plot <- renderPlotly({
    p = build_corr_plot(pretty_choro_inputs, rev_names[input$demo_var],
                        rev_names[input$outcome_var], 
                        show_reg = input$show_reg_line,
                        show_boro = input$show_by_borough)
    ggplotly(p)
  })
  
  
  output$census_plot <- renderPlotly({
    labs = get_pretty_labels()
    p = build_choropleth(pretty_choro_inputs, rev_names[input$demo_var],
                         labs[1])
    ggplotly(p)
    
  })
  
  output$cov_plot <- renderPlotly({
    labs = get_pretty_labels()
    p = build_choropleth(pretty_choro_inputs, rev_names[input$outcome_var],
                         labs[2])
    ggplotly(p)
  })
  
  #Tab 2 plots (time)
  
  output$perc_pos_time <- renderPlotly({
    p = build_time_plot(pretty_tests, 'Percent positive tests', 'Total tests')
    ggplotly(p)
  })
  
  output$num_pos_time <- renderPlotly({
    p = build_time_plot(pretty_tests, 'Positive tests', 'Total tests')
    ggplotly(p)
  })
  
  output$num_tests_time <- renderPlotly({
    p = build_time_plot(pretty_tests, 'Total tests', 'Percent positive tests')
    ggplotly(p)
  })
  
  output$big_time_test_plot <- renderPlotly({
    p = build_combined_time_plot(tests_melted)
    ggplotly(p, tooltip = c("text", "Date", "label"))
  })
  
  output$big_time_death_plot <- renderPlotly({
    p = build_combined_time_plot(tests_melted, plot_type = 'deaths')
    ggplotly(p, tooltip = c("text", "Date", "label"))
  })
  
  ######################
  # Text + UI Outputs
  ######################
  
  #Tab 1 text
  output$correlation_plot_head <- renderText({
    labs = get_pretty_labels()
    paste(sprintf('Correlation Between %s and %s', 
                  labs[2], labs[1]))
  })
  
  stats_url = a("What does this mean?",
                href = "https://statistics.laerd.com/statistical-guides/pearson-correlation-coefficient-statistical-guide.php")
  output$stats_ref <- renderUI({
    temp = df %>% 
      select(-c(ZCTA)) %>% 
      select(c(input$demo_var, input$outcome_var))
    
    corr_vals = get_correlations(temp)
    if(corr_vals[2]<0.001){
      pval = '< 0.001'
    }else{
      pval = sprintf("= %s", round(corr_vals[2], 3))
    }
    new_str = sprintf("Pearson's r = %s, p-value %s. ",
                      as.character(round(corr_vals[1], 2)),
                      pval)
    tagList(new_str, stats_url)
  })
  
  #All tab text
  github_url <- a("on Github", href="https://github.com/silverer/covid_nyc_map/")
  output$app_github_ref <- renderUI({
    tagList("The code for this app is available ", github_url)
  })
  
  acs_url <- a("American Community Survey 2014-2018",
               href="https://www.census.gov/programs-surveys/acs/")
  output$acs_ref <- renderUI({
    tagList("Demographic data are sourced from the US Census ", acs_url)
  })
  covid_url = a("NYC Health Dept",
                href = "https://www.github.com/nychealth/coronavirus-data")
  output$covid_github_ref <- renderUI({
    tagList("COVID-19 data are sourced from the ", covid_url, " and aggregated over all time")
  })
  
  #Tab 3 text
  nyt_url <- a("New York Times",
               href="https://www.nytimes.com/2020/05/18/nyregion/coronavirus-deaths-nyc.html")
  nejm_url <- a("New England Journal of Medicine",
                href = "https://www.nejm.org/doi/full/10.1056/NEJMp2012910")
  harv_paper_url <- a("Dr. Jarvis T. Chen and Dr. Nancy Krieger (2020)",
                      href = "https://cdn1.sph.harvard.edu/wp-content/uploads/sites/1266/2020/04/HCPDS_Volume-19_No_1_20_covid19_RevealingUnequalBurden_HCPDSWorkingPaper_04212020-1.pdf")
  
  hsph_url <- a("Harvard School of Public Health.",
                href = "https://www.hsph.harvard.edu/thegeocodingproject/covid-19-resources/")
  
  output$context_text <- renderUI({
    intro = paste('The purpose of this project is to help visualize disparities in COVID-19 outcomes', 
                  ' by demographic and socioeconomic characteristics of neighborhoods in New York City.',
                  sep = ' ')
    temp_pre_nyt = " These disparities have been reported by the "
    temp_pre_harv = " and a working paper (in other words, not yet reviewed by other scientists) developed by "
    #Chowkwanyun
    temp_pre_nejm = "A recent Perspectives paper in the "
    temp_post_nejm = " authored by Dr. Merlin Chowkwanyun and Dr. Adolph L. Reed, Jr. provides context regarding COVID-19 disparities."
    all_tags = list(intro, tags$br(), tags$br(), 
                    temp_pre_nyt, nyt_url, 
                    temp_pre_harv, harv_paper_url, 
                    " from the ", hsph_url, tags$br(), tags$br(),
                    temp_pre_nejm, nejm_url, temp_post_nejm)
    tagList(all_tags)
  })
  hire_url <- a("Columbia University Medical Center.",
                href = "https://www.genmed.columbia.edu/research/research-centers-and-programs/columbia-outcomes-research-and-decision-analysis-corda-1")
  
  res_gate_url <- a("ResearchGate",
                    href = "https://www.researchgate.net/profile/Elisabeth_Silver2")
  output$about_me <- renderUI({
    intro = paste("Elisabeth Silver developed this application. She graduated from the University of Michigan in 2018 with a Bachelor's of Science,",
                  " and has since been working as a Research Analyst at ",
                  sep = '')
    rg = "To read some of the developer's peer-reviewed research, please visit her profile on "
    contact = "For inquiries, please contact Elisabeth via email: silver.elisabeth@gmail.com"
    all_tags = list(intro, hire_url, tags$br(),
                    rg, res_gate_url, tags$br(), tags$br(),
                    contact, tags$br())
  })
  
}

shinyApp(ui, server)