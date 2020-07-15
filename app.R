library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

source('./src/backend.R')
source('./src/app_text.R')

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
      h5(uiOutput("panel_references"),
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
      h5('Last updated: 15 July 2020', 
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
  
  stats_url <- a("What does this mean?",
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
  
  output$app_github_ref <- renderUI({
    tagList(git_ref)
  })
  
  output$panel_references <- renderUI({
    tagList(panel_refs)
  })
  
  output$context_text <- renderUI({
    tagList(ref_tags)
  })
  
  output$about_me <- renderUI({
    tagList(about_me_tags)
  })
  
}

shinyApp(ui, server)