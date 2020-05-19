library(shiny)
library(dplyr)
library(choroplethrZip)
library(choroplethr)
library(ggplot2)
library(stats)
library(snakecase)

setwd('~/Documents/covid_nyc_map')

df <- read.csv('data/covid_data_w_census.csv', stringsAsFactors = FALSE)
df$ZCTA <- as.character(df$ZCTA)

vars = read.csv('data/census_variables.csv', stringsAsFactors = FALSE)
vars = vars %>% 
  filter(is.na(pretty_label)==FALSE & pretty_label != '')

nyc_fips = c('36005', '36047', '36061', '36081', '36085')


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("COVID-19 in NYC: Comparisons with Census Socioeconomic Data"),
  br(),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      h4(selectInput(inputId = "outcome_var",
                    label = "COVID-19 outcome:",
                    c('Percent Positive COVID Tests' = 'PERCENT_POSITIVE',
                      "Case rate per 100,000" = 'COVID_CASE_RATE',
                      'Death rate per 100,000' = 'COVID_DEATH_RATE'),
                  selected = 'Percent Positive')),
      h4(selectInput(inputId = 'demo_var',
                  label = "Census demographic variable:",
                  c("Median income" = 'median_income',
                    'Percent White' = 'percent_white',
                    'Percent Black' = 'percent_black',
                    'Percent Hispanic' = 'percent_hispanic_latino',
                    'Percent uninsured' = 'percent_uninsured',
                    'Percent receiving public assistance' = 'percent_receiving_public_assistance',
                    'High school completion rate' = 'high_school_completion',
                    'Poverty rate' = 'poverty_rate'),
                  selected = 'Median income')),
      checkboxInput(inputId = 'show_by_borough',
                    label = h4('Show color coding for boroughs?'),
                    value = TRUE),
      checkboxInput(inputId = 'show_reg_line',
                       label = h4('Show linear relationship between variables?'),
                       value = TRUE),
      br(),
      h6('Census data are sourced from the American Community Survey 2013-2018: https://www.census.gov/programs-surveys/acs',
         align = 'center'),
      h6('COVID-19 data are sourced from the NYC Health Dept.: https://github.com/nychealth/coronavirus-data',
         align = 'center')
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "correlationPlot"),
      hr(),
      br(),
      fluidRow(
        column(6,
               h4(textOutput(outputId = "demo_pretty")),
               plotOutput(outputId = "censusPlot")
        ),
        column(6,
            h4(textOutput(outputId = "cov_pretty")),
            plotOutput(outputId = "covPlot")
        )
      ),
      
      br(),
      h5('All data are represented at the ZIP-code level', align = 'center'),
      br(),
      br()
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  get_corr_vars <- reactive({
    vars = df %>% 
      select(c(input$demo_var, input$outcome_var,
               Borough))
    colnames(vars) = c('x', 'y', 'Borough')
    vars
  })
  
  get_pretty_labels <- reactive({
    if(grepl('CASE', input$outcome_var)){
      lab2 = to_sentence_case(input$outcome_var)
      lab2 = paste(lab2, '\nper 100,000', sep = '')
    }else if(grepl('PERCENT', input$outcome_var)){
      lab2 = "Percent Positive\nCovid Tests"
    }else{
      lab2 = to_sentence_case(input$outcome_var)
    }
    labs = c(to_sentence_case(input$demo_var),
             lab2)
    labs
  })
  
  get_choro_df <- reactive({
    temp = df %>% 
      select(c(input$demo_var, input$outcome_var,
               ZCTA)) 
    temp
  })
  
  output$demo_pretty <- renderText({
    pretty = get_pretty_labels()
    paste(pretty[1])
  })
  
  output$cov_pretty <- renderText({
    pretty = get_pretty_labels()
    paste(pretty[2])
  })
  
  output$correlationPlot <- renderPlot({
    temp = get_corr_vars()
    plot_labels = get_pretty_labels()
    if(input$show_by_borough==TRUE){
      if(input$show_reg_line==TRUE){
        p = ggplot(temp, aes(x, y))+
          geom_point(aes(color=Borough)) +
          geom_smooth(method = 'lm', se = FALSE, formula = y ~ x) +
          labs(x = plot_labels[1], y = plot_labels[2])+
          theme(text = element_text(size = 20))
      }else{
        p = ggplot(temp, aes(x, y, color=Borough))+
          geom_point() +
          labs(x = plot_labels[1], y = plot_labels[2])+
          theme(text = element_text(size = 20))
      }
      
    }else{
      p = ggplot(temp, aes(x, y))+
        geom_point() +
        labs(x = plot_labels[1], y = plot_labels[2]) +
        theme(text = element_text(size = 20))
      if(input$show_reg_line){
        p = p + geom_smooth(method="lm", se=FALSE, formula = y ~ x)
      }
    }
    
    p
  })
  
  output$censusPlot <- renderPlot({
    temp = get_choro_df()
    temp_acs = temp %>%
      select(value = input$demo_var,
             region = ZCTA)
    choro = choroplethrZip::ZipChoropleth$new(temp_acs)
    choro$set_zoom_zip(state_zoom = 'new york',county_zoom=nyc_fips, msa_zoom=NULL, 
                       zip_zoom=NULL)
    labs = get_pretty_labels()
    choro$title = ''
    choro$prepare_map()
    choro$ggplot_scale = scale_fill_brewer(name=labs[1], palette=2, drop=TRUE)
    
    choro$render()
  })
  
  output$covPlot <- renderPlot({
    temp = get_choro_df()
    temp_cov = temp %>%
      select(value = input$outcome_var,
             region = ZCTA)
    choro = choroplethrZip::ZipChoropleth$new(temp_cov)
    choro$set_zoom_zip(state_zoom = 'new york',county_zoom=nyc_fips, msa_zoom=NULL, 
                       zip_zoom=NULL)
    labs = get_pretty_labels()
    choro$title = ''
    choro$prepare_map()
    choro$ggplot_scale = scale_fill_brewer(name=labs[2], palette=2, drop=TRUE)
    
    choro$render()
  })
  
}

shinyApp(ui, server)