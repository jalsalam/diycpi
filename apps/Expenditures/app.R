#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(stringr)
library(haven)

# CPI
cpi <- read_tsv("../../BLSdatabases/cu.data.2.summaries") %>%
  separate(series_id, into = c("survey", "seasonal", "x", "area_code", "item_code"), sep = c(2,3,4,8)) %>%
  filter(period == "M13", area_code == "0000", item_code == "SA0") %>%
  mutate(cpi = value / value[max(year)-min(year)+1]) %>%  # took me a long time to get here
  select(year, cpi) 

# Subcategory code & text -- only using "HOUSING"
cx_subcategory <- read_tsv("../../BLSdatabases/cx.subcategory") %>%
  select(category_code, subcategory_code, subcategory_text)

# Breakdown choices
cx_demographics <- read_tsv("../../BLSDatabases/cx.demographics") %>%
  select(demographics_code, demographics_text)
  demographic_choices <- cx_demographics$demographics_code # 14 choices
  names(demographic_choices) <- cx_demographics$demographics_text # named

# Categories of the breakdown choices
cx_characteristics <- read_tsv("../../BLSDatabases/cx.characteristics") %>%
  select(demographics_code, characteristics_code, characteristics_text)  
  c_choices <- cx_characteristics %>%
    filter(demographics_code == "LB01")  # 8 x 3 tibble
  characteristics_choices <- c_choices$characteristics_code  # 8 choices for LB01
  names(characteristics_choices) <- c_choices$characteristics_text # named

# Series  --- has series_title that cx_characteristics does not
cx_series <- read_tsv("../../BLSdatabases/cx.series") %>%
  select(item_code, demographics_code, characteristics_code, series_title) %>%
  separate(series_title, into = c("series_text", "demographics_code2", "characteristics_text"), sep = "-", extra = "merge")

# the data
cx_data_1_AllData <- read_tsv("../../BLSdatabases/cx.data.1.AllData") %>%
  mutate(series_id = str_trim(series_id)) %>%
  separate(series_id, into = c("survey", "seasonal", "rest"),
           sep = c(2,3)) %>%
  separate(rest, into = c("item_code", "demographics_code", "characteristics_code", "process_code"),
           sep = c(-8,-4,-2)) %>%
  select(year, item_code, demographics_code, characteristics_code, value)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Survey of Consumer Expenditures"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("item_code_choice",
                    label = "Select expenditure category:",
                    choices = c("Housing" = "HOUSING", "Shelter" = "SHELTER", "Utilities, etc." = "UTILS"),
                    selected = "HOUSING"
                    ),        
        selectInput("display",
                    label = "Select value to display:",
                    choices = c("2016 dollars" = "value_real",
                                "fraction of total expenditures" = "r",
                                "nominal dollars" = "value"),
                    selected = "2016 dollars"
        ),
        selectInput("demographics_code_choice",
                     label = "Select breakdown variable:",
                     choices = demographic_choices,
                     selected = "LB01"
                     ),
         selectInput("characteristics_codes_choice",
                    label = "Too cluttered? Select specific characteristics:",
                    choices = characteristics_choices,
                    selected = characteristics_choices,  # momentarily all choices are selected and then they disappear
                    multiple = TRUE
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot1"),
         h6("Characteristics codes choosen: "),
         textOutput("text1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, clientData, session) {
   
  observeEvent(input$demographics_code_choice, {
    c_choices <- cx_characteristics %>%
      filter(demographics_code == input$demographics_code_choice)
    characteristics_choices <- c_choices$characteristics_code
    names(characteristics_choices) <- c_choices$characteristics_text
    
    updateSelectInput(session, "characteristics_codes_choice", choices = characteristics_choices)
  })
  
  subset_of_characteristics <- characteristics_choices  # c("01", "02", "03")
  
  observeEvent(input$characteristics_codes_choice, {
    subset_of_characteristics <- input$characteristics_codes_choice
  })
  
  
  data <- reactive({
    
    data <- cx_data_1_AllData %>%
      filter(item_code == input$item_code_choice | item_code == "TOTALEXP") %>%    
      spread(key = item_code, value = value) %>%
      mutate(item_code = input$item_code_choice) %>%   
      rename_(value = input$item_code_choice) %>%    # underscore version necessary 
      filter(demographics_code == input$demographics_code_choice) %>%
      # filter(characteristics_code %in% characteristics_choices) %>%
      left_join(cx_series, by = c("item_code", "demographics_code", "characteristics_code")) %>%
      left_join(cx_demographics, by = c("demographics_code"))  %>%
      mutate(r = value/TOTALEXP) %>%
      left_join(cpi, by = "year") %>%
      mutate(value_real = value / cpi) %>%
      group_by(characteristics_code) 
    
  })
  
  output$plot1 <- renderPlot ({
    
    y_title <- switch(input$display, 
                      "value" = "nominal dollars", 
                      "value_real" = "2016 dollars", 
                      "r" = "Fraction of total expenditures")
    
    data() %>%
      ggplot(aes_(~year, as.name(input$display))) +    
        geom_line(aes(color = characteristics_text)) +
        ylim(0,NA) +
        labs( title = paste(data()$series_text[1],"expenditures"), 
              caption = "Source: BLS.gov",
              x = "Year", y = y_title) +    
              scale_color_discrete(data()$demographics_text[1])
  })

  output$text1 <- renderPrint({
    names(characteristics_choices);
    input$charateristics_codes_choice
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

