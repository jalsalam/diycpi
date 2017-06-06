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
library(haven)
library(stringr)

# CPI
cpi <- read_tsv("../../BLSdatabases/cu.data.2.summaries") %>%
  separate(series_id, into = c("survey", "seasonal", "x", "area_code", "item_code"), sep = c(2,3,4,8)) %>%
  filter(period == "M13", area_code == "0000", item_code == "SA0") %>%
  mutate(cpi = value / value[max(year)-min(year)+1]) %>%  # took me a long time to get here
  select(year, cpi) 

# Breakdown choices
cx_demographics <- read_tsv("../../BLSDatabases/cx.demographics") %>%
  select(demographics_code, demographics_text)
# prep
  demographic_choices <- cx_demographics$demographics_code
  names(demographic_choices) <- cx_demographics$demographics_text

# Categories of the breakdown choices
cx_characteristics <- read_tsv("../../BLSDatabases/cx.characteristics") %>%
  select(demographics_code, characteristics_code, characteristics_text)
# prep
  c <- cx_characteristics %>%
    filter(demographics_code == "LB01") %>%
    select(characteristics_code, characteristics_text)
  initial_characteristics_choices <- c$characteristics_code
  names(initial_characteristics_choices) <- c$characteristics_text

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
   
   # Sidebar 
   sidebarLayout(
      sidebarPanel(
        selectInput("item_code",
                    label = "Select expenditure category:",
                    choices = c("Housing" = "HOUSING", "Shelter" = "SHELTER", "Utilities, etc." = "UTILS"),
                    selected = "HOUSING"
                    ),        
        selectInput("display",
                    label = "Select value to display:",
                    choices = c("2016 dollars" = "value_real",
                                "fraction of total expenditures" = "r",
                                "nominal dollars" = "value"),
                    selected = "value_real"
                    ),
        selectInput("demographics_code",
                     label = "Select demographic variable:",
                     choices = demographic_choices,
                     selected = "LB01"
                     ),
         selectInput("characteristics_codes",
                     label = "Select specific characteristics:",
                     choices = initial_characteristics_choices,
                     selected = c("All Consumer Units" = "01"),  # all
                     multiple = TRUE
                    ),
        actionButton("update", "Update plot")
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot1"),
         textOutput("text1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, clientData, session) {
  
  observeEvent(input$demographics_code, {
    c <- cx_characteristics %>%
      filter(demographics_code == input$demographics_code) %>%
      select(characteristics_code, characteristics_text)
    new_characteristics_choices <- c$characteristics_code
    names(new_characteristics_choices) <- c$characteristics_text
    
    updateSelectInput(session, "characteristics_codes", 
                      choices = new_characteristics_choices, 
                      selected = c("All Consumer Units" = "01"))
  })
  
  data <- eventReactive(input$update, {
    
    data <- cx_data_1_AllData %>%
      filter(item_code == input$item_code | item_code == "TOTALEXP") %>%    
      spread(key = item_code, value = value) %>%
      mutate(item_code = input$item_code) %>%   
      rename_(value = input$item_code) %>%    # underscore version necessary 
      filter(demographics_code == input$demographics_code) %>%
      left_join(cx_characteristics, by = c("demographics_code", "characteristics_code")) %>%
      left_join(cx_demographics, by = c("demographics_code"))  %>%
      mutate(r = value/TOTALEXP) %>%
      left_join(cpi, by = "year") %>%
      mutate(value_real = value / cpi) %>%
      group_by(characteristics_code) %>%
      filter(characteristics_code %in% input$characteristics_codes) 
    
  })

    output$plot1 <- renderPlot ({
    
    y_title <- switch(input$display, 
                "value" = "nominal dollars", 
                "value_real" = "2016 dollars", 
                "r" = "Fraction of total expenditures")

    
    data() %>%
      ggplot(aes_(~year, as.name(input$display))) +    # <-----------
    geom_line(aes(color = characteristics_text)) +
      ylim(0,NA) +
      labs( title = paste(input$item_code," expenditures"), caption = "Source: BLS.gov",
            x = "Year", y = y_title) +    # <-----------
    scale_color_discrete(data()$demographics_text[1])
    
  })

  output$text1 <- renderText({
    input$characteristics_codes
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

