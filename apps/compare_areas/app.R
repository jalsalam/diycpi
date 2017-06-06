#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Requires four datasets to be downloaded from BLS and placed in the BLSdatases directory.
# Runs from a folder under the R folder

library(forcats)
library(tidyverse)
library(shiny)

cu_area <- read_tsv("../../BLSdatabases/cu.area")
cu_item <- read_tsv("../../BLSdatabases/cu.item")

# Rebase the CPI to a common year of 1997
# There must be a more elegant way to rebase the CPI series
# Tried mutate(value = 100*value/value[year == 1997]) but got error
value_1997 <- read_tsv("../../BLSdatabases/cu.data.2.Summaries") %>%
  filter(period == "M13" | period == "S03") %>%
  filter(year == 1997) %>%
  select(series_id, value) %>%
  rename(value_1997 = value)

cu_data_2_summaries <- read_tsv("../../BLSdatabases/cu.data.2.Summaries") %>%
  filter(period == "M13" | period == "S03") %>%
  left_join(value_1997) %>%
  filter(year > 1966) %>%
  separate(series_id, into = c("data_type", "area_code", "item_code"), sep = c(4, 8)) %>%
  left_join(cu_item) %>%
  select(year, area_code, item_code, item_name, value, value_1997) %>%
  left_join(cu_area) %>%
  select(year, area_code, item_code, item_name, area_name, value, value_1997) %>%
  group_by(area_name, item_code) %>%
  mutate(value = 100*value/value_1997) %>%
  mutate(growth_rate = (value - lag(value))/lag(value)) 

value_1997 <- read_tsv("../../BLSdatabases/cu.data.0.Current") %>%
  filter(period == "M13" | period == "S03") %>%
  filter(year == 1997) %>%
  select(series_id, value) %>%
  rename(value_1997 = value)

cu_data_0_current <- read_tsv("../../BLSdatabases/cu.data.0.Current") %>%
  filter(period == "M13" | period == "S03") %>%
  left_join(value_1997) %>%
  filter(year > 1966) %>%
  separate(series_id, into = c("data_type", "area_code", "item_code"), sep = c(4, 8)) %>%
  left_join(cu_item) %>%
  select(year, area_code, item_code, item_name, value, value_1997) %>%
  left_join(cu_area) %>%
  select(year, area_code, item_code, item_name, area_name, value, value_1997) %>%
  group_by(area_name, item_code) %>%
  mutate(value = 100*value/value_1997) %>%
  mutate(growth_rate = (value - lag(value))/lag(value)) 

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Series -- Shelter
  # Comparison series -- all, food, transportation, ... 
  # Area -- nation-wide, NE > 1.5 million, Washington-DC to Baltimore
  # Growth rates or Level
  
   # Application title
   titlePanel("BLS Housing Prices"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(position = "left",
     sidebarPanel(
       helpText("View and compare BLS shelter prices indices by area and subcomponents when available."),
                     
       radioButtons("series", label = "Select series to view:",
                    choices = c("Housing since 1967 depending on area", 
                                "Housing since 1997 by renter/owner"), 
                              selected = "Housing since 1967 depending on area"),
       
       # removed comparison series; keep comparison to areas
       
       selectInput("area", label = "Select area",
                   choices = cu_area$area_name,
                   multiple = TRUE,
                   selected = "Washington-Baltimore, DC-MD-VA-WV"),
       
       selectInput("display", label = "Display:", 
                   choices = c("Level of index", "Annual change of index"), selected = "Level of index")
     ),
           
     mainPanel(
         plotOutput("plot1"),
         verbatimTextOutput("debug")
      )
   )
)

# Define server logic required to draw plot
server <- function(input, output) {
  
   output$debug <- renderText({
    input$area
   }) 
  
   output$plot1 <- renderPlot({

     series = "^S[AE][H][1ABCD]"  
     
     dataset <- switch(input$series,
                       "Housing since 1997 by renter/owner" = cu_data_0_current,
                       "Housing since 1967 depending on area" = cu_data_2_summaries
     )
     
     # area <- filter_(dataset, ~area_name %in% input$area)
     
     display <- switch(input$display, 
                       "Level of index" = "value" ,
                       "Annual change of index" = "growth_rate"
     )
     
     # area_name <- filter_(dataset, ~area_code == area)$area_name[1]
    
     
     dataset %>%
       filter(area_name %in% input$area) %>%
       filter(item_code %in% c("SAH", "SEHA", "SEHC")) %>%
       ggplot(aes_(~year, as.name(display))) +  # , group = interaction(~area_name, ~item_code) with aes_ an error is generator
       geom_line(aes(color = area_name, group = interaction(area_name, item_code))) +  
       geom_point(aes(shape = item_name))+  
       geom_vline(xintercept = 1997) +
       ylim(50, 200) +
       xlim(1980, 2017) +
       labs(x = "Year", 
            y = "Index", 
            title = "Comparison of areas", 
            caption = "Source: BLS")
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

