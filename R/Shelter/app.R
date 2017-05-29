#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Requires four datasets to be downloaded from BLS and placed in the BLSdatases directory.
# Runs from a folder under the R folder

library(forcats)
library(shiny)

cu_area <- read_tsv("../../BLSdatabases/cu.area")
cu_item <- read_tsv("../../BLSdatabases/cu.item")

cu_data_2_summaries <- read_tsv("../../BLSdatabases/cu.data.2.Summaries") %>%
  filter(period == "M13" | period == "S03") %>%
  filter(year > 1966) %>%
  separate(series_id, into = c("data_type", "area_code", "item_code"), sep = c(4, 8)) %>%
  left_join(cu_item) %>%
  select(year, area_code, item_code, item_name, value) %>%
  left_join(cu_area) %>%
  select(year, area_code, item_code, item_name, area_name, value) %>%
  group_by(area_code, item_code) %>%
  mutate(growth_rate = (value - lag(value))/lag(value)) 

cu_data_0_current <- read_tsv("../../BLSdatabases/cu.data.0.Current") %>%
  filter(period == "M13" | period == "S03") %>%
  filter(year > 1966) %>%
  separate(series_id, into = c("data_type", "area_code", "item_code"), sep = c(4, 8)) %>%
  left_join(cu_item) %>%
  select(year, area_code, item_code, item_name, value) %>%
  left_join(cu_area) %>%
  select(year, area_code, item_code, item_name, area_name, value) %>%
  group_by(area_code, item_code) %>%
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
       
       selectInput("comparison", label = "Select comparison series:", 
                    choices = c("All items", "Food", "Medical", "Transportation", "None"), selected = "Food"),
       
       selectInput("area", label = "Select area",
                   choices = cu_area$area_name),
       
       selectInput("display", label = "Display:", 
                   choices = c("Level of index", "Annual change of index"), selected = "Level of index")
     ),
           
                   # Show a plot of educational attainment
                   mainPanel(
                     textOutput("text1"),
                     plotOutput("plot1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$plot1 <- renderPlot({

     series = "^S[AE][H][1ABCD]"  
     
     dataset <- switch(input$series,
                       "Housing since 1997 by renter/owner" = cu_data_0_current,
                       "Housing since 1967 depending on area" = cu_data_2_summaries
     )
     
     comparison <- switch(input$comparison,
                      "All items" = "SA0",
                      "Food" = "SAF",
                      "Medical" = "SAM",
                      "Transportation" = "SAT",
                      "None" = "ZZZ"
     )
     
     area <- switch(input$area, 
                     "Nation"  = "0000",
                     "Northeast with pop > 1.5m" = "A100", 
                     "Wasington-Baltimore" = "A311"
     )
     
     display <- switch(input$display, 
                       "Level of index" = "value" ,
                       "Annual change of index" = "growth_rate"
     )
     
     # area_name <- filter_(dataset, ~area_code == area)$area_name[1]
    
     area <- filter_(dataset, ~area_name == input$area)$area_code[1]
     
     dataset %>%
       filter_(~area_code == area) %>%
       filter_(~item_code %in% c("SAH", "SEHA", "SEHC", comparison)) %>%
       ggplot(aes_(~year, as.name(display))) +
       geom_line(aes(color = item_name)) +  
       geom_vline(xintercept = 1996.5) +
       ylim(50, 300) +
       xlim(1980, 2017) +
       labs(x = "Year", 
            y = "Index", 
            title = input$area, 
            caption = "Source: BLS")
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

