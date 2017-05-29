#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Requires two datasets to be downloaded from BLS and placed in the BLSdatases directory.
# Runs from a folder under the R folder


library(shiny)

cu_data_2_summaries <- read_tsv("../../BLSdatabases/cu.data.2.Summaries") %>%
  filter(period == "M13" | period == "S03") %>%
  filter(year > 1966) %>%
  separate(series_id, into = c("data_type", "area_code", "item_code"), sep = c(4, 8)) %>%
  group_by(area_code, item_code) %>%
  mutate(growth_rate = (value - lag(value))/lag(value)) %>%
  filter(growth_rate > -.2) 

cu_data_0_current <- read_tsv("../../BLSdatabases/cu.data.0.Current") %>%
  filter(period == "M13" | period == "S03") %>%
  filter(year > 1966) %>%
  separate(series_id, into = c("data_type", "area_code", "item_code"), sep = c(4, 8)) %>%
  group_by(area_code, item_code) %>%
  mutate(growth_rate = (value - lag(value))/lag(value)) %>%
  filter(growth_rate > -.2) 


# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Series -- Shelter
  # Comparison series -- all, food, transportation, ... 
  # Area -- nation-wide, NE > 1.5 million, Washington-DC to Baltimore
  # Growth rates or Level
  
   # Application title
   titlePanel("BLS Shelter Prices"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(position = "left",
     sidebarPanel(
       helpText("View and compare BLS shelter prices indices by area and subcomponents when available."),
                     
       radioButtons("series", label = "Select series to view:",
                    choices = c("Shelter since 1997 with more detail", 
                                "Shelter since 1967 with less detail"), 
                              selected = "Shelter since 1997 with more detail"),
       
       radioButtons("comparison", label = "Select comparison series:", 
                    choices = c("All items", "Food", "Transportation"), selected = "Food"),
       
       selectInput("area", label = "Select area",
                   choices = c("Nation", "Northeast with pop > 1.5m", "Wasington-Baltimore"), selected = "Nation"),
       
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
                       "Shelter since 1997 with more detail" = cu_data_0_current,
                       "Shelter since 1967 with less detail" = cu_data_2_summaries
     )
     
     comparison <- switch(input$comparison,
                      "All items" = "SA0",
                      "Food" = "SAF",
                      "Transportation" = "SAT"
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
    
     dataset %>%
       filter_(~area_code == area) %>%
       filter_(~item_code %in% c("SAH", "SEHA", "SEHC", comparison)) %>%
       ggplot(aes_(~year, as.name(display))) +
       geom_line(aes(color = item_code)) + 
       geom_vline(xintercept = 1996.5) +
       labs(x = "Year", y = "Index", title = "Annual Average")
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

