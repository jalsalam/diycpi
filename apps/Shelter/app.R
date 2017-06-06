#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Requires four datasets to be downloaded from BLS and placed in the BLSdatases directory.
# Runs from a folder under the R folder

# setwd('apps/Shelter') #for interactive use
source('../../R/setup.R')

cu_area <- read_tsv("../../BLSdatabases/cu.area")
cu_item <- read_tsv("../../BLSdatabases/cu.item")

cu_process <- function(df) {
  df %>%
    filter(period %in% c("M13", "S03"),
           year > 1966) %>%
           
    separate(series_id, into = c("data_type", "area_code", "item_code"), sep = c(4, 8)) %>%
    filter(item_code %in% c("SAH", "SEHA", "SEHC", "SA0", "SAM", "SAF", "SAT", "ZZZ")) %>%
  
    left_join(cu_item, by = "item_code") %>%
    left_join(cu_area, by = "area_code") %>%
    select(year, area_code, item_code, item_name, area_name, value) %>%
    
    group_by(area_code, item_code, year) %>%
    slice(1) %>% #eliminate cases of multiples in 1984
    ungroup() %>%
    
    group_by(area_code, item_code) %>%
    mutate(growth_rate = (value - lag(value))/lag(value)) %>%
    ungroup()
}

cu_data_2_summaries <- read_tsv("../../BLSdatabases/cu.data.2.Summaries") %>%
  cu_process()

cu_data_0_current <- read_tsv("../../BLSdatabases/cu.data.0.Current") %>%
  cu_process()
  

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
                    choices = c("All items" = "SA0",
                                "Food" = "SAF",
                                "Medical" = "SAM",
                                "Transportation" = "SAT",
                                "None" = "ZZZ"), selected = "Food"),
       
       selectInput("area", label = "Select area",
                   choices = cu_area$area_name),
       
       selectInput("display", label = "Display:", 
                   choices = c("Level of index" = "value" ,
                               "Annual change of index" = "growth_rate"), 
                   selected = "Level of index")
     ),
           
                   # Show a plot of shelter
                   mainPanel(
                     textOutput("text1"),
                     plotOutput("plot1")
      )
   )
)

# Define server logic required to draw a line chart
server <- function(input, output) {
  
   output$plot1 <- renderPlot({

     series = "^S[AE][H][1ABCD]"  
     
     dataset <- switch(input$series,
                       "Housing since 1997 by renter/owner" = cu_data_0_current,
                       "Housing since 1967 depending on area" = cu_data_2_summaries
     )
     
     # comparison <- switch(input$comparison,
     #                  "All items" = "SA0",
     #                  "Food" = "SAF",
     #                  "Medical" = "SAM",
     #                  "Transportation" = "SAT",
     #                  "None" = "ZZZ"
     # )
     
     area <- switch(input$area, 
                     "Nation"  = "0000",
                     "Northeast with pop > 1.5m" = "A100", 
                     "Wasington-Baltimore" = "A311"
     )
     
     # display <- switch(input$display, 
     #                   "Level of index" = "value" ,
     #                   "Annual change of index" = "growth_rate"
     # )
     
     # area_name <- filter_(dataset, ~area_code == area)$area_name[1]
    
     this_area_code <- filter(dataset, area_name == input$area)$area_code[1]
     
     dataset %>%
       filter(area_code == this_area_code,
              item_code %in% c("SAH", "SEHA", "SEHC", input$comparison),
              !are_na(growth_rate)) %>%
       
       ggplot(aes_(~year, sym(input$display))) +
        geom_line(aes(color = item_name)) +  
        geom_vline(xintercept = 1996.5) +
        # ylim(50, 300) + # only works for 'level of index'
        # xlim(1980, 2017) + # only works for 'level of index'
        labs(x = "Year", 
             y = "Index", 
             title = input$area, 
             caption = "Source: BLS")
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

# cu_data_2_summaries %>%
#   filter(area_code == "0000") %>%
#   filter(!are_na(growth_rate)) %>%
#   
#   ggplot(aes(x = year, y = growth_rate, color = item_name)) +
#     geom_line()

