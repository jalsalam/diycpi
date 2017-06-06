library(haven)
library(tidyverse)
library(stringr)

setwd("c:/users/nabee/documents/Rprojects/diycpi/R/expenditures")
cpi <- read_tsv("../../BLSdatabases/cu.data.2.summaries") %>%
  separate(series_id, into = c("survey", "seasonal", "x", "area_code", "item_code"), sep = c(2,3,4,8)) %>%
  filter(period == "M13", area_code == "0000", item_code == "SA0") %>%
  mutate(cpi = value / value[max(year)-min(year)+1]) %>%  # took me a long time to get here
  select(year, cpi) 

cx_subcategory <- read_tsv("../../BLSdatabases/cx.subcategory") %>%
  select(category_code, subcategory_code, subcategory_text)

cx_item <- read_tsv("../../BLSdatabases/cx.item") %>%
  select(subcategory_code, item_code, item_text)

cx_demographics <- read_tsv("../../BLSdatabases/cx.demographics") %>%
  select(demographics_code, demographics_text)

cx_series <- read_tsv("../../BLSdatabases/cx.series") %>%
  select(item_code, demographics_code, characteristics_code, series_title) %>%
  separate(series_title, into = c("series_text", "demographics_code2", "characteristics_text"), sep = "-", extra = "merge")

cx_data_1_AllData <- read_tsv("../../BLSdatabases/cx.data.1.AllData") %>%
  mutate(series_id = str_trim(series_id)) %>%
  separate(series_id, into = c("survey", "seasonal", "rest"),
           sep = c(2,3)) %>%
  separate(rest, into = c("item_code", "demographics_code", "characteristics_code", "process_code"),
           sep = c(-8,-4,-2)) %>%
  select(year, item_code, demographics_code, characteristics_code, value)


# --- plot -- v4
# filter out some of the characteristics codes, e.g. incomplete records
# lay all consumers on a different layer and change appearance
  
# ----- plot --- v3
# convert nominal dollars to CPI deflated dollars

item_code_choice <- "HOUSING"
demographics_code_choice <- "LB08"
y_choice <- "value_real"
characteristics_code_choices <- c("01", "02", "03")

y_title <- switch(y_choice, 
                  "value" = "nominal dollars", 
                  "value_real" = "2016 dollars", 
                  "r" = "Fraction of total expenditures")


data <- cx_data_1_AllData %>%
  filter(item_code == item_code_choice | item_code == "TOTALEXP") %>%    
  spread(key = item_code, value = value) %>%
  mutate(item_code = item_code_choice) %>%   
  rename_(value = item_code_choice) %>%    # underscore version necessary 
  filter(demographics_code == demographics_code_choice) %>%    
  filter(!(characteristics_code %in% characteristics_code_choices)) %>%    
  left_join(cx_series, by = c("item_code", "demographics_code", "characteristics_code")) %>%
  left_join(cx_demographics, by = c("demographics_code"))  %>%
  mutate(r = value/TOTALEXP) %>%
  left_join(cpi, by = "year") %>%
  mutate(value_real = value / cpi)

data %>%
  group_by(characteristics_code) %>%
  ggplot(aes_(~year, as.name(y_choice))) +    # <-----------
  geom_line(aes(color = characteristics_text)) +
  ylim(0,NA) +
  labs( title = paste(data$series_text[1],"expenditures"), caption = "Source: BLS.gov",
        x = "Year", y = y_title) +    # <-----------
        scale_color_discrete(data$demographics_text[1])





# ----- plot --- v2
# Two additions:
# Fraction of TOTALEXP choice
# passing choices as string variables

# item_code: HOUSING, SHELTER, UTILS
# demographics_code: LB01, LB04, LB07, LB08

  item_code_choice <- "HOUSING"
  demographics_code_choice <- "LB08"
  
  data <- cx_data_1_AllData %>%
  filter(item_code == item_code_choice | item_code == "TOTALEXP") %>%    
  spread(key = item_code, value = value) %>%
  mutate(item_code = item_code_choice) %>%   
  rename_(value = item_code_choice) %>%    # underscore version necessary 
  filter(demographics_code == demographics_code_choice) %>%    
  left_join(cx_series, by = c("item_code", "demographics_code", "characteristics_code")) %>%
  left_join(cx_demographics, by = c("demographics_code"))  %>%
  mutate(r = value/TOTALEXP)
  
  data %>%
  group_by(characteristics_code) %>%
  ggplot(aes(year, r)) +    # <-----------
  geom_line(aes(color = characteristics_text)) +
  ylim(0,NA) +
  labs( title = paste(data$series_text[1],"expenditures"), caption = "Source: BLS.gov",
        x = "Year", y = "Fraction of total expenditure") +    # <-----------
  scale_color_discrete(data$demographics_text[1])

  # ----- plot --- v1
  
  # item_code: HOUSING, SHELTER, UTILS
  # demographics_code: LB01, LB04
  
  data <- cx_data_1_AllData %>%
    filter(item_code == "HOUSING") %>%  # <-----------
    filter(demographics_code == "LB04") %>%  # <-----------
    left_join(cx_series, by = c("item_code", "demographics_code", "characteristics_code")) %>%
    left_join(cx_demographics, by = c("demographics_code"))
  
  data %>%
    group_by(characteristics_code) %>%
    ggplot(aes(year, value)) +
    geom_line(aes(color = characteristics_text)) +
    ylim(0,NA) +
    labs( title = data$series_text[1] , x = "Year", y = "Nominal dollars/year") +
    scale_color_discrete(data$demographics_text[1])
  
========== Temporary
