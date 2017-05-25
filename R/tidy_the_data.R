# PRICES ALL URBAN CONSUMERS DATA
library("tidyverse") 
library("readxl")
library("stringr")
library("lubridate")
library("purrr")

# Price indices for 2017 Jan, seasonally adjusted  
cu_data_current <- read_tsv("BLSdatabases/cu.data.0.Current")
cu_data_1_allitems <- read_tsv("BLSdatabases/cu.data.1.AllItems")

# Is the data tidy?
# No. 
# unite year & period to make date
# separate series id

# cu_data_current %>%
cu_monthlyx <- cu_data_1_allitems %>%
  
  
  unite(date, year, period, sep = "") %>%
  separate(series_id, into = c("data_type", "area_code", "item_code"), sep = c(4, 8)) %>%
  mutate(date = str_replace(date, "M", "")) %>%
  mutate(date = ymd(paste0(date, "01"))) %>%
  group_by(data_type)

table(x$area_code) # quite a few
length(unique(x$area_code)) # 45

SA0, AA0

y <- cu_data_1_allitems %>%
  spread(period, value)

filter(cu_data_1_allitems, period == "S03") %>%
  summarise(min = min(year), max = max(year))
# M13 1913-2016
# S03 1984-2016

filter(cu_data_1_allitems, period == "M13" & period == "S03") %>%
  ggplot() +
    geom_bar(aes(series_id))

table(x$data_type)
table(cu_data_1_allitems$period)

filter(x, area_code == "0000" & )

%>%
ggplot(aes(date, value, color = data_type)) +
  geom_point()



summary(cu_data_current)


# CPI data
#   312 (211 items plus aggregates???)
cpi201701 <- cu_data_current %>%
  mutate( item_code = str_sub(series_id, 9) ) %>%
  left_join(cu_item, by = "item_code") %>%
  filter(year == 2017 & period == "M01" & str_detect(series_id, "^CU.R0000S")) %>%
  mutate(item_name = toupper(item_name)) %>%
  select(series_id, item_code, item_name, value) %>%
  arrange(item_code)
write_csv(cpi201701, "output/cpi201701")

