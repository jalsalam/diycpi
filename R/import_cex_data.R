# import CEX data

cx_all_data <- read_tsv("BLSDatabases/cx.data.1.AllData")
cx_characteristics <- read_tsv("BLSDatabases/cx.characteristics")
cx_demographics <- read_tsv("BLSDatabases/cx.demographics")
cx_series <- read_tsv("BLSDatabases/cx.series")
cx_item <- read_tsv("BLSDatabases/cx.item")
cx_subcategory <- read_tsv("BLSDatabases/cx.subcategory")

# goal: try to reproduce relative importance numbers

filter(cx_demographics, demographics_code=="LB15" | demographics_code=="LB05")

filter(cx_characteristics, characteristics_code=="01")

# 14,444 series_id's in 2014! 
filter(cx_all_data, year=="2014")

cx_AllData %>%
  count(year, series_id) %>%
  filter(n>1)

# series_id equivalent to item, demographics, and characteristics code
cx_series %>%
  count(series_id) %>%
  filter(n>1)
cx_series %>%
  count(item_code, demographics_code, characteristics_code) %>%
  filter(n>1)

mutate(name=cx_all_data$series_id[match(cx_series,)], )

# item_code 6 digits
# in series_id 4-9
# cx_item 154 rows
# display_level==0 30 rows
# 19 expenditures: TOTALEXP + FOODTOTL, ..., CHASLI 
# 11 descriptor or categories such as no. in CU, income before & after taxes, thru to own/lease at least 1 vehicle

x <- cx_item %>%
  filter(display_level==0)
x <- x %>%
  arrange(sort_sequence)

# 30 HOUSING subcategory_code's with display_level's (level/number at that level) 0(1), 1(5), 2, and 3
x <- cx_item %>%
  filter(subcategory_code=="HOUSING" & display_level=="1")
x <- x %>%
  arrange(sort_sequence)

# subcategory_code EDUCATN has only 1 entry
x <- cx_item %>%
  filter(subcategory_code=="EDUCATN")
x <- x %>%
  arrange(sort_sequence)


