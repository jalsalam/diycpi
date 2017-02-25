# import data & verify primary key for each tibble
# assumes files have been downloaded to a BLSDatabases folder
# orgina source data location: https://download.bls.gov/pub/time.series/cu/

cu_data_old <- read_tsv("BLSdatabases/cu.data.1.AllItems")
cu_data_current <- read_tsv("BLSdatabases/cu.data.0.Current")
cu_series <-read_tsv("BLSdatabases/cu.series")
cu_item <-read_tsv("BLSdatabases/cu.item")  # Warnings 

# verify primary keys
# this tibble has TWO series: one based on the current 1982-1984 basket and another based on an older one
# so there are two series_ids which differ in the 3rd from last characters
# they are two different "items"

cu_data_current %>%
  count(year, period, series_id) %>%  # verified
  # value: index relative to 1982==100
  filter(n>1)
cu_series %>%
  count(series_id) %>%  # verified
  # item_code
  filter(n>1)
cu_item %>%
  count(item_code) %>%  # verified
  # item_name, display_level, sort_sequences
  filter(n>1)

# =============================================================================================
# Create data table of price changes???

all_cpi_data <- cu_data_current %>%
  left_join(cu_series, by = "series_id") %>%
  left_join(cu_item, by = "item_code")

our_cpi_data <- filter(
  all_cpi_data, 
     area_code=="0000" &  # all areas
     base_code=="S"  & # new base
     seasonal=="U"  & # unadjusted
    (display_level==0 | display_level==1) &  # some double counting
    (year==2015) &   
    (period=="M01")  
  ) %>%
  arrange(year, sort_sequence) %>%
  select(year, period, series_id, area_code, item_code, item_name, seasonal, base_code, value)

# success! we now have expenditures from the cpi data analogous to the expenditure shares table 
# I believe the values are indices relative to 1982==100
# But who do they do the chained index
# research on methodology needed now
# we also have to solve the double counting problem, display_level is not doing the job
# plus with this CPI data they have even more subindices
# i.e. we have to find a way to identify unduplicated expenditure categories
# Will we may have to create a column vector manually?

# ===========================================================================================


# Note: 1496 or 1486 series in each year/period
# odd period: 1496, even period: 1486
# THE MOST DETAILED DATA AVAILABLE
# Note: The series are not mutually exclusive
# There are different sets of mutually exclusive series depending on how far one aggregates

