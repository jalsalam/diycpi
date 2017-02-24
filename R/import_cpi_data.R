# import data & verify primary key for each tibble
# assumes files have been downloaded to a BLSDatabases folder
# orgina source data location: https://download.bls.gov/pub/time.series/cu/

cu_all_items <- read_tsv("BLSdatabases/cu.data.1.AllItems")
cu_data <- read_tsv("BLSdatabases/cu.data.0.Current")
cu_series <-read_tsv("BLSdatabases/cu.series")
cu_item <-read_tsv("BLSdatabases/cu.item")  # Warnings 

# verify primary keys

# this tibble has TWO series: one based on the current 1982-1984 basket and another based on an older one
# so there are two series_ids which differ in the 3rd from last characters
# they are two different "items"

CUALLITEMS %>%
  count(year, period, series_id) %>%
  filter(n>1)


# Note: 1496 or 1486 series in each year/period
# odd period: 1496, even period: 1486
# THE MOST DETAILED DATA AVAILABLE
# Note: The series are not mutually exclusive
# There are different sets of mutually exclusive series depending on how far one aggregates

CUDATA %>%
  count(year, period, series_id) %>%
  filter(n>1)

# information about the series
CUSERIES %>%
  count(series_id) %>%
  filter(n>1)

# item_id is the last 3 characters of the series_id (I believe)
# descriptive labels
CUITEM %>%
  count(item_code) %>%
  filter(n>1)
