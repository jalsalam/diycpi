# Incomplete but decided to push so that you can see where I am going

# Goal A: update the riw weights
# See: https://www.bls.gov/cpi/cpiriar.htm
# Step 1: Import last riw data (and cleanup)
# Step 2: Import CPI data for update month (and filter)
# Step 3: Join by item_name

# Step 1:
# import 2016 riw data
# 322 items
# some are special aggregates, e.g. "All items less ... "
# some are subaggregates, e.g. "Cereals and cereal products" is the aggregate of "Flour and prepared flour mixes" and two others
# TBD: 
# 1) number of mutually exclusive items
# 2) number of mutually exclusive items that can be matched to price indices
source("R/riw_cleanup.R")

# Step 2:
# import CPI data
# 703 series 
# roughly half are seasonally adjusted and half are not
cu_data_current <- read_tsv("BLSdatabases/cu.data.0.Current")
cu_item <-read_tsv("BLSdatabases/cu.item")  
cpi201701 <- cu_data_current %>%
  mutate( item_code = str_sub(series_id, 9) ) %>%
  left_join(cu_item, by = "item_code") %>%
  filter(year == 2017 & period == "M01" & str_detect(series_id, "^CU.R0000S")) %>%
  mutate(item_name = toupper(item_name)) %>%
  select(series_id, item_code, item_name, value) %>%
  arrange(item_code)

# Step 3:
# Join cpi & riw
x <- full_join(cpi201701, riw_2016, by = "item_name") %>%
  # next line doesn't work because it replace riw_cpiu with NA 
  # mutate( riw_cpiu = ifelse((series_id == "CUSR0000SA0L5" | series_id == "CUUR0000SA0L5"), 91.461, riw_cpiu)) %>%
  select(series_id, item_name, value, riw_cpiu, item_code)
count(x, is.na(value))  # 25 riw item have no CPI index
count(x, is.na(riw_cpiu)) # 159 cpi index values have no riw (probably half seasonally adjusted & half not)

# save item_name, cpi index (value), weight (riw_cpiu)

# Goal B: Estimate the contribution of each component to the overall price chanage