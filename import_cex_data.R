# import CEX data

cxAllData <- read_tsv("BLSDatabases/cx.data.1.AllData")
cxcharacteristics <- read_tsv("BLSDatabases/cx.characteristics")
cxdemographics <- read_tsv("BLSDatabases/cx.demographics")
cxseries <- read_tsv("BLSDatabases/cx.series")
cxitem <- read_tsv("BLSDatabases/cx.item")
cxsubcategory <- read_tsv("BLSDatabases/cx.subcategory")

# goal: try to reproduce relative importance numbers

filter(cxdemographics, demographics_code=="LB15" | demographics_code=="LB05")

filter(cxcharacteristics, characteristics_code=="01")

# 14,444 series_id's in 2014! 
filter(cxAllData, year=="2014")

cxAllData %>%
  count(year, series_id) %>%
  filter(n>1)

# series_id equivalent to item, demographics, and characteristics code
cxseries %>%
  count(series_id) %>%
  filter(n>1)
cxseries %>%
  count(item_code, demographics_code, characteristics_code) %>%
  filter(n>1)

mutate(name=dxAllData$series_id[match(cxseries,)], )

# item_code 6 digits
# in series_id 4-9
# cxitem 154 rows
# display_level==0 30 rows
# 19 expenditures: TOTALEXP + FOODTOTL, ..., CHASLI 
# 11 descriptor or categories such as no. in CU, income before & after taxes, thru to own/lease at least 1 vehicle

x <- cxitem %>%
  filter(display_level==0)
x <- x %>%
  arrange(sort_sequence)

# 30 HOUSING subcategory_code's with display_level's (level/number at that level) 0(1), 1(5), 2, and 3
x <- cxitem %>%
  filter(subcategory_code=="HOUSING" & display_level=="1")
x <- x %>%
  arrange(sort_sequence)

# subcategory_code EDUCATN has only 1 entry
x <- cxitem %>%
  filter(subcategory_code=="EDUCATN")
x <- x %>%
  arrange(sort_sequence)


