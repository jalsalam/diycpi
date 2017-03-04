# import CEX data

cx_all_data <- read_tsv("BLSDatabases/cx.data.1.AllData")
cx_series <- read_tsv("BLSDatabases/cx.series")
cx_item <- read_tsv("BLSDatabases/cx.item")

write_excel_csv(cx_item, path="output/cx_item.csv")  # source for creating an unduplicated list of items, uploaded to docs.google
url <- "docs.google.com/spreadsheets/d/1mdDQrNVWDnEG_OMxCTMTz3-wrrziXN2QqJcW_1OYhys"  # sharing link
cx_item <- read_csv(construct_download_url(url), skip = 1) # un_dup_1 is most detail, un_dup_2 has less

cx_characteristics <- read_tsv("BLSDatabases/cx.characteristics")
cx_demographics <- read_tsv("BLSDatabases/cx.demographics")
cx_subcategory <- read_tsv("BLSDatabases/cx.subcategory")

# verify primary keys
cx_all_data %>%
  count(series_id, year) %>%  # verified
  # value
  filter(n>1)

#alternative of primary key check:
assert_that(nrow(
  cx_all_data %>%
    count(series_id, year) %>%
    filter(n>1) 
  ) == 0)

#right now this is an awkward combination of pipes and nesting. I want to figure out how to make it better
#I don't know whether or not assert_that is better than stopifnot
#both of these are functions that do error-check and stop execution if they are not fulfilled, which seems to fit the primary key checks.

#try 2. this is much better code. Error message not good though.
cx_all_data %>%
  count(series_id, year) %>%
  filter(n>1) %>%
  {assert_that(nrow(.)==0)}


cx_series %>% 
  count(series_id) %>%  # verified
  # category_code, subcategory_code, item_code, demographics_code, characteristics code
  filter(n>1)
cx_item %>%
  count(item_code) %>%  # verified
  # subcategory_code, item_code, item_text, display_level
  filter(n>1)
cx_demographics %>% # demographics_code = variable
  count(demographics_code) %>%  # verified; demographics_text
  filter(n>1)
cx_characteristics %>% # demographics_code = variable; char.._code = value
  count(demographics_code, characteristics_code) %>%  # verified; characteristics_text  
  filter(n>1)
cx_subcategory %>%  # category_code (EXPEND, INCOME, CUCHARS, ADDENDA) 
  count(subcategory_code) %>%  # verified; subcategory_text
  filter(n>1)

# =============================================================================================
# Create data table of expenditure breakdown by year with shares
# display_level==0 has 15 categories
# display_level==1 has 26 categories but is missing display_level==0 categories with no subcategories such as alcohol, education
# combining both of those double counts some expenditures
# rows: category_code == "EXPEND" & demographics_code == "LB01" & characteristics_code == "01"
# variables: subcategory_code, year, value

all_all_data <- cx_all_data %>%
  left_join(cx_series, by = "series_id") %>%
  left_join(cx_item, by = "item_code")

our_data <- filter(
  all_all_data, 
  category_code == "EXPEND" & 
  demographics_code == "LB01" & 
  characteristics_code == "01" &
  year == 2015 & 
  un_dup_1 == 1 # Unduplicated items; the test will be if the shares of total expenditures add to 1
                ) %>%
    arrange(year, sort_sequence) %>%
      select(year, series_id, item_text, value, un_dup_1) %>%
      group_by(year) %>%   
      mutate(share_denom = value[1]) %>%
      mutate(expenditure_share=value/share_denom)  

sum(our_data["expenditure_share"]) # Oops! Should add to 2, they add to 2.02; something is duplicated
sum(our_data["value"]) # Oops! Should add to 55978, they don't

# We now have expenditures share for 2014 & 2015 
# next step is to multiply these shares by the price changes and add-up
# well we do have to solve the double counting problem
# i.e. we have to find a better than display_level to identify unduplicated expenditure categories

# ===========================================================================================

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

# do CX_itmes & cu_items line up? 
# NO! Their codes are completely different
cx_item <- read_tsv("BLSDatabases/cx.item")
cu_item <- read_tsv("BLSDatabases/cu.item")
