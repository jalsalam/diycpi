# Import relative importance weights
# December 1986 not there.  I believe this if the file needed to do the 1982-1984 market basket
# December 1987 to December 1996 are apparently "OLD" but they have item_codes
# December 1997 to December 2016 are apprently "NEW" but do not have item_codes

# 3 different ways of using read_fwf
# Note no need to download the data.  Just read it from the website.
riw_1989 <- read_fwf("https://www.bls.gov/cpi/cpiri/usri1989.txt", fwf_empty("https://www.bls.gov/cpi/cpiri/usri1988.txt", col_names = c("item_code", "item_name", "riw_cpiu", "riw_cpiw")), skip = 10)
riw_1988 <- read_fwf("https://www.bls.gov/cpi/cpiri/usri1988.txt", fwf_positions(c(1, 10, 57, 63), c(9, 55, 62, 72), c("item_code", "item_name", "riw_cpiu", "riw_cpiw")), skip = 10)
riw_1987 <- read_fwf("https://www.bls.gov/cpi/cpiri/usri1987.txt", fwf_widths(c(9, 46, 7, 10), c("item_code", "item_name", "riw_cpiu", "riw_cpiw")), skip = 10)

# How does one create a loop where the loop variable is used to change the name of a file or of an R table/tibble?
# I want to loop through the years 1987 to 1996.

###########

#Jameel: try out this progression:
c(1987:1996)
paste('usri_', c(1987:1996), '.txt', sep = "")
riw_file_list <- paste('usri_', c(1987:1996), '.txt', sep = "")

### To do the same thing to each element of a list, read about the map() family of functions from the purrr package.
#Check out: http://r4ds.had.co.nz/iteration.html
#My quickie examples:

# Basic math using the map() approach:
nums <- c(1:10)

map_dbl(nums, ~.x^2) #"anonymous function" approach

fn_ex <- function(mynum){
  mynum^2 + mynum -7
}

map_dbl(nums, fn_ex) #using a longer function previously defined

#so how to apply it to this problem? something like:
read_riw_file <- function(filename) {
  read_fwf(filename, fwf_positions(c(1, 52, 64), c(51, 63, 70), c("item_name", "riw_cpiu", "riw_cpiw")), skip = 23)
}

riw_file_list_with_path <- paste('BLSDatabases/usri_', c(1987:1996), '.txt', sep = "")
list_of_riw_tables <- map(riw_file_list_with_path, read_riw_file) #I haven't downloaded the files, so I haven't tested. Even if it works, it will return a clunk "list" of the tibbles, which isn't really what you want. I don't know how to construct the names of the objects using the index, e.g., riw_new1997, etc, but we can come up with better ways to store this data anyway. The vintage should just be one of the columns, and all the values should be in one. In any case, focus on writing a function which works on one example, and apply it to lots is the easy part.

#############

riw_new1997 <- read_fwf("https://www.bls.gov/cpi/cpiri/usrinew97.txt", fwf_positions(c(1, 52, 64), c(51, 63, 70), c("item_name", "riw_cpiu", "riw_cpiw")), skip = 23)
riw_1998 <- read_fwf("https://www.bls.gov/cpi/cpiri/usri98.txt", fwf_positions(c(1, 55, 63), c(54, 64, 77), c("item_name", "riw_cpiu", "riw_cpiw")), skip = 14) %>%
   filter(!is.na(item_name))  # blank lines dropped
riw_2016 <- read_fwf("https://www.bls.gov/cpi/usri_2016.txt", fwf_positions(c(1, 55, 63), c(54, 64, 77), c("item_name", "riw_cpiu", "riw_cpiw")), skip = 14) %>%
  filter(!is.na(item_name)) %>%  # blank lines dropped
  mutate(item_name = gsub("\\.+", "", item_name)) %>% # drop periods
  mutate(item_name = toupper(item_name)) # convert to uppercase
  
# Now I need to find a file with item_text & item_code so that item_code can be added to the 1997 & latter RIW files
cu_item <- read_tsv("https://download.bls.gov/pub/time.series/cu/cu.item") %>%
  mutate(item_name = toupper(item_name)) # convert to uppercase

riw_2016 <- riw_2016 %>%
  left_join(cu_item, by = "item_name")
# Argh. Several of the item_text lines in riw_2016 wrap.  They need to be unwrapped to get the join to work completely.
# Approach 1
# Write out as .csv, use google sheets to clean up, read back in
write_csv(riw_2016, path = "output/riw_2016.csv")  # manually cleaned up this file
riw_2016 <- read_excel("output/riw_2016.csv.xlsx") %>% # google sheets converted to xlsx and added .xlsx
  rename(item_name = item_text) %>%  # newname = oldname 
  mutate(item_name = toupper(item_name)) %>% # convert to uppercase
  left_join(cu_item, by = "item_name")  # found item_code for all but "unsampled" items which comprise about 1% of expenditures

# Combine with RIW for 1987 to see how much expenditure patterns have changed
riw_1987to2016 <- full_join(riw_1987, riw_2016, by = "item_code") %>%
   select(item_code, item_text.x, item_text.y, riw_cpiu.x, riw_cpiu.y, riw_cpiw.x, riw_cpiw.y, sort_sequence) %>%
   arrange(sort_sequence)
# item_code has changed!  FOOD was SA11 in 1987; Food is SAF1 in 2016
# Will now try joining on item_text 
riw_1987to2016 <- full_join(riw_1987, riw_2016, by = "item_name") %>%
  select(item_name, item_code.x, item_code.y, riw_cpiu.x, riw_cpiu.y, riw_cpiw.x, riw_cpiw.y, sort_sequence) %>%
  arrange(sort_sequence)
# Much better for seeing changes in expenditure shares over the 30 years; more detail in 2016
# Did the full_join do a "cartesan product"?  See rows 29 & 30. 
# Maybe some small item_name changes.  E.g.  "APPAREL AND UPKEEP" vs. "APPAREL"


# Archive -- naming convention is not consistent
# furthermore, there are two versions of several years' RIWs. 

https://www.bls.gov/cpi/cpiriar.htm
           
https://www.bls.gov/cpi/cpiri/cpiri03-04_2005.txt
https://www.bls.gov/cpi/cpiri/usri2002.txt
https://www.bls.gov/cpi/cpiri/cpiri93-95_2001.txt
https://www.bls.gov/cpi/cpiri/cpiri98-00_2001.txt
https://www.bls.gov/cpi/cpiri/usri2000.txt
https://www.bls.gov/cpi/cpiri/usri1999.txt
https://www.bls.gov/cpi/cpiri/usri98.txt

