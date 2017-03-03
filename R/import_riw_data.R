# Import relative importance weights
# December 1986 not there.  I believe this if the file needed to do the 1982-1984 market basket
# December 1987 to December 1996 are apparently "OLD" but they have item_codes
# December 1997 to December 2016 are apprently "NEW" but do not have item_codes

# 3 different ways of using read_fwf
# Note no need to download the data.  Just read it from the website.
riw_1989 <- read_fwf("https://www.bls.gov/cpi/cpiri/usri1989.txt", fwf_empty("https://www.bls.gov/cpi/cpiri/usri1988.txt", col_names = c("item_code", "item_text", "riw_cpiu", "riw_cpiw")), skip = 10)
riw_1988 <- read_fwf("https://www.bls.gov/cpi/cpiri/usri1988.txt", fwf_positions(c(1, 10, 57, 63), c(9, 55, 62, 72), c("item_code", "item_text", "riw_cpiu", "riw_cpiw")), skip = 10)
riw_1987 <- read_fwf("https://www.bls.gov/cpi/cpiri/usri1987.txt", fwf_widths(c(9, 46, 7, 10), c("item_code", "item_text", "riw_cpiu", "riw_cpiw")), skip = 10)

# How does one create a loop where the loop variable is used to change the name of a file or of an R table/tibble?
# I want to loop through the years 1987 to 1996.


riw_new1997 <- read_fwf("https://www.bls.gov/cpi/cpiri/usrinew97.txt", fwf_positions(c(1, 52, 64), c(51, 63, 70), c("item_text", "riw_cpiu", "riw_cpiw")), skip = 23)
riw_1998 <- read_fwf("https://www.bls.gov/cpi/cpiri/usri98.txt", fwf_positions(c(1, 55, 63), c(54, 64, 77), c("item_text", "riw_cpiu", "riw_cpiw")), skip = 14) %>%
   filter(!is.na(item_text))  # blank lines dropped
riw_2016 <- read_fwf("https://www.bls.gov/cpi/usri_2016.txt", fwf_positions(c(1, 55, 63), c(54, 64, 77), c("item_text", "riw_cpiu", "riw_cpiw")), skip = 14) %>%
  filter(!is.na(item_text)) %>%  # blank lines dropped
  mutate(item_text = gsub("\\.+", "", item_text)) %>% # get rids of periods (leading blanks automatically dropped during read)
  mutate(item_text = toupper(item_text)) # convert to uppercase
  
# Now I need to find a file to with item_text & item_code so that item_code can be added to the 1997 & latter RIW files
