# import RIW text file as tibble
# clean up:
# remove dots
# convert to upper case (easier to match to other files)
# unwrap lines
# Make a function of this?

riw_2016 <- read_fwf("BLSDatabases/usri_2016.txt", fwf_positions(c(1, 55, 63), c(54, 64, 77), c("item_name", "riw_cpiu", "riw_cpiw")), skip = 14) %>%
  filter(!is.na(item_name)) %>%  # blank lines dropped
  mutate(item_name = gsub("\\.+", "", item_name)) %>% # drop periods
  mutate(item_name = toupper(item_name)) # convert to uppercase

# unwraps lines 
riw_2016 <- riw_2016 %>%
  mutate(item_name2 = ifelse(is.na(lag(riw_cpiu)), str_c(lag(item_name), item_name, sep = " ") , item_name )) %>% # concatenate item_names where the lagged riw_cpiu is missing
  filter(!is.na(riw_cpiu)) %>%  # drop 1st line of wrapped liens
  mutate(item_name = ifelse(item_name != "ALL ITEMS" , item_name2, item_name)) %>%  # substitute concatendated item_name
  select(item_name, riw_cpiu, riw_cpiw) # drop duplicate item_name

