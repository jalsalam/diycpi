library("tidyverse") 
library("stringr") #string manipulation functions, installed in tidyverse but not loaded by default

# extract cpi-u indices for a particular year & month, both seasonally adjusted and unadjusted
cpi <- function(yr, month) {
  cu_data_current %>%
    mutate( item_code = str_sub(series_id, 9) ) %>%
    left_join(cu_item, by = "item_code") %>%
    filter(year == yr & period == month & str_detect(series_id, "^CU.R0000S")) %>%
    mutate(item_name = toupper(item_name)) %>%
    select(series_id, item_code, item_name, value) %>%
    arrange(item_code)
}

x <-str_c("yr", yr, collapse = "")
prefix<- switch(str_c("yr", yr, collapse = ""),
                yr1987 = "usri",
                yr2004 = "cpiri_",
                yr2005 = "cpiri03-04_",
                yr2006 = "cpiri",
                "usri_")


# file_name prefix is cpi in 2004-2012 vs. us in 1987-2002 & 2013-2016
riw <- function(yr) {
  prefix<- switch(str_c("yr", yr, collapse = ""),
                  yr1987 = "usri",
                  yr2004 = "cpiri_",
                  yr2005 = "cpiri03-04_",
                  yr2006 = "cpiri",
                  "usri_")
  file_name <- str_c("BLSDatabases/", prefix, yr, ".txt", collapse = "")
  riw <- read_fwf(file_name, fwf_positions(c(1, 55, 65), c(54, 64, 80), c("item_name", "riw_cpiu", "riw_cpiw")), col_types = list(col_character(), col_double(), col_double()), skip = 14) %>%
    filter(!is.na(item_name)) %>%  # blank lines dropped
    mutate(item_name = gsub("\\.+", "", item_name)) %>% # drop periods
    mutate(item_name = toupper(item_name)) %>% # convert to uppercase
    mutate(item_name2 = ifelse(is.na(lag(riw_cpiu)), str_c(lag(item_name), item_name, sep = " ") , item_name )) %>% # concatenate item_names where the lagged riw_cpiu is missing
    filter(!is.na(riw_cpiu)) %>%  # drop 1st line of wrapped liens
    mutate(item_name = ifelse(item_name != "ALL ITEMS" , item_name2, item_name)) %>%  # substitute concatendated item_name
    mutate(item_name = ifelse(item_name == "AIRLINE FARE", "AIRLINE FARES", item_name)) %>% # misspelling in Dec 2016 file
    select(item_name, riw_cpiu, riw_cpiw) # drop duplicate item_name
}

riw0 <- riw(2004)
cpi1 <- cpi(2004, "M12")
cpi2 <- cpi(2005, "M09")
cpi3 <- cpi(2005, "M10")

