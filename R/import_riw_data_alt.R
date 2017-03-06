#import_riw_data_alt.R
#An alternative attempt starting from scratch on the RIW data. I am going to give a shot at reading the RIW data from the recent txt files (which are not designed to be machine-readable, unfortunately)

#readr has a lot of different ways to read in data (which I haven't used in the past)
#I am going to experiment with some of the different ones to see what comes out

##########################

#read into a string. This is probably the last resort as I don't know much about string manipulation in R.
riw_2016_txt <- read_file('BLSdatabases/usri_2016.txt')

###########################
# THIS VERSION IS JUST FOR SEEING THE STEPS, VERSION AFTER IS MORE COMPACT

#read_table is intended for text tables with values in fixed positions
riw_2016_table_showsteps <- 
  read_table('BLSdatabases/usri_2016.txt', skip = 15, 
             col_names = c('ex_cat_raw', 'cpiu', 'cpiw')) %>%
  
  #Delete the dots (needs to be escaped b/c . is a special char). I need to learn how to specify trailing dots in multiples
  mutate(ex_cat_c1 = str_replace_all(ex_cat_raw, '\\.', '')) %>%
  
  #trim whitespace -- not sure why it isn't resulting in NAs
  mutate(ex_cat_c2 = str_trim(ex_cat_c1)) %>%
  
  #try to ID places where the category name wraps
  mutate(wrap_flag = if_else(is.na(lag(cpiu)) & !is.na(lag(ex_cat_c1)), "WRAP", "")) %>%
  
  #if the name wraps, combine the names (will drop blanks later I guess)
  mutate(ex_cat = if_else(wrap_flag == "WRAP", 
                          str_c(lag(ex_cat_c1), ex_cat_c1, sep=" "), ex_cat_c1)) %>%
  #drop blank rows
  mutate(keep_flag = !is.na(cpiu) | !is.na(cpiw))
  #filter(!is.na(cpiu) | !is.na(cpiw)) #to actually drop...

###################

#table approach, but not showing the steps

source('R/import_cpi_data.R') #really just for cu_item

item_name_lookup <- cu_item %>%
  select(item_name, item_code) %>%
  mutate(item_name = str_trim(item_name)) #whitespace was messing up some matches

riw_2016_table <- 
  read_table('BLSdatabases/usri_2016.txt', skip = 15, 
             col_names = c('ex_cat_raw', 'cpiu', 'cpiw')) %>%
  
  mutate(ex_cat = str_trim(str_replace_all(ex_cat_raw, '\\.', ''))) %>%
  mutate(ex_cat = 
           if_else(is.na(lag(cpiu)) & is.na(lag(cpiw)) & !is.na(lag(ex_cat)), 
                   str_c(lag(ex_cat), ex_cat, sep=" "), 
                   ex_cat)) %>%
  mutate(ex_cat = str_trim(ex_cat)) %>% #whitespace was messing up some matches
  filter(!is.na(cpiu) | !is.na(cpiw)) %>%
  select(ex_cat, cpiu, cpiw) %>%
  left_join(item_name_lookup, by = c("ex_cat" = "item_name"))

#STATUS: it looks like this basically worked. There is one item that didn't match, but it is "unsampled" which I think means no data. Anyway, too much work, and not perfect, but something.


