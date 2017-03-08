# Goal is to have a xwalk between weights and price indices

cx_item <- read_tsv("output/cx_edited.item", skip = 1) %>%
  rename(un_dup_0 = up_dup_0) %>% # fixing typo
  mutate(item_text = toupper(item_text)) %>%
  arrange(sort_sequence)
cu_item <-read_tsv("BLSdatabases/cu.item") %>%
  arrange(sort_sequence) %>%
  mutate(item_name = toupper(item_name))
riw_1987 <- read_fwf("BLSDatabases/usri1987.txt", fwf_widths(c(9, 46, 7, 10), c("item_code", "item_name", "riw_cpiu", "riw_cpiw")), skip = 10)

# matching on description
cx_cu_item_xwalk <- full_join(cx_item, cu_item, by = c("item_text" = "item_name"))  %>%
  filter(un_dup_1 == 1)  
# 5 of 14 match on un_dup_0
# 9 of 33 match on un_dup_1

# matching on item_code
riw_cu_item_xwalk <- full_join(riw_1987, cu_item, by = "item_code") %>%
  select(item_code, item_name.x, item_name.y, riw_cpiu, riw_cpiw, sort_sequence) %>%
  arrange(sort_sequence)

# matching on item_name
riw_cu_item_xwalk2 <- full_join(riw_1987, cu_item, by = "item_name") %>%
  select(item_name, item_code.x, item_code.y, riw_cpiu, riw_cpiw, sort_sequence) %>%
  arrange(sort_sequence)
# probably the best
# need to create un_dup variables like I did for cx_item
# FOOD AT HOME is SAF11 & SA111 in the two files.  Why the difference?
# ALCOHOLIC BEVERAGES is SE20 & SAF116 in the two files.  ??
# Maybe it is time to ask the BLS experts
