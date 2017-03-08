# Goal is to have a xwalk between weights and price indices

# riw_cu_item_xwalk2 below is probably the best
# it would benefit from manual editing I believe
# or maybe it is time to ask the BLS experts

cx_item <- read_tsv("output/cx_edited.item", skip = 1) %>%
  arrange(sort_sequence)
cu_item <-read_tsv("BLSdatabases/cu_edited.item") %>%
  arrange(sort_sequence) %>%
  mutate(item_name = toupper(item_name))
riw_1987 <- read_fwf("BLSDatabases/usri1987.txt", fwf_widths(c(9, 46, 7, 10), c("item_code", "item_name", "riw_cpiu", "riw_cpiw")), skip = 10)


# matching on description
cx_cu_item_xwalk <- full_join(cx_item, cu_item, by = c("item_text" = "item_name")) 

# matching on item_code
riw_cu_item_xwalk <- full_join(riw_1987, cu_item, by = "item_code") %>%
  select(item_code, item_name.x, item_name.y, riw_cpiu, riw_cpiw, sort_sequence) %>%
  arrange(sort_sequence)

# matching on item_name
riw_cu_item_xwalk2 <- full_join(riw_1987, cu_item, by = "item_name") %>%
  select(item_name, item_code.x, item_code.y, riw_cpiu, riw_cpiw, sort_sequence) %>%
  arrange(sort_sequence)
