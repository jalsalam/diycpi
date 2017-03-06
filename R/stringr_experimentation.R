#stringr experimentation
library("stringr")

#See: http://stringr.tidyverse.org/articles/regular-expressions.html

#stringr::fruit is there...

str_detect(fruit, "berry")
str_subset(fruit, "berry")

str_subset(fruit, ".+ berry") #space is the limiter

pattern <- "(.+) (berry)"

str_subset(fruit, pattern) %>%
  str_match(pattern)



strings <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
             "387 287 6718", "apple", "233.398.9187 ", "482 952 3315",
             "239 923 8115 and 842 566 4692", "Work: 579-499-7527", "$1000",
             "Home: 543.355.3679")

phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"

str_extract(strings, phone)
str_match(strings, phone)


############ back to the topic:
library("tidyverse")

riw_strings <- 
  read_table('BLSdatabases/usri_2016.txt', skip = 15, #na = "",
             col_names = c('ex_cat_raw', 'cpiu', 'cpiw')) %>%
  mutate(start_len = str_length(ex_cat_raw)) %>%
  mutate(thedots = str_extract(ex_cat_raw, "\\.{3,}$")) %>%
  mutate(ex_cat_1 = str_replace(ex_cat_raw, "\\.{3,}$", "")) %>%
  mutate(now_len = str_length(ex_cat_1))


#I'm still not really understanding str_match and how to refer to groups. I think part of the problem is controlling precendence between groups

fruit %>%
  str_subset("(..)\\1")

phone <- regex("
  \\(?     # optional opening parens
               (\\d{3}) # area code
               [)- ]?   # optional closing parens, dash, or space
               (\\d{3}) # another three numbers
               [ -]?    # optional space or dash
               (\\d{3}) # three more numbers
               ", comments = TRUE)

str_match("514-791-8141", phone)
str_match("(514)791-8141", phone)
str_match("(514) 791-8141", phone)

phone <- regex("
  \\(?     # optional opening parens
               (\\d{3}) # area code
               [)- ]?   # optional closing parens, dash, or space
               [\\ ]?     # added extra optional space.
               (\\d{3}) # another three numbers
               [ -]?    # optional space or dash
               (\\d{3}) # three more numbers
               ", comments = TRUE)

str_match("(514) 791-8141", phone)