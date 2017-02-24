#calc.R
#Do a calculation based on the CES data file -- makes a simple bar plot.
#This code is out of date -- it doesn't isolate a single display_level of expenditures, so there is overlap in the categories.
#Maybe I'll just delete it.

key_series <- cx_series %>%
  filter(category_code == "EXPEND",
         demographics_code == "LB01",
         characteristics_code == "01")

key_series_2 <- key_series %>%
  select(series_id, series_title)

df <- cx_all_data %>%
  right_join(key_series_2, by = "series_id") 

filter(df, year == 2015) %>%
  ggplot(aes(x=series_title, y=value)) +
  geom_bar(stat="identity")