#calc.R
#Do a calculation based on the CES data file

key_series <- cxseries %>%
  filter(category_code == "EXPEND",
         demographics_code == "LB01",
         characteristics_code == "01")

key_series_2 <- key_series %>%
  select(series_id, series_title)

df <- cxdata %>%
  right_join(key_series_2, by = "series_id") 

filter(df, year == 2015) %>%
  ggplot(aes(x=series_title, y=value)) +
  geom_bar(stat="identity")