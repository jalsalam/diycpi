#calc_CPI_2.R
#Exploring the housing aspect of CPI

# item_code SAH1 = Shelter (not all of housing, which includes energy, furniture, etc)

# series_id CUUR0000SAH1 = Shelter, all areas, unadjusted, monthly data

housing_items <- c("SAH", "SAH1", "SAH2", "SAH3")

housing_series <- cu_series %>%
  filter(area_code == "0000",           #U.S. city average
         item_code %in% housing_items,
         base_period == "1982-84=100",
         seasonal %in% c("U"),     # S == seaonally adjusted, U = unadjusted
         periodicity_code %in% c("R")) # R == Monthly, S == Semi-Annual
  
cu_data_current %>% count(year) #starts in 1997 for some reason

cu_data_current %>%
  inner_join(housing_series, by = "series_id") %>%
  filter(period == "M13") %>%
  select(year, item_code, value) %>%
  spread(item_code, value) %>% View()

#2016 year-average value for SAH1 is 288.230, meaning that shelter cost increased by +188% since 1982-84.
<<<<<<< HEAD
# Nabeel:  I am pretty sure you need to take ratios.  288.230/100 = 2.882 so the shelter cost increased 288%
# Jameel: I stand by my original, but I can see the confusion because the phrasing "increased by X percent" requires subtracting 100 percent from the ratio. For example, if index value is 150, you could say that as:
# 1) ratio of indexes is 150 / 100 = 1.5
# 2) new prices are 150% of the original value
# 3) prices have increased by +50% (my phrasing) 
=======
>>>>>>> parent of 4fc9c5a... comment on interpretation of results

#To compare, Robert Shiller's 20-city house price index ratio was 3.6, otherwise expressed as increase of +260% from 1983-current. So the housing price index is about 1/3 higher than the shelter cost index. Since housing is 40% of the index, this seems like a noticable difference.
#Link to Shiller housing data here: http://www.econ.yale.edu/~shiller/data/Fig3-1.xls (from : http://www.econ.yale.edu/~shiller/data.htm)

