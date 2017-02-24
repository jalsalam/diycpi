# calc_CPI_1.R
# Reproduce some key CPI data from reports.

# From the Dec 2016 CPI detailed report: https://www.bls.gov/cpi/cpid1612.pdf

############################

# Isolating the values of interest

feature_items <- c("SA0", "SA0E", "SAH1", "SAF1", "SETB01") #All, energy, food, shelter, gasoline

cu_item %>%
  filter(item_code %in% feature_items)

#item_code            item_name display_level selectable sort_sequence
#<chr>                <chr>         <int>      <lgl>         <int>
#1       SA0            All items             0       TRUE             1
#2      SA0E               Energy             1       TRUE           374
#3      SAF1                 Food             1       TRUE             4
#4      SAH1              Shelter             1       TRUE           137
#5    SETB01 Gasoline (all types)             3       TRUE           222

cu_series %>%
  filter(area_code == "0000",           #U.S. city average
         item_code %in% feature_items,
         base_period == "1982-84=100",
         seasonal %in% c("S", "U"),                # S == seaonally adjusted, U = unadjusted
         periodicity_code == "R"        # R == Monthly, S == Semi-Annual
  )

#series_id area_code item_code seasonal periodicity_code base_code base_period
#<chr>     <chr>     <chr>    <chr>            <chr>     <chr>       <chr>
#1     CUSR0000SA0      0000       SA0        S                R         S 1982-84=100
#2    CUSR0000SA0E      0000      SA0E        S                R         S 1982-84=100
#3    CUSR0000SAF1      0000      SAF1        S                R         S 1982-84=100
#4    CUSR0000SAH1      0000      SAH1        S                R         S 1982-84=100
#5  CUSR0000SETB01      0000    SETB01        S                R         S 1982-84=100
#6     CUUR0000SA0      0000       SA0        U                R         S 1982-84=100
#7    CUUR0000SA0E      0000      SA0E        U                R         S 1982-84=100
#8    CUUR0000SAF1      0000      SAF1        U                R         S 1982-84=100
#9    CUUR0000SAH1      0000      SAH1        U                R         S 1982-84=100
#10 CUUR0000SETB01      0000    SETB01        U                R         S 1982-84=100

##########################


# The CPI for All Urban Consumers (CPI-U) increased 0.3 percent in December on a seasonally adjusted basis.
cu_data %>%
  filter(series_id == "CUSR0000SA0",
         year == 2016,
         period %in% c("M11", "M12")) %>%
  group_by(series_id) %>%
  mutate(month_chg = 100 * (value / lag(value) - 1))

# month_chg for M12 is 0.256, matching 0.3 percent above

###########################

# over the last 12 months, the all items index rose 2.1 percent before seasonal adjustment.
cu_data %>%
  filter(series_id == "CUUR0000SA0",
         year %in% 2000:2016,
         period == "M12") %>%
  group_by(series_id) %>%
  mutate(year_chg = 100 * (value / lag(value) - 1))

# year_chg for 2016 is 2.074, matching 2.1 percent above

#############################

#Continuing their recent trends, shelter and gasoline indexes increased in December and were largely responsible for the seasonally adjusted all items increaes. Shelter rose 0.3% in Dec, gasoline increased 3.0%. Food index was unchanged, and energy index advanced 1.5% in December, primarily due to the gasoline index.

cu_data %>%
  filter(series_id %in% c("CUSR0000SA0E", "CUSR0000SAF1", "CUSR0000SAH1", "CUSR0000SETB01"),
         year == 2016,
         period %in% c("M11", "M12")) %>%
  group_by(series_id) %>%
  mutate(month_chg = 100 * (value / lag(value) - 1)) %>%
  left_join(select(cu_series, series_id, item_code), by = "series_id") %>%
  left_join(select(cu_time, item_code, item_name), by = "item_code") %>%
  select(series_id, item_name, year, period, month_chg)

# Values match paragraph above

