#main.R

#Contains the steps necessary to run the project

#load libraries, function files, etc
source('R/setup.R')

#read in CES and CPI data
source('R/import_cex_data.R')
source('R/import_ces_data.R.R')
source('R/import_riw_data_alt.R')

#do some calculations based on the data
source('R/calc_CPI_1.R', echo=TRUE) #reproduce some values from CPI detailed report
