#main.R

#Contains the steps necessary to run the project

#load libraries, function files, etc
source('R/setup.R')
source('R/import_fns.R') # most not yet ready for primetime.
# get_bls_tables() # downloads all of 'em
# get_bls_tables("cu_item") # downloads one

#read in CES and CPI data
source('R/import_cex_data.R')
source('R/import_ces_data.R.R')
source('R/import_riw_data_alt.R')

#do some calculations based on the data
source('R/calc_CPI_1.R', echo=TRUE) #reproduce some values from CPI detailed report
