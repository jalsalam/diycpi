#main.R

#Contains the steps necessary to run the project

#load libraries, function files, etc
source('R/setup.R')

#read in CES data
source('R/read_ces_fns.R')
source('R/read_ces_data.R')

#do a calc based on it
source('R/calc.R', echo=TRUE)
