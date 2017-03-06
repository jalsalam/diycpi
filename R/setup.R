#setup.R

#First time: 
# install.packages("tidyverse")
# install.packages("testthat")
# install.packages("gsheet")

library("tidyverse") 
library("readxl")
library("testthat") # for unit testing functions
library("assertthat") #for error-checking
library("gsheet") # for reading google sheets using the sharing URL
library("stringr") #string manipulation functions