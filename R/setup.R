#setup.R

#First time: 
# install.packages("tidyverse")
# install.packages("testthat")
# install.packages("gsheet")

library("tidyverse") 
library("readxl")
library("testthat") # for unit testing functions
library("assertthat") #for error-checking
library("gsheet") # for reading google sheets using the sharing URL. JRA: really necessary?
library("stringr") #string manipulation functions, installed in tidyverse but not loaded by default
library("purrrlyr") #for data-frame-related purrr functions. Probably unnecessary, but I need to learn pmap still.