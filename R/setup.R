#setup.R

#First time: 
# install.packages("tidyverse")
# install.packages("testthat")
# install.packages("gsheet")

library("rlang")
library("tidyverse") 
library("readxl")
library("testthat") # for unit testing functions
library("assertthat") #for error-checking
library("stringr") #string manipulation functions, installed in tidyverse but not loaded by default
library("purrrlyr") #for data-frame-related purrr functions. Probably unnecessary, but I need to learn pmap still.
library("forcats")
library("shiny")

stopifnot(packageVersion("dplyr") >= '0.6.0') #uses new nse features from dplyr/rlang