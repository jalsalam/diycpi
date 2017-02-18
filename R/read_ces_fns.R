#read_ces_fns.R
#Functions to facilitate reading CES data files

read_ces_ftp_file <- function(filename){
  raw_data <- read_tsv(file = filename)
  
  data <- raw_data
  
  data
}

#test: allseries <- read_ces_ftp_file("extdata/CES/FTP/cx.series")