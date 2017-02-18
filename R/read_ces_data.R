#read_ces_data.R
#Read in appropriate CES data files

cx_files <- list.files("extdata/CES/FTP")
cx_files_with_path <- paste("extdata/CES/FTP/", cx_files, sep = "")

cxdata <- read_ces_ftp_file("extdata/CES/FTP/cx.data.1.AllData")
cxseries <- read_ces_ftp_file("extdata/CES/FTP/cx.series")

