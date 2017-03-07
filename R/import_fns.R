#import_fns.R
#Reorganization of functions to control downloading and importing of BLS files

get_file_data <- function(filepath = "bls_files.csv"){
  # reads in bls_files.csv to get reference information on appropriate files.
  # bls_files.csv should be manually edited to reflect file names/locations/etc where files are found.
  
  bls_files <- read_csv(filepath, col_types = "cccccccc") %>%
    mutate(url = str_c(url_folder, file),
           filepath = str_c("BLSdatabases/", file))
  
  # return df with file data
}

bls_files <- get_file_data("bls_files.csv")

get_bls_tables <- function(tables = "all") {
  # go through a list of files, check to see if they are already present, if not download them.
  # assumes global access to bls_files
  
  if (tables == "all") {
    bls_status <- bls_files %>%
      filter(!file.exists(filepath)) %>%
      by_row(~download.file(.$url, .$filepath), .to = "status")   
  } else {
    bls_status <- bls_files %>%
      filter(name %in% tables) %>%
      filter(!file.exists(filepath)) %>%
      by_row(~download.file(.$url, .$filepath), .to = "status")   
  }

  if (nrow(bls_status) > 0) {
    bls_status %<>%
      mutate(msg = if_else(status==0,
                           str_c(file, " successfully downloaded."),
                           str_c(file, " failed to download."))) %>%
      by_row(~print(.$msg))
  } else {
    print("All files found already, so none downloaded.")
  }
  
  
  bls_status
}

import_cex <- function() {
  # import an individual cx file
  
  
}

import_cpi <- function() {
  # import an individual cpi file (cu and cw series)
  
  
}

import_riw <- function() {
  # import riw files, dispatching to new/old versions
    item_name_lookup <- cu_item %>%
    select(item_name, item_code) %>%
    mutate(item_name = str_trim(item_name)) #whitespace was messing up some matches

    
}

import_riw_new <- function(year) {
  # import a relative weights file (cpi-u and cpi-w) in the newer format
  riw_filename <- paste('BLSdatabases/usri_', year, '.txt', sep="")
  
  riw_table <- read_table(riw_filename, skip = 15, na = "",
                          col_names = c('ex_cat_raw', 'cpiu', 'cpiw'),
                          col_types = "cdd") %>%
    
    mutate(ex_cat = str_trim(str_replace_all(ex_cat_raw, '\\.', ''))) %>%
    mutate(ex_cat = 
             if_else(is.na(lag(cpiu)) & is.na(lag(cpiw)) & !is.na(lag(ex_cat)), 
                     str_c(lag(ex_cat), ex_cat, sep=" "), 
                     ex_cat)) %>%
    mutate(ex_cat = str_trim(ex_cat)) %>% #whitespace was messing up some matches
    filter(!is.na(cpiu) | !is.na(cpiw)) %>%
    select(ex_cat, cpiu, cpiw) %>%
    rename(item_name = ex_cat) %>%
    mutate(riw_year = year) %>%
    left_join(item_name_lookup, by = "item_name") %>%
    group_by(item_name, item_code, riw_year) %>% slice(1) %>% #if duplicate names, take first one
    ungroup()
  
  riw_table #return this
  
}

import_riw_old <- function() {
  # import a relative weights file (cpi-u and cpi-w) in the older format
  
}