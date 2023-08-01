#' @author JC on 20230131
#'
#' @name clean_redcap
#'
#' @title Clean REDCap
#'
#' @section Development:
#' 1.31.23 Began development JC \cr
#' 1.31.23 Looks like it's working correctly. Adapting 06_read_clean_merge. JC \cr
#'
#' @description
#' We have started introducing a data quality email. With the data quality, we need a way to clean and acknowledge missing data. We plan to use 
#' the REDCap Missing Data Codes. \cr
#' NAVU - Not available \cr
#' ASKU - Asked but unknow \cr
#' NASK - Not asked \cr
#' This will introduce characters into numeric data. This script cleans all data and converts NAVE, ASKU, NASK to -999 so that it can be read as either
#' numberic or character
#' 
#' @section Copyright: 
#' Copyright © 2023 University of Kansas
#' 

#### Set REDCAp Folders #####
redcap_import_folder <- file.path(external_data_dir,'TestingVisits')
redcap_export_folder <- file.path(data_dir,'raw','redcap_data')

#### Read in Files ####
redcap_files <- data.frame(files = list.files(redcap_import_folder, full.names = T)) %>%
  mutate(basename = basename(files)) %>%
  filter(!str_detect(basename,"prescription"))

#### Convert REDCap files #####
for(i in 1:nrow(redcap_files)) {
  current_file <- redcap_files$files[i]
  
  file_name <- redcap_files$basename[i]
  
  tmp_file_check <- import(current_file) %>%
    mutate(across(everything(), ~as.character(.))) %>%
    pivot_longer(-c("record_id","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance")) %>%
    filter(grepl("NAVU|ASKU|NASK",value))
  
  print(paste("Number of NAVU|ASKU|NASK:",nrow(tmp_file_check),"in",file_name))
  
  tmp_file <- import(current_file) %>%
    mutate(across(where(is.character), ~str_replace_all(.x,"NAVU","-999"))) %>%
    mutate(across(where(is.character), ~str_replace_all(.x,"ASKU","-998"))) %>%
    mutate(across(where(is.character), ~str_replace_all(.x,"NASK","-997")))
  
  # tmp_file_check_2 <- tmp_file %>%
  #   mutate(across(everything(), ~as.character(.))) %>%
  #   pivot_longer(-c("record_id","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance")) %>%
  #   filter(grepl("NAVU|ASKU|NASK",value))
  # 
  # print(paste("Number of NAVU|ASKU|NASK:",nrow(tmp_file_check_2),"in",file_name))
  
  export(tmp_file, file = file.path(redcap_export_folder,file_name))
  
}
