#' @author EDV on 20210824
#' 
#' @name dxa_aggregation
#' 
#' @title Move and aggregate dxa data
#' 
#' @description 
#' This script is for the COMET study
#' It reads in all available computer-based DXA data files
#' and aggregates them based on ID and timepoint
#' 
#' @section Copyright: 
#' Copyright © 2022 University of Kansas
#' 
#' @section Development
#' 5.10.22 Added dxa_encoding to determine dxa txt encoding. If UTF-16, send an error email and don't continue. If UTF-8, save new file. JC \cr
#' 7.13.22 ID recognition was not working correctly. I think that Eric wrote the script with a different dataset.
#' dxa_ids dataframe was added to recognize study ids, which was added to output df most_recent_dxa_set. JC \cr
#' 7.20.22 My ID recognition didn't work on the shiny server. I rewrote the dxa_ids dataframe on 7.20.22 to function on the server. JC \cr
#' 1.31.23: Adding other dxa variables. Using the mind+soul dxa project I developed. 


#### Directories ####
dxa_destination <- file.path(data_dir,'raw','dxa_data')


#### Identify Most Recent Files ####
#There should always be three dxas 1) Total Body 2) Total Body CoreScan 3) Total Body Comp
#path to most recently saved DXAs
most_recent_comp <- file.info(list.files(file.path(dxa_destination),pattern = ".txt", full.names = TRUE, recursive = T), extra_cols = TRUE) %>%
  rownames_to_column(.,"names")  %>%
  filter(grepl(".txt",names))  %>%
  filter(grepl("Comp", names)) %>%
  mutate(day = as.Date(ctime))  %>%
  arrange(day) %>%
  slice_tail() %>%
  pull(names)

most_recent_core <- file.info(list.files(file.path(dxa_destination),pattern = ".txt", full.names = TRUE, recursive = T), extra_cols = TRUE) %>%
  rownames_to_column(.,"names")  %>%
  filter(grepl(".txt",names))  %>%
  filter(grepl("CoreScan", names)) %>%
  mutate(day = as.Date(ctime))  %>%
  arrange(day) %>%
  slice_tail() %>%
  pull(names)

most_recent_total <- file.info(list.files(file.path(dxa_destination),pattern = ".txt", full.names = TRUE, recursive = T), extra_cols = TRUE) %>%
  rownames_to_column(.,"names")  %>%
  filter(grepl(".txt",names))  %>%
  filter(!grepl("CoreScan", names))  %>%
  filter(!grepl("Comp", names)) %>%
  mutate(day = as.Date(ctime))  %>%
  arrange(day) %>%
  slice_tail() %>%
  pull(names)

if(is_empty(most_recent_comp) | is_empty(most_recent_core) | is_empty(most_recent_total))
{
  stop("Missing a DXA file")
}

most_recent_dxas <- data.frame(file = c(most_recent_comp, most_recent_core, most_recent_total))

message("dxa_encoding")

#Try to guess whether dxa is UTF-16
comp_encoding <- readr::guess_encoding(most_recent_comp)
core_encoding <- readr::guess_encoding(most_recent_core)
total_encoding <- readr::guess_encoding(most_recent_total)

encodings <- data.frame(df = c("Comp","CoreScan","Total"), encoding = c(comp_encoding$encoding[1], core_encoding$encoding[1], total_encoding$encoding[1]))

#If dxa is UTF-16, send a message to the study team to fix. If UTF-8, save and aggregate most recent file
if(any(encodings$encoding == "UTF-16")) {
  
  fix_dxa <- paste(encodings$df[which(encodings$encoding == "UTF-16")], collapse = ' & ')
  stop(paste(fix_dxa,"need to be resaved as UTF-8"))
}
# Email messaging system
# if(grepl("UTF-16",dxa_encoding$encoding)) {
#     text <- paste0('The most recent DXA, (',most_recent_dxa,') is UTF-16. Please resave to UTF-8.')
#     cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
#     mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email.py'), study_coordinator_email, file.path(project_dir,'data','modules','generic_email_module','email.txt'))
#     system(mailcmd)
#   } else {


#### Import Most Recent Files ####    
message("most_recent_dxa_set initialized")   
#Import and join data sets. Remove unnecessary columns.
most_recent_comp_set <- import(most_recent_comp) %>%
  select(-Address, -City, -State, -`Postal Code`, -Country, -contains("Phone"), -contains("Recall"))

most_recent_core_set <- import(most_recent_core) %>%
  select(-Address, -City, -State, -`Postal Code`, -Country, -contains("Phone"), -contains("Recall"))

most_recent_total_set <- import(most_recent_total) %>%
  select(-Address, -City, -State, -`Postal Code`, -Country, -contains("Phone"), -contains("Recall"))

most_recent_dxa_set_temp <- full_join(most_recent_comp_set, most_recent_core_set, by = c('Patient ID','Measure Date', 
                                                                                    'Last Name', 'First Name', 'Most Recent Measurement Date')) %>%
  left_join(., most_recent_total_set, by = c('Patient ID','Measure Date', 
                                             'Last Name', 'First Name', 'Most Recent Measurement Date')) %>%
  mutate(across(-c('Last Name','First Name', 'Patient ID', 'Most Recent Measurement Date','Measure Date'),
                ~ gsub("[[:alpha:]]|[[:space:]]|\\,|[??]|[⁰¹²³]|%","",.))) %>%
  mutate_at(vars(-c('Last Name','First Name', 'Patient ID', 'Most Recent Measurement Date','Measure Date')),
            ~ as.numeric(.)) %>%
  rename(last_name_dxa = `Last Name`,
         first_name_dxa = `First Name`) %>%
  rename_with(., tolower, everything()) 

#### Identify DXA IDs ####   
message("dxa_ids initialized") 

#Recognize IDs
comet_ids_to_join <- comet %>%
  filter(!is.na(comet_study_id)) %>%
  select(redid, comet_study_id)

dxa_ids <- data.frame(str_split(gsub("-|__|_",".",most_recent_dxa_set_temp$`patient id`),"[.]", simplify = T)) %>%
  mutate_all(na_if,"") %>%
  mutate(hsc = "146904") %>%
  mutate(red = case_when(X1 != "146904" & stringr::str_length(X1) > 3 ~ X1,
                         stringr::str_length(X2) > 3 ~ X2))  %>%
  mutate(study_id = case_when(!is.na(X3) ~ X3,
                                    is.na(X3) & stringr::str_length(X2) <= 3 ~ X2)) %>%
  mutate(study_id = str_pad(study_id, width = 3, side = "left", pad = "0")) %>%
  select(hsc, red, study_id) 

#### Fix ID Errors ####
# 4.7.23 DXA on 20230328 was done by 13305_147. Changed id from 134 to 147. JC
dxa_ids_fixed <- dxa_ids %>%
  mutate(study_id = case_when(study_id == "134" & red == "13305" ~ "147",
                              T ~ study_id))


#### Get expected Time Points ####
comet_ids_to_join <- comet %>%
  filter(!is.na(comet_study_id)) %>%
  select(record_id, comet_study_id)
  
dxa_dates <- comet %>%
  select(record_id, redcap_event_name, ex_dxa_date) %>%
  left_join(., comet_ids_to_join, by = "record_id") %>%
  mutate(ex_dxa_date = as.Date(ex_dxa_date))

#### Compile and save complete set ####
#' 4.20.2023: Fixing ID 149 was names as 143 on 4/19/2023 JC
message("bind most_recent_dxa_set and dxa_ids")
most_recent_dxa_set <- bind_cols(dxa_ids_fixed, most_recent_dxa_set_temp) %>%
  mutate(study_id = as.numeric(study_id)) %>%
  mutate("measure date" = mdy(`measure date`)) %>%
  left_join(., dxa_dates, by = c("study_id" = "comet_study_id", "measure date" = "ex_dxa_date")) %>%
  select(hsc,red,study_id, redcap_event_name, everything()) %>%
  mutate(study_id = case_when(study_id == 143 & `measure date` == ymd("20230419") ~ 149,
                              T ~ study_id))
  
    
    
colnames(most_recent_dxa_set)[3:length(colnames(most_recent_dxa_set))] <- gsub(" ","_",colnames(most_recent_dxa_set)[3:length(colnames(most_recent_dxa_set))])
colnames(most_recent_dxa_set)[3:length(colnames(most_recent_dxa_set))] <- gsub("%","pct_",colnames(most_recent_dxa_set)[3:length(colnames(most_recent_dxa_set))])
  
  
##### Save clean workspace ####
save(most_recent_dxa_set, file = file.path(clean_data_destination,'comet_dxa_clean.Rdata'))
  

