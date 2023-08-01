#' @name decrypt_recrypt
#' 
#' @title Encrypt Fitbit Data
#' 
#' @section Copyright: 
#' Copyright © 2022 University of Kansas
#' 
#' @author Jon Clutton
#' 
#' @description
#' This script either decrypts the fitbit data upon running COMET and encrypts the data at rest. If this script doesn't work properly it can cause major issues.
#' It is advised to save a backup copy. However, even with a backup copy of the authorization codes, if the refresh codes aren't stored properly, it can cause 
#' issues. JC \cr
#' 
#' 
#' @section Development: 
#' 04.01.2022: Began development. JC \cr
#' 
#### Shorten Path ####
fitbit_path <- file.path(project_dir,'data','read_write','read_write')

#### Set csv names
client_info_filename <- file.path(fitbit_path,'client_info.csv')
fitbit_users_filename <- file.path(fitbit_path,'fitbit_users.csv')

#### Get CSVs ####
csv_files <- data.frame(files = list.files(fitbit_path, full.names = T)) %>%
  filter(!grepl("encryptr", files)) %>%
  filter(grepl(".csv",files))

#### Get client info ####
client_info_file <- csv_files %>%
  filter(grepl("client_info", files)) %>%
  pull(.)

client_info_file_encrypt <- findFiles(pattern = "client_info.csv.encryptr", file.path(fitbit_path))

##### Get User Info ####
fitbit_users_file <- csv_files %>%
  filter(grepl("fitbit_users", files)) %>%
  pull(.)
fitbit_users_file_encrypt <- findFiles(pattern = "fitbit_users.csv.encrypt", file.path(project_dir,'data','read_write','read_write'))



#### Depending on what is available either encrypt or decrypt ####
# Decrypt Files - no csv files available and beginning of comet_nucleus, need to be decrypted
if(nrow(csv_files) == 0 & last_up == "00_bootstrap_error.R"){
  decrypt_file(client_info_file_encrypt, file = client_info_filename)
  file.remove(client_info_file_encrypt)
  
  decrypt_file(fitbit_users_file_encrypt, file = fitbit_users_filename)
  file.remove(fitbit_users_file_encrypt)

# Encrypt files - only csv files and end of comet_nucleus, needs to be encrypted
} else if(is.null(client_info_file_encrypt) & is.null(fitbit_users_file_encrypt) & successful_execution == 1) {
  encrypt_file(client_info_filename)
  file.remove(client_info_filename)
  
  encrypt_file(fitbit_users_filename)
  file.remove(fitbit_users_filename)
  
# In case fitbit files aren't encrypted the night before. If beginning of comet_nucleus, we don't want to encrypt
} else if(is.null(client_info_file_encrypt) & is.null(fitbit_users_file_encrypt) & last_up == "00_bootstrap_error.R") {
  
  message("Fitbit files were not encrypted last night, likely due to an error in comet_nucleus. Be sure to check")
  
# Error - Not sure what happened. Throw an error.
} else {
  
  error("Fitbit Encryption Issue")
}

