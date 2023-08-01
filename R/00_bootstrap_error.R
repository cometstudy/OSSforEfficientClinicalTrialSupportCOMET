#' @title Bootstrap Download Error
#' 
#' @name 00_bootstrap error
#' 
#' @description This script checks to see if the REDCap bootstrap is functioning properly. 
#' If not, it throws an error and sends an email to the scientists
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas
#' 
#' @section Development notes:
#' 10.25.21 Began developing bc bootstrap failed yesterday. JC \cr
 

########Email Section ###############

last_testing_visit_bootstrap <- ymd_hms(file.mtime(file.path(external_data_dir,'TestingVisits','phone_screening.csv')))
last_exercise_log_bootsrap <- ymd_hms(file.mtime(file.path(external_data_dir,'ExerciseSelfReport','exercise_log.csv')))

##### Mail presets

TO = "misupport@kumc.edu"
#TO = "study_coordinator_2@kumc.edu"

##### Send to python to send email
if(last_testing_visit_bootstrap < today()-1 | last_exercise_log_bootsrap < today()-1) {
  
  body <- paste0('To whom it may concern. This is an automated email from the COMET Study. It looks like last night the automated bootstrap for REDCap project "COMET - Differential Effects of Exercise Modality" did not execute. Please investigate and reinitiate the bootstrap. Thanks! - COMET Team')
  cat(body, file = file.path(data_dir,'modules','generic_email_module','generic_email.txt'))
  mailcmd <- paste("py", file.path(data_dir,'modules','generic_email_module','send_email.py'),TO, file.path(data_dir,'modules','generic_email_module','generic_email.txt'))
  system(mailcmd)
  stop('REDCap bootstrap did not update')
} else {
  
  message('The REDCap bootstrap is up-to-date.')
}


