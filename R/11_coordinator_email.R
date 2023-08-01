#' @title Coordinator email
#' 
#' @name 11_coordinator_email
#' 
#' @description This script will produce a weekly report for study coordinators and study staff that 
#' includes relavent study maintenance information. All code is stored in data_dir,coordinator_email,coordinator_markdown.
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas
#' 
#' @section Development notes:
#' 11.15.21 Began developing - no notes yet \cr
#' 2.23.22 Updated. Added elements JC \cr
#' 4.11.22 Updated again. JC \cr
#' 
#' @section Criteria:
#' None
#' 
####### Check For 50% Enrolled #######
test_randomized <- sum(!is.na(comet$randomization_date))
percent_randomized <- test_randomized / 280 * 100

if(test_randomized >= 140) {
  text <- paste0('This email is to inform the PIs that we have randomized ',test_randomized,' (',percent_randomized,'%) participants. As per the MOP, 23.6.2 Stopping Rules, should more than 10% of participants experience AEs determined to be related to the study and rated 3 or greater according to CTCAE v5.0 the study will automatically be reviewed by the DSMC for possible termination. Please review the latest dsmc report for AEs.' )
  
  cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
  
  mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email_w_subject_and_pis.py'), study_coordinator_email, file.path(project_dir,'data','modules','generic_email_module','email.txt'), '50%_Enrolled')
  
  system(mailcmd)
  
}


########Email Section ###############

  rmarkdown::render(file.path(data_dir,"coordinator_email","coordinator_markdown.Rmd"), output_file = file.path(data_dir,"coordinator_email","coordinator_markdown.html"))
  file.copy(from = file.path(data_dir,"coordinator_email","coordinator_markdown.html"), to = file.path(report_dir,"coordinator_markdown.html"), overwrite = TRUE)
  

  ##### Mail presets
  
  TO = study_coordinator_email
  
  ##### Send to python to send email
  mailcmd<-paste("py", file.path(data_dir,'coordinator_email','send_email.py'),TO, file.path(root_server_dir,"reports","coordinator_markdown.html"), file.path(root_server_dir,"reports","coordinator_markdown.html"))
  
  if(wday(today()) == 2){
    system(mailcmd)
  }
  
  