#' @title Scientist email
#' 
#' @name 09_scientist_email
#' 
#' @description This script will produce a weekly report for scientists and study staff that 
#' includes contents like recruitment feedback and a consort diagram.
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas
#' 
#' @section Development notes:
#' 9.27.21 Began developing - no notes yet \cr
#' 10.1.21 Completed initial build. All of the code occurs in the scientist_markdown.Rmd. The goal of the scientist markdown
#' is to send some basic scientific info in a weekly email. I plan to attach a more in depth DSMB Report Template, which 
#' still needs to be built.JC \cr
#' 10.28.21 Making updates to consort table. JC \cr
#' 11.18.21 Added percents to demographic tables and updated calculation method. Updated graph image. JC \cr
#' 11.22.21 Implemented multiple changes and instituted. Old version can be found in data/scientist_email/archive JC \cr
#' 2.23.22 Initiated running daily and sending email on Mondays. JC \cr
#' 3.31.23 Added option to send securely to stakeholders outside of KU. JC \cr
#' 

########Internal KUMC Email Section ###############

  rmarkdown::render(file.path(data_dir,"scientist_email","scientist_markdown.Rmd"), output_format = "html_document", output_file = file.path(root_server_dir,"reports","scientist_markdown.html"))
  rmarkdown::render(input = file.path(data_dir,"dsmc_report","dsmc_report_open.Rmd"), output_file = file.path(data_dir,"scientist_email","dsmc_report_open.docx"))  

  ##### Mail presets
  
  TO = "study_coordinator_2@kumc.edu"
  
  ##### Send to python to send email
  mailcmd<-paste("py", file.path(data_dir,'scientist_email','send_email.py'),TO, file.path(root_server_dir,'reports','scientist_markdown.html'), file.path(root_server_dir,'reports','scientist_markdown.html'), file.path(data_dir,"scientist_email","dsmc_report_open.docx"))
  
  if(wday(today()) == 2){
    system(mailcmd)
  }
  
######### External Secure Email Section ############
  ##### Mail presets
  
  TO = "study_coordinator_2@kumc.edu"
  
  ##### Send to python to send email
  mailcmd<-paste("py", file.path(data_dir,'scientist_email','send_email_secure.py'),TO, file.path(root_server_dir,'reports','scientist_markdown.html'), file.path(root_server_dir,'reports','scientist_markdown.html'), file.path(data_dir,"scientist_email","dsmc_report_open.docx"))
  
  if(wday(today()) == 2){
    system(mailcmd)
  }
  


