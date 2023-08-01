#' @title Data Quality email
#' 
#' @name 15_data_quality_email
#' 
#' @description This script will produce a weekly report for statistician and unblinded
#' study staff to show missing data and data quality
#' 
#' @section Copyright: 
#' Copyright © 2023 University of Kansas
#' 
#' @section Development notes:
#' 1.23.2023 Copied from 09_scientist_email. Began development. JC \cr
#' 2.27.2023 Finished initial draft of data_quality_markdown. Beginning email. JC \cr
#' 


########Email Section ###############

  rmarkdown::render(file.path(data_dir,"data_quality_email","data_quality_markdown.Rmd"), output_format = "html_document", output_file = file.path(data_dir,"data_quality_email","data_quality_markdown.html"))
  file.copy(from = file.path(data_dir,"data_quality_email","data_quality_markdown.html"), to = file.path(report_dir,"data_quality_markdown.html"), overwrite = TRUE)

  rmarkdown::render(file.path(data_dir,"data_quality_email","data_quality_markdown_blood+mri.Rmd"), output_format = "html_document", output_file = file.path(data_dir,"data_quality_email","data_quality_markdown_markdown_blood+mri.html"))
  file.copy(from = file.path(data_dir,"data_quality_email","data_quality_markdown_blood+mri.html"), to = file.path(report_dir,"data_quality_markdown_blood+mri.html"), overwrite = TRUE)
  file.copy(from = file.path(data_dir,"data_quality_email","data_quality_markdown_blood+mri.html"), to = file.path(report_dir,"data_exports_for_scientists","baseline_gxt_project","data_quality_markdown_blood+mri.html"), overwrite = TRUE)
  
  
  ##### Mail presets
  
  TO = study_coordinator_email
  
  ##### Send to python to send email
  mailcmd<-paste("py", file.path(data_dir,'data_quality_email','send_email.py'),TO, file.path(root_server_dir,'reports','data_quality_markdown.html'), file.path(root_server_dir,'reports','data_quality_markdown.html'))
  
  if(wday(today()) == 2){
    system(mailcmd)
  }
  



