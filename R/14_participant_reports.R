#' @title Visual Check of Exercise Log
#' 
#' @name 14_participant_reports
#' 
#' @description This script will produce a markdown document to help review a participants exercise log
#'  All code is stored in data_dir,exercise_log_vis_check
#'  
#'  @section Copyright: 
#' Copyright © 2022 University of Kansas
#' 
#' @section Development notes:
#' 9.28.22 Began Development \cr
#' 
#' @section Criteria:
#' None
#' 

########## COMET Exercise Log to Create #########
participant_list <- comet %>%
  filter(is.na(comet_study_id) == F) %>%
  filter(redcap_event_name == "baseline_arm_1", presentweeknum >= 2, intervention_status == 1) %>%
  arrange(comet_study_id)


######## Create Markdown Document###############

for(i in 1:nrow(participant_list))
{
  current <- participant_list[i,]
  df_to_save <- c('current') 
  save(list = df_to_save, file = file.path(data_dir,'participant_reports','current.Rdata'))
  
  rmarkdown::render(file.path(data_dir,"participant_reports","participant_reports.Rmd"), output_file = file.path(data_dir,"participant_reports","reports",paste0(current$comet_study_id,".html")))
  file.copy(from = file.path(data_dir,"participant_reports","reports",paste0(current$comet_study_id,".html")), to = file.path(report_dir,"participant_reports",paste0(current$comet_study_id,".html")), overwrite = TRUE)
  
  
}



