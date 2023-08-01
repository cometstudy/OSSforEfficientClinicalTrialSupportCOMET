#' @author JC on 20230123
#'
#' @name mri_transfer
#'
#' @title MRI Transfer
#'
#' @section Development:
#' 1.23.23 Began development JC \cr
#' 3.27.23 Needs fixing. Error being thrown. Removed from comet nucleus. JC \cr
#'
#' @description
#' This script is a temporary solution for our MRI issue. It relies on being run from Jon's computer which has access to the R drive
#' This script transfers a list of MP Rage.nii files to the P drive. It denotes which scans have been done and archived in the R drive


if(Sys.info()["user"]=="study_coordinator_2"){
  #' 1.30.23 Adding to denote TP of mri test. JC 
  comet_tp_to_join <- comet %>%
    filter(redcap_event_name == "baseline_arm_1") %>%
    filter(!is.na(comet_study_id)) %>%
    mutate(redid = as.character(redid)) %>%
    select(redid, comet_study_id, comet_baselinemrisched, comet_week52mrisched) %>%
    pivot_longer(cols = contains("sched")) %>%
    mutate(value = as.Date(value))
  
  
  #Fix Issues
  #' 20230407 Removing MRI on 20210903 by 01. This was a test scan before the study. JC
  mprage_files <- data.frame(str_split(gsub("-|__|_",".",list.files(file.path(r_drive))),"[.]", simplify = T)) %>%
    mutate(X1 = as.numeric(X1),
           X2 = as.numeric(X2), 
           X3 = as.numeric(X3))  %>%
    mutate(hsc = "146904") %>%
    mutate(red = case_when(X1 != 146904 & X1 > 1000 ~ X1,
                           X2 > 1000 ~ X2)) %>%
    mutate(comet_study_id = case_when(!is.na(X3) ~ X3,
                                      X2 < 281 ~ X2)) %>%
    mutate(date = ymd(X5)) %>%
    arrange(comet_study_id) %>%
    select(hsc, red, comet_study_id, date) %>%
    left_join(., comet_tp_to_join, by = c("comet_study_id", "date" = "value")) 
    
    
  
  save(mprage_files,
       file = file.path(clean_data_destination, 'comet_mri_clean.Rdata'))
  
}

