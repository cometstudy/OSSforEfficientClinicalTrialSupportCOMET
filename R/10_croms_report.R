#' @title CROMS Report
#' 
#' @name croms_report
#' 
#' @description The NIH requires monthly uploading of recruitment and enrollment information. This
#' script automatically creates the .csv files to be uploaded. Enrolled is considered date of randomization.
#' Screen fails are considered any pt with a 1 on scrn_elig or a 3,4 on baseline_status.
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas
#' 
#' @section Development notes:
#' 10.21.21 Began developing JC \cr
#' 10.21.21 Implemented in comet_nucleus. JC \cr
#' 10.29.21 Updated export to overwrite current file. JC \cr
#' 
#' 
message("Beginning 10_croms_report")

###### Load Data #####
load(file.path(project_dir,'data','clean','comet_clean.Rdata'))


##### Compute CROMs reports ######
croms_enrolled <- comet %>%
  filter(record_id != 0) %>%
  filter(is.na(randomization_date) == F & is.na(group) == F) %>%
  mutate(`Study ID` = "146904", `Site ID` = 1, `Participant Type` = "Primary Participant") %>%
  select(`Study ID`, `Site ID`, record_id, `Participant Type`, randomization_date, age, gender, race, ethnicity, scrn_address_zip) %>%
  rename(`Participant ID` = record_id, `Study Enrollment Date` = randomization_date, `Age at time of Enrollment` = age, `Gender Choice` = gender, `Race` = race, `Ethnicity` = ethnicity, `Zip Code` = scrn_address_zip)
  
croms_screen_fail <- comet %>%
  filter(record_id != 0) %>%
  filter(scrn_elig == 1 | baseline_status == 3) %>%
  mutate(`Study ID` = "146904", `Site ID` = 1, `Participant Type` = "Primary Participant") %>%
  mutate(`Participant Screen Failure Date` = case_when(scrn_elig == 1 ~ ymd(scrn_date),
                                                       baseline_status == 3  ~ ymd(baseline_date_scrnfail))) %>% 
  select(`Study ID`, `Site ID`, record_id, `Participant Type`, `Participant Screen Failure Date`, scrn_age, gender, race, ethnicity, scrn_address_zip) %>%
  rename(`Participant ID` = record_id, `Age at time of Screening` = scrn_age, `Gender Choice` = gender, `Race` = race, `Ethnicity` = ethnicity, `Zip Code` = scrn_address_zip)

####### Save CROMs reports #########
export(croms_enrolled, file = file.path(data_dir,'croms_report','146904_croms_enrolled.xlsx'), overwrite = TRUE)
export(croms_screen_fail, file = file.path(data_dir,'croms_report','146904_croms_screen_fail.xlsx'), overwrite = TRUE)


