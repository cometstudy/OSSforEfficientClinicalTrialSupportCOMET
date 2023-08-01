#' @section Copyright: 
#' Copyright © 2021 University of Kansas

redcap_data_to_export <- comet %>%
  filter(redcap_event_name == "baseline_arm_1", !is.na(comet_study_id)) %>%
  select(record_id, redcap_event_name, comet_study_id,scrn_age, age, race, ethnicity, gender, scrn_elig, baseline_status, comet_baselinegxtsched) %>%
  rename(age_at_rand = age) %>%
  left_join(., physical_assessment_visit, by = c("redcap_event_name","record_id")) %>%
  mutate(baseline_status = case_when(baseline_status == 1 ~ "Currently Screening",
                                     baseline_status == 2 ~ "Eligible to Randomize",
                                     baseline_status == 3 ~ "Screen Fail")) %>%
  arrange(comet_study_id)

export(redcap_data_to_export, file = file.path(report_dir,'data_exports_for_scientists','baseline_gxt_project','redcap_data.csv'))

p_drive_data_to_export <- vo2_summary_measures %>%
  filter(TP == "baseline_arm_1")

export(p_drive_data_to_export, file = file.path(report_dir,'data_exports_for_scientists','baseline_gxt_project','pdrive_data.csv'))

