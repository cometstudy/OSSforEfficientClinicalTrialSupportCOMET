#' @title Read data, clean, and merge
#' 
#' @name read_clean_merge
#' 
#' @description This script reads in all sources of data in the COMET project and saves a .Rdata file 
#' to be used for all downstream processing scripts.
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas
#' 
#' @section Saved Project-wide Dataframes \cr
#' For data dictionaries see data_dir/data_dictionary \cr
#' steps_all_df: daily steps by comet_study_id \cr
#' hr_all_df: comet_study_id, date, ex_iterative (min), ex_average (min), minutes_worn (min) \cr
#' activities_all_df: comet_study_id, activityParentName (activity type in Fitbit terms),
#' duration (ms), startDate (date), startTime (H:M), end_time (seconds), time_in_zone (min), time_over_zone (min),
#' average (bpm)
#' adverse_event: REDCap adverse_event repeating instrument \cr
#' note_to_file: REDCap note_to_file repeating instrument \cr
#' protocol_deviation: REDCap protocol deviation repeating instrument \cr
#' fidelity_check: REDCAP fidelity_check repeating instrument \cr
#' comet: All non-repeating REDCap instruments joined into one database. Includes some widely used variables 
#' like presentweeknum, group, record_id, redcap_event_name, and intervention related dates like comet_interventionstart,
#' comet_baselinegxtsched, etc \cr
#' exercise_log: Export from "COMET Exercise Log" REDCap project. See project for more details. \cr
#' exercise_prescriptions: All weekly exercise prescriptions. \cr
#' 
#' 
#' 
#' @section Development notes:
#' 5.18.21 Began developing - no notes yet \cr
#' 6.29.21 Added REDCap bootstrap. exercise_log and comet updating nightly \cr
#' 7.23.21 2 global variables are defined in comet - presentweeknum, age,  2 global variables are defines in exercise_prescriptions: hrr_min, and hrr_max.Variables have been verified in excel \cr
#'         presentweeknum = current week of intervention \cr
#'         age = current age \cr
#'         hrr_min = bottom of hrr range \cr
#'         hrr_max = max of hrr range  \cr
#' 7.28.21 Repeating instruments like AEs and exercise prescriptions must be called individually \cr
#' 8.13.21 Saving image was also saving paths, overwriting the current path. Saved image without paths \cr
#' 8.26.21 Modifying to only save dataframes needed i.e., steps_all_df, hr_all_df, activities_all_df, adverse_event, comet, exercise_log, exercise_prescriptions \cr
#' 8.31.21 group = a global group variable, coallesced the group_assign and group_assign_dyad variables \cr
#' 10.22.21 filtered out record_id 8. This was Dreu's test MRI entry. JC \cr
#' 4.27.22 Fixing iDate issues. All dates in exercise_log and comet have been converted to Date. JC \cr
#' 7.12.22 While in the process of creating data dictionaries, I updated the long overdue issues with the 
#' comet dataframe. 1) I removed any duplicating .x or .y variables. 2) I changed the presentweeknum to be in 
#' difftime 'weeks' not difftime 'days'. The output was doublechecked on 7/12/22, but should be tested again as 
#' so many processes depend on presentweeknum. JC \cr
#' #11.8.22: All 64's Treadmill activities before 11/7/22 were actually Workout. JC is modifying in activities_all_df. JC \cr
#' 1.25.23: In developing data_quality project, I realized the psqi wasn't in the COMET database. Just added on 1.25.23 JC \cr
#' 1.31.23: We have added REDCap Missing Data codes to help deal with missing data. These codes are ASKU, NAVE, and NASK. 
#' This causes rio to read in numeric columns as character when they contain a missing data code. All codes are cleaned to -999
#' in 05_clean_redcap.R before being read in here. This keeps character columns and numeric columns as is. JC \cr
#' 3.27.23: All of the redcap imports were pointing to data/r. I've switched them to the external_dir, as this is my bootstrap error check. JC \cr
#' 3.31.23: Switched back redcap imports back to data dir to account for missing data codes. Flow: 05_clean_redcap imports files from external data dir
#' and changes missing data codes to numeric values, then exports to data dir. 06_read_clean merge then imports from data dir. JC \cr


flag_msgs <- 1

message("Beginning 06_read_clean_merge")

external_redcap_dir <- file.path(external_data_dir,'TestingVisits')

##### P: 11132 Step Data ####
#finds last date of download
if(flag_msgs==1){message("Flag date of most recent download")}
steps_last_download = as.data.frame(list.files(file.path(project_dir,'data','raw','step_data'), recursive = TRUE, full.names = TRUE)) %>%
  rename(filename = c(1)) %>%
  mutate(dates = stringr::str_extract(filename, "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")) %>%
  summarize(max = max(dates))

if(flag_msgs==1){message("Flag most recent download")}
#filters all of the files in the folder to only include those of the last download
steps_files <- file.info(list.files(file.path(project_dir,'data','raw','step_data'), full.names = TRUE, recursive = TRUE)) %>%
  filter(ctime >= steps_last_download$max) 

steps_all_list <- lapply(rownames(steps_files), import)

steps_all_df <- bind_rows(steps_all_list) %>%
  mutate(date = ymd(date)) %>%
  select(date, steps, comet_study_id)

if(flag_msgs==1){message("Flag hr data")}
##### P: 11132 HR Data ####
hr_files <- list.files(file.path(project_dir,'data','raw','hr_data'), full.names = TRUE, pattern = ".csv")

hr_all_list <- lapply(hr_files, import)

#A long table of all days for all study ids
hr_all_df <- bind_rows(hr_all_list) %>%
  mutate(date = ymd(date))


##### P: 11132 Activity Data ####

if(flag_msgs==1){message("Flag activity data")}
activities_files <- list.files(file.path(project_dir,'data','raw','activities_data'), full.names = TRUE, pattern = ".csv")

activities_all_list <- lapply(activities_files, import)

#A long table of all days for all study ids
#11.8.22: All 64's Treadmill activities before 11/7/22 were actually Workout. JC is changing it here. JC
#' 20230305: Added statement for 124. We changed her watch from Yoga to Pilates for inclusive reasons. JC
activities_all_df <- bind_rows(activities_all_list) %>%
  mutate(startDate = ymd(startDate)) %>%
  mutate(activityParentName = case_when(comet_study_id == 64 & startDate <= ymd("2022-11-07") & activityParentName == "Treadmill" ~ "Workout",
                                        comet_study_id == 124 & activityParentName == "Pilates" ~ "Yoga",
                                        T ~ activityParentName))

activities_summary <- activities_all_df 

if(flag_msgs==1){message("Flag imports")}

##### REDCap Data Dictionary #####
data_dictionary <- import(file.path(data_dir,'data_dictionary','COMETDifferentialEffectsOfExer_DataDictionary.csv'))

date_fields <- data_dictionary %>% 
  filter(grepl("date",.$`Text Validation Type OR Show Slider Number`)) %>%
  pull(`Variable / Field Name`)

date_ymd <- data_dictionary %>%
  filter(.$`Text Validation Type OR Show Slider Number` == "date_ymd") %>%
  pull(`Variable / Field Name`)

datetime_seconds_ymd <- data_dictionary %>%
  filter(.$`Text Validation Type OR Show Slider Number` == "datetime_seconds_ymd") %>%
  pull(`Variable / Field Name`)

datetime_ymd <- data_dictionary %>%
  filter(.$`Text Validation Type OR Show Slider Number` == "datetime_ymd") %>%
  pull(`Variable / Field Name`)

##### REDCap Field not present in comet db ####
adverse_event <- import(file.path(external_redcap_dir,'adverse_event.csv')) %>%
  filter(record_id != 0) %>%
  mutate(across(where(is.character), ~na_if(., "NAVU"))) %>%
  mutate_at(vars(any_of(date_ymd)), ~as.Date(.,tryFormats=c('%Y-%m-%d','%m-%d-%Y'))) %>%
  mutate_at(vars(any_of(datetime_ymd)), ~ymd_hm(.)) 

note_to_file <- import(file.path(external_redcap_dir,'comet_note_to_file.csv')) %>%
  filter(record_id != 0) %>%
  mutate(across(where(is.character), ~na_if(., "NAVU"))) %>%
  mutate_at(vars(any_of(date_ymd)), ~as.Date(.,tryFormats=c('%Y-%m-%d','%m-%d-%Y'))) %>%
  mutate_at(vars(any_of(datetime_ymd)), ~ymd_hm(.)) 

#' 4.6.2023: Removed protocol deviation on record_id 74 (#28) because was misreported as protocol deviation when it wasn't JC
protocol_deviation <- import(file.path(external_redcap_dir,'comet_protocol_deviation.csv')) %>%
  filter(record_id != 0) %>%
  filter(!(record_id == 74 & redcap_repeat_instance == 1 & redcap_event_name == "safety_and_monitor_arm_1")) %>%
  mutate(across(where(is.character), ~na_if(., "NAVU"))) %>%
  mutate_at(vars(any_of(date_ymd)), ~as.Date(.,tryFormats=c('%Y-%m-%d','%m-%d-%Y'))) %>%
  mutate_at(vars(any_of(datetime_ymd)), ~ymd_hm(.)) 

fidelity_check <- import(file.path(external_redcap_dir,'comet_intervention_fidelity_check.csv')) %>%
  filter(record_id != 0) %>%
  mutate(across(where(is.character), ~na_if(., "NAVU"))) %>%
  mutate_at(vars(any_of(date_ymd)), ~as.Date(.,tryFormats=c('%Y-%m-%d','%m-%d-%Y'))) %>%
  mutate_at(vars(any_of(datetime_ymd)), ~ymd_hm(.)) 

##### REDCap COMET DB #### 
if(flag_msgs==1){message("Flag import1 ")}
barse <- import(file.path(data_dir,'raw','redcap_data','barse.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
cognitive_function_index <- import(file.path(data_dir,'raw','redcap_data','cognitive_function_index.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
cognitive_testing_visit <- import(file.path(data_dir,'raw','redcap_data','cognitive_testing_visit.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
concurrent_medications <- import(file.path(data_dir,'raw','redcap_data','concurrent_medications.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
covid_screen <- import(file.path(data_dir,'raw','redcap_data','covid-19_phone_screening.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
eq5d5l_v10 <- import(file.path(data_dir,'raw','redcap_data','eq5d5l_v10.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
exercise_prescription <- import(file.path(data_dir,'raw','redcap_data','excercise_prescription.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
exse <- import(file.path(data_dir,'raw','redcap_data','exse.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
florida_cognitive_activities_scale <- import(file.path(data_dir,'raw','redcap_data','florida_cognitive_activities_scale.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
godin <- import(file.path(data_dir,'raw','redcap_data','godin.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
inperson_screening <- import(file.path(data_dir,'raw','redcap_data','in-person_screening.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
lse <- import(file.path(data_dir,'raw','redcap_data','lse.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
macarthur_ses <- import(file.path(data_dir,'raw','redcap_data','macarthur_ses.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
mri_visit <- import(file.path(data_dir,'raw','redcap_data','mri_visit.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
nhanes_dsq <- import(file.path(data_dir,'raw','redcap_data','nhanes_dsq.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
pa_self_regulation <- import(file.path(data_dir,'raw','redcap_data','pa_self_regulation.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
perceived_stress_scale_pss10 <- import(file.path(data_dir,'raw','redcap_data','perceived_stress_scale_pss-10.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
phone_call_checkins <- import(file.path(data_dir,'raw','redcap_data','phone_call_checkins.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance) 
phone_screening <- read.csv(file.path(data_dir,'raw','redcap_data','phone_screening.csv'), na.strings = c("","NA")) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
physical_assessment_visit <- import(file.path(data_dir,'raw','redcap_data','physical_assessment_visit.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
pittsburgh_sleep_quality_index_psqi <- import(file.path(data_dir,'raw','redcap_data','pittsburgh_sleep_quality_index_psqi.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
promis_bank_v1.0_anxiety <- import(file.path(data_dir,'raw','redcap_data','promis_bank_v1.0-anxiety.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
promis_bank_v1.0_applied_cog_abilities <- import(file.path(data_dir,'raw','redcap_data','promis_bank_v1.0-applied_cog_abilities.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
promis_bank_v1.0_applied_cog_general_concerns <- import(file.path(data_dir,'raw','redcap_data','promis_bank_v1.0-applied_cog_general_concerns.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
promis_bank_v1.0_depression <- import(file.path(data_dir,'raw','redcap_data','promis_bank_v1.0-depression.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
promis_bank_v1.0_general_life_satisfaction <- import(file.path(data_dir,'raw','redcap_data','promis_bank_v1.0-general_life_satisfaction.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
promis_bank_v1.0_physical_function <- import(file.path(data_dir,'raw','redcap_data','promis_bank_v1.0-physical_function.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
promis_bank_v1.0_social_sat_role <- import(file.path(data_dir,'raw','redcap_data','promis_bank_v1.0-social_sat_role.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
status_summary <- import(file.path(data_dir,'raw','redcap_data','status_summary.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
blood_based_markers <- import(file.path(data_dir,'raw','redcap_data','blood_based_markers.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
motives <- import(file.path(data_dir,'raw','redcap_data','participant_motivation_questionnaire.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
technology_usefulness <- import(file.path(data_dir,'raw','redcap_data','technology_usefulness.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)
prepost_survey <- import(file.path(data_dir,'raw','redcap_data','prepost_survey.csv')) %>%
  select(-redcap_repeat_instrument, -redcap_repeat_instance)

#' In developing data_quality project, I realized the psqi wasn't in the COMET database. Just added on 1.25.23 JC
#' 1.26.22 added blood_based_markers, motives, technology_usefulness, prepost_survey
if(flag_msgs==1){message("Flag join")}
comet <- left_join(barse, cognitive_function_index, by = c("record_id","redcap_event_name")) %>%
  left_join(., cognitive_testing_visit, by = c("record_id","redcap_event_name")) %>%
  left_join(., concurrent_medications, by = c("record_id","redcap_event_name")) %>%
  left_join(., covid_screen, by = c("record_id","redcap_event_name")) %>%
  left_join(., eq5d5l_v10, by = c("record_id","redcap_event_name")) %>%
  left_join(., exse, by = c("record_id","redcap_event_name")) %>%
  left_join(., florida_cognitive_activities_scale, by = c("record_id","redcap_event_name")) %>%
  left_join(., godin, by = c("record_id","redcap_event_name")) %>%
  left_join(.,inperson_screening, by = c("record_id","redcap_event_name")) %>%
  left_join(., lse, by = c("record_id","redcap_event_name")) %>%
  left_join(., macarthur_ses, by = c("record_id","redcap_event_name")) %>%
  left_join(., mri_visit, by = c("record_id","redcap_event_name")) %>%
  left_join(., nhanes_dsq, by = c("record_id","redcap_event_name")) %>%
  left_join(., pa_self_regulation, by = c("record_id","redcap_event_name")) %>%
  left_join(., perceived_stress_scale_pss10, by = c("record_id","redcap_event_name")) %>%
  left_join(., phone_call_checkins, by = c("record_id","redcap_event_name")) %>%
  left_join(., phone_screening, by = c("record_id","redcap_event_name")) %>%
  left_join(., physical_assessment_visit, by = c("record_id","redcap_event_name")) %>%
  left_join(., pittsburgh_sleep_quality_index_psqi, by = c("record_id","redcap_event_name")) %>%
  left_join(., promis_bank_v1.0_anxiety, by = c("record_id","redcap_event_name")) %>%
  left_join(., promis_bank_v1.0_applied_cog_abilities, by = c("record_id","redcap_event_name")) %>%
  left_join(., promis_bank_v1.0_applied_cog_general_concerns, by = c("record_id","redcap_event_name")) %>%
  left_join(., promis_bank_v1.0_depression, by = c("record_id","redcap_event_name")) %>%
  left_join(., promis_bank_v1.0_general_life_satisfaction, by = c("record_id","redcap_event_name")) %>%
  left_join(., promis_bank_v1.0_physical_function, by = c("record_id","redcap_event_name")) %>%
  left_join(., promis_bank_v1.0_social_sat_role, by = c("record_id","redcap_event_name")) %>%
  left_join(., status_summary, by = c("record_id","redcap_event_name")) %>%
  left_join(., blood_based_markers, by = c("record_id","redcap_event_name")) %>%
  left_join(., motives, by = c("record_id","redcap_event_name")) %>%
  left_join(., technology_usefulness, by = c("record_id","redcap_event_name")) %>%
  left_join(., prepost_survey, by = c("record_id","redcap_event_name")) %>%
  mutate(presentweeknum = case_when(is.na(comet_interventionstart) == F ~ as.numeric(floor((as_date(today()) - ymd(comet_interventionstart))/7)+1), #old presentweeknum code. Commented out on 7-12 and kept for now. , 
                                    is.na(comet_interventionstart) == T & is.na(randomization_date)==F ~ 0)) %>%
  mutate(age = floor(lubridate::time_length(difftime(ymd(randomization_date), ymd(scrn_dob)), "years"))) %>%
  mutate(group = coalesce(group_assign, group_assign_dyad2nd, group_assign_randerror)) %>%
  filter(record_id != 8) %>% #remove Dreu's test MRI
  filter(record_id != 0) %>% #remove test gxt and fitibit function
  mutate(scrn_race___1 = 1*scrn_race___1) %>%
  mutate(scrn_race___2 = 2*scrn_race___2) %>%
  mutate(scrn_race___4 = 4*scrn_race___4) %>%
  mutate(scrn_race___8 = 8*scrn_race___8) %>%
  mutate(scrn_race___16 = 16*scrn_race___16) %>%
  mutate(scrn_race___32 = 32*scrn_race___32) %>%
  mutate(scrn_race___64 = 64*scrn_race___64) %>%
  mutate(scrn_race___256 = 256*scrn_race___256) %>%
  mutate(race_added = (scrn_race___1 + scrn_race___2 + scrn_race___4 + scrn_race___8 + scrn_race___16 + scrn_race___32 + scrn_race___64 + scrn_race___256)) %>%
  mutate(race = case_when(race_added == 0 ~ NA_character_,
                          race_added == 1 ~ "White",
                          race_added == 2 ~ "Black, African American, or African",
                          race_added == 4 ~ "Asian",
                          race_added == 8 ~ "American Indian or Alaska Native",
                          race_added == 16 ~ "Native Hawaiian or Other Pacific Islanders",
                          race_added == 32 ~ "None of these fully describe me",
                          race_added == 64 ~ "Prefer not to answer",
                          race_added == 256 ~ "Middle Eastern or North African",
                          race_added == 5 ~ "Asian and White",
                          race_added == 3 ~ "Black, African American, or African and White",
                          race_added == 20 ~ "Asian and Native Hawaiian or Other Pacific Islanders",
                          is.na(race_added) ~ NA_character_,
                          TRUE ~ "Other - need to update")) %>%
  mutate(gender = case_when(scrn_gender == 1 ~ "Man",
                            scrn_gender == 2 ~ "Woman",
                            TRUE ~ NA_character_)) %>%
  mutate(ethnicity  = dplyr::recode(scrn_ethn,
                                    `2` = "Hispanic or Latino",
                                    `1` = "Not Hispanic or Latino",
                                    `-1`= "Refused",
                                    .default = "Other - need to update")) %>%
  mutate(across(where(is.character), ~na_if(., "NAVU"))) %>%
  mutate_at(vars(any_of(date_ymd)), ~ymd(.)) %>%
  mutate_at(vars(any_of(datetime_ymd)), ~ymd_hm(.)) 


if(flag_msgs==1){message("Finished comet join")}




# # #for testing same day changes
# comet <- import("C:/Users/study_coordinator_2/Downloads/COMET_10.12.22.csv") %>%
# mutate(presentweeknum = case_when(is.na(comet_interventionstart) == F ~ floor((as_date(today()) - ymd(comet_interventionstart))/7)+1, #old presentweeknum code. Commented out on 7-12 and kept for now. , 
#                                   is.na(comet_interventionstart) == T & is.na(randomization_date)==F ~ 0)) %>%
#   mutate(age = floor(lubridate::time_length(difftime(ymd(randomization_date), ymd(scrn_dob)), "years"))) %>%
#   mutate(group = coalesce(group_assign, group_assign_dyad2nd, group_assign_randerror)) %>%
#   filter(record_id != 8) %>% #remove Dreu's test MRI
#   filter(record_id != 0) %>% #remove test gxt and fitibit function
#   mutate(scrn_race___1 = 1*scrn_race___1) %>%
#   mutate(scrn_race___2 = 2*scrn_race___2) %>%
#   mutate(scrn_race___4 = 4*scrn_race___4) %>%
#   mutate(scrn_race___8 = 8*scrn_race___8) %>%
#   mutate(scrn_race___16 = 16*scrn_race___16) %>%
#   mutate(scrn_race___32 = 32*scrn_race___32) %>%
#   mutate(scrn_race___64 = 64*scrn_race___64) %>%
#   mutate(scrn_race___256 = 256*scrn_race___256) %>%
#   mutate(race_added = (scrn_race___1 + scrn_race___2 + scrn_race___4 + scrn_race___8 + scrn_race___16 + scrn_race___32 + scrn_race___64 + scrn_race___256)) %>%
#   mutate(race = case_when(race_added == 0 ~ NA_character_,
#                           race_added == 1 ~ "White",
#                           race_added == 2 ~ "Black, African American, or African",
#                           race_added == 4 ~ "Asian",
#                           race_added == 8 ~ "American Indian or Alaska Native",
#                           race_added == 16 ~ "Native Hawaiian or Other Pacific Islanders",
#                           race_added == 32 ~ "None of these fully describe me",
#                           race_added == 64 ~ "Prefer not to answer",
#                           race_added == 256 ~ "Middle Eastern or North African",
#                           race_added == 5 ~ "Asian and White",
#                           race_added == 3 ~ "Black, African American, or African and White",
#                           is.na(race_added) ~ NA_character_,
#                           TRUE ~ "Other - need to update")) %>%
#   mutate(gender = case_when(scrn_gender == 1 ~ "Man",
#                             scrn_gender == 2 ~ "Woman",
#                             TRUE ~ NA_character_)) %>%
#   mutate(ethnicity  = dplyr::recode(scrn_ethn,
#                                     `2` = "Hispanic or Latino",
#                                     `1` = "Not Hispanic or Latino",
#                                     `-1`= "Refused",
#                                     .default = "Other - need to update")) %>%
#   mutate(across(where(is.character), ~na_if(., "NAVU"))) %>%
#   mutate_at(vars(any_of(date_ymd)), ~ymd(.)) %>%
#   mutate_at(vars(any_of(datetime_ymd)), ~ymd_hm(.)) 


#Note to Jon. This had been commented out but it was preventing the left_join below from having a second data frame to join.
#EDV uncommented and tested on 1/23/22
  comet_hr_calculation_values <- comet %>%
    filter(redcap_event_name == "baseline_arm_1") %>%
    select(comet_study_id, presentweeknum, meds_betablocker, age, ex_rest_hr, ex_gxt_maxhr) 




if(flag_msgs==1){message("Flag import exercise log")}
##### REDCap Exercise Log DB #####
ex_log_sc_instrument <- import(file.path(external_data_dir,'ExerciseSelfReport','study_coordinator_instrument.csv')) 
  
exercise_log <- import(file.path(external_data_dir,'ExerciseSelfReport','exercise_log.csv')) %>%
  mutate(log_today_date = ymd(log_today_date)) %>%
  mutate(log_monday = ymd(log_monday)) %>%
  left_join(., ex_log_sc_instrument, by = "record_id")
  
if(flag_msgs==1){message("Flag import exercise prescriptions")}
##### P: 146904 Exercise Prescription ####
exercise_prescription_files <- list.files(file.path(dirname(external_data_dir),'exercise_prescriptions'), full.names = TRUE, pattern = ".xlsx")
exercise_prescription_all_list <- lapply(exercise_prescription_files, import)

if(flag_msgs==1){message("Flag exercise prescriptions imported")}

exercise_prescription_all_df <- bind_rows(exercise_prescription_all_list) 

if(flag_msgs==1){message("Flag exercise prescriptions bound")}


exercise_prescriptions <- left_join(exercise_prescription_all_df, comet_hr_calculation_values, by = c('id' = 'comet_study_id')) %>%
  mutate(hrr_min =  case_when(meds_betablocker == 1 ~ round((min_intensity * (164- .7*age - ex_rest_hr)) + ex_rest_hr),
                              meds_betablocker == 0 ~ round((min_intensity * (ex_gxt_maxhr - ex_rest_hr)) + ex_rest_hr))) %>%
  mutate(hrr_max = case_when(meds_betablocker == 1 ~ round((max_intensity * (164- .7*age - ex_rest_hr)) + ex_rest_hr),
                             meds_betablocker == 0 ~ round((max_intensity * (ex_gxt_maxhr - ex_rest_hr)) + ex_rest_hr)))

if(flag_msgs==1){message("Flag exercise prescriptions joined and curated")}


##### Save clean workspace ####
to_save <- c('steps_all_df','hr_all_df', 'activities_all_df','adverse_event','comet','exercise_log','exercise_prescriptions','note_to_file','protocol_deviation','fidelity_check','covid_screen','phone_screening')
save(list = to_save,file= file.path(clean_data_destination,'comet_clean.Rdata'))
#Changed to only save relevant dataframes. No longer need to remove lists
#rm(list=to_save)

if(flag_msgs==1){message("Flag data saved out")}


