#' @title Consolidate Exercise Data
#' 
#' @name exercise_data_consolidation
#' 
#' @description This script reads in Fitbit and exercise log data and coalesces into one exercise record. 
#' The highest total from each week is chosen. 
#' 
#' @section Copyright: 
#' Copyright © 2022 University of Kansas
#' 
#' @section Output Dataframes
#' clean_all_ex_data \cr
#' ex_log_issues 
#' 
#' @section clean_all_ex_data
#' activityParentName = exercise type Yoga, Workout (endurance), or Weights \cr
#' final_ex_min = total exercise minutes for given week \cr
#' final_ex_min_source = fitbit or exercise log, whichever is higher \cr
#' prescribed_ex_min = expected exercise minutes for that week \cr
#' Notes: If activityParentName and final_ex_min are NA - that week of exercise data is missing
#' 
#' @section Development notes:
#' 6.29.22 Began developing - no notes yet \cr
#' 9.22.22 Updating Exercise log Issues and completing exercise log QC process. JC \cr
#' 11.3.22 Updated for increased accuracy. Was developing exercise adherence measures and
#' used what I learned joining exercise prescriptions from participant_reports.R There
#' should be no NA activityParentName now. JC \cr
#' 1.5.23 - updated the "Check Number of Sessions" in ex_log_errors. Summing log_aex, log_st, log_rt was summing the whole column and causing false positives. 
#' I changed sum into coalesce to fix. JC \cr

message("Beginning 06_exercise_data_consolidation")

#### Load Data ####
load(file.path(data_dir,'clean','comet_clean.Rdata'))

#### Build dataframe with study id and expected info ####
##8.1.22 Threw error bc final_week is NA when there is no intervention status. Added !is.na(intervention_status). Need to update on coordinator report. 
comet_to_join <- comet %>%
  filter(redcap_event_name == "baseline_arm_1") %>%
  filter(presentweeknum > 1 & !is.na(group) & !is.na(intervention_status)) %>%
  mutate(final_week = case_when(intervention_status == 1 & presentweeknum < 53 ~ presentweeknum,
                                intervention_status == 1 & presentweeknum > 52 ~ 52,
                                intervention_status == 2 ~ 52,
                                intervention_status == 3 | intervention_status == 4 ~ as.numeric(floor(difftime(int_withdrew_date, comet_interventionstart, units = c("weeks"))))+1,
                                intervention_status == 5 ~ as.numeric(floor(difftime(int_losttofollowup_date, comet_interventionstart, units = c("weeks"))))+1)) %>%
  select(comet_study_id, presentweeknum, group, comet_interventionstart, final_week) %>%
  mutate(group_name = case_when(group == 2 ~ "Toning",
                                group == 3 ~ "Endurance", 
                                group == 4 ~ "Resistance",
                                group == 5 ~ "Combo"))

#Build Expected weeks ####
expected_weeks <- data.frame()

for(i in 1:nrow(comet_to_join))
{
  current <- comet_to_join[i,] 
  
  temp_weeks <- 1:(current$final_week-1)
  
  expected_current <- data.frame(comet_study_id = current$comet_study_id, week = temp_weeks)
  
  expected_weeks <- expected_weeks %>% bind_rows(expected_current)
} 




##### Summary Activities DF #####
summary_activities <- activities_all_df %>%
  left_join(., comet_to_join, by = c("comet_study_id")) %>%
  mutate(week = floor(difftime(as.Date(startDate), comet_interventionstart, units = c("weeks")))+1) %>%
  mutate(duration = round(duration / 60 / 1000, digits = 0)) %>%
  filter(week >  0 & week != presentweeknum)  %>%
  filter(activityParentName == "Yoga" | activityParentName == "Weights" | activityParentName == "Workout") %>%
  group_by(week, comet_study_id, activityParentName) %>%
  summarize(ex_min_fitbit = sum(duration)) %>%
  mutate(week = as.numeric(week))


#### Sequester issues in exercise log DF #####
duplicated <- exercise_log %>%
  filter(log_exclude == 0 | is.na(log_exclude)) %>%
  filter((duplicated(cbind(comet_study_id, log_monday, log_weeknum)) | duplicated(cbind(comet_study_id, log_monday, log_weeknum), fromLast=TRUE)))

#' 1.5.23 - updated the "Check Number of Sessions". Summing log_aex, log_st, log_rt was summing the whole column and causing false positives. 
#' I changed sum into coalesce to fix. JC
ex_log_issues <- exercise_log %>%
  #mutate(log_monday = log_monday) %>%
  filter(log_exclude == 0 | is.na(log_exclude)) %>%
  left_join(., comet_to_join, by = "comet_study_id") %>%
  mutate(expected_date = comet_interventionstart+(log_weeknum-1)*7) %>%
  mutate(expected_log_weeknum = floor(difftime(log_monday, comet_interventionstart, units = c("weeks")))+1) %>%
  mutate(issue = case_when(wday(log_monday) != 2 ~ "Date is not a Monday",
                           is.na(log_monday) ~ "No date entered", 
                           is.na(log_weeknum) ~ "No week number enterred",
                           log_monday != expected_date ~ "Monday is different than expected",
                           is.na(log_aex_min) & is.na(log_st_min) & is.na(log_rt_min) ~ "No exercise minutes",
                           log_aex_min > 300 | log_st_min > 300 | log_rt_min > 300 ~ "Exercise minutes seem excessive",
                           log_fname == "" | log_lname == "" ~ "Name is blank",
                           is.na(comet_study_id) ~ "Missing Study ID",
                           is.na(log_group) ~ "Missing Group",
                           record_id %in% duplicated$record_id ~ "Duplicated week",
                           (log_aex_min < 10 & log_aex_min > 0) | (log_st_min < 10 & log_st_min > 0) | (log_rt_min < 10 & log_rt_min > 0) ~ "Minutes seem low",
                           is.na(comet_study_id) | (is.na(log_aex_min) & is.na(log_rt_min) & is.na(log_st_min)) | is.na(log_supervised_num) | is.na(log_independent_num)  ~ "Missing Data",
                           !is.na(log_rt_min) & ((is.na(log_legpress) | is.na(log_lorow) | is.na(log_chest) | is.na(log_hirow) | is.na(log_hammy) | is.na(log_bicep) | is.na(log_tricep) | is.na(log_squat) | is.na(log_calf) | is.na(log_bridge) | is.na(log_curl))) ~ " Missing strength exercise",
                           coalesce(log_aex_min, log_st_min, log_rt_min) > 0 & (log_supervised_num == 0) & (log_independent_num == 0) ~ "Check Number of Sessions Reported",
  )) %>%
  filter(!is.na(issue)) %>%
  select(record_id, comet_study_id, log_monday, log_weeknum, issue, expected_date, expected_log_weeknum, log_cor_need_pt) %>%
  arrange(comet_study_id)

ex_log_clean <- anti_join(exercise_log, ex_log_issues, by = "record_id") %>%
  filter(log_exclude == 0 | is.na(log_exclude)) %>%
  rename("week" = log_weeknum, "Workout" = log_aex_min, "Yoga" = log_st_min, "Weights" = log_rt_min) %>% 
  select(comet_study_id, week, Workout, Yoga, Weights) %>%
  pivot_longer(., cols = c("Workout","Yoga","Weights")) %>%
  rename("activityParentName" = name, "ex_min_log" = value) %>%
  filter(!is.na(ex_min_log))


#On 8.17.22 I commented out select so that we can test other ways of presenting data for the dsmc
#### Combine fitbit and exercise log data #####
combined_exercise_log_clean <- right_join(expected_weeks, summary_activities, by = c("comet_study_id","week")) %>%
  full_join(., ex_log_clean, by = c("comet_study_id","week", "activityParentName")) %>%
  mutate(ex_min_log = as.numeric(ex_min_log)) 
#left_join(expected_weeks, summary_activities) causes extra rows to be created, they are removed here
#select(-ex_min_log, -ex_min_fitbit)


#### Combine Final Exercise minutes with prescription ####
exercise_prescription_to_join <- exercise_prescriptions %>%
  filter(id != 0) %>%
  select(id, week, contains("duration")) %>%
  rename("comet_study_id" = id, "Yoga" = cf_duration, "Workout" = aerobic_duration, "Weights" = st_duration) %>%
  pivot_longer(., cols = c("Yoga","Workout","Weights"), names_to = "activityParentName", values_to = "prescribed_ex_min") %>%
  filter(!is.na(prescribed_ex_min))

final_week_to_join <- comet_to_join %>%
  select(comet_study_id, final_week)

clean_all_ex_data <- full_join(combined_exercise_log_clean, exercise_prescription_to_join, by = c("comet_study_id","week", "activityParentName")) %>%
  mutate(final_ex_min = case_when(ex_min_fitbit >= ex_min_log | is.na(ex_min_log) ~ ex_min_fitbit,
                                  ex_min_log > ex_min_fitbit | is.na(ex_min_fitbit)~ ex_min_log)) %>%
  mutate(final_ex_min_source = case_when(is.na(ex_min_log) & is.na(ex_min_fitbit) ~ "Missing",
                                         ex_min_fitbit >= ex_min_log | (is.na(ex_min_log) & !is.na(ex_min_fitbit)) ~ "Fitbit",
                                         ex_min_log > ex_min_fitbit | (is.na(ex_min_fitbit) & !is.na(ex_min_log)) ~ "Exercise Log")) %>%
  arrange(comet_study_id, week, final_ex_min) %>%
  filter(!(duplicated(cbind(comet_study_id, week)) & is.na(activityParentName))) %>%
  left_join(., final_week_to_join, by = "comet_study_id") %>%
  filter(comet_study_id != 0) %>%
  filter(week <= final_week-1)



#Save output data frames
to_save <- c('clean_all_ex_data','ex_log_issues')
save(list = to_save,file= file.path(clean_data_destination,'ex_min_clean.Rdata'))



