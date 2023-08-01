#' @title Exercise Adherence
#' 
#' @name exercise_adherence
#' 
#' @description Testing exercise adherence. Script was built off of
#' 06_exercise_consolidation. Much of the outline is the same. It starts to change
#' towards the bottom when I create exercise adherence scores. Outputs to data_dir/dsmc_report 
#' 
#' @section Output CSVs
#' time_based_adherence
#' performance_based_adherence
#' 
#' @section Copyright: 
#' Copyright © 2022 University of Kansas
#' 
#' 
#' @section Development notes:
#' 11.2.22 Began developing - no notes yet \cr
#' 11.3.22 Completed. Sending to PIs. Output to dsmc_report. JC \cr
#' 11.8.22 Got feedback from PIs. Made a change to yoga performance. Chose highest between number of 
#' exercise sessions reported from the exercise log vs the Fitbit. See yoga_fitbit, yoga_ex_log, and yoga 
#' for details. JC \cr


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


#### Create dataframes to randomize study ID in output dataframes ####
random_ids <- comet_to_join %>%
  select(comet_study_id) %>%
  mutate(random_id = sample(nrow(.), size = nrow(.), replace = F)) 

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
                           !is.na(log_rt_min) & ((is.na(log_legpress) | is.na(log_lorow) | is.na(log_chest) | is.na(log_hirow) | is.na(log_hammy) | is.na(log_bicep) | is.na(log_tricep) | is.na(log_squat) | is.na(log_calf) | is.na(log_bridge) | is.na(log_curl))) ~ " Missing strength exercise")) %>%
  filter(!is.na(issue)) %>%
  select(record_id, comet_study_id, log_monday, log_weeknum, issue, expected_date, expected_log_weeknum, log_cor_need_pt) %>%
  arrange(comet_study_id)

#### Split ex_log_clean so that I can do some filtering with it later. ####

ex_log_clean_temp <- anti_join(exercise_log, ex_log_issues, by = "record_id") %>%
  filter(log_exclude == 0 | is.na(log_exclude)) 

ex_log_clean <- ex_log_clean_temp %>%
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
#Modified so that data is missing less
exercise_prescription_to_join <- exercise_prescriptions %>%
  filter(id != 0) %>%
  select(id, week, contains("duration")) %>%
  rename("comet_study_id" = id, "Yoga" = cf_duration, "Workout" = aerobic_duration, "Weights" = st_duration) %>%
  pivot_longer(., cols = c("Yoga","Workout","Weights"), names_to = "activityParentName", values_to = "prescribed_ex_min") %>%
  filter(!is.na(prescribed_ex_min))

final_week_to_join <- comet_to_join %>%
  select(comet_study_id, final_week, group)

#### Create time based adherence scores ####
adherence_score_temp <- full_join(combined_exercise_log_clean, exercise_prescription_to_join, by = c("comet_study_id","week", "activityParentName")) %>%
  mutate(final_ex_min_highest = case_when(ex_min_fitbit >= ex_min_log | is.na(ex_min_log) ~ ex_min_fitbit,
                                  ex_min_log > ex_min_fitbit | is.na(ex_min_fitbit)~ ex_min_log)) %>%
  mutate(final_ex_min_pref_log = case_when(!is.na(ex_min_log) ~ ex_min_log,
                                  T ~ ex_min_fitbit)) %>%
  mutate(final_ex_min_source_highest = case_when(is.na(ex_min_log) & is.na(ex_min_fitbit) ~ "Missing",
                                         ex_min_fitbit >= ex_min_log | (is.na(ex_min_log) & !is.na(ex_min_fitbit)) ~ "Fitbit",
                                         ex_min_log > ex_min_fitbit | (is.na(ex_min_fitbit) & !is.na(ex_min_log)) ~ "Exercise Log")) %>%
  mutate(final_ex_min_source_pref_log = case_when(is.na(ex_min_log) & is.na(ex_min_fitbit) ~ "Missing",
                                         !is.na(ex_min_log) ~ "Exercise Log",
                                         is.na(ex_min_log) ~ "Fitbit")) %>%
  arrange(comet_study_id, week)  %>%
  filter(!(duplicated(cbind(comet_study_id, week)) & is.na(activityParentName))) %>%
  left_join(., final_week_to_join, by = "comet_study_id") %>%
  filter(comet_study_id != 0) %>%
  filter(week <= final_week-1) %>%
  mutate(adherence_score_highest = case_when(activityParentName == "Yoga" & group == 2 ~ floor(final_ex_min_highest / 30) / 5,
                                             activityParentName == "Yoga" & group == 4 ~ (floor(final_ex_min_highest / 23.33) / 3) / 2,
                                             activityParentName == "Weights" & group == 4 ~ if_else(final_ex_min_highest > 70, floor(final_ex_min_highest / 35) / 2,
                                                                                                    if_else(final_ex_min_highest >= 60, 1,
                                                                                                            if_else(final_ex_min_highest >= 10, .5, 0))) / 2,
                                             activityParentName == "Weights" & group == 5 ~ if_else(final_ex_min_highest > 70, floor(final_ex_min_highest / 35) / 2,
                                                                                                    if_else(final_ex_min_highest >= 60, 1,
                                                                                                            if_else(final_ex_min_highest >= 10, .5, 0))) / 2,
                                             #activityParentName == "Weights" & group == 4 ~ (floor(final_ex_min_highest / 35) / 2) / 2,
                                             #activityParentName == "Weights" & group == 5 ~ (floor(final_ex_min_highest / 35) / 2) / 2,
                                             activityParentName == "Workout" & group == 3 ~ final_ex_min_highest / prescribed_ex_min,
                                             activityParentName == "Workout" & group == 5 ~ final_ex_min_highest / prescribed_ex_min / 2)) %>%
  mutate(adherence_score_pref_log = case_when(activityParentName == "Yoga" & group == 2 ~ floor(final_ex_min_pref_log / 30) / 5,
                                              activityParentName == "Yoga" & group == 4 ~ (floor(final_ex_min_pref_log / 23.33) / 3) / 2,
                                              activityParentName == "Weights" & group == 4 ~ if_else(final_ex_min_pref_log > 70, floor(final_ex_min_pref_log / 35) / 2,
                                                                                                     if_else(final_ex_min_pref_log >= 60, 1,
                                                                                                             if_else(final_ex_min_pref_log >= 10, .5, 0))) / 2,
                                              activityParentName == "Weights" & group == 5 ~ if_else(final_ex_min_pref_log > 70, floor(final_ex_min_pref_log / 35) / 2,
                                                                                                     if_else(final_ex_min_pref_log >= 60, 1,
                                                                                                             if_else(final_ex_min_pref_log >= 10, .5, 0))) / 2,
                                              #activityParentName == "Weights" & group == 4 ~ (floor(final_ex_min_pref_log / 35) / 2) / 2,
                                              #activityParentName == "Weights" & group == 5 ~ (floor(final_ex_min_pref_log / 35) / 2) / 2,
                                              activityParentName == "Workout" & group == 3 ~ final_ex_min_pref_log / prescribed_ex_min,
                                              activityParentName == "Workout" & group == 5 ~ final_ex_min_pref_log / prescribed_ex_min / 2))

time_based_adherence_temp <- adherence_score_temp %>%
  group_by(comet_study_id, week) %>%
  summarize(adherence_score_high = sum(adherence_score_highest), adherence_score_log = sum(adherence_score_pref_log)) 
  
  
time_based_adherence<- time_based_adherence_temp %>%
  left_join(., random_ids, by = "comet_study_id") %>%
  ungroup() %>%
  select(-comet_study_id) %>%
  arrange(random_id) %>%
  filter(!is.na(adherence_score_high) & !is.na(adherence_score_log)) 

#11.8.22: For giving deidentified data to PIs.
#export(adherence_score_by_week, file = file.path(data_dir,'dsmc_report','time_based_adherence.csv'))

##### Begin creating dataframes for performance based adherence
###### Strength #####
strength <- exercise_log %>%
  filter(record_id %in% ex_log_clean_temp$record_id) %>%
  filter(log_group >= 4) %>%
  select(record_id, comet_study_id, log_weeknum, log_legpress, log_lorow, log_chest, log_hirow,
         log_hammy, log_bicep, log_tricep, log_squat, log_calf, log_bridge, log_curl) %>%
  pivot_longer(cols = -c(record_id, comet_study_id, log_weeknum)) %>%
  mutate(status = case_when(value == 1 ~ 0,
                            value == 2 ~ .5,
                            value == 3 ~ 1,
                            value == 4 ~ 1)) %>%
  pivot_wider(id_cols = c(record_id, comet_study_id, log_weeknum), names_from = name, values_from = status) %>%
  mutate(summary = log_legpress + log_lorow+ log_chest+ log_hirow+
              log_hammy+ log_bicep+ log_tricep+ log_squat+ log_calf+ log_bridge+ log_curl) %>%
  mutate(adherence_weights = summary / 11) %>%
  select(comet_study_id, log_weeknum, adherence_weights) %>%
  mutate(activityParentName = "Weights")

#### Endurance ####
endurance <- activities_all_df %>%
  left_join(., comet_to_join, by = c("comet_study_id")) %>%
  mutate(week = floor(difftime(as.Date(startDate), comet_interventionstart, units = c("weeks")))+1) %>%
  mutate(duration = round(duration / 60 / 1000, digits = 0)) %>%
  filter(week >  0 & week != presentweeknum)  %>%
  filter( activityParentName == "Workout") %>%
  filter(group == 3 | group == 5) %>%
  mutate(zone_minutes = case_when((time_in_zone + time_over_zone) > duration ~ duration,
                                  (time_in_zone + time_over_zone) <= duration ~ as.numeric(time_in_zone + time_over_zone))) %>%
  group_by(week, comet_study_id, activityParentName) %>%
  summarize(ex_min_fitbit = sum(zone_minutes)) %>%
  mutate(week = as.numeric(week))

#### Core & Fusion #####
#Use fitbit and exercise log to calculate number of yoga sessions
#Need to figure out what to consider a successful session
yoga_fitbit <- activities_all_df %>%
  left_join(., comet_to_join, by = c("comet_study_id")) %>%
  mutate(week = floor(difftime(as.Date(startDate), comet_interventionstart, units = c("weeks")))+1) %>%
  mutate(duration = round(duration / 60 / 1000, digits = 0)) %>%
  filter(week >  0 & week != presentweeknum) %>%
  filter( activityParentName == "Yoga") %>%
  filter(group == 2 | group == 4) %>%
  group_by(comet_study_id, startDate) %>%
  summarize(comet_study_id = comet_study_id, week = week, activityParentName = activityParentName, duration = sum(duration)) %>%
  distinct() %>%
  filter(duration >= 10) %>%
  group_by(week, comet_study_id, activityParentName) %>%
  summarize(num_yoga_sessions_fitbit = n()) %>%
  mutate(week = as.numeric(week))

yoga_ex_log <- exercise_log %>%
  filter(record_id %in% ex_log_clean_temp$record_id) %>%
  filter(log_group == 4 | log_group == 2) %>%
  mutate(num_yoga_sessions_log = case_when(log_group == 2 ~ log_supervised_num + log_independent_num,
                                           log_group == 4 & log_rt_min > 30 ~ log_supervised_num + log_independent_num - as.integer(2),
                                           log_group == 4 & log_rt_min > 0 ~ log_supervised_num + log_independent_num - as.integer(1),
                                           log_group == 4 & log_rt_min == 0 ~ log_supervised_num + log_independent_num)) %>%
  select(comet_study_id, log_weeknum, num_yoga_sessions_log, log_group) 

yoga <- full_join(yoga_fitbit, yoga_ex_log, by = c("comet_study_id", "week" = "log_weeknum")) %>%
  mutate(yoga_highest = case_when(num_yoga_sessions_fitbit >= num_yoga_sessions_log | is.na(num_yoga_sessions_log) ~ num_yoga_sessions_fitbit,
                                  num_yoga_sessions_fitbit < num_yoga_sessions_log | is.na(num_yoga_sessions_fitbit) ~ num_yoga_sessions_log))  %>%
  mutate(yoga_difference = case_when(num_yoga_sessions_fitbit >= num_yoga_sessions_log ~ num_yoga_sessions_fitbit - num_yoga_sessions_log,
                                    num_yoga_sessions_fitbit < num_yoga_sessions_log ~ num_yoga_sessions_log - num_yoga_sessions_fitbit))  %>%
  mutate(yoga_adherence = case_when(log_group == 2 ~ yoga_highest / 5,
                                    log_group == 4 ~ yoga_highest / 3)) %>%
  select(week, comet_study_id, activityParentName, yoga_adherence) 

##### Performance based adherence dataframe #####
performance_based_adherence_temp <- adherence_score_temp %>%
  select(comet_study_id, week, activityParentName, final_week, group, prescribed_ex_min, adherence_score_pref_log) %>%
  left_join(., strength, by = c("comet_study_id", "week"="log_weeknum", "activityParentName")) %>%
  left_join(., endurance, by = c("comet_study_id","week", "activityParentName")) %>%
  left_join(., yoga, by = c("comet_study_id", "week", "activityParentName")) %>%
  mutate(adherence_endurance = ex_min_fitbit / prescribed_ex_min) %>%
  mutate(adherence = case_when(group == 2 ~ yoga_adherence,
                               group == 3 ~ adherence_endurance, 
                               group == 4 & activityParentName == "Weights" ~ adherence_weights / 2,
                               group == 4 & activityParentName == "Yoga" ~ yoga_adherence / 2,
                               group == 5 & activityParentName == "Weights" ~ adherence_weights / 2,
                               group == 5 & activityParentName == "Workout" ~ adherence_endurance / 2)) %>%
  group_by(comet_study_id, week) %>%
  summarize(adherence_score_performance = sum(adherence)) 

performance_based_adherence <- performance_based_adherence_temp %>%
  left_join(., random_ids, by = "comet_study_id") %>%
  ungroup() %>%
  select(-comet_study_id) %>%
  arrange(random_id) %>%
  filter(!is.na(adherence_score_performance)) 

#### Build Data For Neal ######
to_join <- comet %>%
  filter(!is.na(group)) %>%
  select(comet_study_id, group)

looking_for_outliers <- full_join(time_based_adherence_temp, performance_based_adherence_temp, by = c("comet_study_id","week")) %>%
  left_join(., to_join, by = c("comet_study_id")) %>%
  filter(!is.na(adherence_score_performance)) %>%
  group_by(comet_study_id) %>%
  summarize(performance = mean(adherence_score_performance, na.rm = T), 
            performance_missing = sum(is.na(adherence_score_performance)), 
            log = mean(adherence_score_log, na.rm = T), 
            log_missing = sum(is.na(adherence_score_log)),
            group = mean(group)) %>%
  filter(performance > log)

adherence_by_group <- full_join(time_based_adherence_temp, performance_based_adherence_temp, by = c("comet_study_id","week")) %>%
  left_join(., to_join, by = c("comet_study_id")) 

# ggplot(adherence_by_group, aes(x = factor(group), y = value)) +
#   geom_violin() +
#   stat_summary(fun = mean, geom = "point", size=2, aes(shape = name, color = name),
#                position = position_dodge(width = .75)) +
#   theme_bw() +
#   labs(title = "All Three Approaches by Group") +
#   ylab("Adherence") +
#   xlab("Group") +
#   theme(legend.position = "top") +
#   scale_y_continuous(breaks = seq(1:5)) +
#   geom_abline(slope = 0, intercept = 1, linetype = 3) 




#11.8.22: For giving deidentified data to PIs.
#export(performance_based_adherence, file = file.path(data_dir,'dsmc_report','performance_based_adherence.csv'))


#Save output data frames
to_save <- c('performance_based_adherence','time_based_adherence', 'performance_based_adherence_temp', 'time_based_adherence_temp')
save(list = to_save,file= file.path(clean_data_destination,'adherence.Rdata'))



