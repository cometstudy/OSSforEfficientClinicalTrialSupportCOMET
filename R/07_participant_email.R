#' @author  21 April, 2021 by JC

#' @name participant_email
#' 
#' @title Send participant emails
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas
#'
#' @section Development notes:
#' 5.26.21 Began developing \cr
#' 6.22.21 Final draft complete - Still need rigorous testing \cr
#' 7.16.21 Fixed group_assign issue - group_assign and group_assign_dyad2nd coalesce \cr
#' into a single variable group - use group for all group related queries \cr
#' 8.10.21 Exercise prescriptions are complete and attaching to emails. Last thing I need to do is 
#' complete and attach the "Using my fitbit" document \cr
#' 11.16.21 Added trainers to the emails. JC \cr
#' 12.20.21 Changed the week_2+to_52 email current_exercise_prescription, to be the week before
#' so that it gives feedback on the week before of exercise. JC \cr
#' 1.24.22 Changed activities JC \cr
#' 5.5.22 Checked for date issues. Nothing obvious. JC \cr
#' 5.19.22 Updated missing exercise dataframe to include weeknum. JC \cr
#' 5.20.22 Updated 02_rmd_st to include link in exercise prescription. JC \cr
#' 6.7.22 Updated trainer list from the data/trainer_email to the data/ymca_liason folder. 
#' This will always be the most current trainer list. JC \cr
#' 9.23.22 Added in device info to send different guide for Charge 5 vs Inspire 2 users. JC \cr
#' 10.10.22 Added tryCatch for device data when participants don't have watch until orientation. JC \cr
#' 11.8.22  Added presentweeknum_post to deal with participants past week 52. Will create a post_week52 
#' email for next week and remove the changes to postweenum_post.  JC \cr
#' 11.16.22 Updating the feedback mechanism for a participant frustrated with the Sunday gap of how we
#' collect data. I'm adding the Fitbit client and user data to this script. JC \cr
#' 12.9.22 Added the week 52 testing email. The code is written. I'm waiting for feedback before implementing. JC \cr
#' 3.30.23 Due to apple update, we added the exercise prescription to the body of the email. JC \cr
#' 4.6.23 Removed #69 from receiving emails for memory concerns. Email COMET Pt COG/Memory Concerns on 4.6.23 JC

#' 
#'  
#' @description 
#' This script sends emails to all active participants in comet and all participants waiting for baseline randomization
#' To be described as active a participant must have a study ID, an intervention start date, and not have a study status of successfully 
#' completed (6), withdrawn (7 or 8), or lost to follow-up (9)
#' To be described as a baseline_participant, one must be marked as eligible on the phone screen and must not have a response to the 
#' random_ready question on the status summary
#' The script loops through all active and baseline participants. Retrieves missing exercise and upcoming visits
#' Then calls the appropriate html building function depending on where the participant is in the study
#' These functions are all found in R/00_product_script_functions.R
#' Participants are given contact information, calls to action, feedback about their last week, and information about upcoming visits

##### Set update for each week ####
#12.13.22 Added functionality for multiple emails. JC
email_update <- ''#'<p style="font-family:arial;" ="font-size:125%;"><b>Update:</b> Exercise prescriptions can now be found at the bottom of the email in addition to the PDF for participants still exercising. For this first email, please check both the PDF and the bottom of the email and let us know if you find any issues. Thanks!</p>'


###### Load Data #####
load(file.path(data_dir,'clean','comet_clean.Rdata'))
load(file.path(data_dir,'clean','ex_min_clean.Rdata'))


##### App info ####
client_info <- import(file.path(project_dir,'data','read_write','read_write','client_info.csv'))

#### User info ####
fitbit_users <- import(file.path(project_dir,'data','read_write','read_write','fitbit_users.csv'))

######### Check to see if tokens need to be refreshed #####
state <- get_status(usercode = fitbit_users$user[1], token = fitbit_users$access_token[1])

if(state >= 400) {
  source(file.path(script_dir,'02_refresh_token.R'))
}

###### Load Personal Trainer List #####
trainer_list <- import(file.path(data_dir,'ymca_liason_email','PHITPersonalTrainers.csv')) %>%
  filter(is.na(redcap_repeat_instance)) %>%
  select(record_id, first_name, last_name, work_email, trainer_status, organization, location_y)

########## Modifications to COMET DF for email #########
#11.7.22 Added presentweeknum_post to deal with participants past week 52 JC
comet_participants <- comet %>%
  filter(is.na(comet_study_id) == F) %>%
  filter(is.na(scrn_main_email) == F) %>%
  filter(redcap_event_name == "baseline_arm_1", is.na(comet_interventionstart) == F) %>%
  filter(status___6 == 0 & status___7 == 0 & status___8 == 0 & status___9 == 0) %>%
  arrange(comet_study_id) %>%
  mutate(presentweeknum_post = case_when(presentweeknum > 52 ~ 52,
                                         T ~ as.numeric(presentweeknum))) %>%
  left_join(., fitbit_users, by = c("comet_study_id" = "id")) %>%
  filter(comet_study_id != 69)

########## Baseline Participants ##########
baseline_participants <- comet %>%
  filter(scrn_elig == 0) %>%
  filter(is.na(random_ready) == T) %>%
  filter(baseline_status == 1 | is.na(baseline_status)) %>%
  filter(!is.na(comet_baselinecogsched))

########## Week 52 Testing Participants #######
#Similar subset to comet_participants except must be past week 48 and withdrawn willing to complete are included. JC
#Also need to change present_weeknum post here JC
#' Removed #69 from receiving emails for memory concerns. Email COMET Pt COG/Memory Concerns on 4.6.23 JC
week_52_testing_email_participant <- comet %>%
  filter(is.na(comet_study_id) == F) %>%
  filter(is.na(scrn_main_email) == F) %>%
  filter(redcap_event_name == "baseline_arm_1", is.na(comet_interventionstart) == F) %>%
  filter(presentweeknum >= 49) %>%
  filter(status___6 == 0 & status___8 == 0 & status___9 == 0) %>%
  arrange(comet_study_id) %>%
  mutate(presentweeknum_post = case_when(presentweeknum > 52 ~ 52,
                                         T ~ as.numeric(presentweeknum))) 

########## Consent form ########
consent <- as.data.frame(list.files(file.path(dirname(external_data_dir),'consent_forms'), full.names = TRUE)) %>%
  rename(filename = c(1)) %>%
  filter(grepl("ICF", filename, ignore.case = T) & grepl(".pdf", filename, ignore.case = T)) %>%
  arrange(., desc(date))

message("07_participant for loop")

################### Send Email to comet_participants #############
if(nrow(comet_participants) > 0) {
  for(i in 1:nrow(comet_participants))
  {
    message(paste0("Started ",i))
    
    current <- comet_participants[i,] 
    
    current_ex_log <- exercise_log %>%
      filter(comet_study_id == current$comet_study_id) %>%
      mutate(log_monday = as.character(log_monday))
    
    current_exercise_prescription <- exercise_prescriptions %>%
      filter(id == current$comet_study_id, week == current$presentweeknum_post)
    
    last_week_exercise_prescription <- exercise_prescriptions %>%
      filter(id == current$comet_study_id, week == current$presentweeknum_post - 1)
    
    
    
    #Send email to study coordinators that participant is missing exercise prescription
    if(nrow(current_exercise_prescription) == 0 & current$presentweeknum > 0) {
     text <- paste0('Participant ID #',current$comet_study_id,' does not have an exercise prescription this week. This likely means the participant got sent an incorrect exercise prescription. Create an exercise prescription here - P:/study_dir_a/exercise_prescriptions - and get with the technology lead to send the correct exercise presription for this week.')
     cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
     mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email.py'), study_coordinator_email, file.path(project_dir,'data','modules','generic_email_module','email.txt'))
     system(mailcmd)
    }
    
    message(paste0("Calculate Missing exercise for ",i))
    #### Missing Exercise
    if(current$presentweeknum == 1){
      missing <- data.frame()
    } else {
      missing <- as.data.frame(c(1:(current$presentweeknum-1))) %>%
        rename(weeknum = c(1)) %>%
        mutate(log_monday = as.character(current$comet_interventionstart+(weeknum-1)*7)) %>%
        anti_join(current_ex_log, temp, by="log_monday") %>%
        mutate(log_monday = format(as.Date(log_monday),'%m-%d-%Y')) %>%
        filter(weeknum <= 52) %>%
        rename("Missing Weeks" = log_monday, "Week Number" = weeknum) 
    }
    
    message(paste0("Upcoming visits for ",i))
    
    #### Upcoming Visits
    upcoming_visits <- current %>%
      select(presentweeknum, comet_baselinecogsched, comet_baselinegxtsched, comet_baselinemrisched, comet_week26date, comet_week26cogsched, comet_week52date, comet_week52cogsched, comet_week52gxtsched, comet_week52mrisched) %>%
      # mutate_if(grepl("date", ., T), as.Date) %>% #commented out on 4.27.22 bc reformatted date in 06_read_clean
      # mutate_if(grepl("sched",.,T), ymd_hms) %>%
      mutate_at(vars(contains("date")), ~ format.Date(.x, "%m-%d-%Y")) %>%
      mutate_at(vars(contains("sched")), ~ format.Date(.x, "%m-%d-%Y %H:%M")) %>%
      replace(is.na(.), "To Be Scheduled")  %>%
      mutate(`Week of Testing` = case_when(presentweeknum < 28 ~ 26,
                                           TRUE ~ 52)) %>%
      mutate(`Approximate Date` = case_when(presentweeknum < 28 ~ comet_week26date,
                                            TRUE ~ comet_week52date))  %>%
      mutate(`Scheduled Cognitive Testing` = case_when(presentweeknum < 28 ~ comet_week26cogsched,
                                                       TRUE ~ comet_week52cogsched)) %>%
      mutate(`Scheduled GXT Testing` = case_when(presentweeknum < 28 ~ "No GXT at week 26",
                                                 TRUE ~ comet_week52gxtsched)) %>%
      mutate(`Scheduled MRI Testing` = case_when(presentweeknum < 28 ~ "No MRI at week 26",
                                                 TRUE ~ comet_week52mrisched)) %>%
      select(`Week of Testing`, `Approximate Date`, `Scheduled Cognitive Testing`, `Scheduled GXT Testing`, `Scheduled MRI Testing`)
    
    
    message(paste0("Save workspace for ",i))
    #Save workspace to load in markdown
    df_to_save <- c('current','current_exercise_prescription') 
    save(list = df_to_save, file = file.path(data_dir,'participant_email','current_participant.Rdata'))
    
    ##### Select Fitbit Guide
    #Added tryCatch on 10/10/22 for those without fitbit files yet
    #Added filter of tracker on 3.27.2023. JC
    device <- tryCatch(
      import(file.path(data_dir,'raw','device_data',paste0('146904_',current$comet_study_id,'.csv'))) %>%
        filter(date == max(date),
               type == "TRACKER") %>%
        pull(deviceVersion),
      error = function(e) 
      {e
        return("Charge 5")
      }
    )

    
    if(length(device) == 0) {
      device <- "Charge 5"
    }

    if(device == "Inspire 2"){
      fitbit_guide_path <- file.path(data_dir,'participant_email','fitbit_inspire2.pdf')
    } else {
      fitbit_guide_path <- file.path(data_dir,'participant_email','fitbit_charge5.pdf')
    }
    
    ##### Mail presets
    TO = current$scrn_main_email
    #TO = "study_coordinator_2@kumc.edu"
    trainer_email = trainer_list$work_email[which(trainer_list$record_id == current$gym_trainer)]
    #trainer_email = "study_coordinator_2@kumc.edu"
    
    #Send email to study coordinators that trainer list needs to be updated
    if(length(trainer_email) == 0) {
      text <- paste0('Trainer error - Either the trainer is not updated in the REDCap database or the trainer has not been added to the trainer_list.<br><br>Participant Study ID: ',current$comet_study_id,' <br><br>Participant record_id: ',current$record_id,' <br><br>Participant trainer number: ',current$gym_trainer,' <br><br>If the trainer number is NA, the trainer needs to be updated in the REDCap database. If the trainer has a number, the trainer_list needs to be updated. You can find the trainer_list here - P:/study_dir_a/study_dir_a/COMET/data/ymca_liason_email. Get with the technology lead to the correct the trainer_list.')
      cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
      mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email.py'), study_coordinator_email, file.path(project_dir,'data','modules','generic_email_module','email.txt'))
      system(mailcmd)
    }
    
    message(paste0("Rendering ",i))

    if(current$presentweeknum == 1){
      
      render_exercise_prescription(group = current$group)
      load(file.path(data_dir,'participant_email','current_html_prescription.Rdata'))
      if(current$comet_study_id != id_to_check){
        stop("Error with HTML exercise prescription. ID does not match.")
      }
      week_1_email(missing_data = missing, fname = current$scrn_fname, last_name = current$scrn_lname, group = current$group, comet_study_id = current$comet_study_id, week = current$presentweeknum, upcoming_visits = upcoming_visits)
      mailcmd<-paste("py", file.path(data_dir,'participant_email','sendemail.py'),TO, file.path(data_dir,'participant_email','week1_email.txt'), file.path(data_dir,'participant_email','current_exercise_prescription.pdf'), fitbit_guide_path, trainer_email)
      system(mailcmd)
      
    } else if(current$presentweeknum > 1 & current$presentweeknum <= 52){
      #Changed weeknum to weeknum_post. Need to change back once I have created a post week 52 email. JC 
      render_exercise_prescription(group = current$group)
      load(file.path(data_dir,'participant_email','current_html_prescription.Rdata'))
      if(current$comet_study_id != id_to_check){
        stop("Error with HTML exercise prescription. ID does not match.")
      }
      week_2_to_52_email(missing_data = missing, fname = current$scrn_fname, last_name = current$scrn_lname, activities_log = activities_all_df, hr_log = hr_all_df, step_log = steps_all_df, group = current$group, comet_study_id = current$comet_study_id, week = current$presentweeknum_post, upcoming_visits = upcoming_visits, current_exercise_prescription = last_week_exercise_prescription)
      mailcmd <- paste(python_cmd_prefix, file.path(data_dir,'participant_email','sendemail.py'),TO, file.path(data_dir,'participant_email','email.txt'), file.path(data_dir,'participant_email','current_exercise_prescription.pdf'), fitbit_guide_path, trainer_email)
      system(mailcmd)
    } else if(current$presentweeknum > 52){
      #This email section was created on 12.14.22. JC
      current_ex_log_issues <- ex_log_issues %>%
        filter(comet_study_id == current$comet_study_id) 
      
      current_ex_log_issues <- case_when(sum(ex_log_issues$comet_study_id == current$comet_study_id, na.rm = T) > 0 ~ paste0("<li>Resolve ", sum(ex_log_issues$comet_study_id == current$comet_study_id, na.rm = T), " exercise log issues with Study Coordinator</li>"),
                                         T ~ "")
      
      current_missing_surveys <- comet %>%
        filter(redcap_event_name == "week_52_arm_1" & record_id == current$record_id) %>%
        select(contains("complete")) %>%
        select(barse_complete, cognitive_function_index_complete, eq5d5l_v10_complete, 
               exse_complete, florida_cognitive_activities_scale_complete, godin_complete, lse_complete, nhanes_dsq_complete, pa_self_regulation_complete,
               perceived_stress_scale_pss10_complete, contains("promis")) %>%
        pivot_longer(cols = everything()) %>%
        filter(value != 2) 
      
      current_survey_issues <- case_when(nrow(current_missing_surveys) == 0 ~ "",
                                         T ~ paste0("<li>Complete ",nrow(current_missing_surveys)," missing surveys. </li>"))
      
      all_issues <- paste0(current_ex_log_issues, current_survey_issues)
      
      study_closeout_list <- case_when(nchar(all_issues) == 0 ~ "Nice! You have no remaining issues to close out!",
                                       T ~ all_issues)
      
      post_week_52_email(missing_data = missing, fname = current$scrn_fname, last_name = current$scrn_lname, group = current$group, comet_study_id = current$comet_study_id, week = current$presentweeknum, upcoming_visits = upcoming_visits, study_closeout_list = study_closeout_list)
      mailcmd <- paste(python_cmd_prefix, file.path(data_dir,'participant_email','send_post_52_email.py'),TO, file.path(data_dir,'participant_email','post52_email.txt'), fitbit_guide_path, trainer_email)
      system(mailcmd)
      
    }
    message(paste0("Completed ",i))
  } 
}

################### Send Email to baseline_participants #############
if(nrow(baseline_participants) > 0) {
  for(j in 1:nrow(baseline_participants))
  {
    current <- baseline_participants[j,] 
    
    ##### Mail presets
    TO = current$scrn_main_email
    #TO = "study_coordinator_2@kumc.edu"

    ##### Send email
    baseline_email(fname = current$scrn_fname, mem_testing = current$comet_baselinecogsched, exercise_testing = current$comet_baselinegxtsched, mri = current$comet_baselinemrisched)
    mailcmd<-paste("py", file.path(data_dir,'participant_email','send_baseline_email.py'),TO, file.path(data_dir,'participant_email','baseline_email.txt'), consent$filename[1])
    system(mailcmd)
    
    
    
  }
}

################### Send Email to week_52_testing_participants #############
# Added on 12.9.22 JC
if(nrow(week_52_testing_email_participant) > 0) {
  for(j in 1:nrow(week_52_testing_email_participant))
  {
    current <- week_52_testing_email_participant[j,]

    ##### Mail presets
    TO = current$scrn_main_email
    #TO = "study_coordinator_2@kumc.edu"

    ##### Send email
    week_52_testing_email(fname = current$scrn_fname, mem_testing = current$comet_week52cogsched, exercise_testing = current$comet_week52gxtsched, mri = current$comet_week52mrisched)
    mailcmd<-paste("py", file.path(data_dir,'participant_email','send_week_52_testing_email.py'),TO, file.path(data_dir,'participant_email','week_52_testing_email.txt'))
    system(mailcmd)



  }
}


