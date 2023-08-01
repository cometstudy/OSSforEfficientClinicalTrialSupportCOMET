# '@author Jon Clutton 
#' 
#' @name get_hr_and_activities_script
#' 
#' @title Retrieving and storing all exercise data
#' 
#' @description 
#' This script reads in fitbit_users (csv file of fitbit users and keys)
#' Checks the state of the fitbit_users, and refreshes if need be. 
#' The script runs only for active participants 
#' For all active participants, the script checks file structure
#' Summary hr logs are saved under data/raw/hr_data
#' Intraday hr data are saved under data/raw/hr_data/intraday
#' Summary activity data logs are suved under data/raw/activities_data
#' If the correct file structure is not present, an intraday hr folder is created 
#' and a summary csv file is created (first date is randomization date) 
#' If correct structure is present, a for loop is created for the number of days between 
#' the last transfer (to summary csv) and last data sync
#' All data retrieval and storage occurs in the for loop
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas
#' 
#' @section Hdrive2
#' The for loop analyzes each day's intraday hr data and appends a row to summary csv
#' If the user did not wear a watch NA is returned
#' If the user did wear a watch, the number of minutes above moderate is returned
#' Moderate is determined by 40% of karvonen .4*(max_hr - rest_hr) + rest_hr for all groups
#' This is to capture all activity above 40% intensity to categorize MVPA, as not all groups are 
#' assigned target heart rates. 
#' Wear-time validation will also be stored on this spreadsheet.
#' 
#' @section Activities: 
# Within Fitbit, the types of tagged exercise can be modified
# Upon watch initialization, I'm going to edit the Fitbit to have the following three options
# These options are all different than Auto Recognized Exercises (walk, run, outdoor bike, elliptical, sports, aerobic workout, swimming)
# Yoga = Stretching
# Workout = Aerobic
# Weights = Resistance
# We will train participants to tag these when they do exercise
# The summary activity log will include the type of exercise, duration, minutes of moderate, minutes of vigorous, and average
# Duration will be capped at 90 minutes for an exercise session in case a participant forgets to turn off their watch
# The lines of code that do this are found in 116-117
# Moderate, vigorous and average are obtained by comparing the start/end time of each activity with the intraday hr data
# In a later script, I still need to determine how we categorize exercise -> where do we get it from, what counts, etc.
#' 
#' @section Development:
#' 5.12.21 fitbit_user and COMET REDCAP are combined by fitbit_users$id and comet$comet_study_id - no safeguards are in place \cr
#' 5.13.21 Line 76: Set hr filter to be on fitbit_users sheet -> need to change to REDCap db \cr
#' 5.18.21 Line 31: Redefine active participants according to study - done \cr
#' 5.28.21 To Do: develop a failsafe if a pt forgets to end activity session - done \cr
#' 8.4.21 Needed to update hrr_min and hrr_max locations \cr
#' 9.15.21 Fixed error in refreshing tokens \cr
#' 10.27.21 Adding in weartime validation to hr section JC \cr
#' 12.1.21 Updated hr to ex_forloop and ex_averaging to be 40% intensity JC \cr
#' 1.12.22 Added the email error notification for watches that are syncing but not collecting HR JC \cr
#' 1.17.22 Added in notification that max heart rate is missing. JC \cr
#' 2.1.22 Changed maximum activity duration to 60 minutes JC \cr
#' 2.2.22 Changed active participants to be anyone past their MRI visit JC \cr
#' 5.5.22 File has been checked for potential date issues. Nothing obvious. JC \cr
#' 5.10.22 Added TryCatch to last_sync to catch API call errors and send an email. JC \cr
#' 5.11.22 Added testing to prevent emails from spamming coordinators. JC \cr
#' 5.11.22 Working on error thrown when HR is turned off and an activity is recorded.
#' Changed initialization of time_in_zone = NA, time_over_zone = NA, average = NA from time_in_zone = 0, time_over_zone = 0, average = NA.
#' If HR is turned off and an activity is recorded, the for loop looking for HR during that exercise will be skipped. If statement added. JC \cr
#' 5.19.22 Added condition to TryCatch implemented on 5.10.22 to deal with fitbits that aren't set up until first day at gym. JC \cr
#' 5.19.22 Added "store device data" to store what type of tracker pts are using on daily basis. JC \cr
#' 5.20.22 Updated store device data if statement. JC \cr
#' 5.20.22 Added HR Error module. Participants get automated email if their steps are syncing but hr isnt. JC \cr
#' 6.22.22 Changed device data log error messaging if statement. JC \cr
#' 9.22.22 Updated HR Error module to include Charge 5's. JC \cr
#' 9.22.22 Updated the hr_not_working criteria. Emails were being triggered when a watch recorded very low step counts (<50 steps) and no HR data. 
#' I'm assuming this is caused by random watch movement throughout the day without any actual wear time. To fix this issue, I changed the criteria
#' to require >100 steps to change the hr_not_working marker. JC \cr
#' 11.8.22 Added the check Fitbit exercise shortcuts module. JC \cr
#' 11.16.22 We started using time_in_zone for adherence data. Time_in_zone and time_over_zone were usingthe total duration time. So if a participant logged a session over 60 minutes, the zone time could be
#' considerably longer. I fixed that for on today. So there will be errors before. The best example was a activity from #3 on 12/15/21. She had 90 minutes duration and 140 minutes in zone. We might re-run all
#' of the activity data to get hr data for the core & Fusion group. If we do, that will fix this issue. JC \cr
#' 1.3.23 Added the personal trainers on the heart rate error auto-email. JC \cr
#' 3.27.2023: Changed startDate in activities_log from iDate to date format. JC \cr
#' 3.27.23: Added missing_device to deal with Rstudio upgrade. This converts the device data call into a T or F statement to be used below. JC \cr


#' 
#' @section Errors:
#' 5.11.22 Started section. JC \cr
#' 5.11.22 A more serious error presented. As of writing, the affected section is on line 223
#' The activities data frame captures daily activities through the fitbit API. 
#' A for-loop cycles through the hr dataframe (daily intraday hr), to calculate how many minutes
#' the participant spend in and above their hr zone. This for loop would throw an error because
#' no HR is available. This program was fixed by adding a hr_not_working watcher value and initializing
#' time_in_zone and time_over_zone to NA. If a participant's HR gets shut off on their fitbit, the for 
#' loop appending hr values during a workout session will be skipped and NA will be returned. One thing
#' to note is that if a participant is not in an aerobic group (aerobic or combo), NA will also be returned
#' for these values as they do not have a target hr zone. JC \cr


message("beginning 04_get_hr_and_activities")

###### Load Data #####
load(file.path(data_dir,'clean','comet_clean.Rdata'))

##### App info ####
client_info <- import(file.path(project_dir,'data','read_write','read_write','client_info.csv'))

#### User info ####
fitbit_users <- import(file.path(project_dir,'','data','read_write','read_write','fitbit_users.csv'))

######### Check to see if tokens need to be refreshed #####
state <- get_status(usercode = fitbit_users$user[1], token = fitbit_users$access_token[1])

if(state >= 400) {
  source(file.path(script_dir,'02_refresh_token.R'))
}

#### COMET REDCap and fitbit_users joined ####
#filtered for randomized participants
#11.7.22 Added presentweeknum_post to deal with participants past week 52, who data still need to be collected for. JC
#4.4.23 Removing presentweeknum_post. Only collecting data til week 52 now. JC
active_participants <- comet %>%
  filter(redcap_event_name == 'baseline_arm_1') %>%
  filter(is.na(comet_study_id)==F) %>%
  right_join(., fitbit_users, by = c("comet_study_id" = "id")) %>%
  filter(status___6 == 0 & status___7 == 0 & status___8 == 0 & status___9 == 0) %>%
  filter(intervention_status <= 1 | is.na(intervention_status)) %>%
  filter(is.na(comet_study_id)==F) %>%
  filter(is.na(randomization_date)==F) %>%
  filter(today() > comet_baselinegxtsched + days(7)) %>%
  filter(presentweeknum <= 52) %>%
 # mutate(presentweeknum_post = case_when(presentweeknum > 52 ~ 52,
  #                                       T ~ as.numeric(presentweeknum))) %>%
  arrange(comet_study_id) 




################# Get HR for all active participants ############
for(i in 1:nrow(active_participants))
{
  
  current <- active_participants[i, ]
  paste0("Starting get activities for #",current$comet_study_id," in week ",current$presentweeknum)

  
  
  if(current$presentweeknum >= 1) {
    current_exercise_prescription <- exercise_prescriptions %>%
      filter(id == current$comet_study_id, week == current$presentweeknum) #changed form presentweeknum_post on 4.4.23
    
    ###### Send email to study coordinators that participant is missing exercise prescription #####
    if(nrow(current_exercise_prescription) == 0) {
      text <- paste0('Participant ID #',current$comet_study_id,' does not have an exercise prescription this week. Create an exercise prescription here - P:/study_dir_a/exercise_prescriptions')
      cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
      mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email.py'), study_coordinator_email, file.path(project_dir,'data','modules','generic_email_module','email.txt'))
      if(testing == 0){
        system(mailcmd)
      }
    }
    
  } else if (current$presentweeknum < 1) {
    
    current_exercise_prescription <- exercise_prescriptions %>%
      filter(id == current$comet_study_id, week == 1)
    
    ###### Send email to study coordinators that participant is missing exercise prescription######
    if(nrow(current_exercise_prescription) == 0) {
      text <- paste0('Participant ID #',current$comet_study_id,' does not have an exercise prescription this week. Create an exercise prescription here - P:/study_dir_a/exercise_prescriptions')
      cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
      mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email.py'), study_coordinator_email, file.path(project_dir,'data','modules','generic_email_module','email.txt'))
      if(testing == 0){
        system(mailcmd)
      }
    }
  }
  
  
  ###### Send email if missing max hr ####
  #' 4.3.2023 Modified if statement. Was throwing warning. Changed to read in ext_gxt_stop to with ymd_hms JC
  if(is.na(current$ex_gxt_maxhr) &   today() - as_date(ymd_hms(current$ext_gxt_stop)) > 7) {
    text <- paste0('It has been 7 days since 146904_',current$comet_study_id,' completed their GXT and no max heart rate has been entered. Please enter this participants max hr into redcap to prevent issues retrieving exercise data. ')
    cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
    mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email.py'), study_coordinator_email, file.path(project_dir,'data','modules','generic_email_module','email.txt'))
    if(testing == 0){
      system(mailcmd)
    }
  }
  
  #Store device data for the day
  if(file.exists(file.path(project_dir,'data','raw','device_data',paste0('146904_',current$comet_study_id,'.csv')))==T){ 
    
    device_data_log <- import(file.path(project_dir,'data','raw','device_data',paste0('146904_',current$comet_study_id,'.csv'))) %>%
      mutate(date = ymd(date)) %>%
      mutate(id = as.numeric(id))
    
    #Returns a dataframe of that day's device data
    #If the API call throws an error, a message is sent to the study team. 
    device_data <- tryCatch(
      get_device_data(usercode = current$user, token = current$access_token),
      error = function(e) 
      {e
        text <- paste0('Something weird is happening with Participant ID #',current$comet_study_id,'s fitbit. On ',today(),' there was an errror with retrieving device data, which is caused by an errant devices api call. This could mean an issue with the device. The first time JC resolved this issue, the participant switched their Fitbit account back to their personal email.')
        cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
        mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email.py'), study_coordinator_email, file.path(project_dir,'data','modules','generic_email_module','email.txt'))
        if(testing == 0 & current$comet_interventionstart <= today()){
          system(mailcmd)
        }
        message(paste0(current$comet_study_id," is throwing error"))
        return(NA)
      }
    )
    
    #' 3.27.23: Added to deal with Rstudio upgrade. This converts the device data call into a T or F statement to be used below. 
    missing_device <- FALSE
    if(is.null(nrow(device_data))){
      missing_device <- TRUE
    } 
    #on 5/23, #38 had something weird happen. device_data_log (device api call) returns a data frame with 0 rows. I'm not sure what is causing this. Wrote this notification when it happens
    #This is the same error caused without a device. It changed because I am storing device data on a daily basis and the change in function call leads to a 0 row db being returned instead of an error. 
    #On 6/22, changed if statement to !is.na(current$comet_intervention start to prevent messages for people without watches in baseline)
    #' 1.12.23 Pts started finishing. Some had their fitbit transfered to a personal account before completing the intervention. This message only gets sent out 
    #' when pts are in weeks 1-51 to help prevent this. JC 
    
    #Commenting on 3.27.23, can remove later because device data being null threw an error. Try fixing later. 
    # if(nrow(device_data) == 0){
    #   device_data <- NA
    #   text <- paste0('Something weird is happening with Participant ID #',current$comet_study_id,'s fitbit. On ',today(),' there was an errror with retrieving device data, which is caused by an errant devices api call. This could mean an issue with the device. Past reasons for this error include 1) the participant switched their Fitbit account back to their personal email or 2) no device has been set-up yet')
    #   cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
    #   mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email.py'), study_coordinator_email, file.path(project_dir,'data','modules','generic_email_module','email.txt'))
    #   if(testing == 0 & !is.na(current$comet_interventionstart) & current$presentweeknum <= 52){
    #     system(mailcmd)
    #   }
    # } 
    
    #If device data is returned, bind these rows to the participants log
    #updated if statement on 5.20.22 to only bind rows if today's date is not in the data
    if(missing_device == FALSE & today() %in% device_data_log$date == F){
      device_data_row <- device_data %>%
        mutate(comet_study_id = current$comet_study_id, date = today()) %>%
        mutate(id = as.numeric(id)) %>%
        mutate(lastSyncTime = ymd_hms(lastSyncTime))
      
      #bind rows
      device_data_log <- bind_rows(device_data_log, device_data_row)
      
      export(device_data_log, file.path(project_dir,'data','raw','device_data',paste0('146904_',current$comet_study_id,'.csv')))
    }
  } else {
    #create device log
    temp <- data.frame(comet_study_id = current$comet_study_id, date = today(), battery = "", batteryLevel = NA, deviceVersion = "", id = "", lastSyncTime = NA, mac = "", type = "")
    file.create(file.path(project_dir,'data','raw','device_data',paste0('146904_',active_participants$comet_study_id[i],'.csv')))
    export(temp, file.path(project_dir,'data','raw','device_data',paste0('146904_',active_participants$comet_study_id[i],'.csv')))
  }
  
  
  #Checks for file structure, 
  if(file.exists(file.path(project_dir,'data','raw','hr_data',paste0('146904_',current$comet_study_id,'.csv')))==T){  
    
    #Set min hr for mvpa to measure their total daily activity in or above mvpa
    min_hr_mvpa_40 <- case_when(current$meds_betablocker == 1 ~ (0.4 * (164- .7*current$age - current$ex_rest_hr)) + current$ex_rest_hr,
                                current$meds_betablocker == 0 ~ (0.4 * (current$ex_gxt_maxhr - current$ex_rest_hr)) + current$ex_rest_hr)
    
    
    #imports current participant's ongoing hr log and activities log
    hr_log <- import(file.path(project_dir,'data','raw','hr_data',paste0('146904_',current$comet_study_id,'.csv'))) %>%
      mutate(date = as_date(date))
    #3.27.2023: Changed startDate from iDate to date format. JC
    activities_log <- import(file.path(project_dir,'data','raw','activities_data',paste0('146904_',current$comet_study_id,'.csv'))) %>%
      mutate(end_time = hms::as_hms(end_time),
             startDate = as_date(startDate))
    
    #Check for missing Workout / Yoga / Weights
    recent_activities <- activities_log %>%
      filter(startDate >= today() - weeks(3)) %>%
      group_by(activityParentName) %>%
      summarize(activity_num = n())
    
    #Check Fitbit Exercise Shortcuts Module
    #11.8.22: Sometimes we forget to setup exercise shortcuts on the Fitbits. When that potentially happens, this will send a notification to staff
    if(!("Yoga" %in% recent_activities$activityParentName) & 
       !("Workout" %in% recent_activities$activityParentName) & 
       !("Weights" %in% recent_activities$activityParentName) &
       current$presentweeknum > 1 & current$presentweeknum < 4) {
      text <- paste0('Participant ID #',current$comet_study_id,' may not have had their Fitbit shorcuts setup. Pt is in week ',current$presentweeknum,' and has
                     no records of Yoga, Weights, or Workout. Please check their Fitbit account. \n
                     This message is generated from the 04_get_hr_and_activities.R script.')
      cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
      mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email_w_subject.py'), study_coordinator_email, file.path(project_dir,'data','modules','generic_email_module','email.txt'), "Check_Exercise_Shortcuts")
      if(testing == 0){
        system(mailcmd)
      }
    }
    
    #last date data was transferred from fitbit to p drive
    last_transfer <- hr_log$date[nrow(hr_log)]
    
    #Returns the last full sync day
    #If the API call throws an error, a message is sent to the study team. 
    last_sync <- tryCatch(
      floor_date(ymd_hms(get_last_sync(usercode = active_participants$user[i], token = active_participants$access_token[i])), unit = "day")-days(1),
      error = function(e) 
      {e
        #commented out on 5.19.22 because of duplicate functioning code above
        # text <- paste0('Something weird is happening with Participant ID #',current$comet_study_id,'s fitbit. On ',date,' there was an errror with retrieving last_sync, which is caused by an errant devices api call. This could mean an issue with the device.')
        # cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
        # mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email.py'), study_coordinator_email, file.path(project_dir,'data','modules','generic_email_module','email.txt'))
        # if(testing == 0 & current$comet_interventionstart >= today()){
        #   system(mailcmd)
        # }
        return(last_transfer)
      }
    )
    #number of days since last transfer to last full day of sync
    days <- as.numeric(ymd(last_sync) - ymd(last_transfer))
    
    #' if there are days to sync
    #' 1.13.23 Added if !(is.na(device_data) & current$presentweeknum > 52) to retrieve values if device_data exists or if the week is less than 52
    if(days > 0 & missing_device == FALSE & current$presentweeknum <= 52) {
      #loop through all days between last sync and last transfer
      message(paste0(current$comet_study_id,": collecting data, pt is in week",current$presentweeknum))
      for(j in 1:days)
      { 
        #date of analysis
        date <- ymd(last_transfer)+j
        
        #hr data.frame
        hr <- get_hr(date = date, usercode = current$user, token = current$access_token)
        
        #steps to check if pt wore watch at all during that day
        steps_day_of <- steps_all_df %>%
          filter(comet_study_id == current$comet_study_id) %>%
          filter(.$date == ymd(last_transfer)+j)
        
        hr_not_working <- 0 #Initialize HR watcher value
        #Send email to study coordinators that participant isn't collecting HR
        if(nrow(steps_day_of) == 0) {
          text <- paste0('Participant ID #',current$comet_study_id,'. Date:',date,'.  This participant appears to have synced but does not have steps data. This negates the option to check for no HR. Please fix')
          cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
          mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email.py'), study_coordinator_email, file.path(project_dir,'data','modules','generic_email_module','email.txt'))
          if(testing == 0){
            system(mailcmd)
          }
        } else if(nrow(hr) == 0 & steps_day_of$steps > 100 & nrow(steps_day_of) > 0) {
          hr_not_working <- 1
          
          #Send study coordinator email
          text <- paste0('It appears as though Participant ID #',current$comet_study_id,' wore their watch on ',date,'; however their HR was not being reported. This can be caused by the "On Wrist" setting being changed to "On Clip." It is recommended to do some digging and call the participant to figure out the issue.')
          cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
          mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email.py'), study_coordinator_email, file.path(project_dir,'data','modules','generic_email_module','email.txt'))
          if(testing == 0){
            system(mailcmd)
          }
        #Added else if on 11.4.22 for when pt logs an exercise session but has no steps and HR. This occurs when it is logged through the watch
        } else if(nrow(hr) == 0 & steps_day_of$steps == 0){
          hr_not_working <- 1
        }
        
        #activities.data.frame
        #simplify data.frame, calculate end time of exercise, and create null values for exercise to be calculate
        got_activities <- get_activities(date = date, usercode = current$user, token = current$access_token)
        
        #11.16.22 We started using time_in_zone for adherence data. Time_in_zone and time_over_zone were using
        #the total duration time. So if a participant logged a session over 60 minutes, the zone time could be
        #considerably longer. I fixed that for on today. So there will be errors before. The best example was a 
        #activity from #3 on 12/15/21. She had 90 minutes duration and 140 minutes in zone. We might re-run all
        #of the activity data to get hr data for the core & Fusion group. If we do, that will fix this issue. JC
        if(nrow(got_activities) > 0) {
          activities <- got_activities %>%
            mutate(comet_study_id = current$comet_study_id) %>%
            select(comet_study_id, activityParentName, duration, startDate, startTime) %>%
            mutate(startDate = ymd(startDate)) %>%
            mutate(duration = case_when(duration > 3600000 ~ 3600000,
                                        duration <= 3600000 ~ as.numeric(duration))) %>%
            mutate(end_time = hms::as_hms(hms::parse_hm(startTime) + duration/1000)) %>%
            mutate(time_in_zone = NA, time_over_zone = NA, average = NA) %>%
            mutate(duration = as.double(duration)) 

          
          if(hr_not_working == 0) {
            #isolates each activity and calculates minutes in moderate, vigorous, and average hr during activity
            for(k in 1:nrow(activities))
            {
              activity_hr <- hr %>%
                filter(hms::parse_hms(time) > hms::parse_hm(activities$startTime[k]) & hms::parse_hms(time) < activities$end_time[k]) %>%
                mutate(zone = (value >= current_exercise_prescription$hrr_min & value < current_exercise_prescription$hrr_max), over_zone = value >= current_exercise_prescription$hrr_max)
              
              activities$time_in_zone[k] = sum(activity_hr$zone)
              activities$time_over_zone[k] = sum(activity_hr$over_zone)
              activities$average[k] = mean(activity_hr$value)
            }
          }
          #Export the activity data
          activities_log <- bind_rows(activities_log, activities)
          export(activities_log, file.path(project_dir,'data','raw','activities_data',paste0('146904_',current$comet_study_id,'.csv')))
          
        } 
        
        
        
        #save intraday data
        export(hr, file.path(project_dir,'data','raw','hr_data','intraday',paste0('146904_',current$comet_study_id),paste0(as.character(date),'_146904_',current$comet_study_id,'.csv')))
        
        #if less than 10 minutes of hr data
        if(nrow(hr)<10) 
        {
          ex_forloop <- NA
          ex_averaging <- NA
          minutes_worn <- NA
          
        } else {
          
          #run hr data.frame through forloop and averaging functions
          ex_forloop <- filter_exercise_forloop(df = hr, moderate = min_hr_mvpa_40)
          ex_averaging <- filter_exercise_averaging(df = hr, moderate = min_hr_mvpa_40)
          minutes_worn <- nrow(hr)
        }
        
        #create row to append to hr log
        dates_row <- as.data.frame(current$comet_study_id) %>% 
          rename(comet_study_id = `current$comet_study_id`) %>%
          mutate(date = date, ex_iterative = ex_forloop, ex_averaging = ex_averaging, minutes_worn = minutes_worn)
        
        hr_log <- bind_rows(hr_log, dates_row)
        
        export(hr_log, file.path(project_dir,'data','raw','hr_data',paste0('146904_',current$comet_study_id,'.csv')))
      }
      
      #Send email to participant that their HR isn't working
      #1.3.23 Adding trainer to the email. JC
      if(hr_not_working == 1 & steps_day_of$steps > 100 & nrow(steps_day_of) > 0) {
        
        ##### Select Fitbit Guide
        #Added new guides on 9.22.22
        device <- import(file.path(data_dir,'raw','device_data',paste0('146904_',current$comet_study_id,'.csv'))) %>%
          filter(date == max(date)) %>%
          pull(deviceVersion)
        
        #Format Email
        if(device == "Inspire 2"){
          on_clip_error_email_inspire(fname = current$scrn_fname, email = current$scrn_main_email)
        } else {
          on_clip_error_email_charge(fname = current$scrn_fname, email = current$scrn_main_email)
        }
        
        #Find trainer email
        trainer_email = trainer_list$work_email[which(trainer_list$record_id == current$gym_trainer)]
        
        #Testing command
        #mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','participant_error','send_email.py'), 'study_coordinator_2@kumc.edu', file.path(project_dir,'data','modules','participant_error','email.txt'), 'study_coordinator_2@kumc.edu')
        
        #Actual command
        mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','participant_error','send_email.py'), current$scrn_main_email, file.path(project_dir,'data','modules','participant_error','email.txt'), trainer_email)

        if(testing == 0 & eric_at_home == 0){
          system(mailcmd)
        }
        
      }
      
    } 
  }  else {
    
    #if not there -> create
    #hr log
    temp <- data.frame(comet_study_id=current$comet_study_id, date=current$randomization_date ,ex_iterative="", ex_averaging="", minutes_worn="")
    file.create(file.path(project_dir,'data','raw','hr_data',paste0('146904_',active_participants$comet_study_id[i],'.csv')))
    export(temp, file.path(project_dir,'data','raw','hr_data',paste0('146904_',active_participants$comet_study_id[i],'.csv')))
    dir.create(file.path(project_dir,'data','raw','hr_data','intraday',paste0('146904_',active_participants$comet_study_id[i])))
    
    #activities log
    temp <- data.frame(comet_study_id = current$comet_study_id, activityParentName = "", duration = "", startDate = current$randomization_date, startTime = "", end_time = "", time_in_zone = "", time_over_zone = "", average = "")
    file.create(file.path(project_dir,'data','raw','activities_data',paste0('146904_',active_participants$comet_study_id[i],'.csv')))
    export(temp, file.path(project_dir,'data','raw','activities_data',paste0('146904_',active_participants$comet_study_id[i],'.csv')))
  }
  
  
  
  
  
}

