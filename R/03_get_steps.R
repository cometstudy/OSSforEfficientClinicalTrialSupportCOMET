#' @name get_steps_script
#' 
#' @title Get Steps from Fitbit for all users
#' 
#' @description 
#' This script reads in fitbit_users (csv file of fitbit users and keys)
#' Checks the state of the fitbit_users, and refreshes if need be. 
#' The script queries for steps using get_steps
#' Steps are exported to 'data/raw/step_data/fitbit_users$id' and saved in the format date_study_id.csv
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas
#' 
#' @section Development:
#' 6.15.21 Updated the steps database to include comet_study_id in csv. Also, updated to only download active participants until last_sync \cr
#' 8.11.21 EDV updated the paths to import and export using file.path which makes for a slightly more reliable path build. \cr
#' 9.15.21 Fixed error in refreshing tokens \cr
#' 11.10.21 Implemented tryCatch to last_sync API call in case a sync does not occur at first fitbit setup. JC \cr
#' 8.17.22 06_read_clean_merge runs very slowly because it has to recursively index all step files. As step files are downloaded
#' each day, they are beginning to build up. Here's my fix - Reconcile steps with most recent steps. I imported the most recent
#' steps download and check it against today's steps export to make sure it contains all of the same data with only a couple of rows added. I had 
#' to change the if statement structure, with dir.exists moving to the beginning of the script. See Github if you need specific changed. JC \cr
#' 9.14.22 I updated the changes I made on 8.17.22. The changes were still saving every spreadsheet because the steps as of today() would not always
#' be up to date. I did two fixes 1) I only sync until last_sync and 2) I check the old spreadsheet for matching date and comet_study_id. I'll
#' put a reminder on my calendar to check the impact in one week. JC \cr
#' 
#' @author Jon clutton
message("Beginning 03 get steps")


###### Load Data #####
load(file.path(project_dir,'data','clean','comet_clean.Rdata'))


#### App info
client_info <- import(file.path(project_dir,'data','read_write','read_write','client_info.csv')) 

### User info
fitbit_users <- read.csv(file.path(data_dir,'read_write','read_write','fitbit_users.csv'), na.strings = c("", " "))

######### Check to see if tokens need to be refreshed #####
state <- get_status(usercode = fitbit_users$user[1], token = fitbit_users$access_token[1])

if(state >= 400) {
  source(file.path(script_dir,'02_refresh_token.R'))
}

#### COMET REDCap and fitbit_users joined - CHANGE PATH ####
#filtered for randomized participants
active_participants <- comet %>%
  filter(redcap_event_name == 'baseline_arm_1') %>%
  filter(is.na(comet_study_id)==F) %>%
  right_join(., fitbit_users, by = c("comet_study_id" = "id")) %>%
  filter(status___6 == 0 | status___7 == 0 | status___8 == 0 | status___9 == 0) %>%
  filter(intervention_status <= 1 | is.na(intervention_status)) %>%
  filter(is.na(comet_study_id)==F) %>%
  filter(is.na(randomization_date)==F)

################# Get Steps ############
for(i in 1:nrow(active_participants)){
  current <- active_participants[i, ]
  
  last_sync <- tryCatch(
    floor_date(ymd_hms(get_last_sync(usercode = active_participants$user[i], token = active_participants$access_token[i])), unit = "day")-days(1),
    error = function(e) 
    {e
      current$randomization_date
    }
  )
  
  if(dir.exists(file.path(project_dir,'data','raw','step_data',current$comet_study_id))==T) {
    
    #Added on 8.17.22 from 06_read_clean_merge
    last_download <- file.info(list.files(file.path(project_dir,'data','raw','step_data',current$comet_study_id), full.names = TRUE, recursive = TRUE)) %>%
      arrange(desc(ctime)) %>%
      slice_max(ctime) 
    
    #Added on 8.17.22 to retrieve last archived steps
    most_recent_steps <- import(rownames(last_download)) %>%
      mutate(date = as.character(date)) %>%
      mutate(steps = as.character(steps))
    
    if(last_sync != current$randomization_date) {
      steps <- get_steps(start_date = current$randomization_date, end_date = last_sync, usercode = current$user, token = current$access_token) %>%
        rename("date" = c(1), steps = "activities-steps.value") %>%
        mutate(comet_study_id = current$comet_study_id) 
      
      #Added on 8.17.22. This checks the archived steps download with todays API export. It checks if there is any info in the archived download that is different
      #If so, the old file is kept in addition to the new file. If not, the old file deletes and is replaced by the newest API export
      overlapping <- anti_join(most_recent_steps, steps, by = c("date","comet_study_id"))
      
      if(nrow(overlapping) == 0){   
        file.remove(rownames(last_download))
        export(steps, file.path(project_dir,'data','raw','step_data',current$comet_study_id,paste0(as.character(today()),'_146904_',current$comet_study_id,'.csv')))
      } else if(nrow(overlapping) != 0) {
        export(steps, file.path(project_dir,'data','raw','step_data',current$comet_study_id,paste0(as.character(today()),'_146904_',current$comet_study_id,'.csv')))
      }
    }
    
  } else if(dir.exists(file.path(project_dir,'data','raw','step_data',current$comet_study_id))==F) {
    
    if(last_sync != current$randomization_date) {
      steps <- get_steps(start_date = current$randomization_date, end_date = last_sync, usercode = current$user, token = current$access_token) %>%
        rename("date" = c(1), steps = "activities-steps.value") %>%
        mutate(comet_study_id = current$comet_study_id) 
      
      dir.create(file.path(project_dir,'data','raw','step_data',current$comet_study_id))
      export(steps, file.path(file.path(project_dir,'data','raw','step_data',current$comet_study_id),paste0(as.character(today()),'_146904_',current$comet_study_id,'.csv')))
    }
  }
}


