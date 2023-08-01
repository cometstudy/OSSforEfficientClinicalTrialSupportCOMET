#' @author Jon Clutton
#' 
#' @name texting_system
#' 
#' @title Send Reminder Text Messages
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas
#'
#' @section Development:
#' 8.11.21 Began developing JC \cr
#' 9.15.21 Added battery_level notification module JC \cr
#' 10.14.21 Issue sending messages to pts without a fitbit yet. Fixed quickly by nrow(fitbit_log) != 0. Need to monitor JC \cr
#' 10.21.21 Issue with texters dataframe switched to left_join with fitbit_users JC \cr
#' 11.30.21 Need to add notification to study coordinator and add a prompt to reply Y if they can attend. JC \cr
#' 11.30.21 Notifications to SC have been added. Added a prompt. And Jon is receiving a copy of all outgoing participant messages to test. JC \cr
#' 2.23.22 All texts appear to be working correctly. Removing Jon from send. JC \cr
#' 2.23.22 Coordinator emails weren't sending because cmd line prompt arg[3] started with #. Fixed. JC \cr
#' 3.30.22 Updating texters to remove any screen fails. JC \cr
#' 3.30.22 Updated fitbit reminders to remove inactive or non-adherent (10), stop messages (12), and withdrawn unwilling to complete (7) JC \cr
#' 4.12.22 Added notification log that records any notification sent. JC \cr
#' 4.15.22 Fixed issue with notification log. All messages sent after 4.16.22 should be recorded. JC \cr
#' 5.5.22 After all dates were updated from iDates, some date addition/subtraction language needs to be rewritten. Completed today. JC \cr
#' 6.27.22 Updated congratulation texts to only be from workout, weights, or yoga sessions. JC \cr
#' 7.18.22 Updated consumer cellular extension to mailmymobile.net. Previous was failing to send. JC \cr
#' 8.1.22 A lot of text messages are being declined. Switched sending email to study_coordinator_2 to diagnose. JC \cr
#' 8.2.22 I think that all text message notifications are being blocked. Switching to email. JC \cr
#' 8.15.22 Changed email back to Study. All messages are sending through email until texting issues can be fixed. JC \cr
#' 9.6.22 Updated message to send different reminders to baseline vs intervention participants. Added on Data is lost after 7 days to intervention message. JC \cr
#' 10.12.22 The team has received messages about the syncing messages sending incorrect data. This was caused by the one day delay in how 
#' we collect data (to prevent data loss). I added in a new last_sync module today so that the syncing messages are always up to date. 
#' Because of this, I also added in another error message. If a participant's account does not return a battery, they are prompted to call 
#' JC at 469-995-6928 to fix the issue. JC \cr
#' 11.1.22 I corrected an issue with the new last_sync function that caused the messages to send a day early. JC \cr
#' 11.17.22 Added language about fasting to gxt reminder text. JC \cr
#'    
#' @description 
#' This script sends a reminder text message to participants with upcoming appointments. 
#' It also sends a text message to participants who haven't synced data. 
#' It also sends a text message when participant fitbits go below 30%. 


message("Beginning 08 texting system")

###### Settings ######
# eric_at_home is in comet_nucleus. When set to 1, it stops messages from being sent
# test_coordinator is set to 1 when want to stop messages to participants but keep to study coordinator
 test_coordinator <- 1

###### Load Data #####
load(file.path(project_dir,'data','clean','comet_clean.Rdata'))
load(file.path(data_dir,'clean','text_notification_log.Rdata'))

#### App info
client_info <- import(file.path(project_dir,'data','read_write','read_write','client_info.csv')) 

### User info
fitbit_users <- read.csv(file.path(data_dir,'read_write','read_write','fitbit_users.csv'), na.strings = c("", " "))


######### Check to see if tokens need to be refreshed #####
state <- get_status(usercode = fitbit_users$user[1], token = fitbit_users$access_token[1])

if(state >= 400) {
  source(file.path(script_dir,'02_refresh_token.R'))
}

####### All eligible to receive text ####
#Tested on 4.20.22 - all eligible texters seem to be in this list. 
#' 20230307: Added filter to only send when in weeks less than 53
texters <- comet %>%
  filter(redcap_event_name == "baseline_arm_1") %>%
  filter(status___6 == 0 & status___8 == 0 & status___9 == 0 & baseline_status < 3) %>%
  filter(presentweeknum < 53) %>%
  filter(ipscrn_texts == 1) %>%
  left_join(., fitbit_users, by = c("comet_study_id" = "id")) %>%
  arrange(comet_study_id)



#a flagging tool for debugging
message_flag <- 0
if(message_flag==1){message("launch for statement")}
   
####### Send messages ######

    for(i in 1:nrow(texters)){
        current_texter <- texters[i,]  
        
        if(current_texter$ipscrn_texts_method == 1) {
          number <- gsub("\\D", "", current_texter$scrn_phone)
          mmsclient <- case_when(current_texter$ipscrn_mobile_carrier == 1 ~ '@txt.att.net',
                                 current_texter$ipscrn_mobile_carrier == 2 ~ '@sms.myboostmobile.com',
                                 current_texter$ipscrn_mobile_carrier == 3 ~ '@mms.cricketwireless.net',
                                 current_texter$ipscrn_mobile_carrier == 4 ~ '@msg.fi.google.com',
                                 current_texter$ipscrn_mobile_carrier == 5 ~ '@text.republicwireless.com',
                                 current_texter$ipscrn_mobile_carrier == 6 ~ '@messaging.sprintpcs.com',
                                 current_texter$ipscrn_mobile_carrier == 7 ~ '@vtext.com',
                                 current_texter$ipscrn_mobile_carrier == 8 ~ '@tmomail.net',
                                 current_texter$ipscrn_mobile_carrier == 9 ~ '@message.ting.com',
                                 current_texter$ipscrn_mobile_carrier == 10 ~ '@mmst5.tracfone.com',
                                 current_texter$ipscrn_mobile_carrier == 11 ~ '@email.uscc.net',
                                 current_texter$ipscrn_mobile_carrier == 12 ~ '@vtext.com',
                                 current_texter$ipscrn_mobile_carrier == 13 ~ '@vmobl.com',
                                 current_texter$ipscrn_mobile_carrier == 14 ~ '@mailmymobile.net',
                                 current_texter$ipscrn_mobile_carrier == 15 ~ '@vtext.com')

          to = paste0(number,mmsclient)
          method <- "text"
        } else {
          to = current_texter$scrn_main_email
          method <- "email"
        }
        
        #to = "4699956928@txt.att.net"
        from = "study_coordinator_2@kumc.edu"
        #from = study_coordinator_email
      
        if(message_flag==1){message("For loop: ",i, " of ", nrow(texters))
        message("clients declared")}
        
        print(paste(current_texter$comet_study_id, today() > current_texter$comet_baselinegxtsched + days(10) & !is.na(current_texter$comet_baselinegxtsched) & current_texter$status___7 == 0 & current_texter$status___12 == 0 & current_texter$status___10 == 0))
        ##### Sending Fitbit Reminders ####
        if(today() > current_texter$comet_baselinegxtsched + days(10) & !is.na(current_texter$comet_baselinegxtsched) & current_texter$status___7 == 0 & current_texter$status___12 == 0 & current_texter$status___10 == 0) {
          
          
          fitbit_log <- hr_all_df %>%
            filter(comet_study_id == current_texter$comet_study_id)

          last_transfer <- fitbit_log$date[nrow(fitbit_log)]

          days_since_last_transfer <- as.numeric(today() - ymd(last_transfer))

          recent_sessions <- activities_all_df %>%
            filter(comet_study_id == current_texter$comet_study_id) %>%
            filter(activityParentName == "Workout" | activityParentName == "Yoga" | activityParentName == "Weights") %>%
            mutate(date = ymd(startDate)) %>%
            slice_tail(n = 2)

          if(nrow(recent_sessions) == 2 & !is.na(recent_sessions$duration[1])) {
            new_exercise_session <- recent_sessions$date[2]
            old_exercise_session <- recent_sessions$date[1]
          }
          
          #10.12.22 - Added to make last_sync accurate when messages are sent
          #11.1.22 - I was subtracting 1 day from this, causing it to send after two days. Corrected on 11/1. JC
          last_sync <- tryCatch(
            floor_date(ymd_hms(get_last_sync(usercode = current_texter$user, token = current_texter$access_token)), unit = "day"),
            error = function(e) 
            {e
              NA
            }
          )
          
          #10.12.22 - Added to make last_sync accurate when messages are sent
          days_since_last_sync <- if(!is.na(last_sync)) {
            as.numeric(today() - ymd(last_sync))
          } else {
            -1
          }
          
          if(nrow(fitbit_log) != 0) {
            if(days_since_last_sync == 3 | days_since_last_sync == 5 | days_since_last_sync >= 7 & current_texter$status___7 == 0) {
              
              #9.6.22 Updated message to send different reminders to baseline vs intervention participants. Added on Data is lost after 7 days to intervention message.
              if(today() < current_texter$comet_interventionstart){
                body <- paste0('KU ADC COMET Study - This message is to let you know your Fitbit is not syncing. If you are unsure what to do, you may disregard this message. You will receive reminders like this during the study. KU ADC COMET Study - Your Fitbit has not synced for ',days_since_last_sync,' days. Please sync your fitbit so that no data is lost. Data is lost after 7 days.')
              } else {
                body <- paste0('KU ADC COMET Study - Your Fitbit has not synced for ',days_since_last_sync,' days. Please sync your fitbit so that no data is lost. Data is lost after 7 days.')
              }
              cat(body, file = file.path(project_dir,'data','text_email','fitbit_reminder_text.txt'))
              mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_text_email.py'), to, file.path(project_dir,'data','text_email','fitbit_reminder_text.txt'), from)
             
              if(eric_at_home == 0){
                system(mailcmd)
                
                notification_row <- data.frame(comet_study_id = current_texter$comet_study_id, date_time = now(), notification = paste("Reminder to sync after", days_since_last_sync, "days"), method_sent = method)
                text_notification_log <- bind_rows(text_notification_log, notification_row)
              }
              
              #mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_text_email.py'), "4699956928@txt.att.net", file.path(project_dir,'data','text_email','fitbit_reminder_text.txt'), study_coordinator_email)
              #system(mailcmd)
              
              body2 <- paste0('Participant ID #',current_texter$comet_study_id,' has not synced their data for ',days_since_last_sync,' days. If it has been 5 days or more, please call the participant. Data is lost after 7 days. <br><br> Participant Name: ',current_texter$scrn_fname,' ',current_texter$scrn_lname,'<br>Participant Number: ',current_texter$scrn_phone)
              cat(body2, file = file.path(project_dir,'data','text_email','coordinator_reminder_email.txt'))
              mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_coordinator_email.py'), study_coordinator_email, file.path(project_dir,'data','text_email','coordinator_reminder_email.txt'), "Fitbit_Sync_reminder")
              
              #10.24.22 Added days_since_last_sync so that we get fewer emails. JC
              if((eric_at_home == 0 | test_coordinator == 1) & days_since_last_sync >= 5){
                system(mailcmd)
              }
              
              #10.12.22 Added this else if. If the API to return last sync fails, participant gets error text message.
              #This text message is new as of 10.12.22 JC
            } else if(days_since_last_sync == -1 & !is.na(current_texter$comet_interventionstart) & today() > current_texter$comet_interventionstart){
              body <- paste0('KU ADC COMET Study - We had an error with your Fitbit account. Please call Jon Clutton at 469-995-6928 to troubleshoot.')
              cat(body, file = file.path(project_dir,'data','text_email','fitbit_reminder_text.txt'))
              mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_text_email.py'), to, file.path(project_dir,'data','text_email','fitbit_reminder_text.txt'), from)
              
              if(eric_at_home == 0){
                system(mailcmd)
                
                notification_row <- data.frame(comet_study_id = current_texter$comet_study_id, date_time = now(), notification = paste("Fitbit account error. Last_sync did not return value."), method_sent = method)
                text_notification_log <- bind_rows(text_notification_log, notification_row)
              }
              
              #mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_text_email.py'), "4699956928@txt.att.net", file.path(project_dir,'data','text_email','fitbit_reminder_text.txt'), study_coordinator_email)
              #system(mailcmd)
              
              body2 <- paste0('Participant ID #',current_texter$comet_study_id,' was informed of a generic Fitbit account error. They were prompted to call Jon Clutton at 469-995-6928. This is a new error that was added on 10.12.22. <br><br> Participant Name: ',current_texter$scrn_fname,' ',current_texter$scrn_lname,'<br>Participant Number: ',current_texter$scrn_phone)
              cat(body2, file = file.path(project_dir,'data','text_email','coordinator_reminder_email.txt'))
              mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_coordinator_email.py'), study_coordinator_email, file.path(project_dir,'data','text_email','coordinator_reminder_email.txt'), "Generic_Fitbit_Account_Error")
              
              if(eric_at_home == 0 | test_coordinator == 1){
                system(mailcmd)
              }
              
            }
            if(message_flag==1){message("Fitbit log")}
            
            ##### Low Battery Message ####
            #If the API call throws an error, a message is sent to the study team. 
            battery_level <- tryCatch(
              get_battery_level(usercode = current_texter$user, token = current_texter$access_token),
              error = function(e) 
              {e
                text <- paste0('It appears as though we were unable to return a battery level Participant ID #',current_texter$comet_study_id,'s fitbit. This is likely because the participant is in baseline testing and has not had their Fitbit set up yet. If so, please mark "Stop Fitbit Reminder Texts" in Status Summary until the Fitbit is setup.')
                cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
                mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email.py'), study_coordinator_email, file.path(project_dir,'data','modules','generic_email_module','email.txt'))
                if(testing == 0){
                  system(mailcmd)
                }
                return(100)
              }
            )
            
            #10.12.22, Changed Fitbit battery level to 30%
            if(battery_level < 30 & current_texter$status___7 == 0) {
              
              body <- paste0('KU ADC COMET Study - Your Fitbit battery is below 30%. Remember to charge your Fitbit at night.')
              cat(body, file = file.path(project_dir,'data','text_email','fitbit_battery_text.txt'))
              mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_text_email.py'), to, file.path(project_dir,'data','text_email','fitbit_battery_text.txt'), from)
              
              if(eric_at_home == 0){
                system(mailcmd)
                
                notification_row <- data.frame(comet_study_id = current_texter$comet_study_id, date_time = now(), notification = paste("Reminder to charge"), method_sent = method)
                text_notification_log <- bind_rows(text_notification_log, notification_row)
              }
              
              #mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_text_email.py'), "4699956928@txt.att.net", file.path(project_dir,'data','text_email','fitbit_battery_text.txt'), study_coordinator_email)
              #system(mailcmd)
              
            }
            if(message_flag==1){message("Fitbit Battery Level")}
            
            ##### Congratulatory Text ####
              if(new_exercise_session - old_exercise_session > 7 & last_transfer == today()-1) {
                
                body <- paste0('KU ADC COMET Study - Way to go!! Good job exercising. You are doing great. Keep up the hard work and let us know if you need anything.')
                cat(body, file = file.path(project_dir,'data','text_email','encouragement_text.txt'))
                mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_text_email.py'), to, file.path(project_dir,'data','text_email','encouragement_text.txt'), from)
                
                if(eric_at_home == 0){
                  system(mailcmd)
                  
                  notification_row <- data.frame(comet_study_id = current_texter$comet_study_id, date_time = now(), notification = paste("Congratulatory Text - back to exercise on",new_exercise_session, "after",new_exercise_session - old_exercise_session, "days"), method_sent = method)
                  text_notification_log <- bind_rows(text_notification_log, notification_row)
                }
                
                #mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_text_email.py'), "4699956928@txt.att.net", file.path(project_dir,'data','text_email','fitbit_reminder_text.txt'), study_coordinator_email)
                #system(mailcmd)
                
                body2 <- paste0('Participant ID #',current_texter$comet_study_id,' logged an exercise session for the first time in more than 7 days. A congratulatory text has been sent.')
                cat(body2, file = file.path(project_dir,'data','text_email','coordinator_reminder_email.txt'))
                mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_coordinator_email.py'), study_coordinator_email, file.path(project_dir,'data','text_email','coordinator_reminder_email.txt'), "Congratulations_message")
                
                if(eric_at_home == 0 | test_coordinator == 1){
                  system(mailcmd)
                }
                
                
              }
          }
        }
        ##### Sending Appintment Reminder Module ####
          
          days_to_t1cog <- as.numeric(as_date(current_texter$comet_baselinecogsched) - today())
          days_to_t2cog <- as.numeric(as_date(current_texter$comet_week26cogsched) - today())
          days_to_t3cog <- as.numeric(as_date(current_texter$comet_week52cogsched) - today())
          
          days_to_t1gxt <- as.numeric(as_date(current_texter$comet_baselinegxtsched) - today())
          days_to_t3gxt <- as.numeric(as_date(current_texter$comet_week52gxtsched) - today())
          
          days_to_t1mri <- as.numeric(as_date(current_texter$comet_baselinemrisched) - today())
          days_to_t3mri <- as.numeric(as_date(current_texter$comet_week52mrisched) - today())
          
          format_date <- format.Date(today()+2, "%m-%d-%Y")
          
          if(message_flag==1){message("Through format_date")}
          
               if((days_to_t1cog == 2 & !is.na(days_to_t1cog)) | (days_to_t2cog == 2 & !is.na(days_to_t2cog)) | (days_to_t3cog == 2 & !is.na(days_to_t3cog))) {
                 
                 cog_time <- case_when(days_to_t1cog == 2 ~ format.Date(current_texter$comet_baselinecogsched, "%H:%M"), 
                                       days_to_t2cog == 2 ~ format.Date(current_texter$comet_week26cogsched, "%H:%M"),
                                       days_to_t3cog == 2 ~ format.Date(current_texter$comet_week52cogsched, "%H:%M"))
                 
                 body <- paste0('KU ADC COMET Study - You have an upcoming cognitive appointment on ',format_date,' at ',cog_time,'. If you cannot attend and need to reschedule, please email or call Study Coordinator at coordinator_email@kumc.edu or coordinator_number.')
                 cat(body, file = file.path(project_dir,'data','text_email','appointment_reminder_text.txt'))
                 mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_text_email.py'), to, file.path(project_dir,'data','text_email','appointment_reminder_text.txt'), from)
                 
                 if(eric_at_home == 0){
                   system(mailcmd)
                   
                   notification_row <- data.frame(comet_study_id = current_texter$comet_study_id, date_time = now(), notification = paste("Cog test reminder"), method_sent = method)
                   text_notification_log <- bind_rows(text_notification_log, notification_row)
                 }
                 
                 #mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_text_email.py'), "4699956928@txt.att.net", file.path(project_dir,'data','text_email','appointment_reminder_text.txt'), study_coordinator_email)
                 #system(mailcmd)
                 
                 body2 <- paste0('Participant ID #',current_texter$comet_study_id,' was reminded about their cognitive test on ',format_date,' at ',cog_time,'. They will call or email if they need to reschedule.  <br><br> Participant Name: ',current_texter$scrn_fname,' ',current_texter$scrn_lname,'<br>Participant Number: ',current_texter$scrn_phone)
                 cat(body2, file = file.path(project_dir,'data','text_email','coordinator_reminder_email.txt'))
                 mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_coordinator_email.py'), study_coordinator_email, file.path(project_dir,'data','text_email','coordinator_reminder_email.txt'), paste0(current_texter$comet_study_id, "_Cog_Reminder_Sent"))
                 

                 if(eric_at_home == 0 | test_coordinator == 1){
                   system(mailcmd)
                 }
                 
                 
               }
          
          if(message_flag==1){message("Through days to tcog")}
          
          #11.17.22 Added language about fasting. JC      
          if((days_to_t1gxt == 2 & !is.na(days_to_t1gxt)) |  (days_to_t3gxt == 2 & !is.na(days_to_t3gxt))) {
                  
                  gxt_time <- case_when(days_to_t1gxt == 2 ~ format.Date(current_texter$comet_baselinegxtsched, "%H:%M"), 
                                        days_to_t3gxt == 2 ~ format.Date(current_texter$comet_week52gxtsched, "%H:%M"))
                  
                  body <- paste0('KU ADC COMET Study - You have an upcoming physical assessment appointment on ',format_date,' at ',gxt_time,'.  Please remember to fast for 4 hours prior to appointment time. If you cannot attend and need to reschedule, please email or call Study Coordinator at coordinator_email@kumc.edu or coordinator_number.')
                  cat(body, file = file.path(project_dir,'data','text_email','appointment_reminder_text.txt'))
                  mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_text_email.py'), to, file.path(project_dir,'data','text_email','appointment_reminder_text.txt'), from)
                  
                  if(eric_at_home == 0){
                    system(mailcmd)
                    
                    notification_row <- data.frame(comet_study_id = current_texter$comet_study_id, date_time = now(), notification = paste("GXT test reminder"), method_sent = method)
                    text_notification_log <- bind_rows(text_notification_log, notification_row)
                  }
                  
                  #mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_text_email.py'), "4699956928@txt.att.net", file.path(project_dir,'data','text_email','appointment_reminder_text.txt'), study_coordinator_email)
                  #system(mailcmd)
                  
                  
                  body2 <- paste0('Participant ID #',current_texter$comet_study_id,' was reminded about their gxt on ',format_date,' at ',gxt_time,'. They will call or email if they need to reschedule.  <br><br> Participant Name: ',current_texter$scrn_fname,' ',current_texter$scrn_lname,'<br>Participant Number: ',current_texter$scrn_phone)
                  cat(body2, file = file.path(project_dir,'data','text_email','coordinator_reminder_email.txt'))
                  mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_coordinator_email.py'), study_coordinator_email, file.path(project_dir,'data','text_email','coordinator_reminder_email.txt'), paste0(current_texter$comet_study_id, "_GXT_Reminder_Sent"))
                  
                  if(eric_at_home == 0 | test_coordinator == 1){
                    system(mailcmd)
                  }
                  
                  
                } 
          
          if(message_flag==1){message("Through days to tgxt")}
                
                if((days_to_t1mri == 2 & !is.na(days_to_t1mri)) |  (days_to_t3mri == 2 & !is.na(days_to_t3mri))) {
                  
                  mri_time <- case_when(days_to_t1mri == 2 ~ format.Date(current_texter$comet_baselinemrisched, "%H:%M"), 
                                        days_to_t3mri == 2 ~ format.Date(current_texter$comet_week52mrisched, "%H:%M"))
                  
                  body <- paste0('KU ADC COMET Study - You have an upcoming MRI appointment on ',format_date,' at ',mri_time,'.  If you cannot attend and need to reschedule, please email or call Study Coordinator at coordinator_email@kumc.edu or coordinator_number.')
                  cat(body, file = file.path(project_dir,'data','text_email','appointment_reminder_text.txt'))
                  mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_text_email.py'), to, file.path(project_dir,'data','text_email','appointment_reminder_text.txt'), from)
                  
                  if(eric_at_home == 0){
                    system(mailcmd)
                    
                    notification_row <- data.frame(comet_study_id = current_texter$comet_study_id, date_time = now(), notification = paste("MRI reminder"), method_sent = method)
                    text_notification_log <- bind_rows(text_notification_log, notification_row)
                  }
                  
                  #mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_text_email.py'), "4699956928@txt.att.net", file.path(project_dir,'data','text_email','appointment_reminder_text.txt'), study_coordinator_email)
                  #system(mailcmd)
                  
                  body2 <- paste0('Participant ID #',current_texter$comet_study_id,' was reminded about their mri on ',format_date,' at ',mri_time,'. They will call or email if they need to reschedule.  <br><br> Participant Name: ',current_texter$scrn_fname,' ',current_texter$scrn_lname,'<br>Participant Number: ',current_texter$scrn_phone)
                  cat(body2, file = file.path(project_dir,'data','text_email','coordinator_reminder_email.txt'))
                  mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','text_email','send_coordinator_email.py'), study_coordinator_email, file.path(project_dir,'data','text_email','coordinator_reminder_email.txt'), paste0(current_texter$comet_study_id, "_MRI_Reminder_Sent"))
                  
                  if(eric_at_home == 0 | test_coordinator == 1){
                    system(mailcmd)
                  }
                  
                  
                }  
          if(message_flag==1){message("Through days to tmri")}
          

    }

df_to_save <- c('text_notification_log') 
save(list = df_to_save, file = file.path(data_dir,'clean','text_notification_log.Rdata'))
