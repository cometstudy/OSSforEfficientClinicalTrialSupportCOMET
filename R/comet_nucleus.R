#' @title The parent script of the comet project
#' 
#' @name comet_nucleus
#' 
#' @description 
#' comet_nucleus is a simple, but powerful script called by comet_coma.R on the adc shiny server.
#' The purpose is to declare all essential libraries and launch the process of running scripts created
#' for the COMET study. comet_nucleus can be run as the parent or wrapper script, which is why paths are 
#' delcared if objects don't already exist. This allows Jon Clutton or other staff to work on developing 
#' scripts without needing access to the adc-Shinydev server. Or it can be used as it was meant to be, 
#' by being called from the comet_coma.R wrapper/parent script that lives on the adc-ShinyR server. 
#' This is how it will be used as in it's production mode.
#' Written by Eric Vidoni and Jon Clutton
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas
#' 
#' @section Development:
#' 8.26.21 Committing to GitHub JC \cr
#' 10.1.21 Added code to source 09_scientist_email.R on Mondays \cr
#' 10.22.21 Added in 10_croms_report.R JC \cr
#' 10.25.21 Added in 00_bootstrap_error.R. This could eventually turn into a script for generic errors to check for. JC \cr
#' 12.10.21 EV changed tryCatch and script order to for loop to catch errors and send a message. EDV \cr
#' 12.11.21 Redcap bootstrap is running over and over. Think this is because of the while loop inside the for loop. Changing while to if statement. JC \cr
#' 2.10.22 Save directories.Rdata so that all markdown scripts can load this directory as opposed to create their own. JC \cr
#' 2.11.22 Added ymca_liason email. JC
#' 4.26.22 Added remove workspace command at end of script, so that a new session doesn't need to be opened to run script. JC \cr
#' 4.27.22 Pandoc error. changed eric_at_home to 1 so that Colin can test. JC \cr
#' 5.3.22 Removed 05_get_copy_electronic_data from script list as it duplicated 05_quarantine_file_handler. JC \cr
#' 5.19.22 Rewrote testing variable - sets to 0 if script is being run at cron job time 6:30 AM. JC \cr
#' 5.27.22 Added 05_vo2_aggregation and foreach library. JC \cr
#' 7.13.22 Added 06_exercise_data_consolidation to script order. JC \cr
#' 3.31.23 Moved 05_clean_redcap to beginning of order and added 16_data_export. JC \cr

##### Load Packages #####
if(all(grepl("tidyverse",(.packages()))==FALSE)){require(tidyverse)}
if(all(grepl("rlang",(.packages()))==FALSE)){require(rlang)}
if(all(grepl("R.utils",(.packages()))==FALSE)){require(R.utils)}
if(all(grepl("gtools",(.packages()))==FALSE)){require(gtools)}
if(all(grepl("rio",(.packages()))==FALSE)){require(rio)}
if(all(grepl("htmlTable",(.packages()))==FALSE)){require(htmlTable)}
if(all(grepl("httr",(.packages()))==FALSE)){require(httr)}
if(all(grepl("lubridate",(.packages()))==FALSE)){require(lubridate)}
if(all(grepl("markdown",(.packages()))==FALSE)){require(markdown)}
if(all(grepl("knitr",(.packages()))==FALSE)){require(knitr)}
if(all(grepl("jsonlite",(.packages()))==FALSE)){require(jsonlite)}
if(all(grepl("png",(.packages()))==FALSE)){require(png)}
if(all(grepl("htmlTable",(.packages()))==FALSE)){require(htmlTable)}
if(all(grepl("flextable",(.packages()))==FALSE)){require(flextable)}
if(all(grepl("DT",(.packages()))==FALSE)){require(DT)}
if(all(grepl("gt",(.packages()))==FALSE)){require(gt)}
if(all(grepl("gtsummary",(.packages()))==FALSE)){require(gtsummary)}
if(all(grepl("ggpmisc",(.packages()))==FALSE)){require(ggpmisc)}
if(all(grepl("encryptr",(.packages()))==FALSE)){require(encryptr)}

#message("Required libraries loaded.")
print(Sys.info())

if(hour(now()) == 6) {
  testing <<- 0  #stops all messages to both participant and study team when set to 1
} else {
  testing <<- 1
}
execution <<- 1 #initialize this watcher value
messages_flag <<- 1 #print message for debugging or not
eric_at_home <<- 0 #initialize a flag to prevent messages to study participants

#declare error message function for reuse throughout
error_msg <- function(i,next_up){
	msg_txt <- eval(parse(text="paste(next_up,\"in step\",i,\"appears to have failed. Sending error email.\n\")"))
	message(msg_txt)
	source(list.files(file.path(script_dir),'daily_execution_notification',full.names=TRUE))
}

out <- tryCatch({

  if(messages_flag ==1){message(paste("Try function entered.\n"))}
  

  ##### Build Directories #####
  if(Sys.info()["nodename"]=="workstation_2" & Sys.info()["user"]=="user2"){  #This is Eric's work desktop Mac
    root_p <- file.path("","drives","university_drive2")
    root_server_dir <- file.path(root_p,"study_dir_a")
    python_cmd_prefix <- "python "
  } else if(Sys.info()["nodename"]=="workstation_1" & Sys.info()["user"]=="study_coordinator_2") {
    root_p <- file.path("drive")
    root_server_dir <- file.path(root_p,"study_dir_a")
    python_cmd_prefix <- "py "
    #eric_at_home <<- 1
    testing <<- 0
    r_drive <- file.path("drive2","university_drive3","user2","146904","folder2")
  } else if(Sys.info()["nodename"]=="workstation_3" & Sys.info()["login"]=="sa-study_coordinator_2"){
    root_p <- ""
    root_server_dir <- file.path(root_p,"COMET")
    python_cmd_prefix <- "python "
    eric_at_home <<- 1
  }  else if(Sys.info()["nodename"]=="workstation_4" & Sys.info()["user"]=="user2"){  #This is eric's personal laptop
    root_p <- file.path("Z:")
    root_server_dir <- file.path(root_p,"study_dir_a")
    python_cmd_prefix <- "py "
    eric_at_home <<- 1
  } else if(Sys.info()["nodename"]=="workstation_3" & Sys.info()["user"]=="root"){
    root_p <- ""
    # root_server_dir <- file.path(root_p,"ADC_Data")
    root_server_dir <- file.path(root_p,"COMET")
    python_cmd_prefix <- "python "
  }
  
  print(paste("Eric at home = ",eric_at_home))
  
  message(paste0("Node is ",Sys.info()["nodename"],". Using ", root_server_dir, ' as the root of the directory structure.\n'))
  
  if(Sys.info()["nodename"]=="workstation_3" & Sys.info()["user"]=="root"){
    # external_data_dir <- file.path('','kumc-data01','protected','study_dir_a','RAWDATA') 
    external_data_dir <- file.path(root_server_dir,'RAWDATA')
  } else {
    external_data_dir <- file.path(root_p,'study_dir_a','RAWDATA')
  }
  
  report_dir <- file.path(root_server_dir,'reports')
  

  project_dir <- file.path(root_server_dir,'study_dir_a','COMET') #project level directory
  script_dir <- file.path(project_dir,'R')
  data_dir <- file.path(project_dir,'data')
  

  #Identify the ultimate location for copies of the files we can use to read data
  cog_destination <- file.path(data_dir,'raw','cog_data')
  dxa_destination <- file.path(data_dir,'raw','dxa_data')
  clean_data_destination <- file.path(data_dir,'clean')
  
  
  ###### YMCA Liason Dir ######
  #Julie needs reporting for billing for multiple studies
  #On 10.11.22, I built this into the COMET workflow because it was easier than creating a new recurring script
  ymca_liason_dir <- file.path(root_p,'unversity_drive5','folder')
  
  #message("Echoing script directory: ", script_dir)
  
  ###### Study Coordinator Information ######
  name <- 'Study'
  lname <- 'Coordinator'
  study_coordinator_email <- 'coordinator_email@kumc.edu'
  study_coordinator_number <- 'coordinator_number'

  
  to_save <- c('project_dir','script_dir','data_dir','cog_destination','dxa_destination','clean_data_destination','external_data_dir', 'report_dir','ymca_liason_dir')
  save(list = to_save,file= file.path(clean_data_destination,'directories.Rdata'))
  
  #### Export Installed Packaged ####
  installed_packages <- as.data.frame(installed.packages()[ , c(1, 3:4)]) 
  export(installed_packages, file.path(project_dir,paste0('available_packages_',Sys.info()["nodename"],'.csv')))


  if(execution==1 & messages_flag==1){message("\nPrepration Step. Successfully initialized paths and variables in comet_nucles.R")}  
}, #end trycatch script to evaluate
error = function(err){
  next_up <- "Path and variable initialization in comet_nucleus.R"
  execution <<- 0
  i <- 'Prep'
  error_msg(i, next_up)
},
finally = {})


 
if(messages_flag ==1){message(paste("Launching Everyday Scripts.\n"))}
###### Launch Everyday Scripts #####

  script_order <- c('00_bootstrap_error.R',
                    '00_encrypt_fitbit.R',
                    '05_clean_redcap.R',
                    '06_read_clean_merge.R',
                    '00_fitbit_functions.R',
                    '00_product_script_functions.R',
                    '02_refresh_token.R',
                    '03_get_steps.R',
                    '06_read_clean_merge.R',
                    '04_get_hr_and_activities.R',
                    '05_quarantine_file_handler.R',
                    '05_cog_aggregation.R',
                    '05_dxa_aggregation.R',
                    '05_vo2_aggregation.R',
                    '05_mri_transfer.R',
                    '06_read_clean_merge.R',
                    '06_exercise_data_consolidation.R',
                    '06_exercise_adherence.R',
                    '08_texting_system.R',  
                    '10_croms_report.R',
                    '12_med_monitor_report.R',
                    '11_coordinator_email.R',
                    '13_ymca_liason_email.R',
                    '15_data_quality_email.R',
                    '16_data_export_for_scientists.R',
                    '14_participant_reports.R'
                    
                     )
  
  #if(testing == 1) {script_order <- c('error_it.R')}
#### Run daily regardless of device ####  
  for(i in 1:length(script_order)){
    if (execution == 1) {
      tryCatch({
          last_up <<- script_order[i-1]
          next_up <<- script_order[i]
          if(messages_flag == 1){message(paste("\nChild Script",i,"of",length(script_order),": Sourcing",next_up))}
          source(list.files(file.path(script_dir), next_up, full.names=TRUE)) 
      },
      error = function(err) {
        execution <<- 0
	error_msg(i,next_up)
      }, 
      finally = function(f) {}) #end tryCatch
    } # end if execution
  }#end for



####### Launch Monday Scripts ######
  if(wday(today()) == 2 & execution == 1 & eric_at_home  == 0){
    
    script_order <- c('09_scientist_email.R',
                      'participant_email.R')
    
    #if(testing == 1) {script_order <- c('error_it.R')}

    for(i in 1:length(script_order)){
      if(execution == 1){
        tryCatch({
          last_up <<- script_order[i-1]
          next_up <<- script_order[i]
          if(messages_flag == 1){message(paste("\nEmail Script",i,"of",length(script_order),": Sourcing",next_up))}
	        source(list.files(file.path(script_dir), next_up, full.names=TRUE)) 
        },
        error = function(err) {
          execution <<- 0
          error_msg(i,next_up)
        }, 
        finally = function(f) {}) #end tryCatch
      } #end if
    } #end for
  } #end if weekday

##### Successful Run - Encrypt ####
if(execution == 1) {
  successful_execution <- 1
  source(file.path(script_dir, '00_encrypt_fitbit.R'))
  
  #clear workspace if running on server
  if(Sys.info()["nodename"]!="workstation_1"){
    rm(list = ls())
  }
}



