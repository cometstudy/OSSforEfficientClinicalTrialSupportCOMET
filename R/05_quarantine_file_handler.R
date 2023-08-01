#' @author EDV on 8/5/2021
#' 
#' @name quarantile_file_handler_script
#' 
#' @title Quarantine and Copy Data
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas
#' 
#' @section Development notes:
#' 10.7.21: Error found in file handling. overlap_frozen shows identical advisor.xsl files when trying to copy data. Need to
#' edit logic of overlap_frozen in some way. Consulting Eric before continuing. \cr
#' 10.11.21 Jon added short_name to already_working_files, etc to deal with repeat advisor.tsl in each folder. \cr
#' 11.30.21 Issued preliminary fix to ongoing overlapping files issue. Summary of issue: NIH files do not have a folder so short_name convention does not work.
#' eCog files have a folder structure so short name convention does work, but basename shows overlapping files - advisor.xsl. Preliminary fix is to compare basenames.
#' If basenames are overlapping, look at file name. If it is advisor.xsl, change overlapping back to false. JC \cr
#' 12.1.21 Updated dxa quarantine. DXAs are saved in UTF-16LE format. readlines recognizes a coded line. DXA is imported and changed encoding. Then
#' exported to relevant directories. JC \cr
#' 12.6.21 Added in code to copy pdf version of NIH Assessment Report to the frozen directory JC \cr
#' 4.28.22 Updated VO2 transfer. \cr
#' 5.3.22 Updated DXA transfer. Took language from 05_get_copy_electronic_data JC \cr
#' 5.24.22 First T2 cogs were uploaded. This caused issues with the frozen files / to handle language (checking for duplicate files).
#' Cog files are uploaded either as NIH files (csv or pdf) or VCAP within a larger folder. The folder vs. single file, makes checking 
#' for overlapping files hard. T2 basename files are named the same as T1 files in VCAP. So uploaded files are now separated into 
#' file vs. folder and overlaps are checked depending on type. JC  \cr
#' 5.26.22 A GXT check has been added before quarantining. If GXT files are mislabeled or do not contain all relevant info
#' a message is sent to the coordinators. JC \cr
#' 6.16.22 Added MRI structure to non_cog. MRI task files are tranfered in folders. non_cog_files are separated
#' into folders to transfer (non_cog_folder_to_move) and non_cog_files (non_cog_to_do). JC \cr
#' 7.19.22 Updated the overlapping email function to send a single email with all overlapping files. JC \cr
#' 9.13.22 Occasionally the wrong file types will be uploaded into a to_handle folder, i.e. an outlook email instead of the .csv
#' files for the NIH Toolbox. I created supported file types. If a file type is not supported an error email is sent so that it can 
#' be identified on corrected. This occurs for both non_cog_to_do files and nih_files JC \cr
#' 11.8.22 Corrected message that gets sent when overlap_frozen_files == T. Actually includes file names. JC \cr
#' 
#' @description 
#' This script checks to see what source documentation has been transferred to the P drive "external data directory" by
#' the study team or raters, and puts one copy in a data archive and puts another copy
#' in the primary data storage folder for use.

message("Beginning 05 quarantine file handler")

##### Declare Functions #####
#As of 7/19/22, not using email function anymore
email_fcn <- function(msg, subject, recipients){
  for (i in  1:length(recipients)){
    
    TO = recipients[i]
    
    mailcmd<-paste(python_cmd_prefix, paste(file.path(project_dir,'R',"send_file_handling_email.py")),TO,subject,msg) 
    system(mailcmd)
    
  }
}



##### Identify what files I've already copied to the project_dir "raw" WORKING RAW folder#####
## These are working files that our processing scripts access.
#Not messing with the fitbit files as those have an entirely separate way of being acquired and stored
already_working_files <- list.files(file.path(project_dir,'data','raw',c('cog_data','dxa_data','vo2_data','mritask_data')),".[[:alpha:]]{3,4}$", recursive = TRUE, full.names = TRUE) %>%
  as_tibble_col(column_name = "fullname")  %>%
  mutate(short_name = paste(basename(dirname(fullname)), basename(fullname)))


##### Identify files that have been put in our RAWDATA "FROZEN SOURCE" archive ####
## These are FROZEN source docs that really should never be touched 
## Having a FROZEN source archive has the effect of isolating some original data files that can't be messed up or changed
## accidently by code. Since the P drive is always backed up we'd also theoretically have a record of any accidental or nefarious data changing
frozen_files_folders <- list.files(file.path(external_data_dir,c('CognitiveTests','DXA','GXT','MRITASK')),".[[:alpha:]]{3,4}$", recursive = TRUE, full.names = TRUE) %>%
  as_tibble_col(column_name = "fullname") %>%
  filter(!(basename(dirname(fullname)) == 'CognitiveTests' | basename(dirname(fullname)) == 'DXA' | basename(dirname(fullname)) == 'GXT' | basename(dirname(fullname)) == 'MRITASK')) %>%
  mutate(short_name = paste0(basename(dirname(fullname)), basename(fullname)))

frozen_files_files <- list.files(file.path(external_data_dir,c('CognitiveTests','DXA','GXT','MRITASK')),".[[:alpha:]]{3,4}$", recursive = TRUE, full.names = TRUE) %>%
  as_tibble_col(column_name = "fullname") %>%
  filter(basename(dirname(fullname)) == 'CognitiveTests' | basename(dirname(fullname)) == 'DXA' | basename(dirname(fullname)) == 'GXT' | basename(dirname(fullname)) == 'MRITASK')  %>%
  mutate(short_name = paste0(basename(dirname(fullname)), basename(fullname)))

##### Identify new files put into the "quarantine" folders by the raters  ####
## These are the folders named comet_completed_cog_raw_data_files or comet_completed_gxt_dxa_mritask_raw_data_files
## These should be new data that needs to be handled and transferred into both the FROZEN SOURCE folder and also
## our WORKING RAW folder
to_handle_files <- list.files(file.path(dirname(external_data_dir),c('comet_completed_cog_raw_data_files','comet_completed_gxt_dxa_mritask_raw_data_files')),".[[:alpha:]]{3,4}$", recursive = TRUE, full.names = TRUE) %>%
  as_tibble_col(column_name = "fullname") %>%
  mutate(short_name = paste0(basename(dirname(fullname)), basename(fullname)))

##### Check for overlap between the quarantined files, and the FROZEN and WORKING files ####
overlap_working <- any(basename(to_handle_files$fullname) %in% basename(already_working_files$fullname))

overlap_frozen_folder <- any(to_handle_files$short_name %in% frozen_files_folders$short_name)
overlap_frozen_files <- any(basename(to_handle_files$fullname) %in% basename(frozen_files_files$fullname))


### Look at the overlap_frozen files. If they are all advisor.xsl files, allow them to be copied too and overwrite overlap_frozen
if(overlap_frozen_folder==TRUE) {
  
  overlap_frozen_details <-
    to_handle_files$fullname[which(to_handle_files$short_name %in% frozen_files_folders$short_name)]
  
  if (!any("advisor.xsl" != basename(overlap_frozen_details))) {
    overlap_frozen <- FALSE
  }
}

### Send some emails if an overlap between quarantine and already handled directories are noted. There should never be any ####
if(overlap_working==TRUE | overlap_frozen_folder==TRUE | overlap_frozen_files==TRUE){
  if(overlap_working==TRUE){
    
    overlapping <- NA
    if(overlap_frozen_folder == TRUE){
      overlapping <- paste(to_handle_files$short_name[which(to_handle_files$short_name %in% frozen_files_folders$short_name)], collapse = "\n")
    } else if(overlap_frozen_files ==  TRUE){
      overlapping <- paste(basename(to_handle_files$fullname)[which(basename(to_handle_files$fullname) %in% basename(frozen_files_files$fullname))], collapse = "\n")
    }
    
    #Email arguments
    msg <- paste0("\"\"\"","An overlapping file between the working directory ",project_dir, "\nand the quarantine directory\n", external_data_dir, " has been noted.\r\n",
                  "Here are the overlapping files: \n ", overlapping,'.\r\n')
    subject <- paste("\"\"\"","Probable File Conflict in Quarantine Directory Please Investigate","\"\"\"")
    cat(msg, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
    mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email_w_subject.py'), "study_coordinator_2@kumc.edu", file.path(project_dir,'data','modules','generic_email_module','email.txt'), subject)
    if(testing == 0){
      system(mailcmd)
    }
  } # end if overlap_working
  
  if(overlap_frozen_folder==TRUE){
    
    #Email arguments
    msg <- paste0("\"\"\"","An overlapping file between the working directory ",project_dir, "\nand the quarantine directory\n", external_data_dir, " has been noted.\r\n",
                  "The overlap appears to be with ", to_handle_files$fullname[which(to_handle_files$short_name %in% frozen_files_folders$short_name)],'.\r\n',
                  "Probably should check it out before we proceed with archiving and processing. New files in quarantine shouldn't also be in the FROZEN directory.\n\n Stopping the copy process...","\"\"\"")
    subject <- paste("\"\"\"","Probable File Conflict in Quarantine Directory Please Investigate","\"\"\"")
    recipients <- c("study_coordinator_2@kumc.edu","user5@kumc.edu","user2@kumc.edu")
    
    message(msg)
    email_fcn(msg,subject,recipients)
  } 
  
  if(overlap_frozen_files==TRUE){
    
    list_of_overlapping <- toString(to_handle_files$fullname[basename(to_handle_files$fullname) %in% basename(frozen_files_files$fullname)])
    
    #Email arguments
    msg <- paste0("\"\"\"","An overlapping file between the working directory ",project_dir, "\nand the quarantine directory\n", external_data_dir, " has been noted.\r\n",
                  "The overlap appears to be with ", list_of_overlapping,'.\r\n',
                  "Probably should check it out before we proceed with archiving and processing. New files in quarantine shouldn't also be in the FROZEN directory.\n\n Stopping the copy process...","\"\"\"")
    subject <- paste("\"\"\"","Probable File Conflict in Quarantine Directory Please Investigate","\"\"\"")
    recipients <- c("study_coordinator_2@kumc.edu","user5@kumc.edu","user2@kumc.edu")
    
    message(msg)
    email_fcn(msg,subject,recipients)
  }#end if overlap_frozen
} else {  # overlaps not detected begin handling the files in quarantine
  
  
  
  ###############################################################################
  ###############################################################################
  #########        Handle the New Non-Cognitive Date         ####################
  ###############################################################################  
  ###############################################################################
  
  #loop for all the files "to_handle" that ARE NOT in the external_data_dir quarantine folder
  non_cog_files <- to_handle_files %>%
    filter(grepl("comet_completed_gxt_dxa_mritask_raw_data_files",fullname)) %>%
    pull(fullname)
  
  ############# Copy Source Folder (Mostly MRI NBack) ##########
  #Identify any folders in the gxt_dxa_mritask raw data files folder, into the external_data_dir. 
  non_cog_folders_to_move <- non_cog_files %>%
    as_tibble_col(column_name = "available_folders") %>%
    mutate(dirnames = dirname(available_folders)) %>% group_by(dirnames) %>%
    filter(grepl("AVERAGE",available_folders)) %>%
    filter(! grepl('comet_completed_gxt_dxa_mritask_raw_data_files',basename(dirnames))) #rids us of the parent directory
  
  mri_destination <- file.path(project_dir,'data','raw','mritask_data')
  
  
  
  #Copy these folders into our FROZEN Source directory
  if(nrow(non_cog_folders_to_move) > 0) {
    for(i in 1:length(non_cog_folders_to_move$available_folders)){
      
      if(dir.exists(file.path(external_data_dir,'MRITASk',basename(non_cog_folders_to_move$dirnames[i])))==TRUE){
        
        msg <- paste0("\"\"\"","An overlapping file between the Froze source directory ",file.path(external_data_dir,"RAWDATA","MRITASk"), "\nand the quarantine directory\n", external_data_dir, " has been noted.\r\n",
                      "The overlap appears to be with ", non_cog_folders_to_move$dirnames[i],'.\r\n',
                      "Probably should check it out before we proceed with archiving and processing. New files in quarantine shouldn't also be in the FROZEN directory.\n\n Stopping the copy process...","\"\"\"")
        subject <- paste("\"\"\"","Probable File Conflict in Quarantine Directory Please Investigate","\"\"\"")
        recipients <- c("study_coordinator_2@kumc.edu","user2@kumc.edu")
        message(msg)
        email_fcn(msg,subject,recipients)
        
        break()
        
      }else{
        copyDirectory(non_cog_folders_to_move$dirnames[i], 
                      file.path(external_data_dir,'MRITASK',basename(non_cog_folders_to_move$dirnames[i])), 
                      overwrite = FALSE)  #Copy entire folder into the source archive
        
        # Before deleting the quarantined source folder. Copy the essential source documents {the *_SUMMARY.txt file} into the working directory P:/unversity_drive5/IRB_STUDYstudy_dir_a/COMET/data/raw
        # Plus we have to rename them to avoid the possibility of overwriting because it's not clear that the timepoint is in the file name 
        rename_file <- paste(paste(str_split(basename(dirname(non_cog_folders_to_move$available_folders[i])), "[[:punct:]]", simplify = TRUE), collapse="_"), 'AVERAGE.txt', sep = "_")
        
        copy_status <- file.copy(non_cog_folders_to_move$available_folders[i],file.path(mri_destination,rename_file), overwrite = FALSE)
        
        if(copy_status==TRUE & dir.exists(file.path(external_data_dir,'MRITASK',basename(non_cog_folders_to_move$dirnames[i])))){
          message("Success copying the file ", file.path(external_data_dir,'MRITASK',basename(non_cog_folders_to_move$dirnames[i])), ' to ',cog_destination,
                  "\nAND\nsuccess copying the source folder to ", file.path(external_data_dir,'MRITASK'), ". Deleting version transferred by psychometrists.")
          unlink(non_cog_folders_to_move$dirnames[i], recursive=TRUE)
        }  
        rm(rename_file, copy_status)
      } #end if the quarantined version already exists in the frozen source archve
    } #end for all the folders in quarantine
    
  }  
  
  #Identify any files in the gxt_dxa_mritask raw data files folder, into the external_data_dir. 
  non_cog_to_do <- non_cog_files %>%
    as_tibble_col(column_name = "available_files") %>%
    mutate(dirnames = dirname(available_files)) %>%
    filter(grepl('comet_completed_gxt_dxa_mritask_raw_data_files',basename(dirnames))) %>% #rids us of the parent directory
    pull(available_files)
  
  supported_file_types_non_cog <- c("csv","pdf","txt")
  
  
  if(length(non_cog_to_do) > 0) {
    for(i in 1:length(non_cog_to_do)){
      
      
      #If the file is an unexpected data type, send a warning email. 
      if(!rio::get_ext(non_cog_to_do[i]) %in% supported_file_types_non_cog) {
        text <- paste0(non_cog_to_do[i]," is a file with a data type not supported in the comet data flow. Please investigate and fix.")
        cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
        mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email_w_subject.py'), "study_coordinator_2@kumc.edu", file.path(project_dir,'data','modules','generic_email_module','email.txt'), paste0("Unsupported_data_type_in_quarantine"))
        if(testing == 0){
          system(mailcmd)
        }
        #####gxt
      }  else if(grepl("csv|CSV",basename(non_cog_to_do[i]))) {
        #Probably a GXT file but make sure by sampling and looking for the phrase exercise time
        if(suppressWarnings(any(grepl("Exercise Time", readLines(non_cog_to_do[i] , n = 5000, ok=TRUE, warn = FALSE))))==TRUE){ #This if statement checks if Exercise Time is in the file
          
          temp_name <- gsub("-|__|_",".",basename(non_cog_to_do[i]))
          temp_basename <- str_split(temp_name,"[.]", simplify = T)
          message(temp_basename[1] == "146904")
          message(nchar(temp_basename[2]) == 4 | nchar(temp_basename[2]) == 5)
          message(as.numeric(temp_basename[3]) < 281)
          message(!(temp_basename[1] == "146904" & (nchar(temp_basename[2]) == 4 | nchar(temp_basename[2]) == 5) & as.numeric(temp_basename[3]) < 281))

          if(!(temp_basename[1] == "146904" & (nchar(temp_basename[2]) == 4 | nchar(temp_basename[2]) == 5) & as.numeric(temp_basename[3]) < 281)) {
            message("Error if")
            msg <- paste0("\"\"\"","It appears as though ", non_cog_to_do[i]," is misnamed. Please ensure it is named 146904_XXXX_WWW, where XXXX = RED# and WWW = study id","\"\"\"")
            subject <- paste("\"\"\"","GXT Misnamed, please relabel","\"\"\"")
            recipients <- c("study_coordinator_2@kumc.edu","user2@kumc.edu","coordinator_email@kumc.edu")
            
            message(msg)
            email_fcn(msg,subject,recipients)
          } else {
            
            message("started read gxt")
            message(file.path(non_cog_to_do[i]))
            temp_gxt <- data.frame(read.table(file.path(non_cog_to_do[i]), header = F, stringsAsFactors = F, sep=",", fill=T, col.names=c(paste0("X",seq(1:15)))))
            message("read gxt")
            no_errors <- any(grepl("VO2", temp_gxt$X2)) & any(grepl("VO2/kg", temp_gxt$X3)) & any(grepl("VCO2", temp_gxt$X14)) & any(grepl("VE", temp_gxt$X7)) & any(grepl("RER", temp_gxt$X4)) & any(grepl("HR", temp_gxt$X15))
            
            if(no_errors == T) {
              file.copy(non_cog_to_do[i],file.path(external_data_dir,'GXT',basename(non_cog_to_do[i])))
              file.copy(non_cog_to_do[i],file.path(project_dir,'data','raw','vo2_data',basename(non_cog_to_do[i])))
              file.remove(non_cog_to_do[i])
              message("Copied ",basename(non_cog_to_do[i]))
            } else {
              msg <- paste0("\"\"\"","It appears as though ", non_cog_to_do[i]," does not contain all necessary information. Please visually inspect this file before moving to Frozen and Working directories","\"\"\"")
              subject <- paste0("\"\"\"","GXT does not contain all relevant info","\"\"\"")
              recipients <- c("study_coordinator_2@kumc.edu","user2@kumc.edu","coordinator_email@kumc.edu")
              
              message(msg)
              email_fcn(msg,subject,recipients)
            }
          }
          
        } else {
          msg <- paste0("\"\"\"","Not sure what ", non_cog_to_do[i],'is.\r\n',
                        "It is recognized as a .csv file. Potentially a gxt. Probably should check it out before we proceed with archiving and processing. New files in quarantine shouldn't also be in the WORKING directory.\n\n Stopping the copy process...","\"\"\"")
          subject <- paste("\"\"\"","Quarantine File Type Uncertain Please Investigate","\"\"\"")
          recipients <- c("study_coordinator_2@kumc.edu","user2@kumc.edu")
          
          message(msg)
          email_fcn(msg,subject,recipients)
        } #end if Exercise Time 
       
     #### Commenting out because dxas are txt files. Rewriting to recognize dxa as txt file
      # else if(grepl("csv|CSV", basename(non_cog_to_do[i]))) {
      #   #Probably a dxa file but make sure by sampling and looking for the phrase exercise time
      #   if(suppressWarnings(any(grepl("XXXXXXXXXX", readLines(non_cog_to_do[i] , n = 5000, ok=TRUE, warn = FALSE))))==TRUE){
      #     file.copy(non_cog_to_do[i],file.path(external_data_dir,'DXA',basename(non_cog_to_do[i])))
      #     file.copy(non_cog_to_do[i],file.path(project_dir,'data','raw','dxa_data',,basename(non_cog_to_do[i])))
      #     file.remove(non_cog_to_do[i])
      #     message("Copied ",basename(non_cog_to_do[i]))
      #   } else {
      #     msg <- paste0("\"\"\"","Not sure what ", non_cog_to_do[i],' is.\r\n',
      #                   "Probably should check it out before we proceed with archiving and processing. New files in quarantine shouldn't also be in the WORKING directory.\n\n Stopping the copy process...","\"\"\"")
      #     subject <- paste("\"\"\"","Quarantine File Type Uncertain Please Investigate","\"\"\"")
      #     recipients <- c("study_coordinator_2@kumc.edu","user2@kumc.edu")
      # 
      #     message(msg)
      #     email_fcn(msg,subject,recipients)
      #   } #end if grepl csv
        ##### Commenting out because MRIs are folders. Rewritten below
      # } else if (grepl("txt|TXT", basename(non_cog_to_do[i]))){
      #   # Need and example MRI task dataset here. Probably a mri task file from E Prime but make sure. 
      #   if(suppressWarnings(any(grepl("XXXXXXXXXX", readLines(non_cog_to_do[i] , n = 5000, ok=TRUE, warn = FALSE))))==TRUE){
      #     file.copy(non_cog_to_do[i],file.path(external_data_dir,'MRITASK',basename(non_cog_to_do[i])))
      #     file.copy(non_cog_to_do[i],file.path(project_dir,'data','raw','mritask_data',basename(non_cog_to_do[i])))
      #     file.remove(non_cog_to_do[i])
      #     message("Copied ",basename(non_cog_to_do[i]))
      #     #could also be a dxa export. this checks for dxa
       #MRI
        } else if(grepl("Body", basename(non_cog_to_do[i]))) {

          file.copy(non_cog_to_do[i], 
                    file.path(external_data_dir,'DXA',basename(non_cog_to_do[i])), 
                    overwrite = FALSE)
          
          file.copy(non_cog_to_do[i], 
                    file.path(project_dir,'data','raw','dxa_data',basename(non_cog_to_do[i])), 
                    overwrite = FALSE)
          
          if(file.exists(file.path(external_data_dir,'DXA',basename(non_cog_to_do[i])) ) & file.exists(file.path(project_dir,'data','raw','dxa_data',basename(non_cog_to_do[i])))){
            message("Success copying the folder to ", file.path(external_data_dir,'DXA',basename(non_cog_to_do[i])),
                    " AND success copying the summary file to ", file.exists( file.path(project_dir,'data','raw','dxa_data',basename(non_cog_to_do[i]))), ". Deleting version transferred by study staff.")
            file.remove(non_cog_to_do[i])
          } else {
            message("Warning! One or more issues making copies.")
          }
          
          
           
           
          
        } else {
          msg <- paste0("\"\"\"","Not sure what ", non_cog_to_do[i],'is.\r\n',
                        "It is recognized as a .txt file, either an MRI or dxa. Probably should check it out before we proceed with archiving and processing. New files in quarantine shouldn't also be in the WORKING directory.\n\n Stopping the copy process...","\"\"\"")
          subject <- paste("\"\"\"","Quarantine File Type Uncertain Please Investigate","\"\"\"")
          recipients <- c("study_coordinator_2@kumc.edu","user2@kumc.edu")
          
          message(msg)
          email_fcn(msg,subject,recipients)
        } #end if grepl csv
      } 
      #end checking if xls,csv,txt
    } #end else for non_cog_to_do

  
  
  
  ###############################################################################
  ###############################################################################
  #####  Handle the New VCAP Cognitive Data Folders  (Not the NIH Toolbox)  #####
  ###############################################################################  
  ###############################################################################  
  
  #Now, loop for all the files "to_handle" that ARE in the cog quarantine folder
  cog_to_do <- to_handle_files %>%
    filter(grepl("comet_completed_cog_raw_data_files",fullname)) %>%
    pull(fullname)
  
  
  ############# Copy Source Folder (Mostly VCAP) ##########
  #Identify any folders in the cognitive data raw data files folder, into the external_data_dir. These files were transferred here by the psychometrists.
  folders_to_move <- cog_to_do %>%
    as_tibble_col(column_name = "available_folders") %>%
    mutate(dirnames = dirname(available_folders)) %>% group_by(dirnames) %>%
    filter(grepl("SUMMARY",available_folders)) %>%
    filter(! grepl('comet_completed_cog_raw_data_files',basename(dirnames))) #rids us of the parent directory
  
  
  #Identify the ultimate location for files we can use to read data
  cog_destination <- file.path(project_dir,'data','raw',"cog_data")
  
  #Copy these folders into our FROZEN Source directory
if(nrow(folders_to_move) > 0) {
  for(i in 1:length(folders_to_move$available_folders)){
    
    if(dir.exists(file.path(external_data_dir,'CognitiveTests',basename(folders_to_move$dirnames[i])))==TRUE){
      
      msg <- paste0("\"\"\"","An overlapping file between the Froze source directory ",file.path(external_data_dir,"RAWDATA","CogntiveTests"), "\nand the quarantine directory\n", external_data_dir, " has been noted.\r\n",
                    "The overlap appears to be with ", folders_to_move$dirnames[i],'.\r\n',
                    "Probably should check it out before we proceed with archiving and processing. New files in quarantine shouldn't also be in the FROZEN directory.\n\n Stopping the copy process...","\"\"\"")
      subject <- paste("\"\"\"","Probable File Conflict in Quarantine Directory Please Investigate","\"\"\"")
      recipients <- c("study_coordinator_2@kumc.edu","user2@kumc.edu")
      message(msg)
      email_fcn(msg,subject,recipients)
      
      break()
      
    }else{
      copyDirectory(folders_to_move$dirnames[i], 
                    file.path(external_data_dir,'CognitiveTests',basename(folders_to_move$dirnames[i])), 
                    overwrite = FALSE)  #Copy entire folder into the source archive
      
      # Before deleting the quarantined source folder. Copy the essential source documents {the *_SUMMARY.txt file} into the working directory P:/unversity_drive5/IRB_STUDYstudy_dir_a/COMET/data/raw
      # Plus we have to rename them to avoid the possibility of overwriting because it's not clear that the timepoint is in the file name 
      rename_file <- paste(paste(str_split(basename(dirname(folders_to_move$available_folders[i])), "[[:punct:]]", simplify = TRUE), collapse="_"), 'SUMMARY.txt', sep = "_")
      
      copy_status <- file.copy(folders_to_move$available_folders[i],file.path(cog_destination,rename_file), overwrite = FALSE)
      
      if(copy_status==TRUE & dir.exists(file.path(external_data_dir,'CognitiveTests',basename(folders_to_move$dirnames[i])))){
        message("Success copying the file ", file.path(external_data_dir,'CognitiveTests',basename(folders_to_move$available_folders[i])), ' to ',cog_destination,
                "\nAND\nsuccess copying the source folder to ", file.path(external_data_dir,'CognitiveTests'), ". Deleting version transferred by psychometrists.")
        unlink(folders_to_move$dirnames[i], recursive=TRUE)
      }  
      rm(rename_file, copy_status)
    } #end if the quarantined version already exists in the frozen source archve
  } #end for all the folders in quarantine
  
}  
  
  ###############################################################################
  ###############################################################################
  #####  Handle the New NIH Toolbox Files                                   #####
  ###############################################################################  
  ###############################################################################    
  
  
  #Identify any loose files in the cognitive data raw data files folder, into the external_data_dir. These files were transferred here by the psychometrists.
  nih_files_to_move <- cog_to_do %>%
    as_tibble_col(column_name = "available_files") %>%
    mutate(dirnames = dirname(available_files)) %>%
    filter(grepl('comet_completed_cog_raw_data_files',basename(dirnames))) #rids us of the parent directory
  
  supported_file_types_nih <- c("csv","pdf")
    
 
if(nrow(nih_files_to_move) > 0) {
  
  for(i in 1:length(nih_files_to_move$available_files)){
    
    #If the file is an unexpected data type, send a warning email. 
    if(!rio::get_ext(nih_files_to_move$available_files[i]) %in% supported_file_types_nih) {
      text <- paste0(nih_files_to_move$available_files[i]," is a file with a data type not supported in the comet data flow. Please investigate and fix.")
      cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
      mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email_w_subject.py'), "study_coordinator_2@kumc.edu", file.path(project_dir,'data','modules','generic_email_module','email.txt'), paste0("Unsupported_data_type_in_quarantine"))
      if(testing == 0){
        system(mailcmd)
      }
      
      #Copies the NIH toolbox files
    } else if(file.exists(file.path(external_data_dir,'CognitiveTests',basename(nih_files_to_move$available_files[i])))==TRUE){
      
      msg <- paste0("\"\"\"","An overlapping file between the Frozen source directory ",file.path(external_data_dir,"RAWDATA","CogntiveTests"), "\nand the quarantine directory\n", external_data_dir, " has been noted.\r\n",
                    "The overlap appears to be with ", nih_files_to_move$available_files[i],'.\r\n',
                    "Probably should check it out before we proceed with archiving and processing. New files in quarantine shouldn't also be in the FROZEN directory.\n\n Stopping the copy process...","\"\"\"")
      subject <- paste("\"\"\"","Probable File Conflict in Quarantine Directory Please Investigate","\"\"\"")
      recipients <- c("study_coordinator_2@kumc.edu","user2@kumc.edu")
      message(msg)
      email_fcn(msg,subject,recipients)
      
      break()
    } else if (grepl("pdf|PDF", basename(nih_files_to_move$available_files[i]))) {
      
      #Copies the NIH toolbox files to the Frozen source archive folder
      file.copy(nih_files_to_move$available_files[i], file.path(external_data_dir,'CognitiveTests',basename(nih_files_to_move$available_files[i])), overwrite = FALSE)  #Copy to the source archive
      file.remove(nih_files_to_move$available_files[i])
      

    } else {
      
      #Copies the NIH toolbox files to the Frozen source archive folder
      copy_status1 <- file.copy(nih_files_to_move$available_files[i], file.path(external_data_dir,'CognitiveTests',basename(nih_files_to_move$available_files[i])), overwrite = FALSE)  #Copy to the source archive
      
      #Copies the NIH toolbox files to the source archive folder. But first has to open up the file and change the name because the NIH file naming scheme is bad.
      tmp <- import(nih_files_to_move$available_files[i])
      pin <- gsub("[[:punct:]]","_",tmp$PIN[1])
      tp <-  gsub("[[:alpha:]]|[[:space:]]","",tmp$`Assessment Name`[1])
      
      
      suffix <- ifelse(grepl("Registration",nih_files_to_move$available_files[i]),"_registration",
                       ifelse(grepl("Assessment Scores",nih_files_to_move$available_files[i]),"_summary_scores",
                              ifelse(grepl("Assessment Data",nih_files_to_move$available_files[i]),"_fulldata","_unknown")))
      
      copy_status2 <- file.copy(nih_files_to_move$available_files[i], file.path(cog_destination,paste0(pin,"_",tp,"nih_toolbox", suffix, ".csv")), overwrite = FALSE)  #Copy to the source archive
      
      
      if(copy_status1==TRUE & copy_status2==TRUE){
        message("Success copying the file ", nih_files_to_move$available_files[i], ' to ',cog_destination,
                "\nAND\nto ", file.path(external_data_dir,'CognitiveTests',basename(nih_files_to_move$available_files[i])))
        unlink(nih_files_to_move$available_files[i])
        
      }  
      
      rm(tmp, pin, tp, copy_status1, copy_status2)
    }
  } #end for NIH Toolbox for loop
 }  
}# handling files in quarantine



