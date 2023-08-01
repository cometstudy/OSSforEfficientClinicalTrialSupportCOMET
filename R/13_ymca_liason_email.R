#' @title YMCA Liason Email
#' 
#' @name 13_ymca_liason_email
#' 
#' @description This script will produce a weekly report for the ymca liason.
#'  All code is stored in data_dir,coordinator_email,coordinator_markdown.
#'  
#'  @section Copyright: 
#' Copyright © 2022 University of Kansas
#' 
#' @section Development notes:
#' 2.11.22 Started sending \cr
#' 4.11.22 Added a date of download to report and started running nightly to reports folder. JC \cr
#' 10.13.22 Added the Billing info module for Julie so that she can send to Y for billing. JC \cr
#' 20230306: Added daily export of personal trainer database. JC \cr
#' 
#' @section Criteria:
#' None
#' 

###### Copy Personal Trainer REDCap csv #####
file.copy(from = file.path(ymca_liason_dir,'phit_personal_trainers_v2.csv'), 
          to = file.path(data_dir,'ymca_liason_email','PHITPersonalTrainers.csv'), 
          overwrite = TRUE)

##### Billing Info #####
#Yay! Template for converting numeric REDCap data to named values using the data dictionary. I can make this more generalizable for any REDCap project
date_created <- file.info(file.path(ymca_liason_dir,'PHIT Participant Tracking.csv')) %>%
  pull(mtime)

if(today() - as_date(date_created) > 2) {
  text <- paste0('The PHIT Participant Tracking v2 database was last update at ',date_created,'. It looks to have not updated recently. Reach out to missupport@kumc.edu')
  cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
  mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email.py'), study_coordinator_email, file.path(project_dir,'data','modules','generic_email_module','email.txt'))
  if(testing == 0){
    system(mailcmd)
  }
}

look_up <- import(file.path(data_dir,'ymca_liason_email','PHITParticipantTracking_data_dict.csv')) %>%
  filter(`Field Type` %in% c("radio","dropdown")) %>%
  rename(field = `Variable / Field Name`, choices = contains("Choices")) %>%
  select(field, choices) %>%
  separate(choices, into = as.character(1:25), sep = "[|]") %>%
  pivot_longer(cols = -field) %>%
  filter(!is.na(value)) %>%
  separate(value, into = c("id_num","id_name"), sep = "[,]") %>%
  select(field, id_num, id_name) %>%
  mutate(id_num = trimws(id_num))

phit_participant_tracker <- import(file.path(ymca_liason_dir,'PHIT Participant Tracking.csv'))

ymca_billing <- phit_participant_tracker %>%
  filter(phit_ptstatus == 2 | phit_ptstatus == 6) %>%
  select(record_id, phit_lastname, phit_ptstatus, contains("group"), contains("phit_gym"), phit_trainer, phit_startdate, phit_enddate, d2) %>% 
  mutate(across(everything(), ~ as.character(.))) %>%
  pivot_longer(cols = c("phit_ptstatus", contains("group"), contains("phit_gym"))) %>%
  mutate(name = trimws(name))%>%
  filter(!is.na(value)) %>%
  left_join(., look_up, by = c("name" = "field","value" = "id_num")) %>%
  select(-value) %>%
  pivot_wider(., names_from = "name", values_from = "id_name") %>%
  mutate(Group = coalesce(phit_comet_group)) %>%
  mutate(ymca = phit_gym_ymca) %>%
  mutate(`Current Week` = as.numeric(ceiling((today() - ymd(phit_startdate)) / 7))) %>%
  select(record_id, phit_lastname, `Current Week`, phit_ptstatus, Group, phit_gym, ymca, phit_trainer, phit_startdate, phit_enddate, d2) %>%
  arrange(phit_gym, record_id) %>%
  rename("Participant ID" = record_id, "Last Name" = phit_lastname, "Status" = phit_ptstatus, "Facility" = phit_gym, "Trainer" = phit_trainer, "Start Date" = phit_startdate, "End Date" = phit_enddate, "Notes" = d2)
  
export(ymca_billing, file.path(data_dir,"ymca_liason_email","ymca_billing.csv"))
file.copy(from = file.path(data_dir,"ymca_liason_email","ymca_billing.csv"), to = file.path(report_dir,"ymca_billing.csv"), overwrite = T)



########Email Section ###############

  rmarkdown::render(file.path(data_dir,"ymca_liason_email","ymca_liason_markdown.Rmd"), output_file = file.path(data_dir,"ymca_liason_email","ymca_liason_markdown.html"))
  file.copy(from = file.path(data_dir,"ymca_liason_email","ymca_liason_markdown.html"), to = file.path(report_dir,"ymca_liason_markdown.html"), overwrite = TRUE)

  ##### Mail presets
  
  TO = study_coordinator_email
  
  ##### Send to python to send email
  mailcmd<-paste("py", file.path(data_dir,'ymca_liason_email','send_email.py'),TO, file.path(report_dir,'ymca_liason_markdown.html'), file.path(report_dir,'ymca_liason_markdown.html'), file.path(report_dir,"ymca_billing.csv"))
  
  if(wday(today()) == 2){
    system(mailcmd)
  }



