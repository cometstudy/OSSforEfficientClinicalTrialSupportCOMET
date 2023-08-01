#' @title COMET Medical Monitor Report Generator
#' 
#' @name 12_med_monitor_report.R
#' 
#' @description 
#' This script creates the a demographics and cognitive summary for a medical monitor cognitive adjudication
#' the COMET study.
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas
#' 
#' @section Development:
#' 2021.6.29 Initial development \cr
#' 2021-08-02 by EDV to include electronic tests \cr
#' 2022.1.23 Adapted to reduce api calls and move package declaration to comet nucleus \cr
#' 2022-01-25 Rewrote the ifelse statements EDV added so that data doesn't always show as missing. JC \cr
#' 2022-03-27 Commented out if eric is at home statement to render output while Jon was on vacation. JC \cr
#' 2022-03-30 Working to reduce ADI API calls. Saving adi externally. API call is not returning anything right now. JC \cr
#' 2022-7-19 Modified etmp dataframe. For participants with multiple baseline tests, date of test is matched to REDCap date of test. JC \cr
#' 2022-11-1 When working on participant reports, I realized the search for medications didn't include beta blockers. I adjusted as of 11/1. JC \cr
#' 2023-03-31: Added the WTAR and ranges for tests without norms. JC \cr

message("Beginning 12_med_monitor_report")

###### For running as independent script ######
#data_dir = '/COMET/study_dir_a/COMET/data'
#external_data_dir = '/COMET/RAWDATA'
#library(tidyverse)
#library(rio)
#library(httr)
#library(rlang)
#library(gtools)
#library(lubridate)
#library(markdown)
#library(jsonlite)
#library(png)
#library(htmlTable)
#messages_flag <<- 1
#eric_at_home <<- 1

###### Load Data #####
load(file.path(data_dir,'clean','comet_clean.Rdata'))
load(file.path(data_dir,'clean','comet_ecog_clean.Rdata'))
wtar_norm_table <- import(file.path(data_dir,'med_monitor_report','wtar_norm_table.xlsx'))

###### Saved adi_pctile to reduce api calls ####
saved_adi <- import(file.path(data_dir,'med_monitor_report','adi','adi.csv'))

###### Paths - Temporary until can fully edit file from Eric #####
demo_dir <- file.path(external_data_dir,'TestingVisits')
med_monitor_dir <- file.path(data_dir,'med_monitor_report')
adi_dir <- file.path(med_monitor_dir,'adi')
output_dir <- file.path(dirname(external_data_dir),'comet_med_monitor_reports')
logo <- file.path(med_monitor_dir,'COMETLogo.jpg')

##### Declare Functions #####
#Function to return an Area Deprivation Index from an address
api_return <- function(x){
  y <- content(GET(x))
  return_parsed <- parse_json(y)
  fips_state <-  return_parsed$OutputGeocodes[[1]]$CensusValues[[1]]$CensusValue1[["CensusStateFips"]]
  fips_county <-  return_parsed$OutputGeocodes[[1]]$CensusValues[[1]]$CensusValue1[["CensusCountyFips"]]
  fips_tract <- return_parsed$OutputGeocodes[[1]]$CensusValues[[1]]$CensusValue1[["CensusTract"]]
  fips_block_group <- return_parsed$OutputGeocodes[[1]]$CensusValues[[1]]$CensusValue1[["CensusBlockGroup"]]

  lookup_fips <- as.numeric(str_remove(paste0(fips_state,fips_county,fips_tract,fips_block_group),"[[:punct:]]"))
  adi_pctile <- adi$ADI_NATRANK[ which(adi$FIPS == lookup_fips) ]
  return(adi_pctile)
}

##### Declare Constants #####
#TAMU API key
#study_coordinator_2 account
key <- pull(import(file.path(med_monitor_dir,'api.csv')))


##### Identify MMR Already Created so we can filter out and not query the ADI for them #####
done <- list.files(file.path(output_dir,'signed'), "COMET_MMR_RED", full.names = FALSE, recursive = FALSE) %>%
  as_tibble() %>%
  mutate(value = gsub("Please_DocuSign_","",value)) %>%
  separate(value, into = letters[1:6]) %>%
  mutate(d = as.character(d)) %>%
  pull(d)


##### Read Demographics ######
comet_dat <- comet %>%
  mutate(redid = as.character(redid)) %>%
  filter(redcap_event_name =="baseline_arm_1") %>%
  rowwise() %>%
  mutate(hvlt_imm_sum = sum(cog_hvlt_t1_correct_imm,cog_hvlt_t2_correct_imm, cog_hvlt_t3_correct_imm, na.rm=FALSE),
         lc_sum = sum(cog_lc_part1_total,cog_lc_part2_total, na.rm=FALSE)) 


##### Medications #####
# since some meds are coded in numerics and some typed in as string, i have to lookup each numeric and match it with the name 
# associated with that code.
meds_hash <-  import(list.files(med_monitor_dir,'med_codes',full.names = TRUE)) %>%
  mutate(value = as.character(value))

meds_dat <-  comet %>%
  filter(redcap_event_name =="baseline_arm_1") %>%
  select(record_id, starts_with("meds"), -ends_with("reason"),-contains("date"),-contains("type"),-contains("rsn"),-ends_with("_complete"),-meds_betablocker,
         -contains("none"), -contains("bsln"), -contains("started")) %>%
  as_tibble() %>%
  gather(key, value, contains("meds")) %>%
  left_join(., meds_hash,) %>%
  mutate(med = case_when(is.na(name) == F ~ name,
                          TRUE ~ value)) %>%
  select(record_id, key, med) %>%
  mutate(med = case_when(med == "" ~ NA_character_,
                         T ~ med))

##### Load ADI Data #####
adi <-  import(list.files(adi_dir,'US_2019_ADI',full.names = TRUE)) %>%
  select(-starts_with("X"),-starts_with("GIS"),-contains("RNK"))


##### Join datasets #####
dat <- comet_dat %>% 
  mutate(record_id = str_pad(as.character(record_id),3,"0",side = "left")) %>%
  filter(is.na(redid)==FALSE & is.Date(ymd(scrn_dob))) %>%
  mutate(scrn_address_st = dplyr::recode(scrn_address_st,
      # State codes #####                                       
         `20` =	'KS',
         `29` =	'MO',
         `1` =	'AL',
         `2` =	'AK',
         `4` =	'AZ',
         `5` =	'AR',
         `6` =	'CA',
         `8` =	'CO',
         `9` =	'CT',
         `10` =	'DE',
         `12` =	'FL',
         `13` =	'GA',
         `15` =	'HI',
         `16` =	'ID',
         `17` =	'IL',
         `18` =	'IN',
         `19` =	'IA',
         `21` =	'KY',
         `22` =	'LA',
         `23` =	'ME',
         `24` =	'MD',
         `25` =	'MA',
         `26` = 'MI',
         `27` = 'MN',
         `28` =	'MS',
         `30` =	'MT',
         `31` =	'NE',
         `32` =	'NV',
         `33` =	'NH',
         `34` =	'NJ',
         `35` =	'NM',
         `36` =	'NY',
         `37` =	'NC',
         `38` =	'ND',
         `39` =	'OH',
         `40` =	'OK',
         `41` =	'OR',
         `42` =	'PA',
         `44` =	'RI',
         `45` =	'SC',
         `46` =	'SD',
         `47` =	'TN',
         `48` =	'TX',
         `49` =	'UT',
         `50` =	'VT',
         `51` =	'VA',
         `53` =	'WA',
         `55` =	'WI',
         `56` =	'WY'))

##### Get IDs to loop
reps_ids <- dat %>%
  filter(., status___11 == 1 & status___4 == 0) %>%
  filter(! redid %in% done) %>%
  filter(is.na(cog_date) == F)
  

##### Loop and make markdown files #####
for(i in 1:nrow(reps_ids)){
  #i=1 #for testing
  
  #10.7.22 arranged for record_id for multiple baseline entries. In case one baseline entry has date of NA, this will sort for the last entry.
  comet_current <- comet %>%
    filter(redid == reps_ids$redid[i]) %>%
    arrange(record_id) %>%
    select(redid, cog_date) 
  
  #Added if statement for participants with multiple baseline tests
  if(nrow(comet_current) > 1) {
    etmp <- full_cog %>%
      mutate(pin = gsub(" ", "", pin, fixed = TRUE)) %>%
      filter(pin == reps_ids$redid[i]) %>%
      filter(date == comet_current$cog_date[nrow(comet_current)])
  } else {
    etmp <- full_cog %>%
      mutate(pin = gsub(" ", "", pin, fixed = TRUE)) %>%
      filter(pin == reps_ids$redid[i]) 
  }
  
  tmp <- reps_ids[i,]
  
  if(is.na(tmp$ipscrn_mm_question) | tmp$ipscrn_mm_question == "") { 
    question <- "No Medical Condition"
    } else {question <- tmp$ipscrn_mm_question} 
  
  meds_tmp <- meds_dat %>%
    filter(meds_dat$record_id == as.numeric(tmp$record_id)) %>%
    select(med) %>%
    na.omit()
  
  # Create the empty demographics table for this person ####
  demo_table <- data.frame(matrix(NA,8, 1))
  colnames(demo_table) <- c("")
  row.names(demo_table) <-c("RED ID","Screening ID","Study ID","Gender","Race","Ethnicity","Area Deprivation Index [ADI]","CIRS Comorbidity Score")
  
  
  # Create the empty cognitive test table for this person ####
  cog_table <- data.frame(matrix(NA,14,2))
  colnames(cog_table) <- c("Raw Score","Adjusted Score or Interpretation Guidance")
  row.names(cog_table) <- c("TICS",
                            "Hopkins Verbal Learning Test Immediate",
                            "Hopkins Verbal Learning Test Delayed",
                            "Logical Memory Immediate",
                            "Logical Memory Delayed",
                            "Flanker",
                            "Dimensional Card Sort",
                            "Oral Symbol Digit",
                            "Letter Comparison",
                            "Matrix Reasoning Accuracy",
                            "Task Switching Accuracy",
                            "Spatial Working Memory 4-item Accuracy",
                            "Stroop Interference Accuracy",
                            "Wechsler Test of Adult Reading")
  
  # Build api syntax based on address #####
  tmp_address <- tmp %>%
    select(contains("address")) 
  
  tamu_api_syntax <- paste0("https://geoservices.tamu.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsedDetailed_V04_01.aspx?streetAddress=",
                       str_replace_all(tmp_address$scrn_address_str,"[[:space:]]+","%20"),
                       "&city=",
                       str_replace_all(tmp_address$scrn_address_city,"[[:space:]]+","%20"),
                       "&state=",
                       tolower(tmp_address$scrn_address_st),
                       "&zip=",
                       as.character(tmp_address$scrn_address_zip),
                       "&apikey=",
                       key,
                        "&format=json&census=true&censusYear=2010&notStore=false&version=4.01")
  
  #11.15.22 Format of address was throwing errors in API. Modified for specific record
  #' 4.12.23: Same error thrown. Changing from record_id 279 to record_id 356
  if(tmp$record_id == 279){
    tamu_api_syntax <- "https://geoservices.tamu.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsedDetailed_V04_01.aspx?streetAddress=6721%20128%20Pl.&city=Overland%20Park&state=ks&zip=66209&apikey=ce4a8485af754fac92d95f769fb5a7a9&format=json&census=true&censusYear=2010&notStore=false&version=4.01"
  } else if (tmp$record_id == 356) {
    tamu_api_syntax <- "https://geoservices.tamu.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsedDetailed_V04_01.aspx?streetAddress=700%20Ward%20Pkwy%20408&city=Kansas%20City&state=mo&zip=64112&apikey=ce4a8485af754fac92d95f769fb5a7a9&format=json&census=true&censusYear=2010&notStore=false&version=4.01"
  }
  
  
  current_adi <- saved_adi$adi_pctile[which(saved_adi$id == tmp$comet_study_id)]
  
  if(length(current_adi) == 0) {
    current_adi <- as.numeric(api_return(tamu_api_syntax))
    new_row <- data.frame(id = tmp$comet_study_id, adi_pctile = current_adi)
    saved_adi <- bind_rows(saved_adi, new_row)
  }

  #Find WTAR Normed data
  if(!is.na(tmp$cog_wtar_raw)){
    
    age_table <- case_when(tmp$scrn_age < 70 ~ "70", 
                           tmp$scrn_age < 75 ~ "75",
                           tmp$scrn_age <= 80 ~ "80")
    
    wtar_normed <- wtar_norm_table %>%
      select(wtar_raw_score, age_table) %>%
      filter(wtar_raw_score == tmp$cog_wtar_raw) %>%
      pull(c(2)) 
    
    wtar_age_percent <- round((wtar_normed - 50) / (126 - 50)*100)
      
    
  }
  
  # Build the Demographics Table #####
  demo_table["RED ID",1] <- as.character(tmp$redid)
  demo_table["Screening ID",1] <- as.character(tmp$record_id)
  demo_table["Study ID",1] <- str_pad(as.character(tmp$comet_study_id), width = 3, side = "left", pad = "0")
  demo_table["Gender",1] <- tmp$gender
  demo_table["Race",1] <- tmp$race
  demo_table["Ethnicity",1] <- tmp$ethnicity
  demo_table["Area Deprivation Index [ADI]",1] <- current_adi
  #demo_table["Medications",1] <- paste(meds_tmp$med, collapse = "; ") 
  demo_table["CIRS Comorbidity Score",1] <- tmp %>% 
    select(contains("cirs")) %>% 
    mutate(cirs_total = rowSums(across(where(is.numeric)))) %>%
    pull(cirs_total)
  

  # Build the Cognitive Test Table #####
  cog_table["TICS",1] <- ifelse(!is.na(tmp$scrn_tics_score),as.character(tmp$scrn_tics_score),'Missing')
  cog_table["Hopkins Verbal Learning Test Immediate",1] <- ifelse(!is.na(tmp$hvlt_imm_sum),as.character(tmp$hvlt_imm_sum),'Missing')
  cog_table["Hopkins Verbal Learning Test Delayed",1] <- ifelse(!is.na(tmp$cog_hvlt_t4_correct_delay),as.character(tmp$cog_hvlt_t4_correct_delay),'Missing')
  cog_table["Flanker",1] <- ifelse("flanker_raw_score" %in% etmp$measure_name,as.character(etmp$measure_value[which(etmp$measure_name=="flanker_raw_score")]),'Missing')
  cog_table["Dimensional Card Sort",1] <- ifelse("dcst_raw_score" %in% etmp$measure_name,as.character(etmp$measure_value[which(etmp$measure_name=="dcst_raw_score")]),'Missing')
  cog_table["Oral Symbol Digit",1] <- ifelse("odst_raw_score" %in% etmp$measure_name,as.character(etmp$measure_value[which(etmp$measure_name=="odst_raw_score")]),'Missing')
  cog_table["Logical Memory Immediate",1] <- ifelse(!is.na(tmp$cog_lm_a_imm),as.character(tmp$cog_lm_a_imm),'Missing')
  cog_table["Logical Memory Delayed",1] <- ifelse(!is.na(tmp$cog_lm_a_delay),as.character(tmp$cog_lm_a_delay) ,'Missing')
  cog_table["Letter Comparison",1] <- ifelse(!is.na(tmp$lc_sum),as.character(tmp$lc_sum),'Missing')
  cog_table["Matrix Reasoning Accuracy",1] <- ifelse("matreason_acc" %in% etmp$measure_name,paste0(as.character(round(etmp$measure_value[which(etmp$measure_name=="matreason_acc")],2)*100),"%"),'Missing')
  cog_table["Task Switching Accuracy",1] <- ifelse("taskswitch_switch_acc" %in% etmp$measure_name,paste0(as.character(round(etmp$measure_value[which(etmp$measure_name=="taskswitch_switch_acc")],2)*100),"%"),'Missing')
  cog_table["Spatial Working Memory 4-item Accuracy",1] <- ifelse("spatialwm_4item_acc" %in% etmp$measure_name,paste0(as.character(round(etmp$measure_value[which(etmp$measure_name=="spatialwm_4item_acc")],2)*100),"%"),'Missing')
  cog_table["Stroop Interference Accuracy",1] <- ifelse("stroop_incongruent_acc" %in% etmp$measure_name,paste0(as.character(round(etmp$measure_value[which(etmp$measure_name=="stroop_incongruent_acc")],2)*100) ,"%"),'Missing')
  cog_table["Wechsler Test of Adult Reading",1] <- ifelse(!is.na(tmp$cog_wtar_raw),as.character(tmp$cog_wtar_raw),'Missing')
  
  cog_table["TICS",2] <- paste("<25 cut of for impaired performance")
 #HVLT norms come from Duff PMC4803525
  cog_table["Hopkins Verbal Learning Test Immediate",2] <- paste(as.character(round((tmp$hvlt_imm_sum-24.8)/5.7),2),"sd:  Unadjusted Z-Score")
  cog_table["Hopkins Verbal Learning Test Delayed",2] <-  paste(as.character(round((tmp$cog_hvlt_t4_correct_delay-7.4)/3.6),2),"sd:  Unadjusted Z-Score")
  #NIH norms come from NIH Toolbox
  cog_table["Flanker",2] <- paste0(as.character(etmp$measure_value[which(etmp$measure_name=="flanker_pctile")]),"%: Age Adjusted National Percentile")
  cog_table["Dimensional Card Sort",2] <-  paste0(as.character(etmp$measure_value[which(etmp$measure_name=="dcst_pctile")]),"%: Age Adjusted National Percentile")
  cog_table["Oral Symbol Digit",2] <- paste("Range: 0-144")
  #Logical Memory Norms come from Duff PMC4803525
  cog_table["Logical Memory Immediate",2] <- paste(as.character(round((tmp$cog_lm_a_imm-13.9)/3.9),2),"sd:  Unadjusted Z-Score")
  cog_table["Logical Memory Delayed",2] <- paste(as.character(round((tmp$cog_lm_a_delay-12.6)/4.3),2),"sd:  Unadjusted Z-Score")
  #Letter Comparison norms from Salthouse and Fristoe 1996
  cog_table["Letter Comparison",2] <- paste(as.character(round((tmp$lc_sum-8.7)/2.9),2),"sd:  Unadjusted Z-Score") 
  cog_table["Matrix Reasoning Accuracy",2] <- paste("Range: 0-100%") 
  cog_table["Task Switching Accuracy",2] <- paste("Range: 0-100%")
  cog_table["Spatial Working Memory 4-item Accuracy",2] <- paste("Range: 0-100%") 
  cog_table["Stroop Interference Accuracy",2] <- paste("Range: 0-100%") 
  cog_table["Wechsler Test of Adult Reading",2] <- ifelse(!is.na(tmp$cog_wtar_raw),paste0(wtar_age_percent,"%: Age Adjusted Percentile"),'Range: 0-50')

  if(messages_flag == 1){message(paste("Table built"))}
  
  mmr_to_save <- c('demo_table','meds_tmp','cog_table','question')
  save(list = mmr_to_save, file = file.path(data_dir,'med_monitor_report','current.Rdata'))
  
  #Skip if Eric Working on from home since he doesn't have a latex compiler that works
  #if(eric_at_home == 1){
  # Run in R Markdown ####
  rmarkdown::render(file.path(med_monitor_dir,'med_monitor_report.Rmd'),
                    output_file = file.path(output_dir, paste0("COMET_MMR_RED_",as.character(tmp$redid),"_ID_",as.character(tmp$comet_study_id),".pdf")))
  #}
  
  if(messages_flag == 1){message(paste("Completed",i))}
  rm(tmp)
  
}

export(saved_adi, file.path(data_dir,'med_monitor_report','adi','adi.csv'))





