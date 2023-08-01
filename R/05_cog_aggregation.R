#' @author EDV on 20210802
#'
#' @name cog_aggregation
#'
#' @title Aggregate Cognitive Data
#'
#' @section Development:
#' 8.25.21 Began modifying by JC. Throwing error if length(available_csv$available) == 0. JC \cr
#' 8.25.21 Error was due to directory naming issue, fixed. JC \cr
#' 10.12.21 Issue with reg and summary for loop fixed. JC \cr
#' 11.3.21 Fixed another issue with reg and summary for loop. Selected columns before binding. JC \cr
#' 10.7.21 Fixed pin for nih toolbox when dashes are used instead of underscores. JC \cr
#' 12.14.22 Occasionally ecog data produce blank txt files. An na dataframe was created to bind to all other ecog data
#' in full_cog. Any blank text file produces NA for date, time, and measure_value. JC \cr
#' 1.20.23 Added an way to correct in the code any data entry PIN errors of the full_cog data base. JC \cr
#' 2.1.23 Added file path to the full_cog output dataframe JC \cr
#'
#' @description
#' This script is for the COMET study
#' It reads in all available computer-based cognitive test files
#' and aggregates them based on ID and timepoint
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas

message("Beginning 05 cog aggregation")

available_files <-
  list.files(file.path(data_dir, 'raw', 'cog_data'),
             ".csv|.txt",
             full.names = TRUE) %>%
  as_tibble_col(column_name = "available") %>%
  filter(!grepl("fulldata", available))


output_order <-
  c("pin", "date", "time", "tp", "measure_name", "measure_value","file")


#### Start with the NIH Toolbox ####
#columns we want
reg_cols <-
  c(
    "PIN",
    "MothersEducation",
    "FathersEducation",
    "Gender",
    "Handedness",
    "Race",
    "Ethnicity",
    "Assessment Name"
  )
summary_cols <-
  c(
    "PIN",
    "Assessment Name",
    "Inst",
    "RawScore",
    "DateFinished",
    "Language",
    "Computed Score",
    "Uncorrected Standard Score",
    "Age-Corrected Standard Score",
    "National Percentile (age adjusted)",
    "Fully-Corrected T-score"
  )




available_csv <- available_files %>%
  filter(grepl(".csv", available_files$available))

flag_first_reg <- 0
flag_first_summary <- 0

for (i in 1:length(available_csv$available)) {
  #i=2 #for testing
  tmp <- import(available_csv$available[i]) %>%
    mutate(across(contains("DateFinished"), ~ as.POSIXct(.x,
      tryFormats = c(
        "%m/%d/%Y %H:%M:%S",
        "%m/%d/%Y %H:%M",
        "%m-%d-%Y %H:%M:%S",
        "%m-%d-%Y %H:%M",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d %H:%M", 
        "%Y/%m/%d %H:%M", 
        "%Y/%m/%d %H:%M:%S"
      )
    )))
    
  if (grepl("registration", available_csv$available[i]) == TRUE) {
    if (flag_first_reg == 0) {
      reg <- tmp %>% select(all_of(reg_cols)) %>%
        mutate(file = available_csv$available[i])
      flag_first_reg <-
        1  #change the flag. We do this to get around some issues with declaring a blank reg matrix
    } else {
      tmp <- tmp %>% select(all_of(reg_cols)) %>%
        mutate(file = available_csv$available[i])
      reg <- rbind(reg, tmp)
    }
  } else if (grepl("summary_scores", available_csv$available[i]) == TRUE) {
    if (flag_first_summary == 0) {
      summary <- tmp %>% select(all_of(summary_cols)) %>%
        mutate(file = available_csv$available[i])
      flag_first_summary <-
        1  #change the flag. We do this to get around some issues with declaring a blank reg matrix
    } else {
      tmp <- tmp %>% select(all_of(summary_cols)) %>%
        mutate(file = available_csv$available[i])
      summary <- rbind(summary, tmp)
    }
  }
  
} #end for



#Rename the instruments from their long version
summary <- summary %>%
  mutate(Inst = ifelse(
    grepl("Flanker", Inst),
    "flanker",
    ifelse(grepl("Oral", Inst), "odst",
           ifelse(
             grepl("Dimensional", Inst), "dcst", "unknown"
           ))
  ))


#right now I'm not using any of the registration values. That may change. Not sure. I'm keeping the mechanics of that available to me above
#even though it might take extra time.
#' 2.2.2023: There was an issue with identifying the date on 5744_001_2. The code was identifying it a day after it occurred (5/3/22 instead of 5/2/22).
#' I switched the way date is identified to ymd_hms. JC 
#' 2.2.23: Removing the example only fields from the 146904_7934_007_2 file on 20220502 from the data frame JC
nih_tb <- summary %>% #left_join(summary, reg)  %>%
  rename(
    pin = "PIN",
    tp = "Assessment Name",
    date = "DateFinished",
    raw_score = "RawScore",
    pctile = `National Percentile (age adjusted)`
  )  %>%
  mutate(tp = gsub("Assessment ", "", tp))  %>%
  mutate(date = force_tz(date, tzone = "America/Chicago")) %>%
  mutate(time = strftime(date, format = "%H:%M:%S"))  %>%
  mutate(date = ymd_hms(date)) %>%
  filter(!str_detect(pin,"Example")) %>%  #Removing the example fields JC
  pivot_longer(., c(raw_score, pctile)) %>%
  mutate(measure_name = paste0(Inst, "_", name)) %>%
  rename(measure_value = "value") %>%
  mutate(date = as.Date(date)) %>%
  mutate(pin = gsub("-","_",pin)) %>%
  select(all_of(output_order))



#### Now do VCAP txt files ####
#### Declare some tibbles for each test ####
#columns we want
na_cols <-
  c("pin",
    "date",
    "time",
    "tp",
    "measure_name",
    "measure_value",
    "file")
na <- matrix(NA, 1, length(na_cols)) %>% as_tibble()
colnames(na) <- na_cols

sr_cols <-
  c("pin",
    "date",
    "time",
    "tp",
    "spatial_rel_rt",
    "spatial_rel_acc",
    "file")
sr <- matrix(NA, 1, length(sr_cols)) %>% as_tibble()
colnames(sr) <- sr_cols


matreason_cols <-
  c("pin", "date", "time", "tp", "matreason_rt", "matreason_acc","file")
matreason <- matrix(NA, 1, length(matreason_cols)) %>% as_tibble()
colnames(matreason) <- matreason_cols


stroop_cols <-
  c(
    "pin",
    "date",
    "time",
    "tp",
    "stroop_congruent_rt",
    "stroop_congruent_acc",
    "stroop_incongruent_rt",
    "stroop_incongruent_acc",
    "stroop_neutral_rt",
    "stroop_neutral_acc",
    "file"
  )
stroop <- matrix(NA, 1, length(stroop_cols)) %>% as_tibble()
colnames(stroop) <- stroop_cols


swm_cols <-
  c(
    "pin",
    "date",
    "time",
    "tp",
    "spatialwm_2item_rt",
    "spatialwm_3item_rt",
    "spatialwm_4item_rt",
    "spatialwm_2item_acc",
    "spatialwm_3item_acc",
    "spatialwm_4item_acc",
    "file"
  )
swm <- matrix(NA, 1, length(swm_cols)) %>% as_tibble()
colnames(swm) <- swm_cols

ts_cols <-
  c(
    "pin",
    "date",
    "time",
    "tp",
    "taskswitch_lohi_rt",
    "taskswitch_odev_rt",
    "taskswitch_repeat_rt",
    "taskswitch_switch_rt",
    "taskswitch_lohi_acc",
    "taskswitch_odev_acc",
    "taskswitch_repeat_acc",
    "taskswitch_switch_acc",
    "file"
  )
ts <- matrix(NA, 1, length(ts_cols)) %>% as_tibble()
colnames(ts) <- ts_cols


######
## Get all the available .txt files
available_txt <- available_files %>%
  filter(grepl("SUMMARY.txt", available_files$available))

#initialize tmp
tmp <- NULL

##### read each file and build the tibbles accordingly
for (i in 1:length(available_txt$available)) {

  #Added tryCatch on 12/14/22 for blank files
  tmp <- tryCatch(
    read.delim(available_txt$available[i], header = FALSE, stringsAsFactors = F),
    error = function(e) 
    {e
      return(NA)
    }
  )
  
  #tmp <- read.delim(available_txt$available[i], header = FALSE, stringsAsFactors = F)
  
  if (is.null(nrow(tmp))) {
    
    ids <- data.frame(str_split(gsub("-|__|_",".",basename(available_txt$available[i])),"[.]", simplify = T)) %>%
      mutate(pin = X1) %>%
      mutate(tp = X2) 
    
    if(grepl("SR_SUMMARY", available_txt$available[i]) == TRUE) {
      tmp_na <- data.frame(measure_name = c("spatial_rel_rt","spatial_rel_acc"))
    } else if (grepl("Matrix_SUMMARY", available_txt$available[i]) == TRUE) {
      tmp_na <- data.frame(measure_name = c("matreason_rt","matreason_acc"))
    } else if (grepl("Stroop_SUMMARY", available_txt$available[i]) == TRUE) {
      tmp_na <- data.frame(measure_name = c("stroop_congruent_rt","stroop_congruent_acc","stroop_incongruent_rt","stroop_incongruent_acc",
                                            "stroop_neutral_rt","stroop_neutral_acc"))
    } else if (grepl("SWM_SUMMARY", available_txt$available[i]) == TRUE) {
      tmp_na <- data.frame(measure_name = c("spatialwm_2item_rt","spatialwm_3item_rt","spatialwm_4item_rt",
                                            "spatialwm_2item_acc","spatialwm_3item_acc","spatialwm_4item_acc"))
    } else if (grepl("TS_SUMMARY", available_txt$available[i]) == TRUE) {
      tmp_na <- data.frame(measure_name = c("taskswitch_lohi_rt","taskswitch_odev_rt","taskswitch_repeat_rt","taskswitch_switch_rt",
                                            "taskswitch_lohi_acc","taskswitch_odev_acc","taskswitch_repeat_acc","taskswitch_switch_acc"))
    }
    
    tmp_na <- tmp_na %>%
      mutate(pin = ids$pin) %>%
      mutate(tp = ids$tp) %>%
      mutate(date = NA, time = NA, measure_value = NA) %>%
      mutate(file = available_txt$available[i])
    
    na <- rbind(na, tmp_na)
    
    
  } else if (grepl("SR_SUMMARY", available_txt$available[i]) == TRUE) {
    tmp_sr <- matrix(NA, 1, length(sr_cols)) %>% as_tibble()
    colnames(tmp_sr) <- sr_cols
    
    #Spatial Relations summary
    tmp_sr$pin[1] <- tmp$V1[1]
    tmp_sr$date[1] <- tmp$V2[1]
    tmp_sr$time[1] <- tmp$V3[1]
    tmp_sr$tp[1] <-
      gsub("-", "", str_extract(basename(available_txt$available[i]), "-[[:digit:]]-"))
    tmp_sr$spatial_rel_rt <-   tmp[3, which(tmp[2, ] == "SpatialRel RT")]
    tmp_sr$spatial_rel_acc <-
      tmp[3, which(tmp[2, ] == "SpatialRel ACC")]
    tmp_sr$file <- available_txt$available[i]
    
    sr <- rbind(sr, tmp_sr)
    
    
  } else if (grepl("Matrix_SUMMARY", available_txt$available[i]) == TRUE) {
    tmp_matrix <- matrix(NA, 1, length(matreason_cols)) %>% as_tibble()
    colnames(tmp_matrix) <- matreason_cols
    
    #Spatial Relations summary
    tmp_matrix$pin[1] <- tmp$V1[1]
    tmp_matrix$date[1] <- tmp$V2[1]
    tmp_matrix$time[1] <- tmp$V3[1]
    tmp_matrix$tp[1] <-
      gsub("-", "", str_extract(basename(available_txt$available[i]), "-[[:digit:]]-"))
    tmp_matrix$matreason_rt <-    tmp[3, which(tmp[2, ] == "Matrix RT")]
    tmp_matrix$matreason_acc <-   tmp[3, which(tmp[2, ] == "Matrix ACC")]
    tmp_matrix$file <- available_txt$available[i]
    
    matreason <- rbind(matreason, tmp_matrix)
    
  } else if (grepl("Stroop_SUMMARY", available_txt$available[i]) == TRUE) {
    tmp_stroop <- matrix(NA, 1, length(stroop_cols)) %>% as_tibble()
    colnames(tmp_stroop) <- stroop_cols
    
    #Spatial Relations summary
    tmp_stroop$pin[1] <- tmp$V1[1]
    tmp_stroop$date[1] <- tmp$V2[1]
    tmp_stroop$time[1] <- tmp$V3[1]
    tmp_stroop$tp[1] <-
      gsub("-", "", str_extract(basename(available_txt$available[i]), "-[[:digit:]]-"))
    tmp_stroop$stroop_congruent_rt <-
      tmp[3, which(tmp[2, ] == "Congruent RT")]
    tmp_stroop$stroop_congruent_acc <-
      tmp[3, which(tmp[2, ] == "Congruent ACC")]
    tmp_stroop$stroop_incongruent_rt <-
      tmp[3, which(tmp[2, ] == "Incongruent RT")]
    tmp_stroop$stroop_incongruent_acc <-
      tmp[3, which(tmp[2, ] == "Incongruent ACC")]
    tmp_stroop$stroop_neutral_rt <-
      tmp[3, which(tmp[2, ] == "Neutral RT")]
    tmp_stroop$stroop_neutral_acc <-
      tmp[3, which(tmp[2, ] == "Neutral ACC")]
    tmp_stroop$file <- available_txt$available[i]
    
    stroop <- rbind(stroop, tmp_stroop)
    
  } else if (grepl("SWM_SUMMARY", available_txt$available[i]) == TRUE) {
    tmp_swm <- matrix(NA, 1, length(swm_cols)) %>% as_tibble()
    colnames(tmp_swm) <- swm_cols
    
    #Spatial Relations summary
    tmp_swm$pin[1] <- tmp$V1[1]
    tmp_swm$date[1] <- tmp$V2[1]
    tmp_swm$time[1] <- tmp$V3[1]
    tmp_swm$tp[1] <-
      gsub("-", "", str_extract(basename(available_txt$available[i]), "-[[:digit:]]-"))
    tmp_swm$spatialwm_2item_rt <-
      tmp[3, which(tmp[2, ] == "2-item RT")]
    tmp_swm$spatialwm_3item_rt <-
      tmp[3, which(tmp[2, ] == "3-item RT")]
    tmp_swm$spatialwm_4item_rt <-
      tmp[3, which(tmp[2, ] == "4-item RT")]
    tmp_swm$spatialwm_2item_acc <-
      tmp[3, which(tmp[2, ] == "2-item ACC")]
    tmp_swm$spatialwm_3item_acc <-
      tmp[3, which(tmp[2, ] == "3-item ACC")]
    tmp_swm$spatialwm_4item_acc <-
      tmp[3, which(tmp[2, ] == "4-item ACC")]
    tmp_swm$file <- available_txt$available[i]
    
    swm <- rbind(swm, tmp_swm)
    
  } else if (grepl("TS_SUMMARY", available_txt$available[i]) == TRUE) {
    tmp_ts <- matrix(NA, 1, length(ts_cols)) %>% as_tibble()
    colnames(tmp_ts) <- ts_cols
    
    #Spatial Relations summary
    tmp_ts$pin[1] <- str_remove(tmp$V1[1], "Subject-I")
    tmp_ts$date[1] <- str_remove(tmp$V3[1], "Data collected on: ")
    tmp_ts$time[1] <- NA
    tmp_ts$tp[1] <-
      gsub("-", "", str_extract(basename(available_txt$available[i]), "-[[:digit:]]-"))
    tmp_ts$taskswitch_lohi_rt <-
      tmp[3, which(tmp[2, ] == "Low-High RT")]
    tmp_ts$taskswitch_odev_rt <-
      tmp[3, which(tmp[2, ] == "Odd-Even RT")]
    tmp_ts$taskswitch_repeat_rt <- tmp[3, which(tmp[2, ] == "Repeat RT")]
    tmp_ts$taskswitch_switch_rt <- tmp[3, which(tmp[2, ] == "Switch RT")]
    tmp_ts$taskswitch_lohi_acc <-
      tmp[3, which(tmp[2, ] == "Low-High ACC")]
    tmp_ts$taskswitch_odev_acc <-
      tmp[3, which(tmp[2, ] == "Odd-Even ACC")]
    tmp_ts$taskswitch_repeat_acc <-
      tmp[3, which(tmp[2, ] == "Repeat ACC")]
    tmp_ts$taskswitch_switch_acc <-
      tmp[3, which(tmp[2, ] == "Switch ACC")]
    tmp_ts$file <- available_txt$available[i]
    
    ts <- rbind(ts, tmp_ts)
  }
  
  
} #end for


#### Remove vestigial creation rows of NA, and pivot long for binding
cleanup <- function(x) {
  y <-  x %>%
    filter(., rowSums(is.na(.)) != ncol(.)) %>%
    pivot_longer(.,
                 -c(pin, date, time, tp, file),
                 names_to = "measure_name",
                 values_to = "measure_value") %>%
    mutate(measure_value = as.numeric(measure_value)) %>%
    #mutate(date = format(as.Date(date, "%m-%d-%Y"), "%Y-%m-%d")) %>%
    mutate(date = mdy(date)) %>%
    select(all_of(output_order))
  return(y)
}

#JC added na on 12/14/22 to deal with blank data
na <- na %>% filter(!is.na(pin))
sr <- cleanup(sr)
matreason <- cleanup(matreason)
stroop <- cleanup(stroop)
swm <- cleanup(swm)
ts <- cleanup(ts)

full_cog_temp <- bind_rows(nih_tb, na) %>%
  bind_rows(., sr) %>%
  bind_rows(., matreason) %>%
  bind_rows(., stroop) %>%
  bind_rows(., swm) %>%
  bind_rows(., ts) 


# todo <- c("matreason", "stroop", "swm", "ts", "na")
# for (i in 1:length(todo)) {
#   full_cog_temp <- bind_rows(full_cog, get(todo[i]))
# }

#2/6/23: Commenting out. Don't need. JC
#### Repair IDs/pins ###
# NIH Toolbox comes with the full subject id HSC_StudyID_RED
# VCAP only comes with subject id. So make them the same, only  subject for now.
# for (i in 1:length(full_cog_temp$pin)) {
#   ifelse(
#     grepl("_", full_cog_temp$pin[i]),
#     full_cog_temp[i] <-
#       gsub(
#         "[[:punct:]]",
#         "",
#         str_extract(full_cog_temp$pin[i], "[[:punct:]]{1}[[:digit:]]+[[:punct:]]{1}")
#       ),
#     full_cog_temp$pin[i] <- full_cog_temp$pin[i]
#   )
# }

#' 1.30.23 Adding to denote TP of cog test. JC 
comet_tp_to_join <- comet %>%
  filter(redcap_event_name == "baseline_arm_1") %>%
  filter(!is.na(comet_study_id)) %>%
  mutate(redid = as.character(redid)) %>%
  select(redid, comet_baselinecogsched, comet_week26cogsched, comet_week52cogsched) %>%
  pivot_longer(cols = contains("sched"), names_to = "name_timepoint") %>%
  mutate(value = as.Date(value))
  #mutate(across(contains("sched"), ~ as.Date(.x)))

#Kept in case I want to change later. This uses cog_date  to match data as opposed to scheduled date
# pin_to_join <- comet %>%
#   filter(!is.na(comet_study_id)) %>%
#   select(record_id, redid)
# 
# comet_tp_to_join <- comet %>%
#   select(record_id, cog_date, redcap_event_name) %>%
#   filter(!is.na(cog_date)) %>%
#   left_join(., pin_to_join, by = "record_id") %>%
#   mutate(redid = as.character(redid))

#mutate(across(contains("sched"), ~ as.Date(.x)))

#' Added case_when to deal with data entry errors JC
#' 1.20.2023: 6802_119 was incorrectly entered as 6801_119. It has been corrected below. JC
#' 1.20.2023: Corrected from 10255 to 12055. JC
#' 1.30.2023: Added time points JC
#' 2.1.2023: Corrected 1_13037_SR_SUMMARY - timepoint and id were switched. Fixed id from 1 to 13037 JC
#' 2.1.2023: Corrected 146904_120_0914_2nih_toolbox from id of 120 to 12009 JC 
#' 2.1.2023: Corrected ecog of 914_1 from redid of 914 to 12009 JC
#' 2.1.2023: Removed test cog which occured on 20220730 with pin 1234 JC
#' 2.1.2023: spatial relations was mislabeled changed 02060 on 20221213 to 12060 JC
#' 2.2.2023: Corrected pin on 7/12/22 matreason from 11652 to 11952 JC
#' 4.6.2023: Corrected pin on 20230403 nih toolbox data from 1342 to 13427 JC
#' 4.6.2023: Task switch mislabeled on 20230403 as time point instead of red. Corrected from 1 to 13427. JC
#' 4.7.2023: Test on 20230215 was done by 12009_014. Changed pin from 120 to 12009. JC 
full_cog <- full_cog_temp %>%
  mutate(pin = case_when(str_detect(pin,"_") ~ gsub("[[:punct:]]","",str_extract(pin, "[[:punct:]]{1}[[:digit:]]+[[:punct:]]{1}")),
                         T ~ pin)) %>%
  mutate(pin = case_when(pin == "6801" ~ "6802",
                         pin == "10255" ~ "12055",
                         pin == "1" & date == ymd("20220926") ~ "13037",
                         pin == "120" & date == ymd("20220810") ~ "12009",
                         pin == "0914" & date == ymd("20220107") ~ "12009",
                         pin == "914" & date == ymd("20220107") ~ "12009",
                         pin == "02060" & date == ymd("20221213") ~ "12060",
                         pin == "11652" & date == ymd("20220712") ~ "11952",
                         pin == "12804" & date == ymd("20220824") ~ "12861",
                         pin == " 13037" & date == ymd("20220926") ~ "13037",
                         pin == "1342" & date == ymd("20230403") ~ "13427",
                         pin == "1" & date == ymd("20230403") ~ "13427",
                         pin == "120" & date == ymd("20230215") ~ "12009",
                         T ~ pin)) %>%
  filter(!(pin == "1234" & date == ymd("20210730"))) %>% #Test cog test before study started
  mutate(date = ymd(date))  %>%
  left_join(., comet_tp_to_join, by = c("pin" = "redid", "date" = "value")) 
  

##### Save clean workspace ####
save(full_cog,
     file = file.path(clean_data_destination, 'comet_ecog_clean.Rdata'))
