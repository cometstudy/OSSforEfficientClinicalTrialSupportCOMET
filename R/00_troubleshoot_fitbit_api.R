#' @author Jon Clutton 
#' 
#' @name troubleshoot_fitbit_api
#' 
#' @title Troubleshoot FItbit Functions
#' 
#' @description 
#' Fitbit functions are embeded in functions. These script pulls the script out of functions
#' to test one-off problems. 
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas
#' 
#' @section Development:
#' 1.12.22 Began devlopment JC \cr
#' 
#' 

###### Load Data #####
load(file.path(data_dir,'clean','comet_clean.Rdata'))

#### App info
client_info <- import(file.path(project_dir,'data','read_write','read_write','client_info.csv')) 

### User info
fitbit_users <- read.csv(file.path(data_dir,'read_write','read_write','fitbit_users.csv'), na.strings = c("", " "))


######### Check to see if tokens need to be refreshed #####
state <- get_status(usercode = fitbit_users$user[1], token = fitbit_users$access_token[1])

if(state >= 400) {
  source(file.path(script_dir,'02_refresh_token.R'))
}

#### COMET REDCap and fitbit_users joined ####
#filtered for randomized participants
active_participants <- comet %>%
  filter(redcap_event_name == 'baseline_arm_1') %>%
  filter(is.na(comet_study_id)==F) %>%
  right_join(., fitbit_users, by = c("comet_study_id" = "id")) %>%
  filter(status___6 == 0 | status___7 == 0 | status___8 == 0 | status___9 == 0) %>%
  filter(is.na(comet_study_id)==F) %>%
  filter(is.na(randomization_date)==F)

i <- which(active_participants$comet_study_id == 123)

current <- active_participants[i, ]

usercode <- current$user
token <- current$access_token
start_date <- '2022-01-01'
end_date <- today()#ymd("2023-03-22")
date <- today()#ymd("2023-03-22")

test <- GET(url =
                        paste0('https://api.fitbit.com/1/user/',
                               usercode,
                               '/devices.json'),
                      add_headers(Authorization = paste0("Bearer ", token)))


####### LAST SYNC ###########
last_sync_json <- content(GET(url =
                                paste0('https://api.fitbit.com/1/user/',
                                       usercode,
                                       '/devices.json'),
                              add_headers(Authorization = paste0("Bearer ", token))))

last_sync_df <- jsonlist_to_df(last_sync_json) %>% 
  filter(deviceVersion != "MobileTrack")

battery <- last_sync_json[[1]]$batteryLevel

####### GET Device Data #####
device_data <-  get_device_data(usercode = usercode, token = token)

######## Get Status ########
get_status_json <- GET(url =
                       paste0('https://api.fitbit.com/1/user/',
                              usercode,
                              '/devices.json'),
                     add_headers(Authorization = paste0("Bearer ", token)))
  
  status <- get_status_json$status_code
 
  


####### GET STEPS ####
steps_json <- GET(url =
                    paste0('https://api.fitbit.com/1/user/',
                           usercode,
                           '/activities/steps/date/',
                           start_date,
                           '/',
                           end_date,'.json'),
                  add_headers(Authorization = paste0("Bearer ", token)))

steps <- jsonlist_to_df(content(steps_json))

####### GET HR #####
request <- paste0('https://api.fitbit.com/1/user/',
                  usercode,
                  '/activities/heart/date/',
                  date,
                  '/1d/1min/time/00%3A00/23%3A59.json')

got_hr <- GET(url =
                request,
              add_headers(Authorization = paste0("Bearer ", token)))



hr_json <- fitbit_parse(got_hr)

date <- hr_json$`activities-heart`[[1]]$dateTime

hr_list <- hr_json[[2]][[1]]

hr <- data.table::rbindlist(lapply(hr_list, data.table::as.data.table)) %>%
  mutate(date = date)

####### GET ACTIVITIES #####
request <- paste0('https://api.fitbit.com/1/user/',
                  usercode,
                  '/activities/date/',
                  date,
                  '.json')

got_activities <- GET(url =
                        request,
                      add_headers(Authorization = paste0("Bearer ", token)))

activities_list <- fitbit_parse(got_activities)

activities_list$activities

temp <- purrr::transpose(activities_list$activities) %>%
  purrr::list_modify("distance" = NULL) %>%
  purrr::map_dfr(., unlist) %>%
  as.tibble(., .name_repair = c( "unique"))

activities <- jsonlist_to_df(activities_list$activities)

####### REVOKE TOKEN ######
####### FITBIT PARSE ######
text <- content(got_hr, as = "text")
if (identical(text, "")) stop("No output to parse", call. = FALSE)
jsonlite::fromJSON(text, simplifyVector = FALSE)

