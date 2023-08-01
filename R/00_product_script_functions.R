#' @title Product Scripts Functions
#' @name product_script_functions
#' @section Development:
#' 1.3.2023 Large updates were made to multiple product script functions. All changes are
#' documented in the individual function. The old product scripts were saved under the archive
#' folder as 00_product_script_functions_1-3-23.R. JC \cr
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas




#' @title Data Feedback Generator
#' 
#' @name feedback_generator
#' 
#' @description 
#' This is a COMET specific function that writes a small amount of html to paste
#' into a larger email. The html gives partipants feedback on their exercise performance
#' from the previous week..
#' 
#' @param activities_log dataframe of all activities, all participants
#' @param hr_log data frame of all summarized hr data for all participants
#' @param step_log dataframe of all steps for all participants
#' @param group participants assigned group
#' @param id comet_study_id
#' @param week current week of participant
#' @return statement an html statement matching the correct feedback
#' 
#' @section Development:
#' 11/18/21: Updated endurance_progression and st_progression to be pulled from current_exercise_prescription. JC \cr
#' 11.16.22: Received feedback from participant that the "Sunday Gap" of missing data caused frustration when he 
#' was successfully completing his prescription. I'm working to update the feedback generator to take real-time feedback
#' into account. JC
#' 12.6.22: I've successfully updated to be read in real time. I tested on 12/6/22 and didn't appear to have any errors. 
#' In testing 4 of 61 emails likely would have been more accurate with 8 of 61 emails potentially being different. Anecdotally it looked like a 
#' surprising amount of Sunday sessions were still captured. This was likely from auto-syncing Mondays before 8 AM. 
#' Important Note: In this update, I abandoned a lot of Good Function Practice. 
#' It was really difficult to test the function when all of the variables had to independent of the current participant. Thus most of the function relies
#' on being embedded within its 07_participant_email workflow. The variables it calls are usually declared in 07_participant_email.
#' This has been successfully tested and shouldn't impacted the output. JC \cr
#' 1.3.2023: Retesting changes. JC \cr
#' 1.30.2023: Patched a couple errors. Got a lot of feedback from participants about errors from last week. JC \cr
#' 20230305: Updated to allow core and fusion participants to use Pilates instead of Yoga on fitbit. JC \cr
#' 
#' 
#' 

message("Beginning 00 product script functions")

feedback_generator <- function(activities_log = NULL, hr_log = NULL, step_log = NULL, group = NULL, id = NULL, week = NULL, current_exercise_prescription = NULL){

  message("Beginning feedback_generator")
  #Stored HR Log
  hr_log_current <- filter(hr_all_df, comet_study_id == current$comet_study_id)
  
  #Find Last week and last weeks year
  last_week <- case_when(isoweek(today())-1 == 0 ~ 52,
                         isoweek(today())-1 != 0 ~ isoweek(today())-1)
  
  last_week_year <- year(today()-7)
  
  
  #Get last sync in real time
  last_sync_date <- tryCatch(
    as_date(floor_date(ymd_hms(get_last_sync(usercode = current$user, token = current$access_token)), unit = "day")),
    error = function(e) 
    {e
      as_date(hr_log_current$date[nrow(hr_log_current)])
    }
  )
  
  
  
  #Get last weeks steps in real time  
  last_week_steps <- get_steps(start_date = current$randomization_date, end_date = last_sync_date, usercode = current$user, token = current$access_token) %>%
    rename("date" = c(1), steps = "activities-steps.value") %>%
    mutate(steps = as.numeric(steps)) %>%
    mutate(week = isoweek(date), year = year(date)) %>%
    filter(week == last_week & year == last_week_year)
    
    # steps_json <- GET(url =
    #                     paste0('https://api.fitbit.com/1/user/',
    #                            current$user,
    #                            '/activities/steps/date/',
    #                            '2023/01/23',
    #                            '/',
    #                            '2023/01/29','.json'),
    #                   add_headers(Authorization = paste0("Bearer ", token)))
    # 
    # steps <- jsonlist_to_df(content(steps_json))
  
  #Endurance Prescription
  endurance_progression <- current_exercise_prescription$aerobic_duration
  #Core and Fusion Prescription
  st_progression <- current_exercise_prescription$cf_duration
  #Strength Prescription
  strength_progress <- current_exercise_prescription$st_duration
  
  last_week_activities_temp <- data.frame()
  
  if(nrow(last_week_steps > 0)) {
    

  for(i in 1:nrow(last_week_steps)) {
    
    date <- last_week_steps$date[i]
    
    got_activities <- get_activities(date = date, usercode = current$user, token = current$access_token)
    
    # request <- paste0('https://api.fitbit.com/1/user/',
    #                   current$user,
    #                   '/activities/date/',
    #                   date,
    #                   '.json')
    # 
    # got_activities <- GET(url =
    #                         request,
    #                       add_headers(Authorization = paste0("Bearer ", current$access_token)))
    # 
    # activities_list <- fitbit_parse(got_activities)
    # 
    # activities <- jsonlist_to_df(activities_list$activities)
    
    hr <- get_hr(date = date, usercode = current$user, token = current$access_token)
    
    # request <- paste0('https://api.fitbit.com/1/user/',
    #                   current$user,
    #                   '/activities/heart/date/',
    #                   date,
    #                   '/1d/1min/time/00%3A00/23%3A59.json')
    # 
    # got_hr <- GET(url =
    #                 request,
    #               add_headers(Authorization = paste0("Bearer ", current$access_token)))
    # 
    # 
    # 
    # hr_json <- fitbit_parse(got_hr)
    # 
    # date <- hr_json$`activities-heart`[[1]]$dateTime
    # 
    # hr_list <- hr_json[[2]][[1]]
    # 
    # hr <- data.table::rbindlist(lapply(hr_list, data.table::as.data.table)) %>%
    #   mutate(date = date)
    
    #steps to check if pt wore watch at all during that day
    steps_day_of <- last_week_steps$steps[i]
    
    hr_not_working <- 0 #Initialize HR watcher value
    #Send email to study coordinators that participant isn't collecting HR
    if(nrow(hr) < 2 & steps_day_of > 100 & length(steps_day_of) > 0) {
      hr_not_working <- 1
      
      #Added else if on 11.4.22 for when pt logs an exercise session but has no steps and HR. This occurs when it is logged through the watch
    } else if(nrow(hr) == 0 & steps_day_of == 0){
      hr_not_working <- 1
    }
    
    
    if(nrow(got_activities) > 0) {
      activities <- got_activities %>%
        mutate(comet_study_id = current$comet_study_id) %>%
        select(comet_study_id, activityParentName, duration, startDate, startTime) %>%
        mutate(startDate = ymd(startDate)) %>%
        mutate(end_time = hms::as_hms(hms::parse_hm(startTime) + duration/1000)) %>%
        mutate(time_in_zone = NA, time_over_zone = NA, average = NA) %>%
        mutate(duration = as.double(duration)) %>%
        mutate(duration = case_when(duration > 3600000 ~ 3600000,
                                    duration <= 3600000 ~ duration))
      
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
      
      last_week_activities_temp <- bind_rows(last_week_activities_temp, activities)
      
    } 
    
    
  }
    
  }
  
  if(nrow(last_week_activities_temp) > 0) {
  last_week_activities <- last_week_activities_temp %>%
    mutate(week = isoweek(startDate)) %>%
    filter(week == last_week) %>%
    mutate(duration = round(duration / 60 / 1000, digits = 0)) %>%
    mutate(average = round(average, digits = 2)) %>%
    rename(Activity = activityParentName, Date = startDate, `Minutes in HR Zone` =  time_in_zone, `Average HR` = average, Time = startTime) %>%
    select(Activity, Date, Time, duration, `Minutes in HR Zone`, `Average HR`) 
  
  
  last_week_activities_recorded <- last_week_activities %>%
    filter(Activity == "Workout" | Activity == "Yoga" | Activity == "Weights" | Activity == "Pilates") 
  
  last_week_activities_other <- last_week_activities %>%
    filter(Activity != "Workout" & Activity != "Yoga" & Activity != "Weights")
  
  } else {
    last_week_activities <- data.frame()        
    last_week_activities_recorded <- data.frame()
    last_week_activitis_other <- data.frame()
  }
  
  
  if(today()-last_sync_date > 7) {
    statement <- '<tr>
                        <td align="left" style="padding: 2px 0 5px 0;">
                            <p style= "font-size:100%;"> Uh oh. It looks like your Fitbit did not sync last week, so we do not have any of your exercise data. This is a reminder to go sync your Fitbit.<br><br>
                                                        For reminders about how to use the Fitbit watch, read the attached document called "Using My Fitbit." <br><br>
                                                        Respond to this email or call us at the contact information listed at the top of the email if you need help troubleshooting. <br><br>
                                                        Thanks for being an amazing exercise study participant. We are so thankful for you. You make this study great!                                                         </p>
                        </td>
                       </tr>'
    
  } else if(today()-last_sync_date <= 7 & sum(last_week_steps$steps) == 0) {
    statement <- '<tr>
                        <td align="left" style="padding: 2px 0 5px 0;">
                            <p style= "font-size:100%;"> Did you wear your watch last week? It looks like it has been syncing, but we do not have any of your data. <br><br>
                                                        For reminders about how to use the Fitbit watch, read the attached document called "Using My Fitbit." <br><br>
                                                        If you have been wearing your watch, respond to this email or call us at the contact information listed at the top of the email so that we can help figure out what is going on. <br><br>
                                                        Thanks for being an amazing exercise study participant. We are so thankful for you. You make this study great!                                                         </p>
                        </td>
                       </tr>'
    
  } else if(sum(last_week_steps$steps) > 0 & nrow(last_week_activities) == 0) {
    statement <- '<tr>
                        <td align="left" style="padding: 2px 0 5px 0;">
                            <p style= "font-size:100%;"> It looks like you wore your watch last week, but we did not see any exercise sessions. Remember to start and stop your watch each exercise session. For reminders about how to use the Fitbit watch, read the attached document called "Using My Fitbit."<br><br> If this is just a weird week, feel free to ignore this. But if something more serious is going on, like an injury or other life event, respond to this email or call us at the contact information listed at the top of the email and let us know what is going on and how we can help. <br><br>
                                                        Thanks for being an amazing exercise study participant. We are so thankful for you. You make this study great!                                                         </p>
                        </td>
                       </tr>'
  } else if(nrow(last_week_activities) > 0 & nrow(last_week_activities_recorded) == 0) {
    statement <- paste0('<tr>
                        <td align="left" style="padding: 2px 0 5px 0;">
                            <p style= "font-size:100%;"> Did you remember to start your watch at each exercise session? It looks like your Fitbit auto-recognized some activity, but none of it was manually recorded. Remember to start and stop exercise sessions on your watch to receive credit for doing exercise.  <br><br>
                                                        For reminders about how to use the Fitbit watch, read the attached document called "Using My Fitbit." <br><br>
                                                        Respond to this email or call us at the contact information listed at the top of the email if you need help with anything. <br><br>
                                                        Thanks for being an amazing exercise study participant. We are so thankful for you. You make this study great!<br>                                                         </p>
                        </td>
                       </tr>
                       <tr>
                        <td align="left" style="padding: 15px 0 5px 0;">
                            <p style= "font-size:100%;"> <b>Fitbit Automatically Recognized Exercise</b><br> Fitbit automatically recognizes some exercise behavior like walking or running. These activities were recognized by your watch but are not counted towards your weekly exercise goal. Below is a list of the exercises that your watch auto-recognized. Check to see if any were intended to be exercise sessions. If so, be sure to start and stop exercise sessions on your watch next time. </p>
                        </td>
                       </tr>
                        <tr>
                         <td align="center" style="padding: 5px 0 35px 0;">
                          ',html_table_generator(last_week_activities_other),'
                        </td>
                      </tr>')
  } else if(group == 2 & nrow(last_week_activities_recorded) > 0) {
    
    last_week_activities_yoga <- last_week_activities_recorded %>%
      filter(Activity == "Yoga" | Activity == "Pilates") %>%
      select(-`Minutes in HR Zone`)
    
    if(sum(last_week_activities_yoga$duration) < st_progression) {
      statement <- paste0('
                            <tr>
                          <td align="center" style="padding: 15px 0 5px 0;">
                            <p style="font-size:150%; font-family:Optima; color: #ff3d3d"> Uh oh! You did not quite meet your goal.</p>
                          </td>
                        </tr>
                        <tr>
                            <td align="left" style="padding: 10px 0 5px 0;">
                             <p style= "font-size:110%;"> You acheived <b>',round(sum(last_week_activities_yoga$duration)/st_progression*100, digits = 0),'%</b> of your goal (',sum(last_week_activities_yoga$duration),' min out of ',st_progression,' min).</p>
                           </td>
                         </tr>
                          <tr>
                        <td align="left" style="padding: 10px 0 5px 0;">
                            <p style= "font-size:100%;">Does this seem accurate? If not...<br><br>
                                                        Was your watch syncing? Your last sync date was ',last_sync_date,'.<br><br>
                                                        Do you see all of your exercise sessions in the table below? </p>
                        </td>
                       </tr>
                      <tr>
                        <td align="left" style="padding: 10px 0 5px 0;">
                            <p style= "font-size:100%;"> <b>Exercise Sessions</b><br> This table shows all of your exercise sessions from last week. Are they all there? If not, be sure to start and stop your watch at each exercise session.  For reminders about how to use the Fitbit watch, read the attached document called "Using My Fitbit.</p>
                        </td>
                       </tr>
                        <tr>
                         <td align="center" style="padding: 5px 0 30px 0;">
                          ',html_table_generator(last_week_activities_yoga),'
                        </td>
                      </tr>')
    } else if(sum(last_week_activities_yoga$duration) >= st_progression * 2) {
      statement <- paste0('
                            <tr>
                          <td align="center" style="padding: 15px 0 5px 0;">
                            <p style="font-size:150%; font-family:Optima; color: #ffff00"> You were a lot higher than your goal. Did you mean to do that?</p>
                          </td>
                        </tr>
                        <tr>
                            <td align="left" style="padding: 10px 0 5px 0;">
                             <p style= "font-size:110%;"> You acheived <b>',round(sum(last_week_activities_yoga$duration)/st_progression*100, digits = 0),'%</b> of your goal (',sum(last_week_activities_yoga$duration),' min out of ',st_progression,' min).</p>
                           </td>
                         </tr>
                          <tr>
                        <td align="left" style="padding: 10px 0 5px 0;">
                            <p style= "font-size:100%;">Does this seem accurate? If not...<br><br>
                                                        Was your watch syncing? Your last sync date was ',last_sync_date,'.<br><br>
                                                        Do you see all of your exercise sessions in the table below? </p>
                        </td>
                       </tr>
                      <tr>
                        <td align="left" style="padding: 10px 0 5px 0;">
                            <p style= "font-size:100%;"> <b>Exercise Sessions</b><br> This table shows all of your exercise sessions from last week. Are they all there? If not, be sure to start and stop your watch at each exercise session.  For reminders about how to use the Fitbit watch, read the attached document called "Using My Fitbit.</p>
                        </td>
                       </tr>
                        <tr>
                         <td align="center" style="padding: 5px 0 30px 0;">
                          ',html_table_generator(last_week_activities_yoga),'
                        </td>
                      </tr>')
    } else {
      statement <- paste0('
                        <tr>
                          <td align="center" style="padding: 15px 0 5px 0;">
                            <p style="font-size:150%; font-family:Optima; color: RoyalBlue"> Nicely done! You met your goal!</p>
                          </td>
                        </tr>
                          <tr>
                            <td align="left" style="padding: 10px 0 5px 0;">
                             <p style= "font-size:110%;"> You acheived <b>',round(sum(last_week_activities_yoga$duration)/st_progression*100, digits = 0),'%</b> of your goal (',sum(last_week_activities_yoga$duration),' min out of ',st_progression,' min).</p>
                           </td>
                         </tr>
                      <tr>
                        <td align="left" style="padding: 10px 0 5px 0;">
                            <p style= "font-size:100%;"> <b>Exercise Sessions</b><br> This table shows all of your exercise sessions from last week. Are they all there? If not, be sure to start and stop your watch at each exercise session.  For reminders about how to use the Fitbit watch, read the attached document called "Using My Fitbit.</p>
                        </td>
                       </tr>
                        <tr>
                         <td align="center" style="padding: 5px 0 35px 0;">
                          ',html_table_generator(last_week_activities_yoga),'
                        </td>
                      </tr>')   
    }
    
  } else if(group == 3 & nrow(last_week_activities_recorded) > 0) {
    
    last_week_activities_endurance <- last_week_activities %>%
      filter(Activity == "Workout")
    
    if(sum(last_week_activities_endurance$duration) < endurance_progression) {
      
      statement <- paste0('<tr>
                          <td align="center" style="padding: 15px 0 5px 0;">
                            <p style="font-size:150%; font-family:Optima; color: #ff3d3d"> Uh oh! You did not quite meet your goal.</p>
                          </td>
                        </tr>
                        <tr>
                            <td align="left" style="padding: 10px 0 5px 0;">
                             <p style= "font-size:110%;"> You acheived <b>',round(sum(last_week_activities_endurance$duration)/endurance_progression*100, digits = 0),'%</b> of your goal (',sum(last_week_activities_endurance$duration),' min out of ',endurance_progression,' min).</p>
                           </td>
                         </tr>
                          <tr>
                        <td align="left" style="padding: 10px 0 5px 0;">
                            <p style= "font-size:100%;">Does this seem accurate? If not...<br><br>
                                                        Was your watch syncing? Your last sync date was ',last_sync_date,'.<br><br>
                                                        Do you see all of your exercise sessions in the table below? </p>
                        </td>
                       </tr>
                      <tr>
                        <td align="left" style="padding: 10px 0 5px 0;">
                            <p style= "font-size:100%;"> <b>Exercise Sessions</b><br> This table shows all of your exercise sessions from last week. Are they all there? If not, be sure to start and stop your watch at each exercise session.  For reminders about how to use the Fitbit watch, read the attached document called "Using My Fitbit.</p>
                        </td>
                       </tr>
                        <tr>
                         <td align="center" style="padding: 5px 0 30px 0;">
                          ',html_table_generator(last_week_activities_endurance),'
                        </td>
                      </tr>')
      
    } else {
      statement <- paste0('
                        <tr>
                          <td align="center" style="padding: 15px 0 5px 0;">
                            <p style="font-size:150%; font-family:Optima; color: RoyalBlue"> Nicely done! You met your goal!</p>
                          </td>
                        </tr>
                          <tr>
                            <td align="left" style="padding: 10px 0 5px 0;">
                             <p style= "font-size:110%;"> You acheived <b>',round(sum(last_week_activities_endurance$duration)/endurance_progression*100, digits = 0),'%</b> of your goal (',sum(last_week_activities_endurance$duration),' min out of ',endurance_progression,' min).</p>
                           </td>
                         </tr>
                      <tr>
                        <td align="left" style="padding: 10px 0 5px 0;">
                            <p style= "font-size:100%;"> <b>Exercise Sessions</b><br> This table shows all of your exercise sessions from last week. Are they all there? If not, be sure to start and stop your watch at each exercise session.  For reminders about how to use the Fitbit watch, read the attached document called "Using My Fitbit.</p>
                        </td>
                       </tr>
                        <tr>
                         <td align="center" style="padding: 5px 0 35px 0;">
                          ',html_table_generator(last_week_activities_endurance),'
                        </td>
                      </tr>')
    }
    
  } else if(group == 4 & nrow(last_week_activities_recorded) > 0) {
    
    last_week_activities_wt <- last_week_activities_recorded %>%
      filter(Activity == "Weights")
    
    last_week_activities_yoga <- last_week_activities_recorded %>%
      filter(Activity == "Yoga" | Activity == "Pilates")
    
    if(nrow(last_week_activities_wt) < 2 | sum(last_week_activities_recorded$duration) < 150 | nrow(last_week_activities_yoga) < 3) {
      
      statement <- paste0('<tr>
                          <td align="center" style="padding: 15px 0 5px 0;">
                            <p style="font-size:150%; font-family:Optima; color: #ff3d3d"> Uh oh! You did not quite meet your goal.</p>
                          </td>
                        </tr>
                        <tr>
                            <td align="left" style="padding: 10px 0 5px 0;">
                             <p style= "font-size:110%;"> You acheived <b>',round(nrow(last_week_activities_wt)/2*100, digits = 0),'%</b> of your weight training goal (',nrow(last_week_activities_wt),' sessions out of ',2,' sessions).<br><br>
                                                          You acheived <b>',round(nrow(last_week_activities_yoga)/3*100, digits = 0),'%</b> of your core and fusion goal (',nrow(last_week_activities_yoga),' sessions out of ',3,' sessions).<br><br>
                                                          You acheived <b>',round(sum(last_week_activities_recorded$duration)/150*100, digits = 0),'%</b> of your duration goal (',sum(last_week_activities_recorded$duration),' min out of ',150,' min).</p>
                           </td>
                         </tr>
                          <tr>
                        <td align="left" style="padding: 10px 0 5px 0;">
                            <p style= "font-size:100%;">Does this seem accurate? If not...<br><br>
                                                        Was your watch syncing? Your last sync date was ',last_sync_date,'.<br><br>
                                                        Do you see all of your exercise sessions in the table below? </p>
                        </td>
                       </tr>
                      <tr>
                        <td align="left" style="padding: 10px 0 5px 0;">
                            <p style= "font-size:100%;"> <b>Exercise Sessions</b><br> This table shows all of your exercise sessions from last week. Are they all there? If not, be sure to start and stop your watch at each exercise session.  For reminders about how to use the Fitbit watch, read the attached document called "Using My Fitbit.</p>
                        </td>
                       </tr>
                        <tr>
                         <td align="center" style="padding: 5px 0 30px 0;">
                          ',html_table_generator(last_week_activities_recorded %>% select(-`Minutes in HR Zone`)),'
                        </td>
                      </tr>')
      
    } else {
      statement <- paste0('     
                        <tr>
                          <td align="center" style="padding: 15px 0 5px 0;">
                            <p style="font-size:150%; font-family:Optima; color: RoyalBlue"> Nicely done! You met your goal!</p>
                          </td>
                        </tr>
                        <tr>
                            <td align="left" style="padding: 10px 0 5px 0;">
                             <p style= "font-size:110%;"> You acheived <b>',round(nrow(last_week_activities_wt)/2*100, digits = 0),'%</b> of your weight training goal (',nrow(last_week_activities_wt),' sessions out of ',2,' sessions).<br><br>
                                                          You acheived <b>',round(nrow(last_week_activities_yoga)/3*100, digits = 0),'%</b> of your core and fusion goal (',nrow(last_week_activities_yoga),' sessions out of ',3,' sessions).<br><br>
                                                          You acheived <b>',round(sum(last_week_activities_recorded$duration)/150*100, digits = 0),'%</b> of your duration goal (',sum(last_week_activities_recorded$duration),' min out of ',150,' min).</p>
                           </td>
                         </tr>
                      <tr>
                        <td align="left" style="padding: 10px 0 5px 0;">
                            <p style= "font-size:100%;"> <b>Exercise Sessions</b><br> This table shows all of your exercise sessions from last week. Are they all there? If not, be sure to start and stop your watch at each exercise session.  For reminders about how to use the Fitbit watch, read the attached document called "Using My Fitbit.</p>
                        </td>
                       </tr>
                        <tr>
                         <td align="center" style="padding: 5px 0 35px 0;">
                          ',html_table_generator(last_week_activities_recorded %>% select(-`Minutes in HR Zone`)),'
                        </td>
                      </tr>')
      
    }
    
  } else if(group == 5 & nrow(last_week_activities_recorded) > 0) {
    
    last_week_activities_endurance <- last_week_activities_recorded %>%
      filter(Activity == "Workout")
    
    last_week_activities_wt <- last_week_activities_recorded %>%
      filter(Activity == "Weights")
    
    if(sum(last_week_activities_endurance$duration) < endurance_progression | nrow(last_week_activities_wt) < 2){
      
      statement <- paste0('<tr>
                          <td align="center" style="padding: 15px 0 5px 0;">
                            <p style="font-size:150%; font-family:Optima; color: #ff3d3d"> Uh oh! You did not quite meet your goal.</p>
                          </td>
                        </tr>
                        <tr>
                            <td align="left" style="padding: 10px 0 5px 0;">
                             <p style= "font-size:110%;"> You acheived <b>',round(nrow(last_week_activities_wt)/2*100, digits = 0),'%</b> of your weight training goal (',nrow(last_week_activities_wt),' sessions out of ',2,' sessions).<br><br>
                                                          You acheived <b>',round(sum(last_week_activities_endurance$duration)/endurance_progression*100, digits = 0),'%</b> of your endurance goal (',sum(last_week_activities_endurance$duration),' min out of ',endurance_progression,' min).</p>
                           </td>
                         </tr>
                          <tr>
                        <td align="left" style="padding: 10px 0 5px 0;">
                            <p style= "font-size:100%;">Does this seem accurate? If not...<br><br>
                                                        Was your watch syncing? Your last sync date was ',last_sync_date,'.<br><br>
                                                        Do you see all of your exercise sessions in the table below? </p>
                        </td>
                       </tr>
                      <tr>
                        <td align="left" style="padding: 10px 0 5px 0;">
                            <p style= "font-size:100%;"> <b>Exercise Sessions</b><br> This table shows all of your exercise sessions from last week. Are they all there? If not, be sure to start and stop your watch at each exercise session.  For reminders about how to use the Fitbit watch, read the attached document called "Using My Fitbit.</p>
                        </td>
                       </tr>
                        <tr>
                         <td align="center" style="padding: 5px 0 30px 0;">
                          ',html_table_generator(last_week_activities_recorded),'
                        </td>
                      </tr>')
    } else {
      
      statement <- paste0('
                        <tr>
                          <td align="center" style="padding: 15px 0 5px 0;">
                            <p style="font-size:150%; font-family:Optima; color: RoyalBlue"> Nicely done! You met your goal!</p>
                          </td>
                        </tr>
                        <tr>
                            <td align="left" style="padding: 10px 0 5px 0;">
                             <p style= "font-size:110%;"> You acheived <b>',round(nrow(last_week_activities_wt)/2*100, digits = 0),'%</b> of your weight training goal (',nrow(last_week_activities_wt),' sessions out of ',2,' sessions).<br><br>
                                                          You acheived <b>',round(sum(last_week_activities_endurance$duration)/endurance_progression*100, digits = 0),'%</b> of your endurance goal (',sum(last_week_activities_endurance$duration),' min out of ',endurance_progression,' min).</p>
                           </td>
                         </tr>
                      <tr>
                        <td align="left" style="padding: 10px 0 5px 0;">
                            <p style= "font-size:100%;"> <b>Exercise Sessions</b><br> This table shows all of your exercise sessions from last week. Are they all there? If not, be sure to start and stop your watch at each exercise session.  For reminders about how to use the Fitbit watch, read the attached document called "Using My Fitbit.</p>
                        </td>
                       </tr>
                        <tr>
                         <td align="center" style="padding: 5px 0 35px 0;">
                          ',html_table_generator(last_week_activities_recorded),'
                        </td>
                      </tr>')
    }
    
  }
  print(statement)
}

#' @name html_table_generator
#' 
#' @title Table Generator
#' 
#' @description This is a generic function for formatting tables using the htmlTable function. 
#' 
#' @details If there are 0 rows int the table it returns "You do not have any exercise sessions from last week."
#' 
#' @param table The table input
#' @return html output of a table
#' 
html_table_generator <- function(table = NULL){
  
  if(nrow(table) == 0) {
    '
        <p style= "font-size:100%;"> <b><i>You do not have any exercise sessions from last week.</b></i>  </p>
'
  } else {
    
  table %>% 
    htmlTable::addHtmlTableStyle(align = "c",
        col.rgroup = c("none", "#F7F7F7"),
        css.cell = "height: 10px; padding: 5 px",
        css.table = "border-collapse: separate; border-spacing: 5px 0") %>% 
    htmlTable::htmlTable(.,rnames = F)
  }
}

#' @name week_2_to_52_email
#'
#' @title Week 2-52 Email
#' 
#' @description 
#' This is a COMET specific function that writes an html to create an email for
#' participant ongoing and enrolled.
#' 
#' @param activities_log dataframe of all activities, all participants
#' @param hr_log data frame of all summarized hr data for all participants
#' @param step_log dataframe of all steps for all participants
#' @param missing_data dataframe of participant's missing data
#' @param fname participant's fist name
#' @param group participants assigned group
#' @param comet_study_id comet_study_id
#' @param week current week of participant
#' @return html statement with appropriately formatted email
#' 
#' @section Development
#' This function calls \code{\link[COMET]{html_table_generator}} and \code{\link[COMET]{feedback_generator}}
#' 6.30.21 Reformatted to be an html5 document. This should improve appearance across browser type.
#' 11.9.21 changed study coordinator info to be generic from comet_nucleus JC \cr
#' 11.15.21 Fixed call to feedback_generator - corrected week to be week before present week JC \cr
#' 11.18.21 Updated endurance_progression and st_progression to be from current_exercise_prescription JC \cr
#' 11.22.21 Updated missing data table to have text if no data missing. JC \cr
#' 2.20.22 Added last_name to pipe first and last name into REDCap exercise log JC \cr
#' 8.22.22 Added "according to your Fitbit" to "How did you do last week" at the suggestion of 067 JC \cr
#' 12.13.22 Made a global update option called email_update. It is located in 07_participant_email for now. JC \cr


week_2_to_52_email <- function(activities_log = NULL, hr_log = NULL, step_log = NULL, missing_data = NULL, fname = NULL, last_name = NULL, comet_study_id = NULL, group = NULL, week = NULL, upcoming_visits = NULL, current_exercise_prescription = NULL) {
  
  endurance_progression <- current_exercise_prescription$aerobic_duration
  
  st_progression <- current_exercise_prescription$cf_duration
  
  goals <- data.frame(endurance = endurance_progression, st = st_progression, wt = "2 sessions")
  
  #1.30.23 Commented out because not used in email. Haven't been for a long time. JC
  # if(group == 2) {
  #   goals <- goals %>%
  #     select(st) %>%
  #     rename("Core and Fusion (min)" = st)
  # } else if(group == 3) {
  #   goals <- goals %>%
  #     select(endurance) %>%
  #     rename("Endurance Exercise (min)" = endurance)
  # } else if(group == 4) {
  #   goals <- goals %>%
  #     select(st, wt) %>%
  #     mutate("Total Exercise Time" = "150 min") %>%
  #     mutate(st = "3 sessions") %>%
  #     rename("Core and Fusion" = st, "Weight Training" = wt)
  # } else if(group == 5) {
  #   goals <- goals %>%
  #     select(endurance, wt) %>%
  #     rename("Endurance Exercise (min)" = endurance, "Weight Training" = wt)
  # }
  
  body <- paste0('<!DOCTYPE html>
                  <html lang="en">
                  <head>
                  <meta charset="utf-8">
                  <title><!-- COMET Weekly Email --></title>
                  </head>
                <table align="center" border="0" cellpadding="0" cellspacing="0" width="600">
                    <tr>
                      <td align="center" style="padding: 40px 0 15px 0;">
                      

<img src="data:image/jpeg;base64,
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xfDLwfeeAPhp4W8A6jqf8AbNz4c0qy02S+8ryDdNaQLCZjFvk2F9u4rvbBPU1+Hn/DjH/qtn/lt/8A3yoA/aT/AIX78Cv+ij+G/wDwcWf/AMdr0DQPEfh7xXpketeFtUtdZ06UsqXNnOlxCxQ7WAkjLKSCMHB4Nfg3/wAOMf8Aqtn/AJbf/wB8q/W39k79nv8A4Ze+Cek/B3+3/wDhJv7Lnu5vt32X7Fv+1TNNjyfNmxt3YzvOcZ46UAfEP/BZ/wD5NQ0T/sbbD/0jva9g/wCCVn/JjngH/rtrH/pzua9Y/bP/AGWf+GvPhRY/DD/hJ/8AhE/serW+qfa/sX2/d5EM8Pl+V58GM+dndvONuMc5HYfsq/AT/hmX4HaB8GP7d/4ST+w3vH+3fZfsfm/a7qW5x5PmzbdvmbfvnOM8ZwAD6HooooAKKKKACiiigD//1P38ooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigD/9X9/KKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooA//W/fyiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKAP/2Q==" />


                   </tr>
                    <tr>
                      <td align="left">
                        Hi ',
                        fname,
                        '! 
                      </td>
                    </tr>',
                 email_update,
                  '<tr>
                    <td align="left">
                      <p style="font-family:arial;" ="font-size:125%;"><b>Study Coordinator Contact Information</b></p>
                      <p style= "font-size:100%;"> ',name,' ',lname,' - ',study_coordinator_email,' - ',study_coordinator_number,'</p>
                    </td>
                   </tr>
                  <tr>
                     <td align="left">
                      <p style="font-family:arial;" style= "font-size:125%;"><b>To-Dos</b></p>
                      <p style= "font-size:100%;"> 1) Accomplish your Exercise Prescription (see attached "Exercise_Prescription.pdf").  </p>
                      <p style= "font-size:100%;"> 2) Enter your exercise from last week and any missing weeks using the Exercise Data Entry Form link below.</p>
                      <p style="font-size:100%;"><b>Exercise Data Entry Form </b>- https://redcap.kumc.edu/surveys/?s=FPHDNHKWL7&comet_study_id=',comet_study_id,'&log_group=',group,'&log_fname=',fname,'&log_lname=',last_name,'</b></p>

                    </td>
                    </tr>
                   <tr>
                     <td align="center">
                      <p style="font-family:arial;" style= "font-size:100%;"><b>Missing Exercise Weeks</b></p>
                    </td>
                    </tr>
                    <tr>
                     <td align="center">
                      ',if(nrow(missing_data) > 0){
                        html_table_generator(missing_data)
                      } else {
                        paste0('Nice! You have no missing data.')
                      },'
                    </td>
                  </tr>
                    <tr>
                     <td align="left">
                      <p style="font-family:sans-serif;" style= "font-size:75%;">*Missing weeks are reflective of ',format(today()-1, format = "%m-%d-%y"),'. If you entered data on ',format(today(), format = "%m-%d-%y"),', expect those weeks to still be shown as missing. </p>
                    </td>
                    </tr>
                  <tr>
                     <td align="left">
                      <p style="font-family:arial;" style= "font-size:125%;"><b>How did you do last week according to your Fitbit?</b></p>
                    </td>
                    </tr>',
                      #statement,
                      feedback_generator(activities_log = activities_log, hr_log = hr_log, step_log = step_log, group = group, id = comet_study_id, week = week-1, current_exercise_prescription = current_exercise_prescription),
                 '
                 <tr>
                  <td align="left">
                    <p style="font-family:arial;" style= "font-size:125%;"><b>Upcoming Testing</b></p>
                   </td>
                 </tr>
                  <tr>
                   <td align="left">
                     <p style= "font-size:100%;">All testing occurs at the KU Clinical Research Center (Third Floor) - 4350 Shawnee Mission Parkway, Fairway, KS 66205. If you have any questions or need to reschedule, please contact the study coordinator listed at the top of the email.<br><br> Testing visits are usually scheduled three months before their approximate date. If you are within three months of your next testing and have not been scheduled yet, please contact the study team.</p>
                   </td>
                 </tr>
                  <tr>
                     <td align="center">',
                 html_table_generator(upcoming_visits),'<br><br><br>
                     </td>
                 </tr>',
                 html_ex_prescription)
  
  cat(body, file = file.path(data_dir,'participant_email','email.txt'))
}

#' @name week_1_email
#' 
#' @title Week 1 Email HTML formal
#' 
#' @description
#' This is a COMET specific function that writes an html to create an email for
#' participants in their first week of the intervention. It has a little extra information
#' about how to use the emails
#' 

#' @param missing_data dataframe of participant's missing data
#' @param fname participant's fist name
#' @param last_name participant's last name
#' @param group participants assigned group
#' @param comet_study_id comet_study_id
#' @param week current week of participant
#' @return html statement with appropriately formatted email
#' 
#' @section Development
#' 8.26.21 committing to github
#' 11.9.21 changed study coordinator info to be generic from comet_nucleus JC \cr
#' 2.20.22 Added last_name to autofill redcap form JC \cr

week_1_email <- function(fname = NULL, last_name = NULL, comet_study_id = NULL, group = NULL, week = NULL, upcoming_visits = NULL, missing_data = NULL) {

  
  body <- paste0('<!DOCTYPE html>
                <table align="center" border="0" cellpadding="0" cellspacing="0" width="600">
                    <tr>
                      <td align="center" style="padding: 40px 0 15px 0;">
<img src="data:image/jpeg;base64,
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xfDLwfeeAPhp4W8A6jqf8AbNz4c0qy02S+8ryDdNaQLCZjFvk2F9u4rvbBPU1+Hn/DjH/qtn/lt/8A3yoA/aT/AIX78Cv+ij+G/wDwcWf/AMdr0DQPEfh7xXpketeFtUtdZ06UsqXNnOlxCxQ7WAkjLKSCMHB4Nfg3/wAOMf8Aqtn/AJbf/wB8q/W39k79nv8A4Ze+Cek/B3+3/wDhJv7Lnu5vt32X7Fv+1TNNjyfNmxt3YzvOcZ46UAfEP/BZ/wD5NQ0T/sbbD/0jva9g/wCCVn/JjngH/rtrH/pzua9Y/bP/AGWf+GvPhRY/DD/hJ/8AhE/serW+qfa/sX2/d5EM8Pl+V58GM+dndvONuMc5HYfsq/AT/hmX4HaB8GP7d/4ST+w3vH+3fZfsfm/a7qW5x5PmzbdvmbfvnOM8ZwAD6HooooAKKKKACiiigD//1P38ooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigD/9X9/KKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooA//W/fyiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKAP/2Q==" />
                     </td>
                    </tr>
                    <tr>
                      <td align="left">
                        Hi ',
                 fname,
                 '! <br><br> Welcome to the COMET study! It looks like this is your first week of exercise. Lets give you a little tutorial about what to expect throughout the study. You will start to receive an email every single week, similar to this one. There is important information in each email so be sure to check them. Here is what is included.  
                      </td>
                    </tr>',
                 email_update,
                  '<tr>
                    <td align="left">
                      <p style= "font-size:100%;"> <i> Note: If you have any questions or concerns, the contact information for your study coordinators will be included at the top of every email. Feel free to call or email if you need anything! </i></p>
                      <p style="font-family:arial;" ="font-size:125%;"><b>Study Coordinator Contact Information</b></p>
                      <p style= "font-size:100%;"> ',name,' ',lname,' - ',study_coordinator_email,' - ',study_coordinator_number,' </p>
                    </td>
                   </tr>
                  <tr>
                     <td align="left">
                      <p style= "font-size:100%;"> <i> Note: Each week you will have tasks to accomplish. These can always be found under the To-Dos. </i></p>
                      <p style="font-family:arial;" style= "font-size:125%;"><b>To-Dos</b></p>
                      <p style= "font-size:100%;"> <i> Note: Every week you will be assigned exercise goals. You can find these goals in a document attached to this email. Be sure to open the document and check your goals. You are welcome to print the document if that helps you.</i>
                      <p style= "font-size:100%;"> 1) Accomplish your Exercise Prescription (see attached "Exercise_Prescription.pdf").  </p>
                      <p style= "font-size:100%;"> <i> Note: We need you to send us your exercise each week. Below is a link to a survey for you to enter your exercise, which gets sent to us automatically. You will complete the survey after you have completed a week. So you will not enter this week until next Monday. To complete the survey, click on the link or paste it into your browser to open it. The survey matches your exercise log, so you can transfer the information from your exercise log into the survey. Fill out the first page, press "Next Page", complete the final page and press "Submit". </i></p>
                      <p style= "font-size:100%;"> 2) Enter your exercise from last week and any missing weeks.</p>
                      <p style="font-size:100%;"><b>Exercise Data Entry Form </b>- https://redcap.kumc.edu/surveys/?s=FPHDNHKWL7&comet_study_id=',comet_study_id,'&log_group=',group,'&log_fname=',fname,'&log_lname=',last_name,'</b></p>

                    </td>
                    </tr>
                   <tr>
                     <td align="center">
                                           <p style= "font-size:100%;"> <i> Note: If you have forgotten to enter a week of exercise, it will show up here. If you see missing weeks, be sure to enter them. </i></p>
                      <p style="font-family:arial;" style= "font-size:100%;"><b>Missing Exercise Weeks</b></p>
                    </td>
                    </tr>
                    <tr>
                     <td align="center">
                      ',html_table_generator(missing_data),'
                    </td>
                  </tr>
                  <tr>
                     <td align="left">
                      <p style= "font-size:100%;"> <i> Note: In future emails you will find feedback about your exercise and Fitbit watch here. </i></p>
                      <p style="font-family:arial;" style= "font-size:125%;"><b>How did you do last week?</b></p>
                      <p style= "font-size:100%;"> Welcome to week 1! This will be feedback in the future. </p>
                    </td>
                    </tr> 
                 <tr>
                  <td align="left">
                     <p style= "font-size:100%;"> <i> Note: Every email will include your upcoming study testing. If you forget when your next testing visit is scheduled, you can always check here. </i></p>
                    <p style="font-family:arial;" style= "font-size:125%;"><b>Upcoming Testing</b></p>
                   </td>
                 </tr>
                  <tr>
                   <td align="left">
                     <p style= "font-size:100%;">All testing occurs at the KU Clinical Research Center (Third Floor) - 4350 Shawnee Mission Parkway, Fairway, KS 66205. If you have any questions or need to reschedule, please contact the study coordinator listed at the top of the email.<br><br> Testing visits are usually scheduled three months before their approximate date. If you are within three months of your next testing and have not been scheduled yet, please contact the study team.</p>
                   </td>
                 </tr>
                  <tr>
                     <td align="center">',
                 html_table_generator(upcoming_visits),'<br><br><br>
                     </td>
                 </tr>',
                 html_ex_prescription)
  
  cat(body, file = file.path(data_dir,'participant_email','week1_email.txt'))
}


#' @name baseline_email
#' 
#' @title Baseline Email
#' 
#' @description 
#' This is a COMET specific function that writes an html to create an email for
#' participants before they consent.
#' 
#' @param fname participant's fist name
#' @param mem_testing memory testing datetime
#' @param exercise_testing exercise testing datetime
#' @param mri mri visit datetime
#' @return html statement with visit dates and appropriately formatted email
#' 
#' @section Development
#' 8.26.21 Committing to GitHub. JC \cr
#' 11.1.21 Changed info to Study's. JC \cr
#' 6.30.22 Added Reschedule date and s to blood draw. JC \cr
#' 8.1.22 Added hoglund map image. JC \cr

baseline_email <- function(fname = NULL, mem_testing = NULL, exercise_testing = NULL, mri = NULL) {

  mem_testing <- format(mem_testing, format = "%B %d, %Y at %I:%M %p")
  exercise_testing <- format(exercise_testing, format = "%B %d, %Y at %I:%M %p")
  mri <- format(mri, format = "%B %d, %Y at %I:%M %p")
  
  hoglund_map <- RCurl::base64Encode(readBin(file.path(data_dir,'participant_email',"hoglund_map.png"), "raw", file.info(file.path(data_dir,'participant_email',"hoglund_map.png"))[1, "size"]), "hoglund_map")
  hoglund_map_html <- sprintf('<img src="data:image/png;base64,%s">', hoglund_map)
  
    body <- paste0('<!DOCTYPE html>
                  <table align="center" border="0" cellpadding="0" cellspacing="0" width="600">
                    <tr>
                      <td align="center" style="padding: 40px 0 15px 0;">
<img src="data:image/jpeg;base64,
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xfDLwfeeAPhp4W8A6jqf8AbNz4c0qy02S+8ryDdNaQLCZjFvk2F9u4rvbBPU1+Hn/DjH/qtn/lt/8A3yoA/aT/AIX78Cv+ij+G/wDwcWf/AMdr0DQPEfh7xXpketeFtUtdZ06UsqXNnOlxCxQ7WAkjLKSCMHB4Nfg3/wAOMf8Aqtn/AJbf/wB8q/W39k79nv8A4Ze+Cek/B3+3/wDhJv7Lnu5vt32X7Fv+1TNNjyfNmxt3YzvOcZ46UAfEP/BZ/wD5NQ0T/sbbD/0jva9g/wCCVn/JjngH/rtrH/pzua9Y/bP/AGWf+GvPhRY/DD/hJ/8AhE/serW+qfa/sX2/d5EM8Pl+V58GM+dndvONuMc5HYfsq/AT/hmX4HaB8GP7d/4ST+w3vH+3fZfsfm/a7qW5x5PmzbdvmbfvnOM8ZwAD6HooooAKKKKACiiigD//1P38ooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigD/9X9/KKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooA//W/fyiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKAP/2Q==" />

                   </td>
                    </tr>
                    <tr>
                      <td align="left" style="padding: 0px 0 0px 0;">
                        Hi ',
                 fname,
                 '! <br><br> Welcome to the COMET study! We appreciate your willingness to participate in research at the Alzheimers Disease Research Center. If you need to revisit the consent form, it is attached. All of your upcoming testing information is included below.
                      </td>
                    </tr>
                  <tr>
                    <td align="left">
                      <p style= "font-size:100%;"> If you have any questions or need to reschedule, please contact <b> Study Coordinator at coordinator_number.</b> </p>
                    </td>
                   </tr>
                   <tr>
                     <td align="left">
                      <p style= "font-size:100%;"> <br><b>Reschedule Notice:</b> We are happy to accommodate rescheduling appointments when necessary. However, we see a high volume of patients in our clinic, and last minute reschedules are challenging. If you need to reschedule your MRI appointment, please do so at least 7 days before your appointment as we are charged for all cancellations within one week. Please be aware that we may have to reschedule ALL visits to ensure they are completed within the study timeline. </p>
                    </td>
                    </tr>
                  <tr>
                     <td align="left">
                      <p style= "font-size:110%;"> <br><b> Memory Testing: ',mem_testing,' </b><br>     Location: 4350 Shawnee Mission Parkway, Fairway, KS 66205 - third floor  </p>
                      <p style= "font-size:100%;"> <b>Instructions for Visit:</b> The visit may take up to 4.5 hours to complete. Be sure you have reviewed the consent form prior to the visit. You will need a list of your medications, glasses, and hearing aids.</p>
                    </td>
                    </tr>
                   <tr>
                     <td align="left">
                      <p style= "font-size:110%;"> <br><b> Exercise Testing: ',exercise_testing,' </b><br>      Location: 4350 Shawnee Mission Parkway, Fairway, KS 66205 - third floor </p>
                      <p style= "font-size:100%;"> <b>Instructions for Visit:</b> Exercise testing may take up to 3 hours to complete. <b> No food, caffeine, or smoking 4 hours prior to test time. No moderate to vigorous exercise 24 hours prior to test time.</b> Be sure to drink water and take all normal medications on the day of your visit. You will complete two blood draws, physical fitness testing, body composition scan, and maximal-exercise treadmill test. Wear or bring comfortable and supportive athletic clothing and shoes. Please leave valuable jewelry at home.	We may setup your Fitbit at the end of this visit. Please bring your smart phone, tablet, or other Bluetooth enabled device that the Fitbit can be synced to. Please come prepared with your login credentials that will enable you to download apps (i.e., Apple ID password, Google Play Store password).</p>
                    </td>
                    </tr>
                   <tr>
                     <td align="left">
                      <p style= "font-size:110%;"> <br><b> MRI Brain Scan: ',mri,' </b><br>      Location: Hoglund Brain Imagng Center, 3805 Eaton St., Kansas City, KS 66103 (913)-588-9070 </p>
                      <p style= "font-size:100%;"> <b>Instructions for Visit:</b> The MRI visit may take up to 1.5 hours to complete. If you need to reschedule this visit, please do so at least 7 days before. We are charged for all visits cancelled within 7 days of the visit. Eat and take all your usual medications before this appointment.	If you have corrected vision, contact lenses are preferred for the scan. If you only wear glasses and have difficulty seeing up-close, please bring a copy of your prescription. Remove all metal from your body such as hair pins, hearing aids, jewelry dentures, etc. If you have had any metal or plastic placed in your body (heart stent, etc) please bring your implant records.<br><br> Go to Hoglund Brain Imaging Center (HBIC) for the MRI brain scan, free parking is available right outside of HBIC. Please use the map provided below to navigate to your visit. 39th Street is closed during business hours and this may impact GPS navigation. Please arrive approximately 30 minutes prior to your appointment time listed above. Masks must be worn inside HBIC. If you need to cancel the day of your appointment, please contact Hoglund directly at (913) 588-9070. </p>
                    </td>
                    </tr>
                    <tr>
                     <td align="left">
                      <p style= "font-size:110%;"> <br><b> Map to Hoglund Brain Imaging Center for MRI</b></p>
                      </td>
                    </tr>
                  </tr>
                    <tr>
                     <td align="left">',
                          hoglund_map_html,'
                      </td>
                    </tr>
                    ')
  
  cat(body, file = file.path(data_dir,'participant_email','baseline_email.txt'))
}

#' @name render_exercise_prescription
#' 
#' @title Exercise Prescription
#' 
#' @param group participant's group
#' 
#' @description 
#' This is a COMET specific function that renders an Rmarkdown document with every
#' individual exercise prescription
#' This pulls from the workspace current and current_exercise_prescription. It is not a closed function, just a way to simplify if statements in 07_participant_email.R

render_exercise_prescription <- function(group = NULL) {
  
  
  if(group == 2) {
    
    rmarkdown::render(file.path(data_dir,'participant_email','02_rmd_pt_st.Rmd'), output_format = "pdf_document", output_file = file.path(data_dir,'participant_email','current_exercise_prescription.pdf'))
    
  } else if(group == 3) {
    
    rmarkdown::render(file.path(data_dir,'participant_email','03_rmd_pt_aerobic.Rmd'), output_format = "pdf_document", output_file = file.path(data_dir,'participant_email','current_exercise_prescription.pdf'))
    
  } else if(group == 4) {

    rmarkdown::render(file.path(data_dir,'participant_email','04_rmd_pt_wt.Rmd'), output_format = "pdf_document", output_file = file.path(data_dir,'participant_email','current_exercise_prescription.pdf'))
    
  } else if(group == 5) {

    rmarkdown::render(file.path(data_dir,'participant_email','05_rmd_pt_combo.Rmd'), output_format = "pdf_document", output_file = file.path(data_dir,'participant_email','current_exercise_prescription.pdf'))
    
  }

}


#' @name week_52_testing_email
#' 
#' @title Week 52 Testing Email
#' 
#' @description 
#' This is a COMET specific function that writes an html to create an email to remind 
#' participants of their upcoming week 52 testing
#' 
#' @param fname participant's fist name
#' @param mem_testing memory testing datetime
#' @param exercise_testing exercise testing datetime
#' @param mri mri visit datetime
#' @return html statement with visit dates and appropriately formatted email
#' 
#' @section Development
#' 12.9.22 Began developing. Copied from baseline_email. JC
#' 12.19.22 Started using in project. However, half integrated nature caused errors for this
#' feature. Participants accidentally received copies of another person's email because the new
#' product_script_functions weren't being used. JC \cr
#' 2.2.23 Added reminder to bring exercise log. JC \cr

week_52_testing_email <- function(fname = NULL, mem_testing = NULL, exercise_testing = NULL, mri = NULL) {
  
  mem_testing <- format(mem_testing, format = "%B %d, %Y at %I:%M %p")
  exercise_testing <- format(exercise_testing, format = "%B %d, %Y at %I:%M %p")
  mri <- format(mri, format = "%B %d, %Y at %I:%M %p")
  
  hoglund_map <- RCurl::base64Encode(readBin(file.path(data_dir,'participant_email',"hoglund_map.png"), "raw", file.info(file.path(data_dir,'participant_email',"hoglund_map.png"))[1, "size"]), "hoglund_map")
  hoglund_map_html <- sprintf('<img src="data:image/png;base64,%s">', hoglund_map)
  
  body <- paste0('<!DOCTYPE html>
                  <table align="center" border="0" cellpadding="0" cellspacing="0" width="600">
                    <tr>
                      <td align="center" style="padding: 40px 0 15px 0;">
<img src="data:image/jpeg;base64,
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xfDLwfeeAPhp4W8A6jqf8AbNz4c0qy02S+8ryDdNaQLCZjFvk2F9u4rvbBPU1+Hn/DjH/qtn/lt/8A3yoA/aT/AIX78Cv+ij+G/wDwcWf/AMdr0DQPEfh7xXpketeFtUtdZ06UsqXNnOlxCxQ7WAkjLKSCMHB4Nfg3/wAOMf8Aqtn/AJbf/wB8q/W39k79nv8A4Ze+Cek/B3+3/wDhJv7Lnu5vt32X7Fv+1TNNjyfNmxt3YzvOcZ46UAfEP/BZ/wD5NQ0T/sbbD/0jva9g/wCCVn/JjngH/rtrH/pzua9Y/bP/AGWf+GvPhRY/DD/hJ/8AhE/serW+qfa/sX2/d5EM8Pl+V58GM+dndvONuMc5HYfsq/AT/hmX4HaB8GP7d/4ST+w3vH+3fZfsfm/a7qW5x5PmzbdvmbfvnOM8ZwAD6HooooAKKKKACiiigD//1P38ooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigD/9X9/KKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooA//W/fyiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKAP/2Q==" />

                   </td>
                    </tr>
                    <tr>
                      <td align="left" style="padding: 0px 0 0px 0;">
                        Hi ',
                 fname,
                 '! <br><br> Wow! It looks like you are almost finished with the study. Here is a reminder for all of your upcoming testing.
                      </td>
                    </tr>
                  <tr>
                    <td align="left">
                      <p style= "font-size:100%;"> If you have any questions or need to reschedule, please contact <b> Study Coordinator at coordinator_number.</b> </p>
                    </td>
                   </tr>
                   <tr>
                     <td align="left">
                      <p style= "font-size:100%;"> <br><b>Reschedule Notice:</b> We are happy to accommodate rescheduling appointments when necessary. However, we see a high volume of patients in our clinic, and last minute reschedules are challenging. If you need to reschedule your MRI appointment, please do so at least 7 days before your appointment as we are charged for all cancellations within one week. Please be aware that we may have to reschedule ALL visits to ensure they are completed within the study timeline. </p>
                    </td>
                    </tr>
                  <tr>
                     <td align="left">
                      <p style= "font-size:110%;"> <br><b> Memory Testing: ',mem_testing,' </b><br>     Location: 4350 Shawnee Mission Parkway, Fairway, KS 66205 - third floor  </p>
                      <p style= "font-size:100%;"> <b>Instructions for Visit:</b> The visit may take up to 3 hours to complete. You will need a list of your medications, glasses, and hearing aids.<b> Please remember to bring your exercise log to this visit. </b></p>
                    </td>
                    </tr>
                   <tr>
                     <td align="left">
                      <p style= "font-size:110%;"> <br><b> Exercise Testing: ',exercise_testing,' </b><br>      Location: 4350 Shawnee Mission Parkway, Fairway, KS 66205 - third floor </p>
                      <p style= "font-size:100%;"> <b>Instructions for Visit:</b> Exercise testing may take up to 3 hours to complete. <b> No food, caffeine, or smoking 4 hours prior to test time. No moderate to vigorous exercise 24 hours prior to test time.</b> Be sure to drink water and take all normal medications on the day of your visit. You will complete two blood draws, physical fitness testing, body composition scan, and maximal-exercise treadmill test. Wear or bring comfortable and supportive athletic clothing and shoes. Please leave valuable jewelry at home.	We may setup your Fitbit at the end of this visit. Please bring your smart phone, tablet, or other Bluetooth enabled device that the Fitbit can be synced to. Please come prepared with your login credentials that will enable you to download apps (i.e., Apple ID password, Google Play Store password). <b> Please remember to bring your exercise log to this visit. </b></p>
                    </td>
                    </tr>
                   <tr>
                     <td align="left">
                      <p style= "font-size:110%;"> <br><b> MRI Brain Scan: ',mri,' </b><br>      Location: Hoglund Brain Imagng Center, 3805 Eaton St., Kansas City, KS 66103 (913)-588-9070 </p>
                      <p style= "font-size:100%;"> <b>Instructions for Visit:</b> The MRI visit may take up to 1.5 hours to complete. If you need to reschedule this visit, please do so at least 7 days before. We are charged for all visits cancelled within 7 days of the visit. Eat and take all your usual medications before this appointment.	If you have corrected vision, contact lenses are preferred for the scan. If you only wear glasses and have difficulty seeing up-close, please bring a copy of your prescription. Remove all metal from your body such as hair pins, hearing aids, jewelry dentures, etc. If you have had any metal or plastic placed in your body (heart stent, etc) please bring your implant records.<br><br> Go to Hoglund Brain Imaging Center (HBIC) for the MRI brain scan, free parking is available right outside of HBIC. Please use the map provided below to navigate to your visit. 39th Street is closed during business hours and this may impact GPS navigation. Please arrive approximately 30 minutes prior to your appointment time listed above. Masks must be worn inside HBIC. If you need to cancel the day of your appointment, please contact Hoglund directly at (913) 588-9070. </p>
                    </td>
                    </tr>
                    <tr>
                     <td align="left">
                      <p style= "font-size:110%;"> <br><b> Map to Hoglund Brain Imaging Center for MRI</b></p>
                      </td>
                    </tr>
                  </tr>
                    <tr>
                     <td align="left">',
                 hoglund_map_html,'
                      </td>
                    </tr>
                    ')
  
  cat(body, file = file.path(data_dir,'participant_email','week_52_testing_email.txt'))
}

#' @name post_week_52_email
#'
#' @title Post Week 52 Email
#' 
#' @description 
#' This is a COMET specific function that writes an html to create an email for
#' participants who are past their last week of intervention
#' 
#' @param missing_data dataframe of participant's missing data
#' @param fname participant's fist name
#' @param group participants assigned group
#' @param comet_study_id comet_study_id
#' @param week current week of participant
#' @param study_closeout_list to do list for participants finishing study
#' @return html statement with appropriately formatted email
#' 
#' @section Development
#' This function calls \code{\link[COMET]{html_table_generator}} and \code{\link[COMET]{feedback_generator}}
#' 12.13.22 Copying from week_2_to_52_email function. Began development. JC \cr
#' 1.3.22 Testing JC \cr



post_week_52_email <- function(missing_data = NULL, fname = NULL, last_name = NULL, comet_study_id = NULL, group = NULL, week = NULL, upcoming_visits = NULL, study_closeout_list = NULL) {

  
  body <- paste0('<!DOCTYPE html>
                  <html lang="en">
                  <head>
                  <meta charset="utf-8">
                  <title><!-- COMET Weekly Email --></title>
                  </head>
                <table align="center" border="0" cellpadding="0" cellspacing="0" width="600">
                    <tr>
                      <td align="center" style="padding: 40px 0 15px 0;">
                      

<img src="data:image/jpeg;base64,
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xfDLwfeeAPhp4W8A6jqf8AbNz4c0qy02S+8ryDdNaQLCZjFvk2F9u4rvbBPU1+Hn/DjH/qtn/lt/8A3yoA/aT/AIX78Cv+ij+G/wDwcWf/AMdr0DQPEfh7xXpketeFtUtdZ06UsqXNnOlxCxQ7WAkjLKSCMHB4Nfg3/wAOMf8Aqtn/AJbf/wB8q/W39k79nv8A4Ze+Cek/B3+3/wDhJv7Lnu5vt32X7Fv+1TNNjyfNmxt3YzvOcZ46UAfEP/BZ/wD5NQ0T/sbbD/0jva9g/wCCVn/JjngH/rtrH/pzua9Y/bP/AGWf+GvPhRY/DD/hJ/8AhE/serW+qfa/sX2/d5EM8Pl+V58GM+dndvONuMc5HYfsq/AT/hmX4HaB8GP7d/4ST+w3vH+3fZfsfm/a7qW5x5PmzbdvmbfvnOM8ZwAD6HooooAKKKKACiiigD//1P38ooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigD/9X9/KKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooA//W/fyiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKACiiigAooooAKKKKAP/2Q==" />


                   </tr>
                    <tr>
                      <td align="left">
                       <p> Hi ',
                 fname,
                 '! </p>',
                 email_update,
                 '
                      <p style= ="font-size:100%;">Wow! Nicely done completing the intervention! This is your post intervention email. It will help us tidy up the last few things.</p>
                      <p style="font-family:arial;" ="font-size:125%;"><b>Study Coordinator Contact Information</b></p>
                      <p style= "font-size:100%;"> ',name,' ',lname,' - ',study_coordinator_email,' - ',study_coordinator_number,'</p>
                    </td>
                   </tr>
                  <tr>
                     <td align="left">
                      <p style="font-family:arial;" style= "font-size:125%;"><b>To-Dos</b></p>
                      <p style= "font-size:100%;"> 1) Enter any missing exercise weeks using the Exercise Data Entry Form link below.</p>
                      <p style= "font-size:100%;"> 2) Complete items in the study closeout list. </p>
                      <p style="font-size:100%;"><b>Exercise Data Entry Form </b>- https://redcap.kumc.edu/surveys/?s=FPHDNHKWL7&comet_study_id=',comet_study_id,'&log_group=',group,'&log_fname=',fname,'&log_lname=',last_name,'</b></p>
                    </td>
                    </tr>
                   <tr>
                     <td align="center">
                      <p style="font-family:arial;" style= "font-size:100%;"><b>Missing Exercise Weeks</b></p>
                    </td>
                    </tr>
                    <tr>
                     <td align="center">
                      ',if(nrow(missing_data) > 0){
                        html_table_generator(missing_data)
                      } else {
                        paste0('Nice! You have no missing data.')
                      },'
                    </td>
                  </tr>
                    <tr>
                     <td align="left">
                      <p style="font-family:sans-serif;" style= "font-size:75%;">*Missing weeks are reflective of ',format(today()-1, format = "%m-%d-%y"),'. If you entered data on ',format(today(), format = "%m-%d-%y"),', expect those weeks to still be shown as missing. </p>
                    </td>
                    </tr>
                  <tr>
                     <td align="left">
                      <p style="font-family:arial;" style= "font-size:125%;"><b>Study Closeout List</b></p>
                      <p style="font-family:arial;" style= "font-size:100%;"><ol>',study_closeout_list,'</ol></p>
                      </td>
                    </tr>
                 <tr>
                  <td align="left">
                    <p style="font-family:arial;" style= "font-size:125%;"><b>Upcoming Testing</b></p>
                   </td>
                 </tr>
                  <tr>
                   <td align="left">
                     <p style= "font-size:100%;">All testing occurs at the KU Clinical Research Center (Third Floor) - 4350 Shawnee Mission Parkway, Fairway, KS 66205. If you have any questions or need to reschedule, please contact the study coordinator listed at the top of the email.<br><br> Testing visits are usually scheduled three months before their approximate date. If you are within three months of your next testing and have not been scheduled yet, please contact the study team.</p>
                   </td>
                 </tr>
                  <tr>
                     <td align="center">',
                 html_table_generator(upcoming_visits),'<br><br><br>
                     </td>
                 </tr>')
  
  cat(body, file = file.path(data_dir,'participant_email','post52_email.txt'))
}

