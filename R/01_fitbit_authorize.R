#' @name fitbit_authorize
#' 
#' @title Authorize fitbit API access
#' 
#' @descrription
#' This script authorizes fitbit api access for all fitbits in the COMET exercise study. 
#' There will be 280 participants in the study. This script allows the study coordinator to batch
#' authorize as many accounts as they would like to. 
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas
#' 
#' @author Jon Clutton
#' 
#' @section Development:
#' 4.4.2022 Authorized 0-60 IDs. JC \cr
#' 6.3.2022 Authorized 61-99 IDs. Also added filter under unauthorized users to cap at a certain number so that I do not need to 
#' authorize every fitbit before I roxygenize the COMET project. JC \cr
#' 


#### App info
client_info <- import(file.path(project_dir,'data','read_write','read_write','client_info.csv')) 

### User info
fitbit_users <- read.csv(file.path(data_dir,'read_write','read_write','fitbit_users.csv'), na.strings = c("", " "))

#Users that haven't been authorized yet
unauthorized_users <- fitbit_users %>%
  filter(is.na(fitbit_users$access_token == T & is.na(fitbit_users$email) == F))
  
##This loop 1) opens fitbit to log into the appropriate account, 2) obtains the user, 3) obtains the auth_code, and 4) writes the user, auth_code, access token, and refresh token to fitbit_users
if(nrow(unauthorized_users>0)) {
    for(i in 1:nrow(unauthorized_users)) {
      
      BROWSE('https://accounts.fitbit.com/login?targetUrl=https%3A%2F%2Fwww.fitbit.com%2Fglobal%2Fus%2Fhome')
      
      print(paste0("Please enter the Fitbit User ID in the following readlne - ID ",unauthorized_users$id[i]))
      line <- readline()
      
      unauthorized_users$user[i] <- line
      
      ##Authorization Page
      auth_url <- paste0('https://www.fitbit.com/oauth2/authorize?response_type=code&client_id=',
                       client_info$client_id,
                       '&redirect_uri=https%3A%2F%2Flocalhost&scope=activity%20heartrate%20profile%20settings%20sleep')
      
      BROWSE(auth_url)
      
      print("Please enter the Fitbit auth_code in the following readlne")
      authcode <- readline()
      
      unauthorized_users$auth_code[i] <- authcode
      
      
      res <- POST(url = 'https://api.fitbit.com/oauth2/token',
                  authenticate(client_info$client_id, client_info$client_secret, type = "basic"),
                  body = list(code = unauthorized_users$auth_code[i], grant_type = "authorization_code",  redirect_uri = "https://localhost"),
                  encode = "form")
      res2 <- content(res,"parsed")
      
      unauthorized_users$access_token[i] <- res2$access_token
      unauthorized_users$refresh_token[i] <- res2$refresh_token
      
      rm(res, res2)
      
      print("Congratulations, you authorized that Fitbit. Logout and login to the next fitbit.")
      
    }


##### Write to fitbit_users.csv
  fitbit_users2 <- fitbit_users %>%
    filter(!is.na(fitbit_users$access_token)) %>%
    mutate(access_token = as.character(access_token), refresh_token = as.character(refresh_token), auth_code = as.character(auth_code))
  
  #Added temp lines on 9.12.22 so that I don't have to complete document the project and I don't have to initialize all new fitbits
  temp <- bind_rows(fitbit_users2,unauthorized_users)
  
  temp_2 <- anti_join(fitbit_users, temp, by = "id")
  
  fitbit_users <- full_join(temp, temp_2)

  export(fitbit_users, file.path(data_dir,'read_write','read_write','fitbit_users.csv'))

}  



