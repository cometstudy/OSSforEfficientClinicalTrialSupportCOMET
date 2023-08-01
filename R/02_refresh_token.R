#' @name refresh_token
#' 
#' @title Refresh API Token
#' 
#' @description 
#' User fitbit codes, authorization tokens, access tokens, and refresh tokens are stored
#' in a .csv file under the /data/read_write/read_write folder. This script accesses that 
#' file and refreshes all access tokens. 
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas
#' 
#' @author Jon Clutton
#' 
#' @section Development
#' 8.2.22: Script has worked reliably until today. It was one of the earliest COMET scripts. 
#'  Invalid tokens causing errors in 03_get_steps. I added an email module to notify when tokens are not being refreshed. JC \cr
#' 9.14.22: Added filter line to fitbit_users to keep script from trying to refresh non-authorized Fitbits. JC \cr

message("Beginning 02 refresh token")



##### App info ####
client_info <- import(file.path(project_dir,'data','read_write','read_write','client_info.csv')) 

#### User info ####
fitbit_users <- import(file.path(project_dir,'data','read_write','read_write','fitbit_users.csv')) %>%
  filter(user != "")


############# Refresh Tokens #####
for(i in 1:nrow(fitbit_users))
{
  
  
  response <- POST("https://api.fitbit.com/oauth2/token",
                   encode = "form",
                   body = list(grant_type = "refresh_token",
                               refresh_token = fitbit_users$refresh_token[i]
                   ),
                   add_headers(Authorization =
                                 paste("Basic",
                                       RCurl::base64(paste0(client_info$client_id,":", client_info$client_secret)))
                   ),
                   content_type("application/x-www-form-urlencoded")
  )
  
  
  
  refresh_data <- content(response)
  
  if(!("access_token" %in% names(refresh_data))) {
    warning(paste("Unable to refresh token for user", fitbit_users$id[i]))
    warning(refresh_data$errors[[1]]$message)
    
    text <- paste0('Unable to refresh token for user ', fitbit_users$id[i],' ',refresh_data$errors[[1]]$message)
    cat(text, file = file.path(project_dir,'data','modules','generic_email_module','email.txt'))
    mailcmd <- paste(python_cmd_prefix, file.path(project_dir,'data','modules','generic_email_module','send_email_w_subject.py'), "study_coordinator_2@kumc.edu", file.path(project_dir,'data','modules','generic_email_module','email.txt'), paste0("Error_w_token_",fitbit_users$id[i]))
    if(testing == 0){
      system(mailcmd)
    }
    
    return(NULL)
  } else {
    fitbit_users$access_token[i] <- refresh_data$access_token
    fitbit_users$refresh_token[i] <- refresh_data$refresh_token
  }
  
}
### Write new access and refresh tokens to log
export(fitbit_users, file.path(project_dir,'data','read_write','read_write','fitbit_users.csv'))
