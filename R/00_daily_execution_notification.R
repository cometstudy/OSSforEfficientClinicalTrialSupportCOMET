#' @author  20210811 by EDV
#' 
#' @description 
#' This sends an email to Eric and Jon telling them if the Cron jobs and other activities have executed correctly
#' 
#' @section Copyright: 
#' Copyright © 2021 University of Kansas
#' 
#' @section Development:
#' 2.23.22: Jon modified to included all recipients on one email. JC \cr


TO <- 'user2@kumc.edu'

## status index for printing messages for debugging
messages_flag <- 0


if(messages_flag == 1){
  cat("Sending completed message to Eric\n")
  env_print(current_env())
  message(paste("Execution state detected:", execution))
}


  
  if(messages_flag == 1){message(print(execution))}
  
  if(execution == 1){
    if(messages_flag ==1){message(paste("Entering Successful if condition:", execution))}
    body_plain <- paste0('\"','\"','\"','The COMET script seems to have SUCCESSFULLY executed.', '\"','\"','\"')

  } else {
    if(messages_flag ==1){message(paste("Entering Unsuccessful if condition:", execution))}
    body_plain <- paste0('\"','\"','\"', 'The COMET script: ', next_up, ' seems to have generated an ERROR.','\"','\"','\"')
  }
  
  mailcmd<-paste(python_cmd_prefix, file.path(script_dir,'send_daily_executed_email.py'),TO, body_plain)
  
  cat(mailcmd)
  
  if(.Platform$OS.type=="unix"){
    system(mailcmd)
  } else{
    system2(mailcmd)
  }
  cat(paste0("Completed message sent to ", TO, "\n"))

