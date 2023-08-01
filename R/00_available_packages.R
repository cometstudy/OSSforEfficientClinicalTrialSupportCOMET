#' @section Copyright: 
#' Copyright © 2021 University of Kansas

library(rio)
library(tidyverse)

if(Sys.info()["nodename"]=="workstation_2" & Sys.info()["user"]=="user2"){  #This is Eric's work desktop Mac
  root_p <- file.path("Root_Path")
  root_server_dir <- file.path(root_p,"study_dir_a")
  python_cmd_prefix <- "python "
} else if(Sys.info()["nodename"]=="workstation_1" & Sys.info()["user"]=="study_coordinator_2") {
  root_p <- file.path("drive")
  root_server_dir <- file.path(root_p,"study_dir_a")
  python_cmd_prefix <- "py "
  eric_at_home <<- 1
} else if(Sys.info()["nodename"]=="workstation_3" & Sys.info()["user"]=="root" & Sys.info()["login"]=="study_coordinator_2"){
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


project_dir <- file.path(root_server_dir,'study_dir_a','COMET') #project level directory
script_dir <- file.path(project_dir,'R')
data_dir <- file.path(project_dir,'data')


#Identify the ultimate location for copies of the files we can use to read data
cog_destination <- file.path(data_dir,'raw','cog_data')
dxa_destination <- file.path(data_dir,'raw','dxa_data')
clean_data_destination <- file.path(data_dir,'clean')


my_packages <- as.data.frame(installed.packages()[ , c(1, 3:4)]) 

print(my_packages)

export(my_packages, file.path(project_dir,'available_packages.csv'))
