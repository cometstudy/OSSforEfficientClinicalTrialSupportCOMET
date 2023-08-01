#' @name create_new_data_dictionary
#' 
#' @title Create New Data Dictionary
#' 
#' @description 
#' This script is designed to assist the data_quality_email module. 
#' All project-wide output dataframes saved in the data/clean .Rdata files are converted into 
#' data dictionaries. The purpose is to be able to search for missing data. 
#' 
#' @section Development:
#' 1.23.23 Began development JC \cr
#' 1.23.23 Created new data dictionaries JC \cr
#' 
#' @author Jon clutton
message("Begin Creating New Data Dictionaries")

#### Source Scripts ####
source(file.path(data_dir,'data_quality_email','00_data_dictionary_function.R'))

project_wide_dfs <- c('steps_all_df','hr_all_df', 'activities_all_df','adverse_event','comet',  #06_read_clean
                      'exercise_log','exercise_prescriptions','note_to_file','protocol_deviation', #06_read_clean
                      'fidelity_check','covid_screen','phone_screening', #06_read_clean
                      'full_cog', #05_cog_aggregation
                      'most_recent_dxa_set', #05_dxa_aggregation
                      'vo2_data_final','vo2_summary_measures', #05_vo2_aggregation
                      'mprage_files', #05_mri_transfer
                      'performance_based_adherence','time_based_adherence', 'performance_based_adherence_temp', 'time_based_adherence_temp', #06_exercise_adherence
                      'clean_all_ex_data','ex_log_issues' #06_exercise_data_consolidation
                      )


################# Get Steps ############
for(i in 1:length(project_wide_dfs)){
 

  data_dictionary_function(data = get(project_wide_dfs[i]), df_name = paste0(project_wide_dfs[i],"_dd"))
  
}


