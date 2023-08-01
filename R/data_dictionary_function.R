data_dictionary_function <- function(data, df_name) {
  
  temp <- data.frame(summary.default(data)) %>%
    pivot_wider(, names_from = Var2, values_from = Freq) %>%
    mutate(data_frame_name = df_name) %>%
    mutate(possible_values = NA)
  
  for(i in 1:ncol(data)) {
    current <- data[i]
    
    name <- colnames(current)
    
    unique_temp_df <- current %>%
      unique() %>%
      arrange(.)
    
    string <- paste(current %>% unique() , collapse = " ")
    
    summary_string <- paste(summary(current), collapse = " ")
    
    if(temp$Mode[which(temp$Var1 == name)] == "character") {
      temp$possible_values[which(temp$Var1 == name)] <- string
    } else if (temp$Mode[which(temp$Var1 == name)] == "numeric") {
      temp$possible_values[which(temp$Var1 == name)] <- summary_string
    } else if (temp$Mode[which(temp$Var1 == name)] == "logical") {
      temp$possible_values[which(temp$Var1 == name)] <- string
    } else {
      temp$possible_values[which(temp$Var1 == name)] <- "Unknown Error"
    }
    
  }
  
  temp_return <- temp %>%
    select(-Length) %>%
    rename(variable = Var1)
  
  rio::export(temp_return, file = file.path(data_dir,'data_dictionary',paste0(df_name,'.csv')))
  
  return(temp_return)
  
  
}

