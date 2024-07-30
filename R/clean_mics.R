
# Function that cleans all MICS surveys
# `surveys` is a vector with survey ids or can be specified as "all"
# If surveys="all", then all available MICS surveys are cleaned

clean_mics <- function(surveys="all") {
  
  # Get the entire list of surveys when surveys="all"
  if (length(surveys)==1 & surveys=="all") {
    files <- list.files("./R/mics_cleaning_functions/")
    surveys <- str_sub(str_sub(files, 7), 1, -3)
  }

  # Spreadsheet listing variable names for clean dataset
  survey_var <- read_excel("data/ref_data/analysis-variables.xlsx")
  
  # Create directory
  dir.create("data/mics/data_dictionaries", showWarnings = FALSE)
  
  # Empty list to store the data frames
  data_list <- list() 
  
  # progress bar
  pb = txtProgressBar(min = 0, max = length(surveys), initial = 0, style = 3) 
  
  for (i in 1:length(surveys)) {
    # Get name of function for cleaning specific survey
    function_name <- paste("clean_", surveys[i], sep = "")
    
    # Run the function if it exists and save to data list
    if (exists(function_name)) {
      data_list[[surveys[i]]] <- do.call(function_name, list(survey_var))
    }
    setTxtProgressBar(pb,i) # update progress bar
  }
  
  close(pb) # close the progress bar
  
  # Return the list of dataframes
  data_list
}


