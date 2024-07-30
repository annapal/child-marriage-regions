
# Function that cleans all DHS surveys
# `surveys` is a vector with survey ids or can be specified as "all"
# If surveys=="all", then all available DHS surveys are cleaned
# If download=TRUE, then DHS surveys are downloaded using package rdhs
# config is the object created from set_rdhs_config() needed to access the DHS data 

clean_dhs <- function(surveys="all", download=TRUE, config=NA) {
  
  if (length(surveys)==1 & surveys=="all") {
    # Get all DHS survey ids that are to be included
    dhs_surveys <- read_excel("data/ref_data/dhs_surveys.xlsx") %>%
      filter(Include=="Yes")
    surveys <- dhs_surveys$SurveyId
  }
  
  # Spreadsheet listing variable names for clean dataset
  survey_var <- read_excel("data/ref_data/analysis-variables.xlsx")
  
  # Create directories to save DHS data
  dir.create("data/dhs/raw_data", showWarnings = FALSE)
  dir.create("data/dhs/geo_data", showWarnings = FALSE)
    
  # Download the data using rdhs
  if (download==TRUE) {
    download_dhs() # Download surveys
    download_dhs_geo() # Download geocodes
  }
  
  data_list <- list() # empty list to store the data frames
  
  # progress bar
  pb = txtProgressBar(min = 0, max = length(surveys), initial = 0, style = 3)
  
  for (i in 1:length(surveys)) {
    survey_id <- surveys[i] # set survey id
    
    # Read in the data
    data <- read_dhs(survey_id) # read in dhs data
    geodata <- read_dhs_geo(survey_id) # read in geocodes
    
    # Create clean dataset
    cleandat <- clean_dhs_data(survey_id, data, geodata, survey_var)
    data_list[[surveys[i]]] <- cleandat # save clean data to list
    
    setTxtProgressBar(pb,i) # update progress bar
  }
  close(pb) # close progress bar
  
  data_list # Return data list
}
