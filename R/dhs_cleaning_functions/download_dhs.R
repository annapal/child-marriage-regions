
# Download all DHS Individual Recode data

download_dhs <- function() {
  
  # Get datasets
  datasets <- dhs_datasets(fileFormat = "stata",
                           surveyType = "DHS",
                           fileType = "IR")
  
  # Datasets that won't be downloaded
  dat_remove <- c(
    # Datasets that will be downloaded separately
    "ET2019DHS", "GA2000DHS", "IA1993DHS", "IA2015DHS",
    "IA2020DHS", "PE2004DHS", "RW2008DHS", "TR1998DHS",
    "ZW2015DHS", "BF2021DHS",
    # Datasets that are restricted and won't be included
    "BT1988DHS", "ER1995DHS", "ER2002DHS", "ET2019DHS",
    "IA1993DHS", "MR2000DHS", "PE2007DHS", "RW2008DHS",
    "TM2000DHS", "YE1997DHS")
  
  # Remove datasets that won't be downloaded
  datasets <- datasets %>%
    filter(!(SurveyId %in% dat_remove)) %>%
    filter(SurveyId!="IA1999DHS" |  # There are multiple datasets for India 1999
             (SurveyId=="IA1999DHS" & FileName=="IAIR42DT.ZIP"))
  
  # Download and save the datasets
  for (i in 1:nrow(datasets)) {
    file_name <- datasets$FileName[i] # get file name
    data <- readRDS(get_datasets(file_name)[[1]]) # Read in the data
    saveRDS(data, file=paste0("data/dhs/raw_data/", datasets$SurveyId[i], ".rds"))
  }
}

# Download all DHS Geocodes where available
  
download_dhs_geo <- function() {
  
  # Get datasets
  datasets <- dhs_datasets(surveyType = "DHS",
                           fileType = "GE")
  
  # Datasets that won't be downloaded
  dat_remove <- c(
    # Datasets that will be downloaded separately
    "ET2019DHS", "NP2022DHS", "IA2015DHS", "IA2020DHS",
    "PE2004DHS", "RW2008DHS", "ZW2015DHS", "BF2021DHS",
    # Datasets that are restricted and won't be included
    "PE2007DHS", "RW2008DHS")
  
  # Remove datasets that won't be downloaded
  datasets <- datasets %>%
    filter(!(SurveyId %in% dat_remove))
  
  # Download and save the rest of the datasets
  for (i in 1:nrow(datasets)) {
    file_name <- datasets$FileName[i] # get file name
    geo_dat <- readRDS(get_datasets(file_name)[[1]]) # Read in the data
    saveRDS(geo_dat, file=paste0("data/dhs/geo_data/", datasets$SurveyId[i], ".rds"))
  }
}
