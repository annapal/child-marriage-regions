
# Function that creates one dataset for each country
# (merges surveys conducted in the same country)
# Also adds region matches to the dataset (Reg, Adm1, Adm2)

merge_data <- function(dhs_data, mics_data_imp) {
  
  # Get list of countries that have data
  country_cdes <- read_excel("data/ref_data/country-codes.xlsx")
  
  # Create list to store merged dataframes
  merged_data <- list()
  
  # Merge all datasets
  for (i in 1:nrow(country_cdes)) {
  
    dhs_cd <- country_cdes$dhs_cd[i] # DHS country code
    iso3 <- country_cdes$iso3[i] # 3 letter iso code
    
    # Get surveys for a particular country
    tot_subs <- get_country_surveys(dhs_cd, iso3, dhs_data, mics_data_imp)
    
    # Create list to store survey data from particular country
    dat_list <- list()
    
    # Add each dataset to the list
    for (j in 1:length(tot_subs)) {
    
      data <- tot_subs[[j]] # Get dataset
      
      # Read in matches spreadsheet
      merge_dat <- suppressMessages(read_excel(paste0("data/wbs/matches_", iso3, ".xlsx"),
                              sheet = data$surveyid[1]))
      
      # Match region names
      if (merge_dat$exclude[1]==TRUE) { 
        # When survey excluded, set all region variables to missing
        data$Reg <- NA
        data$Adm1 <- NA
        data$Adm2 <- NA
      } else if (merge_dat$geocodes[1]==TRUE) { 
        # When geocodes are available, use them to set Adm1 & Adm2
        data <- get_reg_from_coord(data, iso3)
      } else if (merge_dat$geocodes[1]==FALSE) {
        # When reported survey regions are available, use them to set Reg, Adm1 & Adm2
        data <- get_reg_from_data(data, merge_dat)
      }
      
      # Match other variables
      data <- match_other_variables(data, merge_dat)
      
      # Add denormalised weights and unique PSU/Strata variables
      pop_data <- read.csv("ref_data/pop_year.csv") # Data on population sizes
      data <- denorm_weights(data, pop_data, iso=iso3)
      
      # Add data to data list
      dat_list[[j]] <- data
    }
    
    # Merge all datasets for a particular country
    all_data <- suppressMessages(Reduce(full_join, dat_list))
    
    # Add data to list
    merged_data[[iso3]] <- all_data
  
    # Save a copy of the merged dataset
    saveRDS(all_data, file=paste0("data/merged/country_data/", iso3, ".rds"))
  }
  
  # Return list of merged data
  merged_data
}