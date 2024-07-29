
# Get list of surveys for a particular country

get_country_surveys <- function(dhs_cd, iso3, dhs_data, mics_data_imp) {
  
  # Get survey IDS
  dhs_ids <- names(dhs_data) # Get DHS survey ids
  mics_ids <- names(mics_data_imp) # Get MICS survey ids
  
  # Get list of DHS surveys for particular country
  if (is.na(dhs_cd)) {
    dhs_subs <- list()
  } else {
    dhs_subs <- dhs_data[grepl(dhs_cd, dhs_ids)]
  }
  
  # Get list of MICS surveys for a particular country
  mics_subs <- mics_data_imp[grepl(iso3, mics_ids)]
  
  # Merge lists together
  tot_subs <- c(dhs_subs, mics_subs) 
  
  # Return the list
  tot_subs
}
