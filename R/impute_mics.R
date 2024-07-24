
# Function that imputes missing date information in MICS surveys
# `data` is a MICS dataframe that has ungone initial cleaning

impute_mics <- function(data) {

  # Perform some initial data edits
  data <- data_edits(data)
  
  # Calculate plausible ranges for date of birth and first marriage
  # `flag` shows the number of values removed at each step
  # If any observations are removed, the process restarts
  data_flg <- list(data=data, flag=1)
  while(data_flg$flag>0) {
    data_flg <- logical_ranges_birth(data_flg$data)
    if (data_flg$flag>0) next
    data_flg <- isolated_constraints_birth(data_flg$data)
    if (data_flg$flag>0) next
    data_flg <- logical_ranges_marriage(data_flg$data)
    if (data_flg$flag>0) next
    data_flg <- isolated_constraints_marriage(data_flg$data)
    if (data_flg$flag>0) next
    data_flg <- ancillary_constraints1(data_flg$data)
    if (data_flg$flag>0) next
    data_flg <- ancillary_constraints2(data_flg$data)
    if (data_flg$flag>0) next
    data_flg <- min_interval_forward(data_flg$data)
    if (data_flg$flag>0) next
    data_flg <- min_interval_backward(data_flg$data)
    if (data_flg$flag>0) next
    break
  }
  
  # Increase gap between birth and marriage
  data <- gap_increasing(data_flg$data)
  
  # Create vectors of possible values
  vect <- create_vectors(data)
  
  # Add imputation codes
  data <- imputation_codes(data)
  
  # Impute dates
  data <- impute_birth(data, vect$birth_vect)
  data <- impute_marriage(data, vect$mar_vect)
  
  # Run date conversion code if applicable
  survey_id <- data$surveyid[1]
  if (survey_id%in%c("AFG_MICS6")) {
    data <- convert_AFG_dates(data)
  } else if (survey_id%in%c("THA_MICS6", "THA_MICS6_2", "THA_MICS5", "THA_MICS4", "THA_MICS3")) {
    data <- convert_THA_dates(data)
  } else if (survey_id%in%c("NPL_MICS6", "NPL_MICS5")) {
    data <- convert_NPL_dates(data)
  }
  
  # Select only the necessary columns
  data_final <- data %>%
    select(iso3, survey, phase, surveyid, samp, clust, hh, line, wt, int_m, int_y, psu, strata, 
           region, prov, dist, res, birth_m, birth_y, birth_imp, age, relig, ethn,
           al_lived, yrs_lived, prev_reg, mar_status, mar_m, mar_y, mar_imp, mar_age)
  
  # Add lat and lon variable (leave empty for now) & case id
  data_final <- data_final %>%
    mutate(lat = NA, long = NA, 
           case_id = paste(clust, hh, line, sep = "_"))
  
  # Return the imputed dataframe
  data_final
}
