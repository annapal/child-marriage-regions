
# Create a DHS dataset for analysis

clean_dhs_data <- function(survey_id, data, geodata, survey_var) {
  
  # Survey wave
  survey_wave <- ifelse(nchar(data$v000[1])==3, substr(data$v000[1], 3, 3), "0")
  if (survey_id=="VN1997DHS") survey_wave <- "3" # Fix Vietnam 1997 survey
  if (survey_id=="VN2002DHS") survey_wave <- "4" # Fix Vietnam 2002 survey
  
  # Country codes
  dhs_cde <- str_sub(survey_id, 1, 2)
  iso <- countrycode(dhs_cde, "dhs", "iso3c")
  
  # Create new data frame for clean data
  variables <- survey_var$Name[1:31]
  data_clean <- as.data.frame(matrix(nrow=nrow(data), ncol=length(variables)))
  colnames(data_clean) <- variables
  
  # Add in meta data
  data_clean <- data_clean %>%
    mutate(
      iso3 = iso,
      survey = "DHS",
      phase = as.numeric(survey_wave),
      surveyid = survey_id
    )
  
  # Survey variables
  data_clean <- data_clean %>%
    mutate(
      clust = as.numeric(data$v001), # Cluster number
      hh = as.numeric(data$v002), # Household number
      line = as.numeric(data$v003), # Line number
      case_id = paste0(clust, "_", hh, "_", line), # Case ID
      wt = as.numeric(data$v005)/1000000, # Survey weight
      int_m = as.numeric(data$v006), # Interview month
      int_y = fix_year(as.numeric(data$v007), iso), # Interview year
      region = tolower(as.character(as_factor(data$v101))), # Region
      res = tolower(as.character(as_factor(data$v102))) # Rural/Urban
    )
  
  # Add psu and strata for all surveys (except DHS0)
  if (survey_wave!="0") {
    data_clean$psu <- as.numeric(data$v021) # Primary sampling unit
    data_clean$strata <- as.numeric(data$v022) # Strata
  } else {
    data_clean$psu <- NA
    data_clean$strata <- NA
  }
  
  # If PSU is missing, use the cluster number
  if (sum(is.na(data_clean$psu))>0) {
    data_clean$psu <- data_clean$clust
  }
  
  # If strata is missing, use region/res combination to assign strata
  if (sum(is.na(data_clean$strata))>0) {
    data_clean$strata <- as.numeric(factor(paste(data_clean$region, data_clean$res)))
  }
    
  # If PSU and strata numbers are really high, re-label them to make them smaller
  if ((max(data_clean$psu)>=10000)|(max(data_clean$strata)>=10000)) {
    data_clean$psu <- as.numeric(as.factor(data_clean$psu))
    data_clean$strata <- as.numeric(as.factor(data_clean$strata))
  }
  
  # Individual information
  data_clean <- data_clean %>%
    mutate(
      birth_m = as.numeric(data$v009), # Birth month
      birth_y = as.numeric(fix_year(data$v010, iso)), # Birth year
      birth_imp = fix_imp_cde(as.numeric(data$v014), survey_wave), # Birth imputation
      age = as.numeric(data$v012), # Age at interview
      relig = tolower(as.character(as_factor(data$v130))), # Religion
      ethn = tolower(as.character(as_factor(data$v131))) # Ethnicity
    )
  
  # Extract province and district information if available
  if ("sprov" %in% colnames(data)) {
    data_clean$prov <- tolower(as.character(as_factor(data$sprov)))
  }
  if ("sprovin" %in% colnames(data)) {
    data_clean$prov <- tolower(as.character(as_factor(data$sprovin)))
  }
  if ("sdist" %in% colnames(data)) {
    data_clean$dist <- tolower(as.character(as_factor(data$sdist)))
  }
  
  # Always lived in same city/village
  data_clean$al_lived <- ifelse(is.na(data$v104)|(data$v104%in%c(99,98,97,96)), NA,
                                ifelse(data$v104==95, 1, 0))
  
  # Number of years lived in city/village
  data_clean$yrs_lived <- ifelse(is.na(data$v104)|(data$v104%in% c(99,98,97,96,95)), NA,
                                 as.numeric(data$v104))
  
  # Region of previous residence
  if ("v105a"%in%colnames(data)) {
    data_clean$prev_reg <- tolower(as.character(as_factor(data$v105a)))
  }
  
  # Current marriage status
  data_clean$mar_status <- ifelse(data$v502==1, "currently", 
                                  ifelse(data$v502==2, "formerly", 
                                         ifelse(data$v502==0, "never", NA)))
  
  # Ever-married sample indicator
  data_clean$samp <- ifelse(sum(data_clean$mar_status=="never")==0, "Ever Married Women",
                            "All Women")
  
  # Marriage month, year, and age
  data_clean <- data_clean %>%
    mutate(
      mar_m = as.numeric(data$v507), # month of marriage
      mar_y = fix_year(as.numeric(data$v508), iso), # year of marriage
      mar_imp = fix_imp_cde(as.numeric(data$v510), survey_wave), # marriage imputation
      mar_age = as.numeric(data$v511) # marriage age
    )
  
  # Fix marriage status where marriage date is given
  data_clean <- data_clean %>%
    mutate(
      mar_status = ifelse(is.na(mar_status) & !is.na(mar_y), "currently", mar_status),
      mar_status = as.factor(mar_status)
    )
  
  # Add geocodes if available
  if (!is.null(geodata)) {
    data_clean <- merge(data_clean, geodata, by="clust", all.x=TRUE, sort=FALSE)
  } else {
    data_clean$lat <- NA
    data_clean$long <- NA
  }
  
  # Convert dates to greogrian calendar (if necessary)
  
  # Nepalese calendar
  if (iso=="NPL") {
    data_clean[, c("int_m", "int_y")] <- convert_npl(data_clean$int_m, data_clean$int_y)
    data_clean[, c("birth_m", "birth_y")] <- convert_npl(data_clean$birth_m, data_clean$birth_y)
    data_clean[, c("mar_m", "mar_y")] <- convert_npl(data_clean$mar_m, data_clean$mar_y)
  }
  
  # Ethiopian calendar
  if (iso=="ETH") {
    data_clean[, c("int_m", "int_y")] <- convert_eth(data_clean$int_m, data_clean$int_y)
    data_clean[, c("birth_m", "birth_y")] <- convert_eth(data_clean$birth_m, data_clean$birth_y)
    data_clean[, c("mar_m", "mar_y")] <- convert_eth(data_clean$mar_m, data_clean$mar_y)
  }
  
  # Perisan calendar
  if (iso=="AFG") {
    data_clean[, c("int_m", "int_y")] <- convert_afg(data_clean$int_m, data_clean$int_y)
    data_clean[, c("birth_m", "birth_y")] <- convert_afg(data_clean$birth_m, data_clean$birth_y)
    data_clean[, c("mar_m", "mar_y")] <- convert_afg(data_clean$mar_m, data_clean$mar_y)
  }
  
  # Return dataframe
  data_clean
  
}
