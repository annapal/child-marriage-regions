
# Add same imputation codes as used in DHS

imputation_codes <- function(data) {

  # Codes for date of birth "missingness"
  data$birth_imp <- NA
  data <- data %>%
    mutate(birth_imp = 
             case_when(
               # Month and year specified - no imputation necessary
               !is.na(birth_m) & !is.na(birth_y) ~ 1,
               # Month and age of respondent specified - year imputed
               !is.na(birth_m) & is.na(birth_y) & !is.na(age) ~ 2,
               # Year and age of respondent specified - month imputed
               is.na(birth_m) & !is.na(birth_y) & !is.na(age) ~ 3,
               # Year specified, but not age - month imputed
               is.na(birth_m) & !is.na(birth_y) & is.na(age) ~ 5,
               # Age specified - year and month imputed
               is.na(birth_m) & is.na(birth_y) & !is.na(age) ~ 6,
               # Month specified, but not age/year - year imputed
               !is.na(birth_m) & is.na(birth_y) & is.na(age) ~ 7,
               # No information on date of birth
               is.na(birth_m) & is.na(birth_y) & is.na(age) ~ 8,
               TRUE ~ NA
             ))
  
  # Codes for date of marriage "missingness"
  data$mar_imp <- NA
  data <- data %>%
    mutate(mar_imp = 
             case_when(
               # Never-married respondents
               mar_status %in% c("never", NA) ~ NA,
               # Month and year specified - no imputation necessary
               !is.na(mar_m) & !is.na(mar_y) ~ 1,
               # Month and age of marriage specified - year imputed
               !is.na(mar_m) & is.na(mar_y) & !is.na(mar_age) ~ 2,
               # Year and age of marriage specified - month imputed
               is.na(mar_m) & !is.na(mar_y) & !is.na(mar_age) ~ 3,
               # Year specified, but not age - month imputed
               is.na(mar_m) & !is.na(mar_y) & is.na(mar_age) ~ 5,
               # Age specified - year and month imputed
               is.na(mar_m) & is.na(mar_y) & !is.na(mar_age) ~ 6,
               # Month specified, but not age/year - year imputed
               !is.na(mar_m) & is.na(mar_y) & is.na(mar_age) ~ 7,
               # No information on date of marriage
               is.na(mar_m) & is.na(mar_y) & is.na(mar_age) ~ 8,
               TRUE ~ NA
             ))
  
  # Return the dataframe
  data
}
