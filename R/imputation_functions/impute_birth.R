
# Impute the month and year of birth (where needed)

impute_birth <- function(data, birth_vect) {

  # Impute birth CMC by randomly sampling from vector
  set.seed(123)
  data$birth_cmc_imp <- as.numeric(lapply(birth_vect, resample))
  
  # Convert to date
  data$birth_y_imp <- cmc_y(data$birth_cmc_imp)
  data$birth_m_imp <- cmc_m(data$birth_cmc_imp)
  
  # SENSE CHECKS
  
  # Create directory
  dir.create("data/mics/imputation_checks", showWarnings = FALSE)
  
  # Save sense check output in text file
  survey_id <- data$surveyid[1]
  sink(paste0("data/mics/imputation_checks/", survey_id, "_imp_check.txt"))
  cat("Birth Imputation \n")
  
  # Check imputed values match month response (where applicable)
  obs <- which(data$birth_imp%in%c(1,2,7))
  cat(paste0("Reported month doesn't match imputed: ", 
               sum(!(data[obs,"birth_m"]==data[obs,"birth_m_imp"])), " obs \n"))
  
  # Check imputed values match year response (where applicable)
  obs <- which(data$birth_imp%in%c(1,3,5))
  cat(paste0("Reported year doesn't match imputed: ", 
               sum(!(data[obs,"birth_y"]==data[obs,"birth_y_imp"])), " obs \n"))
  
  # Calculate imputed age
  data$int_cmc <- cmc(data$int_m, data$int_y)
  data$age_imp <- floor((data$int_cmc-data$birth_cmc_imp)/12)
  
  # Check imputed age matches reported age
  cat(paste0("Reported age doesn't match imputed: ", 
               sum(!is.na(data$age) & data$age_imp!=data$age &
                     data$age_imp!=data$age+1), " obs \n"))
  
  # Calculated age may be 1 year off if birthday is after interview date
  # This only happens when birth month = interview month
  obs <- which(data$age_imp==data$age+1)
  cat(paste0("Where age is +1, birth & int month don't match: ", 
               sum(!(data[obs, "birth_m_imp"]==data[obs,"int_m"])), " obs \n"))
  
  sink()
  
  # APPLY IMPUTED DATES OF BIRTH
  
  # Replace with imputed values
  data$birth_m <- data$birth_m_imp
  data$birth_y <- data$birth_y_imp
  
  # Replace age with imputed age
  # Note that this won't be the same as reported age in all cases (due to rounding)
  data$age <- data$age_imp
  
  # Return the dataframe
  data
}
