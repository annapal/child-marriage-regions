
impute_marriage <- function(data, mar_vect) {
  
  # For observations where age of marriage is reported, remove CMCs that correspond to other ages
  for (i in 1:nrow(data)) {
    if (!is.na(data$mar_age[i])) {
      cmc_lb <- data$birth_cmc_imp[i] + data$mar_age[i]*12
      cmc_ub <- data$birth_cmc_imp[i] + (data$mar_age[i]+1)*12
      range <- cmc_lb:cmc_ub
      range2 <- intersect(cmc_lb:cmc_ub, mar_vect[[i]])
      if (length(range2)) {
        mar_vect[[i]] <- range2
      }
    }
  }
  
  # Impute marriage CMC by randomly sampling from vector
  set.seed(123)
  data$mar_cmc_imp <- as.numeric(lapply(mar_vect, resample))
  
  # Convert to date
  data$mar_y_imp <- cmc_y(data$mar_cmc_imp)
  data$mar_m_imp <- cmc_m(data$mar_cmc_imp)
  
  # SENSE CHECKS ----------------------------------------------
  
  survey_id <- data$surveyid[1]
  text_file <- paste0("data/mics/imputation_checks/", survey_id, "_imp_check.txt")
  cat("Marriage Imputation \n",
      file = text_file, append = TRUE)
  
  # Check imputed values match month response (where applicable)
  obs <- which(data$mar_imp%in%c(1,2,7))
  cat(paste0("Reported month doesn't match imputed: ", 
               sum(!(data[obs,"mar_m"]==data[obs,"mar_m_imp"])), " obs \n"),
      file = text_file, append = TRUE)
  
  # Check imputed values match year response (where applicable)
  obs <- which(data$mar_imp%in%c(1,3,5))
  cat(paste0("Reported year doesn't match imputed: ", 
               sum(!(data[obs,"mar_y"]==data[obs,"mar_y_imp"])), " obs \n"),
      file = text_file, append = TRUE)
  
  # Calculate imputed age of marriage
  data$mar_age_imp <- floor((data$mar_cmc_imp - data$birth_cmc_imp)/12)
  
  # Check imputed age matches reported age
  cat(paste0("Reported marriage age doesn't match imputed: ", 
               sum(!is.na(data$mar_age) & data$mar_age_imp!=data$mar_age &
                     data$mar_age_imp!=data$mar_age+1), " obs \n"),
      file = text_file, append = TRUE)
  
  # Calculated age may be 1 year off if birthday is after interview date
  # This only happens when birth month = interview month
  obs <- which(data$mar_age_imp==data$mar_age+1)
  cat(paste0("Where age is +1, marriage & birth month don't match: ", 
               sum(!(data[obs, "mar_m_imp"]==data[obs,"birth_m"])), " obs \n"),
        file = text_file, append = TRUE)
  
  # APPLY IMPUTED DATES OF MARIAGE --------------------------------------------
  
  # Replace with imputed values
  data$mar_m <- data$mar_m_imp
  data$mar_y <- data$mar_y_imp
  
  # Replace age with imputed age
  # Note that this won't be the same as reported age in all cases (due to rounding)
  data$mar_age <- data$mar_age_imp
  
  # Remove imputed dates for those who have never been married
  data <- data %>%
    mutate(
      mar_m = ifelse(mar_status == "never" | is.na(mar_status), NA, mar_m),
      mar_y = ifelse(mar_status == "never" | is.na(mar_status), NA, mar_y),
      mar_age = ifelse(mar_status == "never" | is.na(mar_status), NA, mar_age)
    )
  
  data
}

