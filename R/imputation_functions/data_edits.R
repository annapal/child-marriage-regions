
# Perform some initial data edits

data_edits <- function(data) {
  
  data <- data %>%
    mutate(
      # Where marriage status is missing/never, but marriage dates are recorded
      # Set respondents to currently married
      mar_status = 
             case_when(
               # When marriage status is missing
               is.na(mar_status) & 
                 (!is.na(mar_m)|!is.na(mar_y)|!is.na(mar_age)) ~ "currently",
               # When marriage status is never
               mar_status=="never" & 
                 (!is.na(mar_m)|!is.na(mar_y)|!is.na(mar_age)) ~ "currently",
               TRUE ~ mar_status
             ),
      
      # Remove marriage ages < 10 (deemed as unreliable)
      mar_age = ifelse(mar_age < 10, NA, mar_age),
      
      # Remove ages of first sex < 10 (deemed as unreliable)
      sex_age = ifelse(sex_age < 10, NA, sex_age))

  data # Return the dataframe
}

