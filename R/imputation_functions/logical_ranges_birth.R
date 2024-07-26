
# Create logical ranges for DOB so they make sense with survey dates

logical_ranges_birth <- function(data) {

  # Interview date in CMC
  data$int_cmc <- cmc(data$int_m, data$int_y)
  
  # Min and max birth dates (based on survey age range)
  data$max_birth <- data$int_cmc-15*12 # min age must be 15
  data$min_birth <- data$int_cmc-50*12 # max age must be 49
  
  # Compute lower bound
  data <- data %>%
    mutate(S1LB_birth = 
             case_when(
               # When birth year and month are available
               !is.na(birth_y) & !is.na(birth_m) ~ cmc(birth_m, birth_y),
               # When month is available
               !is.na(birth_m) & is.na(birth_y) & int_m<=birth_m ~ cmc(birth_m, int_y-50),
               !is.na(birth_m) & is.na(birth_y) & int_m>birth_m ~ cmc(birth_m, int_y-49),
               # When year is available
               is.na(birth_m) & !is.na(birth_y) & birth_y==int_y-50 ~ min_birth,
               is.na(birth_m) & !is.na(birth_y) ~ cmc(1, birth_y),
               # When no information is available
               is.na(birth_m) & is.na(birth_y) ~ min_birth,
               TRUE ~ NA
             ))
  
  # Compute upper bound
  data <- data %>%
    mutate(S1UB_birth = 
             case_when(
               # When birth year and month are available
               !is.na(birth_y) & !is.na(birth_m) ~ cmc(birth_m, birth_y),
               # When month is available
               !is.na(birth_m) & is.na(birth_y) & int_m<birth_m ~ cmc(birth_m, int_y-16),
               !is.na(birth_m) & is.na(birth_y) & int_m>=birth_m ~ cmc(birth_m, int_y-15),
               # When year is available
               is.na(birth_m) & !is.na(birth_y) & birth_y==int_y-15 ~ max_birth,
               is.na(birth_m) & !is.na(birth_y) ~ cmc(12, birth_y),
               # When no information is available
               is.na(birth_m) & is.na(birth_y) ~ max_birth,
               TRUE ~ NA
             ))
  
  # Flag number of obs with inconsistent birth month & year
  flag <- sum(data$S1LB_birth < data$min_birth | data$S1UB_birth > data$max_birth)
  
  # Remove birth month and year inconsistent with survey age range
  data <- data %>%
    mutate(birth_m = if_else(S1LB_birth < min_birth | S1UB_birth > max_birth, NA, birth_m),
           birth_y = if_else(S1LB_birth < min_birth | S1UB_birth > max_birth, NA, birth_y))
  
  # Return data and number of observations with inconsistent birth month & year
  list(data = data, flag = flag)
  
}

