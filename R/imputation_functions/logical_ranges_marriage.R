
# Restrict dates of marriage so they make sense with survey dates

logical_ranges_marriage <- function(data) {
  
  # Min and max marriage dates (based on survey age range)
  data$max_mar <- data$int_cmc
  data$min_mar <- data$int_cmc-40*12 # Marriage had to occur at a min of 10 years
  
  # Compute lower bound
  data <- data %>%
    mutate(S1LB_mar = 
             case_when(
               # When marriage year and month are available
               !is.na(mar_y) & !is.na(mar_m) ~ cmc(mar_m, mar_y),
               # When month is available
               !is.na(mar_m) & is.na(mar_y) & int_m<=mar_m ~ cmc(mar_m, int_y-40),
               !is.na(mar_m) & is.na(mar_y) & int_m>mar_m ~ cmc(mar_m, int_y-39),
               # When year is available
               is.na(mar_m) & !is.na(mar_y) & mar_y==int_y-40 ~ min_mar,
               is.na(mar_m) & !is.na(mar_y) ~ cmc(1, mar_y),
               # When no information is available
               is.na(mar_m) & is.na(mar_y) ~ min_mar,
               TRUE ~ NA
             ))
  
  # Compute upper bound
  data <- data %>%
    mutate(S1UB_mar = 
             case_when(
               # When birth year and month are available
               !is.na(mar_y) & !is.na(mar_m) ~ cmc(mar_m, mar_y),
               # When month is available
               !is.na(mar_m) & is.na(mar_y) & int_m<mar_m ~ cmc(mar_m, int_y-1),
               !is.na(mar_m) & is.na(mar_y) & int_m>=mar_m ~ cmc(mar_m, int_y),
               # When year is available
               is.na(mar_m) & !is.na(mar_y) & mar_y==int_y ~ max_mar,
               is.na(mar_m) & !is.na(mar_y) ~ cmc(12, mar_y),
               # When no information is available
               is.na(mar_m) & is.na(mar_y) ~ max_mar,
               TRUE ~ NA
             ))
  
  # Flag number of obs with inconsistent marriage month & year
  flag <- sum(data$S1LB_mar < data$min_mar | data$S1UB_mar > data$max_mar)
  
  # Remove marriage month and year inconsistent with survey dates
  data <- data %>%
    mutate(mar_m = if_else(S1LB_mar < min_mar | S1UB_mar > max_mar, NA, mar_m),
           mar_y = if_else(S1LB_mar < min_mar | S1UB_mar > max_mar, NA, mar_y))
  
  # Return data and number of observations with inconsistent marriage month/year
  list(data = data, flag = flag)
}

