
# Ensure that there is min 10 years between birth and marriage (backward)

min_interval_backward <- function(data) {

  # Compute lower bound
  data <- data %>%
    mutate(S3LB_birth = 
             case_when(
               # When marriage age is known
               !is.na(mar_age) & mar_age>=10 ~ S5LB_mar_c - (mar_age+1)*12,
               # When no information is available
               TRUE ~ S5LB_mar_c - 50*12
             ))
  
  # Compute upper bound
  data <- data %>%
    mutate(S3UB_birth = 
             case_when(
               # When marriage age is known
               !is.na(mar_age) & mar_age>=10 ~ S5UB_mar_c - mar_age*12,
               # When no information is available
               TRUE ~ S5UB_mar_c - 10*12
             ))
  
  # Calculate new constrained ranges
  data <- data %>%
    mutate(S3LB_birth_c = pmax(S2LB_birth_c, S3LB_birth),
           S3UB_birth_c = pmin(S2UB_birth_c, S3UB_birth),
           S3RG_birth_c = S3UB_birth_c - S3LB_birth_c)
  
  # Flag number of obs with inconsistent dates
  flag <- sum(data$S3RG_birth_c<0)
  
  # Remove inconsistent dates
  data <- data %>%
    mutate(
      sex_age = ifelse(S3RG_birth_c < 0 & is.na(mar_y) & is.na(mar_age), NA, sex_age),
      birth1_m = ifelse(S3RG_birth_c < 0 & is.na(mar_y) & is.na(mar_age), NA, birth1_m),
      birth1_y = ifelse(S3RG_birth_c < 0 & is.na(mar_y) & is.na(mar_age), NA, birth1_y),
      mar_age = ifelse(S3RG_birth_c < 0, NA, mar_age),
      mar_y = ifelse(S3RG_birth_c < 0, NA, mar_y),
      mar_m = ifelse(S3RG_birth_c < 0, NA, mar_m))

  # Set final ranges
  data <- data %>%
    mutate(birth_cmc_lb = S3LB_birth_c,
           birth_cmc_ub = S3UB_birth_c,
           mar_cmc_lb = S5LB_mar_c,
           mar_cmc_ub = S5UB_mar_c)
  
  # Return data and number of inconsistent observations
  list(data = data, flag = flag)
}

