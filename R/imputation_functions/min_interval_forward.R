
min_interval_forward <- function(data) {
  
  # Ensure that there is min 10 years between birth and marriage (forward)

  # Compute lower cmc
  data <- data %>%
    mutate(S5LB_mar = 
             case_when(
               # When marriage age is known
               !is.na(mar_age) ~ S2LB_birth_c + mar_age*12,
               # When no information is available
               TRUE ~ S2LB_birth_c + 10*12
             ))
  
  # Compute upper cmc
  data <- data %>%
    mutate(S5UB_mar = 
             case_when(
               # When marriage age is known
               !is.na(mar_age) ~ S2UB_birth_c + (mar_age+1)*12,
               # When no information is available
               TRUE ~ S2UB_birth_c + 50*12
             ))
  
  # Calculate new constrained ranges
  data <- data %>%
    mutate(S5LB_mar_c = pmax(S4LB_mar_c, S5LB_mar),
           S5UB_mar_c = pmin(S4UB_mar_c, S5UB_mar),
           S5RG_mar_c = S5UB_mar_c - S5LB_mar_c)
  
  # Flag number of obs with inconsistent dates
  flag <- sum(data$S5RG_mar_c<0)
  
  # Remove inconsistent dates
  data <- data %>%
    mutate(
      sex_age = ifelse(S5RG_mar_c < 0 & is.na(mar_y) & is.na(mar_age), NA, sex_age),
      birth1_m = ifelse(S5RG_mar_c < 0 & is.na(mar_y) & is.na(mar_age), NA, birth1_m),
      birth1_y = ifelse(S5RG_mar_c < 0 & is.na(mar_y) & is.na(mar_age), NA, birth1_y),
      mar_age = ifelse(S5RG_mar_c < 0, NA, mar_age),
      mar_y = ifelse(S5RG_mar_c < 0, NA, mar_y),
      mar_m = ifelse(S5RG_mar_c < 0, NA, mar_m))
  
  list(data = data, flag = flag)
  
}
