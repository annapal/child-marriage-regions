
isolated_constraints_birth <- function(data) {
  
  # Constrain birth dates so they make sense with reported age
  
  # Compute lower cmc
  data <- data %>%
    mutate(S2LB_birth = 
             case_when(
               # When age is known
               !is.na(age) ~ int_cmc - (age+1)*12,
               # When no information is available
               TRUE ~ S1LB_birth
             ))
  
  # Compute upper cmc
  data <- data %>%
    mutate(S2UB_birth = 
             case_when(
               # When age is known
               !is.na(age) ~ int_cmc - (age)*12,
               # When no information is available
               TRUE ~ S1UB_birth
             ))
  
  # Calculate new constrained ranges
  data <- data %>%
    mutate(S2LB_birth_c = pmax(S1LB_birth, S2LB_birth),
           S2UB_birth_c = pmin(S1UB_birth, S2UB_birth),
           S2RG_birth_c = S2UB_birth_c - S2LB_birth_c)
  
  # Flag number of obs with inconsistent age
  flag <- sum(data$S2RG_birth_c<0)
  
  # Remove ages inconsistent with reported month/year of birth
  data <- data %>%
    mutate(age = if_else(data$S2RG_birth_c<0, NA, age))
  
  list(data = data, flag = flag)
  
}
