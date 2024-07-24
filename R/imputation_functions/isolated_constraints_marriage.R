
isolated_constraints_marriage <- function(data) {
  
  # Constrain marraige dates so they make sense with reported marriage age
  
  # Compute lower cmc
  data <- data %>%
    mutate(S2LB_mar = 
             case_when(
               # When age is known
               !is.na(mar_age) ~ S2LB_birth_c + mar_age*12,
               # When no information is available
               TRUE ~ S1LB_mar
             ))
  
  # Compute upper cmc
  data <- data %>%
    mutate(S2UB_mar = 
             case_when(
               # When age is known
               !is.na(mar_age) ~ S2UB_birth_c + (mar_age+1)*12,
               # When no information is available
               TRUE ~ S1UB_mar
             ))
  
  # Calculate new constrained ranges
  data <- data %>%
    mutate(S2LB_mar_c = pmax(S1LB_mar, S2LB_mar),
           S2UB_mar_c = pmin(S1UB_mar, S2UB_mar),
           S2RG_mar_c = S2UB_mar_c - S2LB_mar_c)
  
  # Flag number of obs with inconsistent marriage age
  flag <- sum(data$S2RG_mar_c<0)
  
  # Remove ages inconsistent with reported month/year of marriage
  data <- data %>%
    mutate(mar_age = if_else(data$S2RG_mar_c<0, NA, mar_age))
  
  list(data = data, flag = flag)
  
}

