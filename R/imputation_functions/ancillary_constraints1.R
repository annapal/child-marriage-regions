
# Restrict first union to be at or after age of first sex
# Only when there is no information on marriage year or age

ancillary_constraints1 <- function(data) {

  # Compute lower bound
  data <- data %>%
    mutate(S3LB_mar = 
             case_when(
               # When no information on year/age of first union
               is.na(mar_y) & is.na(mar_age) & !is.na(sex_age) ~ S2LB_birth_c + sex_age*12,
               # When marriage information available
               TRUE ~ S2LB_mar_c
             ))
  
  # Set upper bound
  data$S3UB_mar <- data$S2UB_mar_c
  
  # Calculate new constrained ranges
  data <- data %>%
    mutate(S3LB_mar_c = pmax(S2LB_mar_c, S3LB_mar),
           S3UB_mar_c = pmin(S2UB_mar_c, S3UB_mar),
           S3RG_mar_c = S3UB_mar_c - S3LB_mar_c)
  
  # Flag number of obs with inconsistent sex age
  flag <- sum(data$S3RG_mar_c<0)
  
  # Remove inconsistent ages of first sex
  data <- data %>%
    mutate(sex_age = if_else(data$S3RG_mar_c<0, NA, sex_age))

  # Return data and number of observations that have inconsistent age of first sex
  list(data = data, flag = flag)
}

