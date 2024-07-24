
ancillary_constraints2 <- function(data) {
  
  # Restrict first union to be before first child's birth

  # Set lower bound
  data$S4LB_mar <- data$S3LB_mar_c
  
  # Compute upper cmc
  data <- data %>%
    mutate(S4UB_mar = 
             case_when(
               # When month & year of first child's birth available
               is.na(mar_y) & is.na(mar_age) & !is.na(birth1_m) & !is.na(birth1_y) ~ cmc(birth1_m, birth1_y),
               # When of first child's birth available
               is.na(mar_y) & is.na(mar_age) & !is.na(birth1_y) ~ cmc(12, birth1_y),
               # When no information is available
               TRUE ~ S3UB_mar_c
             ))
  
  # Calculate new constrained ranges
  data <- data %>%
    mutate(S4LB_mar_c = pmax(S3LB_mar_c, S4LB_mar),
           S4UB_mar_c = pmin(S3UB_mar_c, S4UB_mar),
           S4RG_mar_c = S4UB_mar_c - S4LB_mar_c)
  
  # Flag number of obs with inconsistent birth1
  flag <- sum(data$S4RG_mar_c<0)
  
  # Remove inconsistent birth1 month and year
  data <- data %>%
    mutate(birth1_m = if_else(data$S4RG_mar_c<0, NA, birth1_m),
           birth1_y = if_else(data$S4RG_mar_c<0, NA, birth1_y))

  list(data = data, flag = flag)
}

