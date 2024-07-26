
gap_increasing <- function(data) {
  
  # Increase gap between birth and marriage
  # I.e. make sure that the upper bound for birth is lower than the lower bound for marriage
  # If there is an overlap, subtract half of the overlap from the DOB ub and add half to the DOM lb
  
  # Calculate gap
  data <- data %>%
    mutate(gap1 =
             case_when(
               mar_status=="never" ~ 0,
               TRUE ~ (birth_cmc_ub - mar_cmc_lb + 9*12)/2
               ))
  
  data <- data %>%
    mutate(
      # Delete gaps that are negative 
      # this means there is a sufficient gap already
      gap1 = ifelse(gap1 < 0, 0, gap1),
      
      # Subtract gap from upper bound of DOB, and add to lower bound of DOM
      birth_cmc_ub = birth_cmc_ub - gap1,
      mar_cmc_lb = mar_cmc_lb + gap1,
      
      # Round birth ranges down
      birth_cmc_lb = floor(birth_cmc_lb),
      birth_cmc_ub = floor(birth_cmc_ub),
      
      # Round marriage ranges up
      mar_cmc_lb = ceiling(mar_cmc_lb),
      mar_cmc_ub = ceiling(mar_cmc_ub)
    )
  
  # Return the data
  data
}
