
# Create a list of vectors showing plausible dates for marriage and birth
# Based on final ranges produced

create_vectors <- function(data) {
  
  # CREATE BIRTH VECTORS

  # Create a list of vectors that have plausible CMCs for DOB
  birth_vect <- list()
  for (i in 1:nrow(data)) {
    birth_vect[[i]] <- seq(data$birth_cmc_lb[i], data$birth_cmc_ub[i])
  }
  
  # For observations with month specified, remove CMCs that correspond to other months
  for (i in 1:nrow(data)) {
    if (!is.na(data$birth_m[i])) {
      index <- which(sapply(birth_vect[[i]], cmc_m)==data$birth_m[i])
      if (length(index)) {
        birth_vect[[i]] <- birth_vect[[i]][index]
      }
    }
  }
  
  # CREATE MARRIAGE VECTORS
  
  # Create a list of vectors that have plausible CMCs for DOM
  mar_vect <- list()
  for (i in 1:nrow(data)) {
    mar_vect[[i]] <- seq(data$mar_cmc_lb[i], data$mar_cmc_ub[i])
  }
  
  # For observations with month specified, remove CMCs that correspond to other months
  for (i in 1:nrow(data)) {
    if (!is.na(data$mar_m[i])) {
      index <- which(sapply(mar_vect[[i]], cmc_m)==data$mar_m[i])
      if (length(index)) {
        mar_vect[[i]] <- mar_vect[[i]][index]
      }
    }
  }
  
  # Return the vectors of possible values
  list(birth_vect=birth_vect, mar_vect=mar_vect)
}
