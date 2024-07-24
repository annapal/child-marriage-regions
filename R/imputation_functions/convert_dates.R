
convert_AFG_dates <- function(data) {
  
  # Convert interview date ------------------------------------------------------
  
  data$int_cmc_p <- cmc_persian(data$int_m, data$int_y)
  data$int_cmc <- persian_to_greg(cmc=data$int_cmc_p)
  
  data$int_y <- cmc_y(data$int_cmc)
  data$int_m <- cmc_m(data$int_cmc)
  
  # Convert birth date ------------------------------------------------------
  
  data$birth_cmc_p <- cmc_persian(data$birth_m, data$birth_y)
  data$birth_cmc <- persian_to_greg(cmc=data$birth_cmc_p)
  
  data$birth_y <- cmc_y(data$birth_cmc)
  data$birth_m <- cmc_m(data$birth_cmc)
  
  # Convert marriage date ---------------------------------------------------
  
  data$mar_cmc_p <- cmc_persian(data$mar_m, data$mar_y)
  data$mar_cmc <- persian_to_greg(cmc=data$mar_cmc_p)
  
  data$mar_y <- cmc_y(data$mar_cmc)
  data$mar_m <- cmc_m(data$mar_cmc)
  
  data
}

convert_NPL_dates <- function(data) {
  
  # Convert interview date ------------------------------------------------------
  
  data$int_cmc_n <- cmc(data$int_m, data$int_y)
  data$int_cmc <- nepal_to_greg(cmc=data$int_cmc_n)
  
  data$int_y <- cmc_y(data$int_cmc)
  data$int_m <- cmc_m(data$int_cmc)
  
  # Convert birth date ------------------------------------------------------
  
  data$birth_cmc_n <- cmc(data$birth_m, data$birth_y)
  data$birth_cmc <- nepal_to_greg(cmc=data$birth_cmc_n)
  
  data$birth_y <- cmc_y(data$birth_cmc)
  data$birth_m <- cmc_m(data$birth_cmc)
  
  # Convert marriage date ---------------------------------------------------
  
  data$mar_cmc_n <- cmc(data$mar_m, data$mar_y)
  data$mar_cmc <- nepal_to_greg(cmc=data$mar_cmc_n)
  
  data$mar_y <- cmc_y(data$mar_cmc)
  data$mar_m <- cmc_m(data$mar_cmc)
  
  data
}

convert_THA_dates <- function(data) {
  
  # Convert birth date ------------------------------------------------------
  
  data$int_cmc_t <- cmc(data$int_m, data$int_y)
  data$int_cmc <- thai_to_greg(cmc=data$int_cmc_t)
  
  data$int_y <- cmc_y(data$int_cmc)
  data$int_m <- cmc_m(data$int_cmc)
  
  # Convert birth date ------------------------------------------------------
  
  data$birth_cmc_t <- cmc(data$birth_m, data$birth_y)
  data$birth_cmc <- thai_to_greg(cmc=data$birth_cmc_t)
  
  data$birth_y <- cmc_y(data$birth_cmc)
  data$birth_m <- cmc_m(data$birth_cmc)
  
  # Convert marriage date ---------------------------------------------------
  
  data$mar_cmc_t <- cmc(data$mar_m, data$mar_y)
  data$mar_cmc <- thai_to_greg(cmc=data$mar_cmc_t)
  
  data$mar_y <- cmc_y(data$mar_cmc)
  data$mar_m <- cmc_m(data$mar_cmc)
  
  data
}

