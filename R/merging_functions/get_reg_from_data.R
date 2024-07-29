
get_reg_from_data <- function(data, merge_dat) {
  # Match regions reported in survey to GADM (using spreadsheet merge_dat)
  
  # Reg
  var <- merge_dat$Reg_var[1] # Survey variable to match to Adm1
  if (!is.na(var)) {
    data <- merge(data, merge_dat[,c("Reg", "Reg_match")], 
                  by.x = var, by.y = "Reg_match", all.x = TRUE)
  } else {
    data$Reg <- NA
  }
  
  # Adm1
  var <- merge_dat$Adm1_var[1] # Survey variable to match to Adm1
  if (!is.na(var)) {
    data <- merge(data, merge_dat[,c("Adm1", "Adm1_match")], 
                  by.x = var, by.y = "Adm1_match", all.x = TRUE)
  } else {
    data$Adm1 <- NA
  }
  
  # Adm2
  var <- merge_dat$Adm2_var[1] # Survey variable to match to Adm1
  if (!is.na(var)) {
    data <- merge(data, merge_dat[,c("Adm2", "Adm2_match")], 
                  by.x = var, by.y = "Adm2_match", all.x = TRUE)
  } else {
    data$Adm2 <- NA
  }
  
  data
}
