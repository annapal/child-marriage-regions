
match_other_variables <- function(data, merge_dat) {
  
  # Match other variables reported in survey
  
  # Previous Region (Reg)
  if (!is.na(merge_dat$Prev_Reg_match[1])) {
    data <- merge(data, na.omit(merge_dat[,c("Prev_Reg", "Prev_Reg_match")]), 
                  by.x = "prev_reg", by.y = "Prev_Reg_match", all.x = TRUE)
  } else {
    data$Prev_Reg <- NA
  }
  
  # Previous Region (Adm1)
  if (!is.na(merge_dat$Prev_Adm1_match[1])) {
    data <- merge(data, na.omit(merge_dat[,c("Prev_Adm1", "Prev_Adm1_match")]), 
                  by.x = "prev_reg", by.y = "Prev_Adm1_match", all.x = TRUE)
  } else {
    data$Prev_Adm1 <- NA
  }
  
  # Previous Region (Adm2)
  if (!is.na(merge_dat$Prev_Adm2_match[1])) {
    data <- merge(data, na.omit(merge_dat[,c("Prev_Adm2", "Prev_Adm2_match")]), 
                  by.x = "prev_reg", by.y = "Prev_Adm2_match", all.x = TRUE)
  } else {
    data$Prev_Adm2 <- NA
  }
  
  # Religion
  if (!is.na(merge_dat$Religion_match[1])) {
    data <- merge(data, na.omit(merge_dat[,c("Religion", "Religion_match")]), 
                  by.x = "relig", by.y = "Religion_match", all.x = TRUE)
  } else {
    data$Religion <- NA
  }
  
  # Ethnicity
  if (!is.na(merge_dat$Ethnicity_match[1])) {
    data <- merge(data, na.omit(merge_dat[,c("Ethnicity", "Ethnicity_match")]), 
                  by.x = "ethn", by.y = "Ethnicity_match", all.x = TRUE)
  } else {
    data$Ethnicity <- NA
  }
  
  data
}
