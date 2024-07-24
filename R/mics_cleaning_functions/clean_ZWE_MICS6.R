
clean_ZWE_MICS6 <- function(survey_var) {

  # Import MICS data files
  wm <- read_sav("data/mics/raw_data/Zimbabwe MICS6 SPSS Datasets/Zimbabwe MICS6 SPSS Datasets/wm.sav")
  hh <- read_sav("data/mics/raw_data/Zimbabwe MICS6 SPSS Datasets/Zimbabwe MICS6 SPSS Datasets/hh.sav")
  bh <- read_sav("data/mics/raw_data/Zimbabwe MICS6 SPSS Datasets/Zimbabwe MICS6 SPSS Datasets/bh.sav")
  
  # Create data dictionaries
  wm.dict <- labelled::generate_dictionary(wm)
  hh.dict <- labelled::generate_dictionary(hh)
  bh.dict <- labelled::generate_dictionary(bh)
  
  # Write data dictionaries to excel file
  write_xlsx(wm.dict, "data/mics/data_dictionaries/ZWE_MICS6_wm_dict.xlsx")
  write_xlsx(hh.dict, "data/mics/data_dictionaries/ZWE_MICS6_hh_dict.xlsx")
  write_xlsx(bh.dict, "data/mics/data_dictionaries/ZWE_MICS6_bh_dict.xlsx")
  
  # Merge womens and household survey
  mics <- merge(wm, hh, by=c("HH1", "HH2"), suffixes = c("", ".hh"))
  
  # Subset first born children
  bh <- subset(bh, BHLN==1)
  
  # Merge mics data and birth history data
  mics <- merge(mics, bh, by=c("HH1", "HH2", "LN"), all.x=TRUE, suffixes = c("", ".bh"))
  
  # Remove any incomplete surveys
  table(mics$WM17)
  val_labels(mics$WM17)
  mics <- subset(mics, WM17==1)
  
  # Create new data frame to store clean data
  data <- as.data.frame(matrix(nrow=nrow(mics), ncol=nrow(survey_var)))
  colnames(data) <- survey_var$Name
  
  # Meta data -----------------------------------
  
  # Survey information
  data$iso3 <- "ZWE"
  data$survey <- "MICS"
  data$phase <- 6
  data$surveyid <- "ZWE_MICS6"
  
  # Check if the sample contains only ever married women
  val_labels(mics$MSTATUS)
  table(mics$MSTATUS)
  data$samp <- "All Women" 
  
  # Household data -------------------------------
  
  # Cluster number
  data$clust <- as.numeric(mics$WM1)
  summary(data$clust)
  
  # Household number
  data$hh <- as.numeric(mics$WM2)
  summary(data$hh)
  
  # Women's line number
  data$line <- as.numeric(mics$WM3)
  summary(data$line)
  
  # Women's survey weight
  data$wt <- as.numeric(mics$wmweight)
  summary(data$wt)
  
  # Interview month and year
  data$int_m <- as.numeric(mics$WM6M)
  summary(data$int_m)
  data$int_y <- as.numeric(mics$WM6Y)
  summary(data$int_y)
  
  # Primary sampling unit and strata
  data$psu <- as.numeric(mics$psu)
  summary(data$psu)
  data$strata <- as.numeric(mics$stratum)
  summary(data$strata)
  
  # Sub-national region names
  data$region <- tolower(as.character(as_factor(mics$HH7)))
  data$prov <- NA
  data$dist <- NA
  
  # Rural-urban place of residence
  data$res <- ifelse(mics$HH6==1, "urban", ifelse(mics$HH6==2, "rural", NA))
  data$res <- as.factor(data$res)
  table(data$res, useNA = "always")
  
  # Individual information ---------------------------------------
  
  # Birth month
  data$birth_m <- as.numeric(mics$WB3M)
  data[data$birth_m%in%c(97,98,99), "birth_m"] <- NA
  summary(data$birth_m)
  
  # Birth year
  data$birth_y <- as.numeric(mics$WB3Y)
  data[data$birth_y%in%c(9997,9998,9999), "birth_y"] <- NA
  summary(data$birth_y)
  
  # Age at interview
  data$age <- as.numeric(mics$WB4)
  summary(data$age)
  
  # Religion
  data$relig <- tolower(as.character(as_factor(mics$religion)))
  data$relig <- ifelse(data$relig=="no response", NA, data$relig)
  table(data$relig, useNA = "always")
  
  # Ethnicity
  data$ethn <- NA
  
  # Always lived in same city/village
  data$al_lived <- ifelse(is.na(mics$WB15)|mics$WB15==99, NA,
                          ifelse(mics$WB15==95, 1, 0))
  table(data$al_lived, useNA = "always")
  
  # Number of years lived in city/village
  data$yrs_lived <- ifelse(is.na(mics$WB15)|(mics$WB15 %in% c(99, 95)), NA,
                           as.numeric(mics$WB15))
  summary(data$yrs_lived)
  
  # Region of previous residence
  data$prev_reg <- tolower(as.character(as_factor(mics$WB17)))
  data$prev_reg <- ifelse(data$prev_reg=="no response", NA, data$prev_reg)
  data$prev_reg <- ifelse(data$prev_reg=="outside of zimbabwe", "international", data$prev_reg)
  table(data$prev_reg, useNA = "always")
  
  # Marriage information ------------------------------------------
  
  # Current marriage status
  data$mar_status <- ifelse(mics$MSTATUS==1, "currently", 
                            ifelse(mics$MSTATUS==2, "formerly", 
                                   ifelse(mics$MSTATUS==3, "never", NA)))
  data$mar_status <- as.factor(data$mar_status)
  table(data$mar_status, useNA = "always")
  
  # Marriage month
  data$mar_m <- as.numeric(mics$MA8M)
  data[data$mar_m%in%c(97,98,99), "mar_m"] <- NA
  summary(data$mar_m)
  
  # Marriage year
  data$mar_y <- as.numeric(mics$MA8Y)
  data[data$mar_y%in%c(9997,9998,9999), "mar_y"] <- NA
  summary(data$mar_y)
  
  # Marriage age
  data$mar_age <- as.numeric(mics$MA11)
  data[data$mar_age%in%c(97,98,99), "mar_age"] <- NA
  summary(data$mar_age)
  
  # Other info ------------------------------------------
  
  # Ever given birth
  data$birth1_ev <- as.numeric(mics$CM1)
  data$birth1_ev <- ifelse(data$birth1_ev==1, 1,
                           ifelse(data$birth1_ev==2, 0, NA))
  table(data$birth1_ev, useNA = "always")
  
  # First child's month of birth
  data$birth1_m <- as.numeric(mics$BH4M)
  data[data$birth1_m%in%c(97,98,99), "birth1_m"] <- NA
  summary(data$birth1_m)
  
  # First child's year of birth
  data$birth1_y <- as.numeric(mics$BH4Y)
  data[data$birth1_y%in%c(9997,9998,9999), "birth1_y"] <- NA
  summary(data$birth1_y)
  
  # Age of first child at last birthday
  data$birth1_age <- as.numeric(mics$BH6)
  data[data$birth1_age%in%c(97,98,99), "birth1_age"] <- NA
  summary(data$birth1_age)
  
  # Ever had sex
  data$sex_ev <- ifelse(mics$SB1==99, NA, 
                        ifelse(mics$SB1==0, 0, 
                               ifelse(mics$SB1>0, 1, NA)))
  table(data$sex_ev, useNA="always")
  
  # Age at first sexual intercourse
  data$sex_age <- as.numeric(mics$SB1)
  data$sex_age <- ifelse((data$sex_age==99)|(data$sex_age==0), NA, 
                         ifelse(data$sex_age==95, data$mar_age, data$sex_age))
  summary(data$sex_age)
  
  data
  
}