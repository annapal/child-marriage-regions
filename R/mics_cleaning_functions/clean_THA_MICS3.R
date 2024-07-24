
clean_THA_MICS3 <- function(survey_var) {

  # Import MICS data files
  wm <- read_sav("data/mics/raw_data/Thailand 2005-06 MICS_Datasets/Thailand MICS 2005-2006 SPSS Datasets/wm.sav")
  hh <- read_sav("data/mics/raw_data/Thailand 2005-06 MICS_Datasets/Thailand MICS 2005-2006 SPSS Datasets/hh.sav")
  
  # Create data dictionaries
  wm.dict <- labelled::generate_dictionary(wm)
  hh.dict <- labelled::generate_dictionary(hh)
  
  # Write data dictionaries to excel file
  write_xlsx(wm.dict, "data/mics/data_dictionaries/THA_MICS3_wm_dict.xlsx")
  write_xlsx(hh.dict, "data/mics/data_dictionaries/THA_MICS3_hh_dict.xlsx")
  
  # Merge womens and household survey
  mics <- merge(wm, hh, by=c("HH1", "HH2"), suffixes = c("", ".hh"))
  
  # Remove any incomplete surveys
  table(mics$wm7)
  val_labels(mics$wm7)
  mics <- subset(mics, wm7==1)
  
  # Create new data frame to store clean data
  data <- as.data.frame(matrix(nrow=nrow(mics), ncol=nrow(survey_var)))
  colnames(data) <- survey_var$Name
  
  # Meta data -----------------------------------
  
  # Survey information
  data$iso3 <- "THA"
  data$survey <- "MICS"
  data$phase <- 3
  data$surveyid <- "THA_MICS3"
  
  # Check if the sample contains only ever married women
  val_labels(mics$mstatus)
  table(mics$mstatus)
  data$samp <- "All Women" 
  
  # Household data -------------------------------
  
  # Cluster number
  data$clust <- as.numeric(mics$wm1)
  summary(data$clust)
  
  # Household number
  data$hh <- as.numeric(mics$wm2)
  summary(data$hh)
  
  # Women's line number
  data$line <- as.numeric(mics$ln)
  summary(data$line)
  
  # Women's survey weight
  data$wt <- as.numeric(mics$wmweight)
  summary(data$wt)
  
  # Interview month and year
  data$int_m <- as.numeric(mics$wm6m)
  summary(data$int_m)
  data$int_y <- as.numeric(mics$wm6y)
  summary(data$int_y)
  
  # Primary sampling unit
  data$psu <- as.numeric(mics$psu_no)
  
  # Sub-national region names
  data$region <- tolower(as.character(as_factor(mics$hh7)))
  data$prov <- NA
  data$dist <- NA
  
  # Rural-urban place of residence
  data$res <- ifelse(mics$hh6==1, "rural", ifelse(mics$hh6==2, "urban", NA))
  data$res <- as.factor(data$res)
  table(data$res, useNA = "always")
  
  # Strata
  data$strata <- as.numeric(factor(paste(data$region, data$res)))
  
  # Individual information ---------------------------------------
  
  # Birth month
  data$birth_m <- as.numeric(mics$wm8m)
  data[data$birth_m%in%c(97,98,99), "birth_m"] <- NA
  summary(data$birth_m)
  
  # Birth year
  data$birth_y <- as.numeric(mics$wm8y)
  data[data$birth_y%in%c(9997,9998,9999), "birth_y"] <- NA
  summary(data$birth_y)
  
  # Age at interview
  data$age <- as.numeric(mics$wm9)
  summary(data$age)
  
  # Religion
  data$relig <- tolower(as.character(as_factor(mics$hc1a)))
  data$relig <- ifelse(data$relig=="no response", NA, data$relig)
  table(data$relig, useNA = "always")
  
  # Ethnicity
  data$ethn <- tolower(as.character(as_factor(mics$hc1c)))
  data$ethn <- ifelse(data$ethn=="no response", NA, data$ethn)
  table(data$ethn, useNA = "always") 
  
  # Always lived in same city/village
  data$al_lived <- NA
  
  # Number of years lived in city/village
  data$yrs_lived <- NA
  
  # Region of previous residence
  data$prev_reg <- NA
  
  # Marriage information ------------------------------------------
  
  # Current marriage status
  data$mar_status <- ifelse(mics$mstatus==1, "currently", 
                            ifelse(mics$mstatus==2, "formerly", 
                                   ifelse(mics$mstatus==3, "never", NA)))
  data$mar_status <- as.factor(data$mar_status)
  table(data$mar_status, useNA = "always")
  
  # Marriage month
  data$mar_m <- as.numeric(mics$ma6m)
  data[data$mar_m%in%c(97,98,99), "mar_m"] <- NA
  summary(data$mar_m)
  
  # Marriage year
  data$mar_y <- as.numeric(mics$ma6y)
  data[data$mar_y%in%c(9997,9998,9999), "mar_y"] <- NA
  summary(data$mar_y)
  
  # Marriage age
  data$mar_age <- as.numeric(mics$ma8)
  data[data$mar_age%in%c(97,98,99), "mar_age"] <- NA
  summary(data$mar_age)
  
  # Other info ------------------------------------------
  
  # Ever given birth
  data$birth1_ev <- as.numeric(mics$cm1)
  data$birth1_ev <- ifelse(data$birth1_ev==1, 1,
                           ifelse(data$birth1_ev==2, 0, NA))
  table(data$birth1_ev)
  
  # First child's month of birth
  data$birth1_m <- as.numeric(mics$cm2am)
  data[data$birth1_m%in%c(97,98,99), "birth1_m"] <- NA
  summary(data$birth1_m)
  
  # First child's year of birth
  data$birth1_y <- as.numeric(mics$cm2ay)
  data[data$birth1_y%in%c(9997,9998,9999), "birth1_y"] <- NA
  summary(data$birth1_y)
  
  # Age of first child at last birthday
  data$birth1_age <- as.numeric(mics$cm2b)
  data[data$birth1_age%in%c(97,98,99), "birth1_age"] <- NA
  summary(data$birth1_age)
  
  # Ever had sex
  data$sex_ev <- NA
  
  # Age at first sexual intercourse
  data$sex_age <- NA
  
  data
}