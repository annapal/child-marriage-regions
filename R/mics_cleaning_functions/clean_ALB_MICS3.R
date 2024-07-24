
clean_ALB_MICS3 <- function(survey_var) {

  # Import MICS data files
  wm <- read_sav("data/mics/raw_data/Albania MICS 2005 SPSS Datasets/wm.sav")
  hh <- read_sav("data/mics/raw_data/Albania MICS 2005 SPSS Datasets/hh.sav")
  
  # Create data dictionaries
  wm.dict <- labelled::generate_dictionary(wm)
  hh.dict <- labelled::generate_dictionary(hh)
  
  # Write data dictionaries to excel file
  write_xlsx(wm.dict, "data/mics/data_dictionaries/ALB_MICS3_wm_dict.xlsx")
  write_xlsx(hh.dict, "data/mics/data_dictionaries/ALB_MICS3_hh_dict.xlsx")
  
  # Merge womens and household survey
  mics <- merge(wm, hh, by=c("HH1", "HH2"), suffixes = c("", ".hh"))
  
  # Remove any incomplete surveys
  mics <- subset(mics, WM7==1)
  
  # Create new data frame to store clean data
  data <- as.data.frame(matrix(nrow=nrow(mics), ncol=nrow(survey_var)))
  colnames(data) <- survey_var$Name
  
  # Meta data -----------------------------------
  
  # Survey information
  data$iso3 <- "ALB"
  data$survey <- "MICS"
  data$phase <- 3
  data$surveyid <- "ALB_MICS3"
  
  # Check if the sample contains only ever married women
  data$samp <- "All Women" 
  
  # Household data -------------------------------
  
  # Cluster number
  data$clust <- as.numeric(mics$HH1)
  
  # Household number
  data$hh <- as.numeric(mics$HH2)
  
  # Women's line number
  data$line <- as.numeric(mics$WMID)
  
  # Women's survey weight
  data$wt <- as.numeric(mics$wmweight)
  
  # Interview month and year
  data$int_m <- as.numeric(mics$HH5M)
  data$int_y <- 2005
  
  # Primary sampling unit
  data$psu <- data$clust
  
  # Sub-national region names
  data$region <- tolower(as.character(as_factor(mics$HH7)))
  data$prov <- NA
  data$dist <- NA
  
  # Rural-urban place of residence
  data$res <- ifelse(mics$HH6==1, "urban", ifelse(mics$HH6==2, "rural", NA))
  data$res <- as.factor(data$res)
  
  # Strata
  data$strata <- as.numeric(factor(paste(data$region, data$res)))
  
  # Individual information ---------------------------------------
  
  # Birth month
  data$birth_m <- as.numeric(mics$WM8M)
  data[data$birth_m%in%c(97,98,99), "birth_m"] <- NA
  
  # Birth year
  data$birth_y <- as.numeric(mics$WM8Y)
  data[data$birth_y%in%c(9997,9998,9999), "birth_y"] <- NA
  
  # Age at interview
  data$age <- as.numeric(mics$WM9)
  
  # Religion
  data$relig <- tolower(as.character(as_factor(mics$HC1A)))
  data$relig <- ifelse(data$relig=="no response", NA, data$relig)
  
  # Ethnicity
  data$ethn <- NA
  
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
  
  # Marriage month
  data$mar_m <- as.numeric(mics$MA6M)
  data[data$mar_m%in%c(97,98,99), "mar_m"] <- NA
  
  # Marriage year
  data$mar_y <- as.numeric(mics$MA6Y)
  data[data$mar_y%in%c(9997,9998,9999), "mar_y"] <- NA
  
  # Marriage age
  data$mar_age <- as.numeric(mics$MA8)
  data[data$mar_age%in%c(97,98,99), "mar_age"] <- NA
  
  # Other info ------------------------------------------
  
  # Ever given birth
  data$birth1_ev <- as.numeric(mics$CM1)
  data$birth1_ev <- ifelse(data$birth1_ev==1, 1,
                           ifelse(data$birth1_ev==2, 0, NA))
  
  # First child's month of birth
  data$birth1_m <- as.numeric(mics$CM2AM)
  data[data$birth1_m%in%c(97,98,99), "birth1_m"] <- NA
  
  # First child's year of birth
  data$birth1_y <- as.numeric(mics$CM2AY)
  data[data$birth1_y%in%c(9997,9998,9999), "birth1_y"] <- NA
  
  # Age of first child at last birthday
  data$birth1_age <- as.numeric(mics$CM2B)
  data[data$birth1_age%in%c(97,98,99), "birth1_age"] <- NA
  
  # Ever had sex
  data$sex_ev <- NA
  
  # Age at first sexual intercourse
  data$sex_age <- NA
  
  data
}
