
clean_ARG_MICS6 <- function(survey_var) {

  # Import MICS data files
  wm <- read_sav("data/mics/raw_data/Argentina MICS6 Datasets/Argentina MICS6 SPSS Datasets/wm.sav")
  hh <- read_sav("data/mics/raw_data/Argentina MICS6 Datasets/Argentina MICS6 SPSS Datasets/hh.sav")
  
  # Create data dictionaries
  wm.dict <- labelled::generate_dictionary(wm)
  hh.dict <- labelled::generate_dictionary(hh)
  
  # Write data dictionaries to excel file
  write_xlsx(wm.dict, "data/mics/data_dictionaries/ARG_MICS6_wm_dict.xlsx")
  write_xlsx(hh.dict, "data/mics/data_dictionaries/ARG_MICS6_hh_dict.xlsx")
  
  # Merge womens and household survey
  mics <- merge(wm, hh, by=c("HH1", "HH2"), suffixes = c("", ".hh"))
  
  # Remove any incomplete surveys
  mics <- subset(mics, WM17==1)
  
  # Create new data frame to store clean data
  data <- as.data.frame(matrix(nrow=nrow(mics), ncol=nrow(survey_var)))
  colnames(data) <- survey_var$Name
  
  # Meta data -----------------------------------
  
  # Survey information
  data$iso3 <- "ARG" 
  data$survey <- "MICS" 
  data$phase <- 6 
  data$surveyid <- "ARG_MICS6"
  
  # Check if the sample contains only ever married women
  data$samp <- "All Women" 
  
  # Household data -------------------------------
  
  # Cluster number
  data$clust <- as.numeric(mics$WM1)
  
  # Household number
  data$hh <- as.numeric(mics$WM2)
  
  # Women's line number
  data$line <- as.numeric(mics$WM3)
  
  # Women's survey weight
  data$wt <- as.numeric(mics$wmweight)
  
  # Interview month and year
  data$int_m <- as.numeric(mics$WM6M)
  data$int_y <- as.numeric(mics$WM6Y)
  
  # Primary sampling unit and strata
  data$psu <- as.numeric(mics$psu)
  data$strata <- as.numeric(mics$stratum)
  
  # Sub-national region names
  data$region <- tolower(as.character(as_factor(mics$stratum)))
  data$prov <- NA
  data$dist <- NA
  
  # Rural-urban
  data$res <- NA
  
  # Individual information ---------------------------------------
  
  # Birth month
  data$birth_m <- as.numeric(mics$WB3M)
  data[data$birth_m%in%c(97,98,99), "birth_m"] <- NA
  
  # Birth year
  data$birth_y <- as.numeric(mics$WB3Y)
  data[data$birth_y%in%c(9997,9998,9999), "birth_y"] <- NA
  
  # Age at interview
  data$age <- as.numeric(mics$WB4)
  
  # Religion
  data$relig <- NA
  
  # Ethnicity
  data$ethn <- ifelse(is.na(mics$ethnicity)|mics$ethnicity==9, NA,
                      ifelse(mics$ethnicity==1, "indigenous", "non-indigenous"))
  
  # Always lived in same city/village
  data$al_lived <- ifelse(is.na(mics$WB15)|mics$WB15==99, NA,
                          ifelse(mics$WB15==95, 1, 0))
  
  # Number of years lived in city/village
  data$yrs_lived <- ifelse(is.na(mics$WB15)|(mics$WB15 %in% c(99, 95)), NA,
                           as.numeric(mics$WB15))
  
  # Region of previous residence
  data$prev_reg <- tolower(as.character(as_factor(mics$WB17)))
  data$prev_reg <- ifelse(data$prev_reg=="no responde", NA, data$prev_reg)
  data$prev_reg <- ifelse(data$prev_reg=="fuera del país", "international", data$prev_reg)
  
  # Marriage information ------------------------------------------
  
  # Current marriage status
  data$mar_status <- ifelse(mics$MSTATUS==1, "currently", 
                            ifelse(mics$MSTATUS==2, "formerly", 
                                   ifelse(mics$MSTATUS==3, "never", NA)))
  data$mar_status <- as.factor(data$mar_status)
  
  # Marriage month
  data$mar_m <- as.numeric(mics$MA8M)
  data[data$mar_m%in%c(97,98,99), "mar_m"] <- NA
  
  # Marriage year
  data$mar_y <- as.numeric(mics$MA8Y)
  data[data$mar_y%in%c(9997,9998,9999), "mar_y"] <- NA
  
  # Marriage age
  data$mar_age <- as.numeric(mics$MA11)
  data[data$mar_age%in%c(97,98,99), "mar_age"] <- NA
  
  # Other info ------------------------------------------
  
  # Ever given birth
  data$birth1_ev <- as.numeric(mics$CM1)
  data$birth1_ev <- ifelse(data$birth1_ev==1, 1,
                           ifelse(data$birth1_ev==2, 0, NA))
  
  # First child's month of birth
  data$birth1_m <- as.numeric(mics$CM16BM)
  data[data$birth1_m%in%c(97,98,99), "birth1_m"] <- NA
  
  # First child's year of birth
  data$birth1_y <- as.numeric(mics$CM16BY)
  data[data$birth1_y%in%c(9997,9998,9999), "birth1_y"] <- NA
  
  # Age of first child at interview
  data$birth1_age <- NA
  
  # Ever had sex
  data$sex_ev <- ifelse(mics$SB1==99, NA, 
                        ifelse(mics$SB1==0, 0, 
                               ifelse(mics$SB1>0, 1, NA)))
  
  # Age at first sexual intercourse
  data$sex_age <- as.numeric(mics$SB1)
  data$sex_age <- ifelse((data$sex_age==99)|(data$sex_age==0), NA, 
                         ifelse(data$sex_age==95, data$mar_age, data$sex_age))
  
  data
}
  
