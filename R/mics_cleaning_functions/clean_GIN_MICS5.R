
clean_GIN_MICS5 <- function(survey_var) {

  # Import MICS data files
  wm <- read_sav("data/mics/raw_data/Guinea_MICS5_Datasets/Guinea MICS 2016 SPSS Datasets/wm.sav")
  hh <- read_sav("data/mics/raw_data/Guinea_MICS5_Datasets/Guinea MICS 2016 SPSS Datasets/hh.sav")
  bh <- read_sav("data/mics/raw_data/Guinea_MICS5_Datasets/Guinea MICS 2016 SPSS Datasets/bh.sav")
  
  # Create data dictionaries
  wm.dict <- labelled::generate_dictionary(wm)
  hh.dict <- labelled::generate_dictionary(hh)
  bh.dict <- labelled::generate_dictionary(bh)
  
  # Write data dictionaries to excel file
  write_xlsx(wm.dict, "data/mics/data_dictionaries/GIN_MICS5_wm_dict.xlsx")
  write_xlsx(hh.dict, "data/mics/data_dictionaries/GIN_MICS5_hh_dict.xlsx")
  write_xlsx(bh.dict, "data/mics/data_dictionaries/GIN_MICS5_bh_dict.xlsx")
  
  # Merge womens and household survey
  mics <- merge(wm, hh, by=c("HH1", "HH2"), suffixes = c("", ".hh"))
  
  # Get only first born children
  bh <- subset(bh, BHLN==1)
  
  # Merge mics data and birth history data
  mics <- merge(mics, bh, by=c("HH1", "HH2", "LN"), all.x=TRUE, suffixes = c("", ".bh"))
  
  # Remove any incomplete surveys
  table(mics$WM7)
  val_labels(mics$WM7)
  mics <- subset(mics, WM7==1)
  
  # Create new data frame to store clean data
  data <- as.data.frame(matrix(nrow=nrow(mics), ncol=nrow(survey_var)))
  colnames(data) <- survey_var$Name
  
  # Meta data -----------------------------------
  
  # Survey information
  data$iso3 <- "GIN"
  data$survey <- "MICS"
  data$phase <- 5
  data$surveyid <- "GIN_MICS5"
  
  # Check if the sample contains only ever married women
  val_labels(mics$MSTATUS)
  table(mics$MSTATUS)
  data$samp <- "All Women" 
  
  # Household data -------------------------------
  
  # Cluster number
  data$clust <- as.numeric(mics$HH1)
  summary(data$clust)
  
  # Household number
  data$hh <- as.numeric(mics$HH2)
  summary(data$hh)
  
  # Women's line number
  data$line <- as.numeric(mics$LN)
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
  data$psu <- as.numeric(mics$PSU)
  summary(data$psu)
  data$strata <- as.numeric(mics$stratum)
  summary(data$strata)
  
  # Sub-national region names
  data$region <- tolower(as.character(as_factor(mics$HH7)))
  data$prov <- NA
  data$dist <- NA
  
  # Rural-urban
  data$res <- ifelse(mics$HH6==1, "urban", ifelse(mics$HH6==2, "rural", NA))
  data$res <- as.factor(data$res)
  table(data$res, useNA = "always")
  
  # Individual information ---------------------------------------
  
  # Birth month
  data$birth_m <- as.numeric(mics$WB1M)
  data[data$birth_m%in%c(97,98,99), "birth_m"] <- NA
  summary(data$birth_m)
  
  # Birth year
  data$birth_y <- as.numeric(mics$WB1Y)
  data[data$birth_y%in%c(9997,9998,9999), "birth_y"] <- NA
  summary(data$birth_y)
  
  # Age at interview
  data$age <- as.numeric(mics$WB2)
  summary(data$age)
  
  # Religion
  data$relig <- tolower(as.character(as_factor(mics$religion)))
  data$relig <- ifelse(data$relig=="animiste", "animist", data$relig)
  data$relig <- ifelse(data$relig=="autre religion", "other religion", data$relig)
  data$relig <- ifelse(data$relig=="chrétien", "christian", data$relig)
  data$relig <- ifelse(data$relig=="musulman", "muslim", data$relig)
  data$relig <- ifelse(data$relig=="sans religion", "no religion", data$relig)
  table(data$relig, useNA = "always")
  
  # Ethnicity
  data$ethn <- tolower(as.character(as_factor(mics$ethnicity)))
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
  data$mar_age <- as.numeric(mics$MA9)
  data[data$mar_age%in%c(97,98,99), "mar_age"] <- NA
  summary(data$mar_age)
  
  # Other info ------------------------------------------
  
  # Ever given birth
  data$birth1_ev <- as.numeric(mics$CM1)
  data$birth1_ev <- ifelse(data$birth1_ev==1, 1,
                           ifelse(data$birth1_ev==2, 0, NA))
  table(data$birth1_ev, useNA = "always")
  
  # First child's month of birth
  data$birth1_m <- as.numeric(mics$CM2M)
  data[data$birth1_m%in%c(97,98,99), "birth1_m"] <- NA
  summary(data$birth1_m)
  
  # First child's year of birth
  data$birth1_y <- as.numeric(mics$CM2Y)
  data[data$birth1_y%in%c(9997,9998,9999), "birth1_y"] <- NA
  summary(data$birth1_y)
  
  # Age of first child at interview
  data$birth1_age <- as.numeric(mics$CM3)
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
