
clean_JAM_MICS4 <- function(survey_var) {

  # Import MICS data files
  wm <- read_sav("data/mics/raw_data/Jamaica_MICS4_Datasets/Jamaica MICS 2011 SPSS Datasets/wm.sav")
  hh <- read_sav("data/mics/raw_data/Jamaica_MICS4_Datasets/Jamaica MICS 2011 SPSS Datasets/hh.sav")
  
  # Create data dictionaries
  wm.dict <- labelled::generate_dictionary(wm)
  hh.dict <- labelled::generate_dictionary(hh)
  
  # Write data dictionaries to excel file
  write_xlsx(wm.dict, "data/mics/data_dictionaries/JAM_MICS4_wm_dict.xlsx")
  write_xlsx(hh.dict, "data/mics/data_dictionaries/JAM_MICS4_hh_dict.xlsx")
  
  # Merge womens and household survey
  mics <- merge(wm, hh, by=c("HH1", "HH2"), suffixes = c("", ".hh"))
  
  # Remove any incomplete surveys
  table(mics$WM7)
  val_labels(mics$WM7)
  mics <- subset(mics, WM7==1)
  
  # Create new data frame to store clean data
  data <- as.data.frame(matrix(nrow=nrow(mics), ncol=nrow(survey_var)))
  colnames(data) <- survey_var$Name
  
  # Meta data -----------------------------------
  
  # Survey information
  data$iso3 <- "JAM"
  data$survey <- "MICS"
  data$phase <- 4
  data$surveyid <- "JAM_MICS4"
  
  # Check if the sample contains only ever married women
  val_labels(mics$MSTATUS)
  table(mics$MSTATUS)
  data$samp <- "All Women" 
  
  # Household data -------------------------------
  
  # Cluster number
  data$clust <- as.numeric(mics$WM1)
  
  # Household number
  data$hh <- as.numeric(mics$WM2)
  
  # Women's line number
  data$line <- as.numeric(mics$WM4)
  
  # Women's survey weight
  data$wt <- as.numeric(mics$wmweight)
  
  # Interview month and year
  data$int_m <- as.numeric(mics$WM6M)
  data$int_y <- as.numeric(mics$WM6Y)
  
  # Primary sampling unit and strata
  data$psu <- as.numeric(mics$PSU)
  data$strata <- as.numeric(mics$stratum)
  
  # Sub-national region names
  data$region <- case_when(
    mics$HH1A == 1 ~ "Kingston",
    mics$HH1A == 2 ~ "St. Andrew",
    mics$HH1A == 3 ~ "St. Thomas",
    mics$HH1A == 4 ~ "Portland",
    mics$HH1A == 5 ~ "St. Mary",
    mics$HH1A == 6 ~ "St. Ann",
    mics$HH1A == 7 ~ "Trelawny",
    mics$HH1A == 8 ~ "St. James",
    mics$HH1A == 9 ~ "Hanover",
    mics$HH1A == 10 ~ "Westmoreland",
    mics$HH1A == 11 ~ "St. Elizabeth",
    mics$HH1A == 12 ~ "Manchester",
    mics$HH1A == 13 ~ "Clarendon",
    mics$HH1A == 14 ~ "St. Catherine"
  )
  data$prov <- NA
  data$dist <- NA
  
  # Rural-urban place of residence
  data$res <- ifelse(mics$HH6%in%c(1, 3), "urban", ifelse(mics$HH6==2, "rural", NA))
  data$res <- as.factor(data$res)
  
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
  data$relig <- tolower(as.character(as_factor(mics$HC1A)))
  data$relig <- ifelse(data$relig=="missing", NA, data$relig)
  table(data$relig, useNA = "always")
  
  # Ethnicity
  data$ethn <- tolower(as.character(as_factor(mics$HC1C)))
  data$ethn <- ifelse(data$ethn=="missing", NA, data$ethn)
  table(data$ethn, useNA = "always") 
  
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
  
  data
  
}
