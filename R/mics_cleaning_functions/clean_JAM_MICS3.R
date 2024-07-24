
clean_JAM_MICS3 <- function(survey_var) {

  # Import MICS data files
  wm <- read_sav("data/mics/raw_data/Jamaica 2005 MICS_Datasets/Jamaica MICS 2005 SPSS Datasets/wm.sav")
  hh <- read_sav("data/mics/raw_data/Jamaica 2005 MICS_Datasets/Jamaica MICS 2005 SPSS Datasets/hh.sav")
  
  # Create data dictionaries
  wm.dict <- labelled::generate_dictionary(wm)
  hh.dict <- labelled::generate_dictionary(hh)
  
  # Write data dictionaries to excel file
  write_xlsx(wm.dict, "data/mics/data_dictionaries/JAM_MICS3_wm_dict.xlsx")
  write_xlsx(hh.dict, "data/mics/data_dictionaries/JAM_MICS3_hh_dict.xlsx")
  
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
  data$phase <- 3
  data$surveyid <- "JAM_MICS3"
  
  # Check if the sample contains only ever married women
  val_labels(mics$mstatus)
  table(mics$mstatus)
  data$samp <- "All Women" 
  
  # Household data -------------------------------
  
  # Cluster number
  data$clust <- as.numeric(mics$HH1)
  
  # Household number
  data$hh <- as.numeric(mics$HH2)
  
  # Women's line number
  data$line <- as.numeric(mics$WM4)
  
  # Women's survey weight
  data$wt <- as.numeric(mics$wmweight)
  
  # Interview month and year
  data$int_m <- as.numeric(mics$WM6M)
  data$int_y <- as.numeric(mics$WM6Y)
  
  # Primary sampling unit
  data$psu <- as.numeric(as.factor(mics$HH1))
  
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
  data$res <- ifelse(mics$HH6%in%c(1, 2), "urban", ifelse(mics$HH6==3, "rural", NA))
  data$res <- as.factor(data$res)
  
  # Strata
  data$strata <- as.numeric(factor(paste(data$region, data$res)))
  
  # Individual information ---------------------------------------
  
  # Birth month
  data$birth_m <- as.numeric(mics$WM8M)
  data[data$birth_m%in%c(97,98,99), "birth_m"] <- NA
  summary(data$birth_m)
  
  # Birth year
  data$birth_y <- as.numeric(mics$WM8Y)
  data[data$birth_y%in%c(9997,9998,9999), "birth_y"] <- NA
  summary(data$birth_y)
  
  # Age at interview
  data$age <- as.numeric(mics$WM9)
  summary(data$age)
  
  # Marriage information ------------------------------------------
  
  # Current marriage status
  data$mar_status <- ifelse(mics$mstatus==1, "currently", 
                            ifelse(mics$mstatus==2, "formerly", 
                                   ifelse(mics$mstatus==3, "never", NA)))
  data$mar_status <- as.factor(data$mar_status)
  table(data$mar_status, useNA = "always")
  
  # Marriage month
  data$mar_m <- as.numeric(mics$MA6M)
  data[data$mar_m%in%c(97,98,99), "mar_m"] <- NA
  summary(data$mar_m)
  
  # Marriage year
  data$mar_y <- as.numeric(mics$MA6Y)
  data[data$mar_y%in%c(9997,9998,9999), "mar_y"] <- NA
  summary(data$mar_y)
  
  # Marriage age
  data$mar_age <- as.numeric(mics$MA8)
  data[data$mar_age%in%c(97,98,99), "mar_age"] <- NA
  summary(data$mar_age)
  
  # Other info ------------------------------------------
  
  # Ever given birth
  data$birth1_ev <- as.numeric(mics$CM1)
  data$birth1_ev <- ifelse(data$birth1_ev==1, 1,
                           ifelse(data$birth1_ev==2, 0, NA))
  table(data$birth1_ev, useNA = "always")
  
  # First child's month of birth
  data$birth1_m <- as.numeric(mics$CM2AM)
  data[data$birth1_m%in%c(97,98,99), "birth1_m"] <- NA
  summary(data$birth1_m)
  
  # First child's year of birth
  data$birth1_y <- as.numeric(mics$CM2AY)
  data[data$birth1_y%in%c(9997,9998,9999), "birth1_y"] <- NA
  summary(data$birth1_y)
  
  # Age of first child at last birthday
  data$birth1_age <- as.numeric(mics$CM2B)
  data[data$birth1_age%in%c(97,98,99), "birth1_age"] <- NA
  summary(data$birth1_age)
  
  data

}
