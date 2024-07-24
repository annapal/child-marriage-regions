
clean_MOZ_MICS3 <- function(survey_var) {

  # Import MICS data files
  wm <- read_sav("data/mics/raw_data/Mozambique MICS 2008 Datasets/wm.sav")
  hh <- read_sav("data/mics/raw_data/Mozambique MICS 2008 Datasets/hh.sav")
  bh <- read_sav("data/mics/raw_data/Mozambique MICS 2008 Datasets/bh.sav")
  
  # Create data dictionaries
  wm.dict <- labelled::generate_dictionary(wm)
  hh.dict <- labelled::generate_dictionary(hh)
  bh.dict <- labelled::generate_dictionary(bh)
  
  # Write data dictionaries to excel file
  write_xlsx(wm.dict, "data/mics/data_dictionaries/MOZ_MICS3_wm_dict.xlsx")
  write_xlsx(hh.dict, "data/mics/data_dictionaries/MOZ_MICS3_hh_dict.xlsx")
  write_xlsx(bh.dict, "data/mics/data_dictionaries/MOZ_MICS3_bh_dict.xlsx")
  
  # Merge womens and household survey
  mics <- merge(wm, hh, by=c("HH1", "HH2"), suffixes = c("", ".hh"))
  
  # Get only first born children
  bh <- subset(bh, HN9==1)
  
  # Merge womens and birth history data of first born child
  mics <- merge(mics, bh, by=c("XX1", "MEMID"), all.x=TRUE, suffixes = c("", ".bh"))
  
  # Remove any incomplete surveys
  table(mics$WM7)
  val_labels(mics$WM7)
  mics <- subset(mics, WM7==1)
  
  # Create new data frame to store clean data
  data <- as.data.frame(matrix(nrow=nrow(mics), ncol=nrow(survey_var)))
  colnames(data) <- survey_var$Name
  
  # Meta data -----------------------------------
  
  # Survey information
  data$iso3 <- "MOZ"
  data$survey <- "MICS"
  data$phase <- 3
  data$surveyid <- "MOZ_MICS3"
  
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
  data$wt <- as.numeric(mics$WMWEIGHT)
  summary(data$wt)
  
  # Interview month and year
  # Again check there is no missing data
  data$int_m <- as.numeric(mics$WM6M)
  summary(data$int_m)
  data$int_y <- as.numeric(mics$WM6Y)
  summary(data$int_y)
  
  # Primary sampling unit
  data$psu <- data$clust
  
  # Sub-national region names
  data$region <- tolower(as.character(as_factor(mics$HH7)))
  data$prov <- NA
  data$dist <- NA
  
  # Rural-urban
  data$res <- ifelse(mics$HH6==1, "urban", ifelse(mics$HH6==2, "rural", NA))
  data$res <- as.factor(data$res)
  table(data$res, useNA = "always")
  
  # Strata
  data$strata <- as.numeric(factor(paste(data$region, data$res)))
  
  # Individual information ---------------------------------------
  
  # Birth month
  data$birth_m <- as.numeric(mics$WM8M)
  data[data$birth_m%in%c(97,98,99), "birth_m"] <- NA
  summary(data$birth_m)
  
  # Birth year
  data$birth_y <- as.numeric(mics$WM8A)
  data[data$birth_y%in%c(9997,9998,9999), "birth_y"] <- NA
  summary(data$birth_y)
  
  # Age at interview
  data$age <- as.numeric(mics$WM9)
  summary(data$age)
  
  # Religion
  data$relig <- tolower(as.character(as_factor(mics$HC1A)))
  data$relig <- ifelse(data$relig=="anglicana", "anglican", data$relig)
  data$relig <- ifelse(data$relig=="católica", "catholic", data$relig)
  data$relig <- ifelse(data$relig=="evangélica/pentecostal", "evangelical/pentecostal", data$relig)
  data$relig <- ifelse(data$relig=="islâmica", "islamic", data$relig)
  data$relig <- ifelse(data$relig=="outra religião", "other religion", data$relig)
  data$relig <- ifelse(data$relig=="sem info", NA, data$relig)
  data$relig <- ifelse(data$relig=="sem religião (ateu, agnóstico,  animista)", "no religion", data$relig)
  data$relig <- ifelse(data$relig=="zione/sião", "zion", data$relig)
  table(data$relig, useNA = "always") # Missing values OK. Sometimes missing for entire sample
  
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
  data$mar_status <- ifelse(mics$MSTATUS==1, "currently", 
                            ifelse(mics$MSTATUS==2, "formerly", 
                                   ifelse(mics$MSTATUS==3, "never", NA)))
  data$mar_status <- as.factor(data$mar_status)
  table(data$mar_status, useNA = "always")
  
  # Marriage month
  data$mar_m <- as.numeric(mics$MA6_MES)
  data[data$mar_m%in%c(17,97,98,99), "mar_m"] <- NA
  summary(data$mar_m)
  
  # Marriage year
  data$mar_y <- as.numeric(mics$MA6_ANO)
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
  data$birth1_m <- NA
  
  # First child's year of birth
  # Estimate from age when had first child
  age <- as.numeric(mics$CM1A)
  age <- ifelse(age%in%c(97,98,99), NA, age)
  data$birth1_y <- data$birth_y + age
  summary(data$birth1_y)
  
  # Age of first child at interview
  data$birth1_age <- NA
  
  # Ever had sex
  data$sex_ev <- ifelse(mics$MA8A==9, NA, 
                        ifelse(mics$MA8A==2, 0, 1))
  table(data$sex_ev, useNA="always") # Missing values OK
  
  # Age at first sexual intercourse
  data$sex_age <- as.numeric(mics$SB1)
  data$sex_age <- ifelse((data$sex_age%in%c(98,99))|(data$sex_age==0), NA, 
                         ifelse(data$sex_age==95, data$mar_age, data$sex_age))
  summary(data$sex_age)
  
  # Fix sex ever variable
  data$sex_ev <- ifelse(!is.na(data$sex_age), 1, data$sex_ev)
  
  data
  
}