
clean_SOM_MICS3 <- function(survey_var) {

  # Import MICS data files
  wm <- read_sav("data/mics/raw_data/Somalia 2006 MICS_Datasets/Somalia MICS 2006 SPSS Datasets/wm.sav")
  hh <- read_sav("data/mics/raw_data/Somalia 2006 MICS_Datasets/Somalia MICS 2006 SPSS Datasets/hh.sav")
  bh <- read_sav("data/mics/raw_data/Somalia 2006 MICS_Datasets/Somalia MICS 2006 SPSS Datasets/bh.sav")
  
  # Create data dictionaries
  wm.dict <- labelled::generate_dictionary(wm)
  hh.dict <- labelled::generate_dictionary(hh)
  bh.dict <- labelled::generate_dictionary(bh)
  
  # Write data dictionaries to excel file
  write_xlsx(wm.dict, "data/mics/data_dictionaries/SOM_MICS3_wm_dict.xlsx")
  write_xlsx(hh.dict, "data/mics/data_dictionaries/SOM_MICS3_hh_dict.xlsx")
  write_xlsx(bh.dict, "data/mics/data_dictionaries/SOM_MICS3_bh_dict.xlsx")
  
  # Merge womens and household survey
  mics <- merge(wm, hh, by=c("HH1", "HH2"), suffixes = c("", ".hh"))
  
  # Subset first born children
  bh <- subset(bh, BH1==1)
  
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
  data$iso3 <- "SOM"
  data$survey <- "MICS"
  data$phase <- 3
  data$surveyid <- "SOM_MICS3"
  
  # Check if the sample contains only ever married women
  val_labels(mics$mstatus)
  table(mics$mstatus)
  data$samp <- "All Women" 
  
  # Household data -------------------------------
  
  # Cluster number
  data$clust <- as.numeric(mics$WM1)
  summary(data$clust)
  
  # Household number
  data$hh <- as.numeric(mics$WM2)
  summary(data$hh)
  
  # Women's line number
  data$line <- as.numeric(mics$WM4)
  summary(data$line)
  
  # Women's survey weight
  data$wt <- as.numeric(mics$wmweight)
  summary(data$wt)
  
  # Interview month and year
  data$int_m <- as.numeric(mics$WM6M)
  summary(data$int_m)
  data$int_y <- as.numeric(mics$WM6Y)
  summary(data$int_y)
  
  # Primary sampling unit
  data$psu <- as.numeric(mics$WM1)
  
  # Sub-national region names
  data$region <- tolower(as.character(as_factor(mics$HH7)))
  data$prov <- tolower(as.character(as_factor(mics$HH7A)))
  data$dist <- NA
  
  # Rural-urban place of residence
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
  
  data
  
}