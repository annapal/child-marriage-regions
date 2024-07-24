
clean_BEN_MICS5 <- function(survey_var) {

  # Import MICS data files
  wm <- read_sav("data/mics/raw_data/Benin_MICS5_Datasets/Benin MICS 2014 SPSS Datasets/wm.sav")
  hh <- read_sav("data/mics/raw_data/Benin_MICS5_Datasets/Benin MICS 2014 SPSS Datasets/hh.sav")
  bh <- read_sav("data/mics/raw_data/Benin_MICS5_Datasets/Benin MICS 2014 SPSS Datasets/bh.sav")
  
  # Create data dictionaries
  wm.dict <- labelled::generate_dictionary(wm)
  hh.dict <- labelled::generate_dictionary(hh)
  bh.dict <- labelled::generate_dictionary(bh)
  
  # Write data dictionaries to excel file
  write_xlsx(wm.dict, "data/mics/data_dictionaries/BEN_MICS5_wm_dict.xlsx")
  write_xlsx(hh.dict, "data/mics/data_dictionaries/BEN_MICS5_hh_dict.xlsx")
  write_xlsx(bh.dict, "data/mics/data_dictionaries/BEN_MICS5_bh_dict.xlsx")
  
  # Merge womens and household survey
  mics <- merge(wm, hh, by=c("HH1", "HH2"), suffixes = c("", ".hh"))
  
  # Subset first born children
  bh <- subset(bh, BHLN==1)
  
  # Merge mics data and birth history data
  mics <- merge(mics, bh, by=c("HH1", "HH2", "LN"), all.x=TRUE, suffixes = c("", ".bh"))
  
  # Remove any incomplete surveys
  mics <- subset(mics, WM7==1)
  
  # Create new data frame to store clean data
  data <- as.data.frame(matrix(nrow=nrow(mics), ncol=nrow(survey_var)))
  colnames(data) <- survey_var$Name
  
  # Meta data -----------------------------------
  
  # Survey information
  data$iso3 <- "BEN"
  data$survey <- "MICS"
  data$phase <- 5
  data$surveyid <- "BEN_MICS5"
  
  # Check if the sample contains only ever married women
  data$samp <- "All Women" 
  
  # Household data -------------------------------
  
  # Cluster number
  data$clust <- as.numeric(mics$HH1)
  
  # Household number
  data$hh <- as.numeric(mics$HH2)
  
  # Women's line number
  data$line <- as.numeric(mics$LN)
  
  # Women's survey weight
  data$wt <- as.numeric(mics$wmweight)
  
  # Interview month and year
  data$int_m <- as.numeric(mics$WM6M)
  data$int_y <- as.numeric(mics$WM6Y)
  
  # Primary sampling unit and strata
  data$psu <- as.numeric(mics$PSU)
  data$strata <- as.numeric(mics$stratum)
  
  # Sub-national region names
  data$region <- tolower(as.character(as_factor(mics$HH7)))
  data$prov <- NA
  data$dist <- NA
  
  # Rural-urban place of residence
  data$res <- ifelse(mics$HH6==1, "urban", ifelse(mics$HH6==2, "rural", NA))
  data$res <- as.factor(data$res)
  
  # Individual information ---------------------------------------
  
  # Birth month
  data$birth_m <- as.numeric(mics$WB1M)
  data[data$birth_m%in%c(97,98,99), "birth_m"] <- NA
  
  # Birth year
  data$birth_y <- as.numeric(mics$WB1Y)
  data[data$birth_y%in%c(9997,9998,9999), "birth_y"] <- NA
  
  # Age at interview
  data$age <- as.numeric(mics$WB2)
  
  # Religion
  data$relig <- tolower(as.character(as_factor(mics$HC1A)))
  data$relig <- ifelse(data$relig=="non déclaré/pas de réponse", NA, data$relig)
  data$relig <- ifelse(data$relig=="autre religion", "other religion", data$relig)
  data$relig <- ifelse(data$relig=="autres chrétiens", "other christian religion", data$relig)
  data$relig <- ifelse(data$relig=="autres protestants", "other protestant religion", data$relig)
  data$relig <- ifelse(data$relig=="autres traditionnelles", "other traditional religion", data$relig)
  data$relig <- ifelse(data$relig=="catholique", "catholic", data$relig)
  data$relig <- ifelse(data$relig=="céleste", "celestial church", data$relig)
  data$relig <- ifelse(data$relig=="pas de religion", "no religion", data$relig)
  data$relig <- ifelse(data$relig=="protestant méthodiste", "methodist protestant", data$relig)
  data$relig <- ifelse(data$relig=="vodoun", "vodun", data$relig)
  
  # Ethnicity
  data$ethn <- tolower(as.character(as_factor(mics$HC1C))) 
  
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
  data$mar_age <- as.numeric(mics$MA9)
  data[data$mar_age%in%c(97,98,99), "mar_age"] <- NA
  
  # Other info ------------------------------------------
  
  # Ever given birth
  data$birth1_ev <- as.numeric(mics$CM1)
  data$birth1_ev <- ifelse(data$birth1_ev==1, 1,
                           ifelse(data$birth1_ev==2, 0, NA))
  
  # First child's month of birth
  data$birth1_m <- as.numeric(mics$BH4M)
  data[data$birth1_m%in%c(97,98,99), "birth1_m"] <- NA
  
  # First child's year of birth
  data$birth1_y <- as.numeric(mics$BH4Y)
  data[data$birth1_y%in%c(9997,9998,9999), "birth1_y"] <- NA
  
  # Age of first child at last birthday
  data$birth1_age <- as.numeric(mics$BH6)
  data[data$birth1_age%in%c(97,98,99), "birth1_age"] <- NA
  
  # Ever had sex
  data$sex_ev <- ifelse(mics$SB1==99|mics$SB1==97, NA, 
                        ifelse(mics$SB1==0, 0, 
                               ifelse(mics$SB1>0, 1, NA)))
  
  # Age at first sexual intercourse
  data$sex_age <- as.numeric(mics$SB1)
  data$sex_age <- ifelse((data$sex_age==99)|(data$sex_age==0)|(data$sex_age==97), NA, 
                         ifelse(data$sex_age==95, data$mar_age, data$sex_age))
  
  data
}
