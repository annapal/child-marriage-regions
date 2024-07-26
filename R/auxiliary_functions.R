
# Calculate CMC from month and year
cmc <- function(month, year) {
  return((year - 1900)*12 + month)}

# Calcualte persian CMC
cmc_persian <- function(month, year) {
  return((year - 1300)*12 + month)}

# Assesses whether CMC intervals overlap
overlap <- function(lb1, ub1, lb2, ub2) {
  return((pmin(lb1, ub1) <= pmax(lb2, ub2)) &
           (pmax(lb1, ub1) >= pmin(lb2, ub2)))}

# Get calendar month from CMC
cmc_m <- function(cmc_dat) {
  month = cmc_dat %% 12
  month <- replace(month, month == 0, 12)
  return(month)
}

# Get calendar year from CMC
cmc_y <- function(cmc_dat) {
  return(1900+(cmc_dat-cmc_m(cmc_dat))/12)
}

# Randomly select integer between two integers
resample <- function(x) x[sample.int(length(x), 1)]

# Convert 2-digit years to 4-digit years
fix_year <- function(years, iso) {
  # Check variable is not completely missing
  if (!all(is.na(years))) {
    # Extract one year to check if two digit or 4 digit
    test_yr <- sample(years[!is.na(years)], 1)
    tw_dg <- nchar(as.character(test_yr))<=2
    
    # If 2-digit add 1900 or 2000 to years
    if (tw_dg) {
      if (iso=="NPL") {
        years <- ifelse(years>80, years+1900, years+2000)
      } else if (iso=="ETH") {
        years <- ifelse(years>15, years+1900, years+2000)
      } else {
        years <- ifelse(years>23, years+1900, years+2000)
      }
      
    }
  }
  return(years)
}

# Function to convert nepal dates to gregorian dates
convert_npl <- function(month, year) {
  cmc <- cmc(month, year)
  cmc2 <- nepal_to_greg(cmc=cmc)
  month2 <- cmc_m(cmc2)
  year2 <- cmc_y(cmc2)
  return(cbind(month2, year2))
}

# Function to convert ethiopian dates to gregorian dates
convert_eth <- function(month, year) {
  cmc <- cmc(month, year)
  cmc2 <- ethiopian_to_greg(cmc=cmc)
  month2 <- cmc_m(cmc2)
  year2 <- cmc_y(cmc2)
  return(cbind(month2, year2))
}

# Function to convert persian dates to gregorian dates
convert_afg <- function(month, year) {
  cmc <- cmc_persian(month, year)
  cmc2 <- persian_to_greg(cmc=cmc)
  month2 <- cmc_m(cmc2)
  year2 <- cmc_y(cmc2)
  return(cbind(month2, year2))
}

# Correct date imputation codes for DHS0
fix_imp_cde <- function(imp_cde, survey_wave) {
  if (survey_wave=="0") {
    imp_cde <- case_when(
      imp_cde==1 ~ 1,
      imp_cde==2 ~ 3,
      imp_cde==3 ~ 5,
      imp_cde==4 ~ 6,
      imp_cde==5 ~ 7,
      imp_cde==6 ~ 8)
  }
  return(imp_cde)
}
