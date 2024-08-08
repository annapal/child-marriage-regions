
# Convert the merged data into long format
# I.e. create an entry for each person-year at risk of child marriage
# Includes calendar years in which a girl turned 13-17

create_long_data <- function() {
  
  # Get list of countries that have data
  country_cdes <- read_excel("data/ref_data/country-codes.xlsx")
  
  # Create directory for long dataframes
  if (!dir.exists("data/long_data")){
    dir.create("data/long_data")
  }
  
  for (iso in country_cdes$iso3) {
    
    data <- readRDS(paste0("data/wide_data/", iso, ".rds")) # Get the data for a country
    data$case_id <- paste(data$surveyid, data$case_id, sep="_") # Make case_id unique
    
    # Add rows for each year between birth_y+13 and birth_y+17
    dat_long <- data %>%
      rowwise() %>%
      mutate(year = list(seq(birth_y+13, birth_y+17))) %>%
      unnest(cols = c(year))
    
    merge_dat <- dat_long %>%
      # Calculate the age the person turned in each year
      mutate(age_turned = year - birth_y) %>%
      # Create variable to indicate if person was married in that year 
      mutate(married = case_when(
        mar_status == "never" ~ 0,
        TRUE ~ as.integer(between(mar_y, birth_y, year)))) %>%
      # Filter years that occurred after int_y - 1
      filter(year <= (int_y-1)) %>%
      # Sort the data by case_id and year
      arrange(case_id, year)
    
    # Remove rows where people are already married
    final_data <- merge_dat %>%
      filter(!is.na(married)) %>% # Remove those with missing marriage status
      group_by(case_id) %>%
      filter((married==0)|(row_number() <= which.max(married))) %>%
      # Remove those who married in the years before the year they turned 13
      filter(!(year > mar_y & age_turned == 13)) %>%
      arrange(case_id, year)
    
    # Get age at which 95% of women are married
    mar_min <- read.csv("data/ref_data/min_mar_age.csv")
    age_val <- as.numeric(mar_min[which(mar_min$iso3==iso), "AgeEnd"])

    final_data <- final_data %>%
      # If ever-married sample, exclude women who are interviewed at a young age
      filter(if (length(age_val) == 0) (samp == "All Women")
             else ((samp == "All Women")|(samp == "Ever Married Women" & age >= age_val))) %>%
      # Remove any observations with 0 wt
      filter(wt!=0)
    
    # Save long data
    saveRDS(final_data, file=paste0("data/long_data/", iso, ".rds"))
  }
}
