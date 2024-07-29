
denorm_weights <- function(data, pop_data, iso) {
  
  # Get population data for country
  pop_data_country <- pop_data %>%
    filter(ISO3_code == iso, Time %in% unique(data$int_y)) %>%
    rename(iso3 = ISO3_code, int_y = Time, Population = PopFemale)
  
  if (iso=="XKO") {
    pop_data_country <- pop_data %>%
      filter(ISO3_code == "XKX", Time %in% unique(data$int_y)) %>%
      rename(iso3 = ISO3_code, int_y = Time, Population = PopFemale)
  }
  
  # Get sample sizes for surveys
  samp_sizes <- as.data.frame(table(data$surveyid))
  colnames(samp_sizes) <- c("surveyid", "Sample")
  
  # Create dataset to merge
  dat_merge <- pop_data_country %>%
    left_join(distinct(data, surveyid, int_y), by = "int_y") %>%
    group_by(surveyid) %>%
    summarise(Population = mean(Population)) %>%
    left_join(samp_sizes, by = "surveyid") %>%
    mutate(Denorm_Factor = Population / Sample,
           Survey_Identifier = (1:length(unique(data$surveyid)))*100000)
  
  # Merge with country data
  data <- merge(data, dat_merge, all.x=TRUE)
  data$Denorm_Wt <- data$wt*data$Denorm_Factor
  
  # Make PSU and Strata unique between surveys
  data$Strata_2 <- data$strata + data$Survey_Identifier
  data$PSU_2 <- data$psu + data$Survey_Identifier
  
  data
}