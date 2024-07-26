## Load packages
source("./packages.R")

## Load R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

# Configuration for accessing DHS data using rdhs
# You will need to add your own credentials to access the data
config <- set_rdhs_config(
  email = "anna.palmer@mail.mcgill.ca",
  project = "Generating evidence on climate change and child marriage using satellite, environmental, and social data",
  config_path = "rdhs.json", global = FALSE,
  cache_path = paste0(getwd(), "/data/dhs")
)

# pipeline ----------------------------------------------------------------

# Clean MICS surveys
mics_data = clean_mics(surveys = "all")

# Impute MICS data
mics_data_imp = pblapply(mics_data, impute_mics)
save(mics_data_imp, file = "data/mics/clean_data_mics.RData")
# load("data/mics/clean_data_mics.RData")

# Clean DHS
dhs_data <- clean_dhs(surveys="all", download=TRUE, config)
save(dhs_data, file = "data/dhs/clean_data_dhs.RData")
# load("data/dhs/clean_data_dhs.RData")

# Merge datasets
# make_wbs(dhs_data, mics_data_imp) # Create workbooks for matching if needed
merged_data <- merge_data(dhs_data, mics_data_imp)
save(merged_data, file = "data/merged/clean_data_merged.RData")
# load("data/merged/clean_data_merged.RData")

# Create long datasets
long_data <- create_long_data(merged_data)
save(long_data, file = "data/merged/clean_data_long.RData")
# load("data/merged/clean_data_long.RData")



