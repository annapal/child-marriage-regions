## Load packages
source("./packages.R")

## Load R files
lapply(list.files("./R", full.names = TRUE, recursive = TRUE), source)

# Config for accessing DHS data using rdhs package
# You will need to add your own credentials to access the data
config <- set_rdhs_config(
  email = "anna.palmer@mail.mcgill.ca",
  project = "Generating evidence on climate change and child marriage using satellite, environmental, and social data",
  config_path = "rdhs.json", global = FALSE,
  cache_path = paste0(getwd(), "/data/dhs")
)

# pipeline ----------------------------------------------------------------

# Clean MICS surveys
mics_data = clean_mics()

# Impute MICS data
mics_data_imp = pblapply(mics_data, impute_mics)

# Download and clean DHS surveys
dhs_data <- clean_dhs(surveys="all", download=FALSE, config)

# Merge datasets for each country
merge_data(dhs_data, mics_data_imp)

# Create long datasets
create_long_data()

