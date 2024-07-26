
# Read in the DHS data

read_dhs <- function(survey_id) {
  
  # If data isn't available from rdhs, import it here
  if (survey_id=="ET2019DHS") {
    data <- read_dta("data/dhs/missing_data_files/ETIR81DT/ETIR81FL.DTA")
  } else if (survey_id=="GA2000DHS") {
    data <- read_dta("data/dhs/missing_data_files/GAIR41DT/GAIR41FL.DTA", encoding = "latin1")
  } else if (survey_id=="IA1993DHS") {
    data <- read_dta("data/dhs/missing_data_files/IAIR23DT/IAIR23FL.DTA")
  } else if (survey_id=="IA2015DHS") {
    data <- read_dta("data/dhs/missing_data_files/IAIR74DT/IAIR74FL.DTA")
  } else if (survey_id=="IA2020DHS") {
    data <- read_dta("data/dhs/missing_data_files/IAIR7EDT/IAIR7EFL.DTA") 
  } else if (survey_id=="PE2004DHS") {
    data <- read_dta("data/dhs/missing_data_files/PEIR51DT/PEIR51FL.DTA") 
  } else if (survey_id=="RW2008DHS") {
    data <- read_dta("data/dhs/missing_data_files/RWIR5ADT/RWIR5AFL.DTA") 
  } else if (survey_id=="TR1998DHS") {
    data <- read_dta("data/dhs/missing_data_files/TRIR41DT/TRIR41FL.DTA", encoding = "latin1") 
  } else if (survey_id=="ZW2015DHS") {
    data <- read_dta("data/dhs/missing_data_files/ZWIR72DT/ZWIR72FL.DTA") 
  } else if (survey_id=="BF2021DHS") {
    data <- read_dta("data/dhs/missing_data_files/BFIR81DT/BFIR81FL.DTA") 
  } else {
    # Otherwise import the data downloaded using rdhs
    data <- readRDS(paste0("data/dhs/raw_data/", survey_id, ".rds"))
  }
  data # Return the dataframe
}

# Read in the DHS geocoded data

read_dhs_geo <- function(survey_id) {
  
  # Geodata files
  files <- str_sub(list.files("data/dhs/geo_data/"), 1,9)
  
  # If geodata isn't available from rdhs, import it here
  if (survey_id=="ET2019DHS") {
    geo_dat <- read_sf(dsn=paste0(getwd(),"/data/dhs/missing_shp_files/ETGE81FL/"), layer="ETGE81FL")
  } else if (survey_id=="NP2022DHS") {
    geo_dat <- read_sf(dsn=paste0(getwd(),"/data/dhs/missing_shp_files/NPGE82FL/"), layer="NPGE82FL")
  } else if (survey_id=="IA2015DHS") {
    geo_dat <- read_sf(dsn=paste0(getwd(),"/data/dhs/missing_shp_files/IAGE71FL/"), layer="IAGE71FL")
  } else if (survey_id=="IA2020DHS") {
    geo_dat <- read_sf(dsn=paste0(getwd(),"/data/dhs/missing_shp_files/IAGE7AFL/"), layer="IAGE7AFL")
  } else if (survey_id=="PE2004DHS") {
    geo_dat <- read_sf(dsn=paste0(getwd(),"/data/dhs/missing_shp_files/PEGE52FL/"), layer="PEGE52FL")
  } else if (survey_id=="RW2008DHS") {
    geo_dat <- read_sf(dsn=paste0(getwd(),"/data/dhs/missing_shp_files/RWGE5BFL/"), layer="RWGE5BFL")
  } else if (survey_id=="ZW2015DHS") {
    geo_dat <- read_sf(dsn=paste0(getwd(),"/data/dhs/missing_shp_files/ZWGE72FL/"), layer="ZWGE72FL")
  } else if (survey_id=="BF2021DHS") {
    geo_dat <- read_sf(dsn=paste0(getwd(),"/data/dhs/missing_shp_files/BFGE81FL/"), layer="BFGE81FL")
  } else if (survey_id %in% files) {
    # Otherwise import the data downloaded from rdhs
    geo_dat <- readRDS(paste0("data/dhs/geo_data/", survey_id, ".rds"))
  } else {
    geo_dat <- NULL
  }
  if (!is.null(geo_dat)) {
    # Make dataframe for cluster number, latitude, and longitude
    geo_dataframe <- data.frame(clust=geo_dat$DHSCLUST, lat=geo_dat$LATNUM,
                                long=geo_dat$LONGNUM)
    geo_dataframe # Return the dataframe
  }
}
