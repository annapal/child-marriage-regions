# Child Marriage Regions
This repo collates and cleans child marriage data across DHS and MICS surveys (available as of October 2023). Two datasets are produced for each country (one wide and one long), pooling all available surveys for that country.
Where possible, the data has been linked to geographic administrative boundaries listed in the Global Administrative Areas Database (https://gadm.org/) at the first and second administrative division.

The code was developed to clean data for the following papers:
- Palmer, A.Y., Masuda, Y.J., Harou, A.P., Greene, M.E., Das, J.K., Bawah, A.A., Kwauk, C.T., MacDonald, G.K., Robinson, B.E., Baumgartner, J. and Koski, A. (2025) “The effect of drought on the rate of child marriage in 61 countries,” (Under Review).

## Code organisation
- `child-marriage-regions.Rproj` is the project file.
- `_run.R` is the main script that runs the entire analysis. *This is the only script that needs to be run by the user*. The functions called in this script perform the following:
  - `clean_mics()` opens MICS survey data and relabels variables in a consistent way.
  - `impute_mics()` imputes missing birth and marriage dates in the MICS data. This is done using similar methodology as the DHS: https://dhsprogram.com/pubs/pdf/DHSG3/DHS_Data_Editing.pdf.
  - `clean_dhs()` downloads the DHS data (and geocodes) and relabels variables in a consistent way.
  - `merge_data()`
      - Uses sub-national region of residence, or available geocodes, to link surveys to the GADM at the Adm1 & Adm2 level (where applicable).
      - Denormalises survey weights and creates unique PSU and strata variables across surveys.
      - Pools all DHS and MICS surveys for a country into one dataset.
  - `create_long_data()`
      - Converts the pooled dataset into long format, where each row represents a person-year between the ages of 13 and 17, where the person was unmarried at the start of that year.
      - Removes women interviewed at young ages within ever-married samples (to avoid bias when calculating the rate of child marriage)
- The directory `R` contains the functions that support the `_run.R` file
- The directory `data` contains the raw MICS and DHS survey files and other reference data. Specifically:
  - `data/ref_data/analysis-variables.xlsx` is a data dictionary showing the consistent variable names in the clean dataset
  - `data/ref_data/country-codes.xlsx` contains country names and codes for all included countries
  - `data/ref_data/dhs_surveys.xlsx` contains a list of DHS surveys included
  - `data/ref_data/min_mar_age.csv` contains data on the average age at which 95% of the population is married (needed to remove observations in "ever-married" samples), downloaded from *World Marriage Data* https://population.un.org/MarriageData/Index.html#/home.
  - `data/ref_data/pop_year.csv` contains data on the population of women aged 15-49 years (needed to denormalise survey weights), downloaded from *World Population Prospects* https://population.un.org/wpp/.
  - `data/wbs` contains a workbook for each country which helps to match region names to the GADM in each survey.

## Outputs
After running the code, there should be two main sets of outputs:
- `data/wide_data` contains cleaned pooled country data files. There is one rds file for each country.
- `data/long_data` contains cleaned long data files. There is one file for rds each country.

## Instructions for reproducing the data
Running the code requires some management by the user. DHS and MICS raw data files are not uploaded. You will need to request permission to access these data at https://dhsprogram.com/ and https://mics.unicef.org/surveys. 
Access to geocdes is also required for DHS surveys. Only surveys without restricted access are included.

To reproduce the clean data, perform the following steps:

1. Go to https://mics.unicef.org/surveys, click "Download manager", select all MICS rounds, and click "Download XXX datasets". Once downloaded, unzip all files (select all and double click on Mac). Save all folders in the directory `data/mics/raw_data`. It's important that the file path is not changed. As a sanity check, the file path for Afghanistan MICS4 should be `data/mics/raw_data/Afghanistan_MICS4_Datasets/Afghanistan MICS 2010-2011 SPSS Datasets/wm.sav`.

2. Open the `_run.R` file. Add your email and the project title you used when you requested persmission to access the DHS data. This will allow you to download the relevant DHS files using the `rdhs` package.

3. Some DHS surveys can't be downloaded using `rdhs`, and you will need to download them manually. Specifically, these surveys are: Burkina Faso 2021, Ethiopia 2019, Gabon 2000, India 2019-21, India 2015-16, India 1992-93, Peru 2004-06, Rwanda 2007-08, Turkey 1998, and Zimbabwe 2015. Go to https://dhsprogram.com/ and log in. Navigate to each of these survey pages and download the Individual Recode Stata dataset. Unzip each folder, and save folders in `data/dhs/missing_data_files`. As a sanity check, the file path for the Burkina Faso 2021 DHS should be `data/dhs/missing_data_files/BFIR81DT/BFIR81FL.DTA`.

4. Some geocodes for DHS surveys can't be downloaded using `rdhs` either. These surveys include: Burkina Faso 2021, Ethiopia 2019, India 2019-21, India 2015-16, Nepal 2022, Peru 2004-06, Rwanda 2007-08, and Zimbabwe 2015. Go to https://dhsprogram.com/ and log in. Navigate to each of these survey pages and download the Geographic Data zip file. Unzip the folder, and save folders in `data/dhs/missing_shp_files`.

5. Open the `packages.R` file and make sure that all packages have been installed. The package `DHSmortality` may need to be installed manually. Refer to https://rdrr.io/github/hesohn/DHSmortality/ for installation.

6. Source the `_run.R` file.

Please direct questions/comments/issues to anna.palmer@mail.mcgill.ca.
