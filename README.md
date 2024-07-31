# Child Marriage Regions
This repo collates and cleans child marriage data across DHS and MICS surveys (available as of October 2023). One dataset is produced for each country, which pools all available surveys for that country.
Where possible, the data has been linked to geographic administrative boundaries listed in the Global Administrative Areas Database (https://gadm.org/) at the first and second administrative division.

The code was developed to clean data for the following papers:


## Code organisation
- `child-marriage-regions.Rproj` is the project file. Open this file first to make sure you have the correct working directory.
- `_run.R` is the main script that runs the entire analysis. This is the only script that needs to be run in order to produce the clean datasets. This script performs the following:
- 
- The directory `R` contains the functions that support the `_run.R` file
- The directory `data` contains the raw MICS and DHS survey files, workbooks needed to match region names to the GADM, and reference spreadsheets needed to clean the data.


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
