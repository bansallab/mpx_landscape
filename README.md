# The global landscape of smallpox vaccination history -- Data & Code

This repository provides the data and source code for the following study: Juliana C. Taube, Eva C. Rest, James O. Lloyd-Smith, Shweta Bansal. "The global landscape of smallpox vaccination history: Implications for current and future orthopoxvirus susceptibility." https://doi.org/10.1101/2022.07.29.22278217

## How to use this resource
To rerun the analysis and reproduce the figures, start by opening `mpx_landscape.Rproj`. You will need to download GADM shapefile data version 4.0 to reproduce the maps and US Census data to reproduce fine US estimates. From here, run the `make_figures.R` script to reproduce the main figures in the text and many of the supplementary figures. To reproduce Figure 3, run the `global_susceptibility_profiles.R` script. Brief descriptions of the other scripts can be found below.

We hope for this to be a living database, with updated historical and future data on vaccination, and invite the global health community to contribute. (We'll add details on how you can contribute soon. In the meantime, please reach out at shweta.bansal@georgetown.edu with any data you would like to share). 

## Supplementary data
* `TableS1.csv` contains smallpox vaccination coverage and cessation estimates, lower and upper bounds, and sources for values used in our analyses. We anticipate this resource will change as more historical data become available. The *non-updated* version used in our analyses, `cessation_coverage_estimates.csv`, can be found in the data folder. The grading scheme for vaccination cessation and coverage data is as follows:
  * **A.** Good evidence (e.g., known scar/serum survey; known cessation date from literature)
  * **B.** Some evidence (e.g., vaccination coverage estimate mentioned in text or model without known scar/serum survey data; cessation date range)
  * **C.** No direct evidence, use country-specific assumptions (e.g., average of coverage estimates from neighboring countries; only one known bound (high or low) for cessation date)
  * **D.** Default values

## Estimates (`estimates/`)
Pre-run analyses and estimates that can be used by others without re-running the code. 
* `uncertainty-differences.csv` contains mean percentage vaccinated estimates and standard deviations from our parametric bootstrapping uncertainty analysis
* `vaxxed-us-pumas.csv` contains percentage vaccinated estimates at the PUMAs level in the U.S.
* `vaxxed-world-by-country.csv` contains percentage vaccinated estimates at the country level
* `vaxxed-world.csv` contains percentage vaccinated estimates at the admin-1 level


## Data (`data/`)
Inputs for data analysis can be found in this folder, outputs from running the code will populate here. some intermediate data inputs called in the code are provided, and others can be reproduced.
* `age_dist_absolute_differences_stdevs.csv` contains country-specific standard deviations for adding age distribution uncertainty based on a comparison of 2010 GPW data and 2020 UN age distribution data
* `bootstrapped_estimates_country_age_5000_mu_sig.csv` contains results of parametric bootstrapping analysis with mean and standard deviation of vaccination percentage for each age group in each country
* `cessation_coverage_estimates.csv`, non-updated version of Table S1 that can be used to reproduce results in our paper, contains upper and lower bounds of vaccination coverage and cessation estimates for uncertainty analysis
* `cleaned_gadm_data_no_shapefile.csv`, GADM data without shapefiles, precursor to mapping 
* `cleaned_gpw_age_data_props.csv`, cleaned GPW admin-1 level age distribution data
* `gpw_to_gadm_country_join.csv` and `one_gpw_to_multiple_gadm.csv` allow for correct joining of GPW and GADM data
* `national_age_dist.csv` and `world_age_dist.csv` contain age distribution data if all admin-1s within a country or across the world adopt the national or global average, respectively
* `natural_immunity.csv` contains case count data from Fenner et al.
* `pock_survey_coverage.csv` contains pock mark survey data
* `polio95_3dose_states.csv` contains coverage estimates for 3 dose polio vaccination in the U.S. at the state level, used to add spatial heterogeneity to national smallpox vaccination coverage in U.S.
* `state_fips.csv` helps converting PUMAs level estimates in the U.S. to the state level
* `WPP2022_POP_F02_1_POPULATION_5-YEAR_AGE_GROUPS_BOTH_SEXES copy.csv` is a converted version of UN age distribution data in 5-year age groups with both sexes from: https://population.un.org/wpp/Download/Standard/Population/

The files below are quite large and will be added in the future. To access immediately, you can download from the respective websites.
* `extracted_pums_2019data_age_birthplace_weights_region.csv` from https://www.census.gov/programs-surveys/acs/microdata/access/2019.html contains PUMS 2019 data used in U.S. analyses
* `gadm404-levels.gpkg` from https://gadm.org/data.html contains the GADM shape files data version 4.0
* `gpw-v4-admin-unit-center-points-population-estimates-rev11_global_csv 2/` from https://sedac.ciesin.columbia.edu/data/set/gpw-v4-basic-demographic-characteristics-rev11 contains GPW data

## Code (`scripts/`)
Scripts to prepare demography data, join mapping data, calculate the proportion of a population vaccinated, calculate world and national age distributions, and calculate the landscape of orthopoxvirus susceptibility can be found in this folder.
* `admin1_avg_age.r` calculates the average age for each admin-1, maps it, and calculates the global average age
* `age_dist_comparison.r` compares aggregated GPW 2010 age distribution data with 2020 UN country level age distribution data
* `calc_immunity_split.r` contains the functions to calculate the percent *vaccinated* at the admin-1 level across all age groups or at the country level for each age group
* `calc_immunity_us.r` contains the functions to calculate the percent *vaccinated* at the PUMAs level in the U.S.
* `draw_maps.r` contains the functions to map global admin-1 level data
* `global_susceptibility_profile.r` calculates and plots susceptibility profiles for each country, and produces Figure 3 with four case studies with various vaccine effectiveness and waning rates
* `gpw_admin2.r` cleans admin-2 level and admin-3 level GPW data for some spatial analyses
* `load_files_for_run.r` loads all files and functions to make figures
* `make_figures.r` runs the analysis and creates most figures in the paper
* `monkeypox_world_data_cleaning.r` output is used to make figures, this cleans and prepares for joining the demographic (GPW data) and shapefiles (GADM data)
* `natural_immunity.r` analyzes and plots natural immunity and pock mark survey data
* `scar_survey_coverage_calcs.r` contains functions to calculate the proportion of an age group eligible and subsequently vaccinated based on cessation dates and vaccination coverage data
* `spatial_analysis` calculates Moran's I and tests associations between population size and density and current vaccination history
* `uncertainty_analysis.r` runs the parametric bootstrapping uncertainty analysis (note this code takes longer to run)
* `us_gpw_data.r` cleans and aggregates GPW data on U.S. age distributions not provided in the main GPW dataset
* `world_national_age_dists.r` calculates average global and national age distributions for use in counterfactual analyses

## Figures (`figures/`)
When the code is run, this folder will contain the figure outputs shown in the manuscript.
