# The global landscape of susceptibility to orthopoxviruses: The legacy of smallpox vaccination

This repository provides the data and source code for the following study: Juliana C. Taube, Eva C. Rest, James O. Lloyd-Smith, Shweta Bansal. "The global landscape of susceptibility to orthopoxviruses: The legacy of smallpox vaccination." (in submission)

## How to use this resource
To rerun the analysis and reproduce the figures, start by opening `mpx_landscape.Rproj`. From here, run the `make_figures.R` script to reproduce the main figures in the text and many of the supplementary figures. Brief descriptions of the other scripts can be found below.

We hope for this to be a living database, with updated historical and future data on vaccination, and invite the global health community to contribute. (We'll add details on how you can contribute soon. In the meantime, please reach out at shweta.bansal@georgetown.edu with any data you would like to share). 

## Supplementary data
* `TableS1.csv` contains smallpox vaccination coverage and cessation estimates, lower and upper bounds, and sources for values used in our analyses. We anticipate this resource will change as more historical data become available.
* `TableS2.csv` contains a manipulation of the GPW data to show the expected 2020 population size in each 5-year age class for each admin-1 region

## Estimates (`estimates/`)
Pre-run analyses and estimates that can be used by others without re-running the code. All assume 80.7% cross-immunity between smallpox vaccination and monkeypox infection unless otherwise noted.
* `world.csv` contains percentage susceptible estimates at the admin-1 level
* `world-by-country.csv` contains percentage susceptible estimates at the country level 
* `us.csv` contains percentage susceptible estimates at the PUMAs level in the U.S.
* `us-by-state.csv` contains percentage susceptible estimates at the state level in the U.S.
* `variola-major-sensitivity.csv` contains percentage susceptible estimates at the admin-1 level for _Variola major_ (assuming 91.1% vaccine effectiveness)
* `variola-minor-sensitivity.csv` contains percentage susceptible estimates at the admin-1 level for _Variola minor_ (assuming 74.9% vaccine effectiveness)

## Data (`data/`)
Inputs for data analysis can be found in this folder, outputs from running the code will populate here. Intermediate data inputs called in the code are not provided, but can be reproduced by running commented out code.
* `cessation_coverage_estimates.csv`, equivalent to Table S1, contains upper and lower bounds of vaccination coverage and cessation estimates for uncertainty analysis
* `polio95_3dose_states.csv` contains coverage estimates for 3 dose polio vaccination in the U.S. at the state level, used to add spatial heterogeneity to national smallpox vaccination coverage in U.S.
* `gpw_to_gadm_country_join.csv` and `one_gpw_to_multiple_gadm.csv` allow for correct joining of GPW and GADM data
* `natural_immunity.csv` contains case count data from the Red Book
* `pock_survey_coverage.csv` contains pock mark survey data
* `state_fips.csv` helps converting PUMAs level estimates in the U.S. to the state-level

The files below are quite large and will be added in the future. To access immediately, you can download from the respective websites.
* `extracted_pums_2019data_age_birthplace_weights_region.csv` from https://www.census.gov/programs-surveys/acs/microdata/access/2019.html contains PUMS 2019 data used in U.S. analyses
* `gadm404-levels.gpkg` from https://gadm.org/data.html contains the GADM shape files data
* `gpw-v4-admin-unit-center-points-population-estimates-rev11_global_csv 2/` from https://sedac.ciesin.columbia.edu/data/set/gpw-v4-basic-demographic-characteristics-rev11 contains GPW data

## Code (`scripts/`)
Scripts to prepare demography data, join mapping data, calculate the proportion of a population vaccinated, calculate world and national age distributions, and calculate the landscape of orthopoxvirus susceptibility can be found in this folder.
* `make_figures.r` runs the analysis and creates figures in the main text and some supplementary figures
  * `load_files_for_run.r`, `calc_immunity.r`, `calc_immunity_us.r`, `draw_maps.r`, and `scar_survey_coverage_calcs.r` are all called within this main script
* `admin1_avg_age.r` calculates and maps the average age of each admin1 region
* `uncertainty_analysis.r` runs the parametric bootstrapping uncertainty analysis (note this code takes longer to run)
* `global_susceptibility_profile.r` calculates and plots susceptibility profiles for each country
* `monkeypox_world_data_cleaning.r` output is used to make figures, this cleans and prepares for joining the demographic and shapefiles
* `natural_immunity.r` analyzes and plots natural immunity and pock mark survey data
* `us_susceptibility_profile` calculates the susceptibility profile for the U.S.
* `world_national_age_dists.r` calculates average global and national age distributions for use in counterfactual analyses

## Figures (`figures/`)
When the code is run, this folder will contain the figure outputs shown in the manuscript.
