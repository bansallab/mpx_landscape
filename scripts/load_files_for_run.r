### LOADS PRECLEANED DATA, FUNCTIONS, CONSTANTS TO RUN ANALYSES AND MAKE FIGURES
### Juliana Taube

library(tidyverse)
#library(tidylog)
library(sf)
library(stringi)
library(MetBrewer)
library(ggpubr)
library(cowplot)
library(patchwork)
library(tidycensus)
library(tigris)
library(tmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(extraDistr)

# RUN monkeypox_world_data_cleaning.r FIRST
# RUN world_national_age_dists.r FIRST
# its contents get saved, don't need to clog environment

### source other scripts
source("scripts/scar_survey_coverage_calcs.r")
source("scripts/calc_immunity.r")
source("scripts/calc_immunity_us.r")
source("scripts/draw_maps.r")
source("scripts/calc_country_ests.r")

### set constants
#WANING <- 0.807
DEFAULT_COVERAGE <- 0.8
US_SPX_COVG <- 0.95
PERC_MISSED_CASES <- 0.5 # 50%

### read in other datasets

# vaccine cessation and scar survey data
cessation_coverage_data <- read_csv("data/cessation_coverage_estimates.csv") %>% 
  rename(ISO = Country_ISO_Code, 
         POBP = Country_PUMS_Code, # this is a country code though
         vax_stopped = cessdate_orig,
         Survey_Year = cvg_survey_yr)

# us vax coverage data
polio_proxy_coverage <- read_csv("data/polio95_3dose_states.csv") %>% 
  select(FIPS, State, `24 mos. Cvg`) %>% 
  mutate(coverage = `24 mos. Cvg`/100) %>% 
  rename(fips_of_birth = FIPS,
         state_of_birth = State) %>% 
  mutate(mu = mean(coverage, na.rm = T)) %>% 
  rowwise() %>% 
  mutate(spx_coverage = (coverage / mu) * US_SPX_COVG,
         spx_coverage = ifelse(is.na(spx_coverage), 0.95,
                                     ifelse(spx_coverage > 1, 1, 
                                            spx_coverage))) # factor in spx national avg

# world age data
gpw <- read_csv("data/cleaned_gpw_age_data.csv")

# us age and place of birth data
pums_data <- read_csv("data/extracted_pums_2019data_age_birthplace_weights_region.csv") %>% 
  mutate(birth_year = 2022 - AGEP) 

# aggregated age distributions
national_age_dist <- read_csv("data/national_age_dist.csv")
world_age_dist <- read_csv("data/world_age_dist.csv")
new_national_weights <- read_csv("data/us_national_weights_age_dist.csv")
new_world_weights <- read_csv("data/us_world_weights_age_dist.csv")

# gadm shapefiles
orig_gadm404 <- st_read("data/gadm404-levels.gpkg",
                        layer = "level1")
gadm404 <- orig_gadm404 %>% select(ID_0, COUNTRY, ID_1, NAME_1, geom) %>%
  mutate(COUNTRY = stri_trans_general(str = tolower(COUNTRY), id = "Latin-ASCII"),
         NAME_1 = stri_trans_general(str = tolower(NAME_1), id = "Latin-ASCII"),
         ID_1 = gsub("^...\\.", "", gsub("_1$", "", ID_1))) %>%
  rename(ISO = ID_0)
rm(orig_gadm404)

# precleaned gadm data without shapefiles
gadm_clean <- read_csv("data/cleaned_gadm_data_noshapefile.csv")

# gpw/gadm name fixes
spread_gpw <- read_csv("data/one_gpw_to_multiple_gadms.csv")

# natural immunity estimates
nat_immunity <- read_csv("data/nat_immunity_cumulative_prop_infected.csv") %>% 
  select(-country)

# for state conversion
state_fips <- read_csv("data/state_fips.csv") %>% 
  mutate(state = tolower(stname),
         fips = as.numeric(st))
