### GOAL: make neighbor adjacency matrix using spdep poly2nb with shapefiles, then calculate Moran's I
### Also analyze susceptibility by population size (we had this in gpw data, but may not have carried over,
###   so rejoin, or have it be included from get go -- I THINK WE DID CARRY IT OVER YAY)

library(tidyverse)
library(spdep)
library(sf)
library(stringi)

# do we need nb2mat?

# need to load an sf object, usually we save a dataframe not with the sf, so changed functions
#   for mapping so we can save sf object
# start off with 3414 rows in main_estimates csv (before gadm join & spread), 3658 in fig1sf
# then remove NA perc_susc and have 3029 and 3205 rows respectively
# there's a lot going one with the one gpw to mult gadm and then gadm matching
# hard to keep track of exactly how many rows we want
fig1sf <- readRDS("data/world-estimates-vaxxed-sf.RDS") %>% 
  filter(! is.na(perc_vaxxed)) # need to remove NAs

# poly2nb throws an error for overlapping edges at this row (Maldonado, Uruguay), remove for now
# Loop 1 is not valid: Edge 832 crosses edge 834.
test <- fig1sf %>% filter(row_number() != 3149)

# find neighboring polygons
# sf::sf_use_s2(FALSE) # have to use to avoid crossing lines issue and make nonspherical -- bad approach
nb <- poly2nb(test, queen = TRUE)

# some regions have no neighbors (cardinality 0), we need to remove them from analysis
retain_data <- subset(test, subset = card(nb) > 0)
retain_nb <- subset(nb, subset = card(nb) > 0)

# assign equal weights to each neighboring polygon
lw <- nb2listw(retain_nb, style = "W", zero.policy = TRUE)

# calculate moran's I
I <- moran(retain_data$perc_vaxxed, lw, length(retain_nb), Szero(lw))[1]

moran.test(retain_data$perc_vaxxed, lw, alternative="two.sided")


### now try the susceptibility by population size (trying to do like urban/rural)
data <- read_csv("estimates/vaxxed-world.csv") %>% filter(! is.na(perc_vaxxed))
data %>% ggplot(aes(x = tot_popn_2020, y = perc_vaxxed)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  geom_smooth()

cor.test(data$tot_popn_2020, data$perc_vaxxed)

# population density was originally available in gpw data, so may be able to add but will take a while
gpw <- read_csv("data/gpw-v4-admin-unit-center-points-population-estimates-rev11_global_csv 2/gpw_v4_admin_unit_center_points_population_estimates_rev11_global.csv")

# need to summarise to admin1 level, faster than for age data
gpw_agg <- gpw %>% group_by(ISOALPHA, COUNTRYNM, NAME1) %>%
  summarise(pop_dens_2020 = mean(UN_2020_DS, na.rm = T))

# if need to clean to match other estimates
clean_gpw <- function(admin1name){
  fix_1 <- gsub("[ ]region$|[ ]governorate$|[ ]administration$|[ ]gov.|^dpto.[ ]|[ ]district|[ ]territory|region[ ]de[ ]|district[ ]de[ ]|^bourough[ ]of[ ]|[ ]county$|[ ]province$|province[ ]de[ ]|province[ ]du[ ]|province[ ]de[ ]l[ ]|province[ ]de[ ]la[ ]|[ ]state$|region[ ]of[ ]|[ ]division|[ ]-[ ]metro[ ]city$|^raionul[ ]|^wilayah[ ]persekutuan[ ]|^region[ ]autonoma[ ]|^regiao[ ]autonoma[ ]da[ ]|^regiao[ ]autonoma[ ]dos[ ]|[ ]municipio$|[ ]oblast$|-ken$|'|`",
                "", admin1name)
  fix_2 <- gsub("&", "and", fix_1)
  fix_3 <- gsub("/[ ]", "-", fix_2)
  fix_4 <- gsub("^st\\.[ ]|^st[ ]", "saint ", fix_3)
  fix_5 <- gsub("'", "", fix_4)
  return(fix_5)
}

# don't sum up quite right, even to 2010 population totals, but on same order of magnitude
gpw_agg2 <- gpw_agg %>% 
  rowwise() %>% 
  mutate(COUNTRYNM = stri_trans_general(str = tolower(COUNTRYNM), id = "Latin-ASCII"),
         NAME1 = stri_trans_general(str = tolower(NAME1), id = "Latin-ASCII"),
         NAME1 = trimws(clean_gpw(NAME1))) %>% 
  # need to strip white space
  distinct() %>% 
  filter(!is.na(NAME1)) %>% 
  mutate(NAME1 = trimws(NAME1), # add version back below
         ISOALPHA = ifelse(ISOALPHA == "KOS", "XKO", ISOALPHA)) %>% # gadm has different kosovo isoalpha
  filter(NAME1 != "bikini") %>% # 1 person, old nuclear tests
  select(ISOALPHA, COUNTRYNM, NAME1, pop_dens_2020) # reordering columns

# fix gpw names from MANUAL comparison to gadm
gpw_gadm_name_fix <- read_csv("data/gpw_to_gadm_country_join.csv")
gpw_agg_join <- gpw_agg2 %>% left_join(gpw_gadm_name_fix, by = c("ISOALPHA" = "ISOALPHA",
                                                                  "COUNTRYNM",
                                                                  "NAME1" = "original_gpw_name")) %>% 
  mutate(NAME1 = ifelse(is.na(updated_gpw_name), NAME1, updated_gpw_name))

# now try to join
data2 <- data %>% mutate(ISOALPHA = ifelse(ISOALPHA == "KOS", "XKO", ISOALPHA)) %>% 
  left_join(gpw_agg_join) %>% 
  filter(! is.na(pop_dens_2020)) # excludes US and naming issues that will be resolved with rerun
data2 %>% ggplot(aes(x = pop_dens_2020, y = perc_vaxxed)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  geom_smooth()
cor.test(data2$pop_dens_2020, data2$perc_vaxxed)


### try admin2 level ###
# have to rewrite function since now summarising to admin2 level
gpw2 <- read_csv("data/cleaned_gpw_admin2_age_data.csv")
# exclude US for right now
stop_cvg_data <- read_csv("data/cessation_coverage_estimates.csv") %>% 
  rename(ISO = Country_ISO_Code, 
         POBP = Country_PUMS_Code, # this is a country code though
         vax_stopped = cessdate_orig,
         Survey_Year = cvg_survey_yr)
data_long <- gpw2 %>% pivot_longer(cols = A00_04B:A85PLUSB, values_to = "popn", names_to = "age") 

data_long <- data_long %>% rowwise() %>% 
  mutate(min_age = as.numeric(substring(age, 2, 3)),
         max_age = ifelse(age == "A85PLUSB", 100, as.numeric(substring(age, 5, 6))), # can ignore warning
         mid_age = (max_age + min_age)/2,
         min_birth_year = 2022 - max_age,
         max_birth_year = 2022 - min_age) %>%
  left_join(stop_cvg_data, by = c("ISOALPHA" = "ISO"))
source("scripts/scar_survey_coverage_calcs.r")
vaxxed_by_age <- data_long %>% 
  rowwise() %>%
  mutate(prop_vaxxed_by_age = calc_vax_coverage(min_birth_year, max_birth_year, 
                                                Survey_Year, vax_stopped,
                                                cvg_514_orig, cvg_over14_orig, 
                                                cvg_tot_orig))
  
vaxxed_by_admin2 <- vaxxed_by_age %>% group_by(ISOALPHA, COUNTRYNM, NAME1, NAME2, tot_popn_2020) %>% 
  summarise(prop_vaxxed = weighted.mean(prop_vaxxed_by_age, popn, na.rm = T),
            pop_size = sum(popn), # from sum
            pop_dens_2020 = mean(pop_dens_2020)) %>% 
  ungroup() %>% # applies function to multiple columns
  mutate(across(prop_vaxxed, ~ ifelse(is.nan(.x), NA_real_, .x))) %>% # replace NaN with NA
  mutate(perc_vaxxed = prop_vaxxed * 100) 

vaxxed_by_admin2 <- vaxxed_by_admin2 %>% 
  filter(! is.na(perc_vaxxed))
vaxxed_by_admin2 %>% ggplot(aes(x = pop_dens_2020, y = perc_vaxxed)) +
  geom_point(alpha = 0.5) +
  #scale_x_log10() +
  geom_smooth()
cor.test(vaxxed_by_admin2$pop_dens_2020, 
         vaxxed_by_admin2$perc_vaxxed)
vaxxed_by_admin2 %>% ggplot(aes(x = tot_popn_2020, y = perc_vaxxed)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  geom_smooth()
cor.test(vaxxed_by_admin2$tot_popn_2020, vaxxed_by_admin2$perc_vaxxed)

### try admin3 level ###
# have to rewrite function since now summarising to admin3 level
gpw3 <- read_csv("data/cleaned_gpw_admin3_age_data.csv")
# exclude US for right now
stop_cvg_data <- read_csv("data/cessation_coverage_estimates.csv") %>% 
  rename(ISO = Country_ISO_Code, 
         POBP = Country_PUMS_Code, # this is a country code though
         vax_stopped = cessdate_orig,
         Survey_Year = cvg_survey_yr)
data_long <- gpw3 %>% pivot_longer(cols = A00_04B:A85PLUSB, values_to = "popn", names_to = "age") 

data_long <- data_long %>% rowwise() %>% 
  mutate(min_age = as.numeric(substring(age, 2, 3)),
         max_age = ifelse(age == "A85PLUSB", 100, as.numeric(substring(age, 5, 6))), # can ignore warning
         mid_age = (max_age + min_age)/2,
         min_birth_year = 2022 - max_age,
         max_birth_year = 2022 - min_age) %>%
  left_join(stop_cvg_data, by = c("ISOALPHA" = "ISO"))
source("scripts/scar_survey_coverage_calcs.r")
vaxxed_by_age <- data_long %>% 
  rowwise() %>%
  mutate(prop_vaxxed_by_age = calc_vax_coverage(min_birth_year, max_birth_year, 
                                                Survey_Year, vax_stopped,
                                                cvg_514_orig, cvg_over14_orig, 
                                                cvg_tot_orig))

vaxxed_by_admin3 <- vaxxed_by_age %>% group_by(ISOALPHA, COUNTRYNM, NAME1, NAME2, NAME3, tot_popn_2020) %>% 
  summarise(prop_vaxxed = weighted.mean(prop_vaxxed_by_age, popn, na.rm = T),
            pop_size = sum(popn),
            pop_dens_2020 = mean(pop_dens_2020)) %>% 
  ungroup() %>% # applies function to multiple columns
  mutate(across(prop_vaxxed, ~ ifelse(is.nan(.x), NA_real_, .x))) %>% # replace NaN with NA
  mutate(perc_vaxxed = prop_vaxxed * 100) 

vaxxed_by_admin3 <- vaxxed_by_admin3 %>% 
  filter(! is.na(perc_vaxxed))
vaxxed_by_admin3 %>% ggplot(aes(x = pop_dens_2020, y = perc_vaxxed)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  geom_smooth()
cor.test(vaxxed_by_admin3$pop_dens_2020, 
         vaxxed_by_admin3$perc_vaxxed)
vaxxed_by_admin3 %>% ggplot(aes(x = tot_popn_2020, y = perc_vaxxed)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  geom_smooth()
cor.test(vaxxed_by_admin3$tot_popn_2020, vaxxed_by_admin3$perc_vaxxed)


