### CLEANS AND FORMATS GPW DATA FOR ADMIN2 and 3 LEVEL SUSCEPTIBILITY ESTIMATES
### ANALYSIS  is in spatial_analysis.r
### Juliana Taube

library(tidyverse)
library(tidylog)
library(stringi)

# # skip to read in data_agg
# data <- read_csv("data/gpw-v4-admin-unit-center-points-population-estimates-rev11_global_csv 2/gpw_v4_admin_unit_center_points_population_estimates_rev11_global.csv")
# 
# # need to summarise to admin1 level, this may take a while
# data_agg <- data %>% group_by(ISOALPHA, COUNTRYNM, NAME1, NAME2) %>%
#   summarise(tot_popn_2020 = sum(UN_2020_E),
#             tot_popn_2010 = sum(UN_2010_E),
#             pop_dens_2020 = mean(UN_2020_DS),
#             pop_change = tot_popn_2020/tot_popn_2010,
#             A00_04B = as.integer(sum(A00_04B) * pop_change),
#             A05_09B = as.integer(sum(A05_09B) * pop_change),
#             A10_14B = as.integer(sum(A10_14B) * pop_change),
#             A15_19B = as.integer(sum(A15_19B) * pop_change),
#             A20_24B = as.integer(sum(A20_24B) * pop_change),
#             A25_29B = as.integer(sum(A25_29B) * pop_change),
#             A30_34B = as.integer(sum(A30_34B) * pop_change),
#             A35_39B = as.integer(sum(A35_39B) * pop_change),
#             A40_44B = as.integer(sum(A40_44B) * pop_change),
#             A45_49B = as.integer(sum(A45_49B) * pop_change),
#             A50_54B = as.integer(sum(A50_54B) * pop_change),
#             A55_59B = as.integer(sum(A55_59B) * pop_change),
#             A60_64B = as.integer(sum(A60_64B) * pop_change),
#             A65_69B = as.integer(sum(A65_69B) * pop_change),
#             A70_74B = as.integer(sum(A70_74B) * pop_change),
#             A75_79B = as.integer(sum(A75_79B) * pop_change),
#             A80_84B = as.integer(sum(A80_84B) * pop_change),
#             A85PLUSB = as.integer(sum(A85PLUSB) * pop_change))
# 
# write_csv(data_agg, "data/world_agg_age_data_admin2_2020.csv")

data_agg_orig <- read_csv("data/world_agg_age_data_admin2_2020.csv")
# not sure we actually need to clean the names much since not mapping/joining with gadm?

# don't sum up quite right, even to 2010 population totals, but on same order of magnitude
data_agg <- data_agg_orig %>% filter(tot_popn_2020 != 0) %>% 
  rowwise() %>% 
  mutate(COUNTRYNM = stri_trans_general(str = tolower(COUNTRYNM), id = "Latin-ASCII"),
         NAME1 = trimws(stri_trans_general(str = tolower(NAME1), id = "Latin-ASCII")),
         NAME2 = trimws(stri_trans_general(str = tolower(NAME2), id = "Latin-ASCII"))) %>% 
  # need to strip white space
  distinct() %>% 
  # deal with duplicate admin1s with different populations, e.g., ile-de-france, and sum to get total popn
  group_by(ISOALPHA, NAME1, NAME2) %>% 
  summarise(COUNTRYNM = COUNTRYNM,
            NAME1 = NAME1,
            NAME2 = NAME2,
            tot_popn_2020 = sum(tot_popn_2020), 
            pop_dens_2020 = mean(pop_dens_2020),
            A00_04B = sum(A00_04B), A05_09B = sum(A05_09B), A10_14B = sum(A10_14B),
            A15_19B = sum(A15_19B), A20_24B = sum(A20_24B), A25_29B = sum(A25_29B), 
            A30_34B = sum(A30_34B), A35_39B = sum(A35_39B), A40_44B = sum(A40_44B),
            A45_49B = sum(A45_49B), A50_54B = sum(A50_54B), A55_59B = sum(A55_59B),
            A60_64B = sum(A60_64B), A65_69B = sum(A65_69B), A70_74B = sum(A70_74B),
            A75_79B = sum(A75_79B), A80_84B = sum(A80_84B), A85PLUSB = sum(A85PLUSB)) %>%
  ungroup() %>% 
  distinct() %>% 
  mutate(admin_id = row_number()) %>% 
  filter(!is.na(NAME2), !is.na(NAME1)) %>% 
  mutate(NAME1 = trimws(NAME1),
         NAME2 = trimws(NAME2), # add version back below
         ISOALPHA = ifelse(ISOALPHA == "KOS", "XKO", ISOALPHA)) %>% # gadm has different kosovo isoalpha
  filter(NAME1 != "bikini") %>% # 1 person, old nuclear tests
  select(ISOALPHA, COUNTRYNM, NAME1, NAME2, admin_id, tot_popn_2020, pop_dens_2020, contains("A")) # reordering columns

write_csv(data_agg, "data/cleaned_gpw_admin2_age_data.csv")

### ADMIN3
# data <- read_csv("data/gpw-v4-admin-unit-center-points-population-estimates-rev11_global_csv 2/gpw_v4_admin_unit_center_points_population_estimates_rev11_global.csv")
# 
# # need to summarise to admin1 level, this may take a while
# data_agg <- data %>% group_by(ISOALPHA, COUNTRYNM, NAME1, NAME2, NAME3) %>%
#   summarise(tot_popn_2020 = sum(UN_2020_E),
#             tot_popn_2010 = sum(UN_2010_E),
#             pop_dens_2020 = mean(UN_2020_DS),
#             pop_change = tot_popn_2020/tot_popn_2010,
#             A00_04B = as.integer(sum(A00_04B) * pop_change),
#             A05_09B = as.integer(sum(A05_09B) * pop_change),
#             A10_14B = as.integer(sum(A10_14B) * pop_change),
#             A15_19B = as.integer(sum(A15_19B) * pop_change),
#             A20_24B = as.integer(sum(A20_24B) * pop_change),
#             A25_29B = as.integer(sum(A25_29B) * pop_change),
#             A30_34B = as.integer(sum(A30_34B) * pop_change),
#             A35_39B = as.integer(sum(A35_39B) * pop_change),
#             A40_44B = as.integer(sum(A40_44B) * pop_change),
#             A45_49B = as.integer(sum(A45_49B) * pop_change),
#             A50_54B = as.integer(sum(A50_54B) * pop_change),
#             A55_59B = as.integer(sum(A55_59B) * pop_change),
#             A60_64B = as.integer(sum(A60_64B) * pop_change),
#             A65_69B = as.integer(sum(A65_69B) * pop_change),
#             A70_74B = as.integer(sum(A70_74B) * pop_change),
#             A75_79B = as.integer(sum(A75_79B) * pop_change),
#             A80_84B = as.integer(sum(A80_84B) * pop_change),
#             A85PLUSB = as.integer(sum(A85PLUSB) * pop_change))
# 
# write_csv(data_agg, "data/world_agg_age_data_admin3_2020.csv")

data_agg_orig <- read_csv("data/world_agg_age_data_admin3_2020.csv")
# not sure we actually need to clean the names much since not mapping/joining with gadm?

# don't sum up quite right, even to 2010 population totals, but on same order of magnitude
data_agg <- data_agg_orig %>% filter(tot_popn_2020 != 0) %>% 
  rowwise() %>% 
  mutate(COUNTRYNM = stri_trans_general(str = tolower(COUNTRYNM), id = "Latin-ASCII"),
         NAME1 = trimws(stri_trans_general(str = tolower(NAME1), id = "Latin-ASCII")),
         NAME2 = trimws(stri_trans_general(str = trimws(tolower(NAME2)), id = "Latin-ASCII")),
         NAME3 = trimws(stri_trans_general(str = tolower(NAME3), id = "Latin-ASCII"))) %>% 
  # need to strip white space
  distinct() %>% 
  # deal with duplicate admin1s with different populations, e.g., ile-de-france, and sum to get total popn
  group_by(ISOALPHA, NAME1, NAME2, NAME3) %>% 
  summarise(COUNTRYNM = COUNTRYNM,
            NAME1 = NAME1,
            NAME2 = NAME2,
            NAME3 = NAME3,
            tot_popn_2020 = sum(tot_popn_2020), 
            pop_dens_2020 = mean(pop_dens_2020),
            A00_04B = sum(A00_04B), A05_09B = sum(A05_09B), A10_14B = sum(A10_14B),
            A15_19B = sum(A15_19B), A20_24B = sum(A20_24B), A25_29B = sum(A25_29B), 
            A30_34B = sum(A30_34B), A35_39B = sum(A35_39B), A40_44B = sum(A40_44B),
            A45_49B = sum(A45_49B), A50_54B = sum(A50_54B), A55_59B = sum(A55_59B),
            A60_64B = sum(A60_64B), A65_69B = sum(A65_69B), A70_74B = sum(A70_74B),
            A75_79B = sum(A75_79B), A80_84B = sum(A80_84B), A85PLUSB = sum(A85PLUSB)) %>%
  ungroup() %>% 
  distinct() %>% 
  mutate(admin_id = row_number()) %>% 
  filter(!is.na(NAME2), !is.na(NAME1), !is.na(NAME3)) %>% 
  mutate(NAME1 = trimws(NAME1),
         NAME2 = trimws(NAME2), 
         NAME3 = trimws(NAME3), # add version back below
         ISOALPHA = ifelse(ISOALPHA == "KOS", "XKO", ISOALPHA)) %>% # gadm has different kosovo isoalpha
  filter(NAME1 != "bikini") %>% # 1 person, old nuclear tests
  select(ISOALPHA, COUNTRYNM, NAME1, NAME2, admin_id, tot_popn_2020, pop_dens_2020, contains("A")) # reordering columns

write_csv(data_agg, "data/cleaned_gpw_admin3_age_data.csv")
