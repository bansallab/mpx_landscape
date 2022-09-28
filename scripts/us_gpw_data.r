library(tidyverse)
library(tidylog)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(stringi)
library(MetBrewer)

# aggregate us data to the state level, same as for other countries
us_midwest <- read_csv("data/gpw-v4-admin-unit-center-points-population-estimates-rev11_global_csv 2/gpw_v4_admin_unit_center_points_population_estimates_rev11_usa_midwest.csv")
us_northeast <- read_csv("data/gpw-v4-admin-unit-center-points-population-estimates-rev11_global_csv 2/gpw_v4_admin_unit_center_points_population_estimates_rev11_usa_northeast.csv")
us_south <- read_csv("data/gpw-v4-admin-unit-center-points-population-estimates-rev11_global_csv 2/gpw_v4_admin_unit_center_points_population_estimates_rev11_usa_south.csv")
us_west <- read_csv("data/gpw-v4-admin-unit-center-points-population-estimates-rev11_global_csv 2/gpw_v4_admin_unit_center_points_population_estimates_rev11_usa_west.csv")

aggregate_to_state <- function(data){
  out <- data %>% group_by(ISOALPHA, COUNTRYNM, NAME1) %>%
    summarise(tot_popn_2010 = sum(UN_2010_E),
              tot_popn_2020 = sum(UN_2020_E),
              A00_04B = sum(A00_04B),
              A05_09B = sum(A05_09B),
              A10_14B = sum(A10_14B),
              A15_19B = sum(A15_19B),
              A20_24B = sum(A20_24B),
              A25_29B = sum(A25_29B),
              A30_34B = sum(A30_34B),
              A35_39B = sum(A35_39B),
              A40_44B = sum(A40_44B),
              A45_49B = sum(A45_49B),
              A50_54B = sum(A50_54B),
              A55_59B = sum(A55_59B),
              A60_64B = sum(A60_64B),
              A65_69B = sum(A65_69B),
              A70_74B = sum(A70_74B),
              A75_79B = sum(A75_79B),
              A80_84B = sum(A80_84B),
              A85PLUSB = sum(A85PLUSB))
  return(out)
}

us_midwest_agg <- aggregate_to_state(us_midwest)
us_northeast_agg <- aggregate_to_state(us_northeast)
us_south_agg <- aggregate_to_state(us_south)
us_west_agg <- aggregate_to_state(us_west)

all_us <- us_midwest_agg %>% bind_rows(us_northeast_agg, us_south_agg, us_west_agg)

write_csv(all_us, "data/us_agg_age_data_admin1.csv")

data_agg_orig <- read_csv("data/us_agg_age_data_admin1.csv")

data_agg <- data_agg_orig %>% filter(tot_popn_2020 != 0) %>% 
  rowwise() %>% 
  mutate(COUNTRYNM = stri_trans_general(str = tolower(COUNTRYNM), id = "Latin-ASCII"),
         NAME1 = stri_trans_general(str = tolower(NAME1), id = "Latin-ASCII"),
         NAME1 = trimws(NAME1)) #%>% 
  #select(ISOALPHA, COUNTRYNM, NAME1, tot_popn_2010, tot_popn_2020, contains("A")) # reordering columns

write_csv(data_agg, "data/cleaned_gpw_us_age_data.csv")
