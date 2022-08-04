### CALCULATES AVERAGE GLOBAL AND NATIONAL AGE DISTRIBUTIONS FOR HOMOGENEOUS COUNTERFACTUALS
### ALSO CALCULATES NEW WEIGHTS FOR US HOMOGENEOUS AGE COUNTERFACTUALS SINCE ORIGINAL PUMS WEIGHTS 
###   NO LONGER APPLY WHEN GROUPED INTO 5 YEAR AGE GROUPS
### Juliana Taube

library(tidyverse)
library(tidylog)

# read in age data
gpw <- read_csv("data/cleaned_gpw_age_data.csv")
pums_data <- read_csv("data/extracted_pums_2019data_age_birthplace_weights_region.csv") %>% 
  mutate(birth_year = 2022 - AGEP)

### WORLD AGE DISTRIBUTION ###
world_age_dist_wo_us <- gpw %>% summarise(tot_popn_2020 = sum(tot_popn_2020), 
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
                                    A85PLUSB = sum(A85PLUSB)) %>% 
  pivot_longer(cols = tot_popn_2020:A85PLUSB, values_to = "age_dist", names_to = "age")

tot_world_popn <- world_age_dist_wo_us$age_dist[[1]]

# need to include US before calculating prop in each age group
us_age_recat <- pums_data %>%
  mutate(age_5 = case_when(AGEP < 5 ~ "A00_04B",
                           AGEP < 10 ~ "A05_09B",
                           AGEP < 15 ~ "A10_14B",
                           AGEP < 20 ~ "A15_19B",
                           AGEP < 25 ~ "A20_24B",
                           AGEP < 30 ~ "A25_29B",
                           AGEP < 35 ~ "A30_34B",
                           AGEP < 40 ~ "A35_39B",
                           AGEP < 45 ~ "A40_44B",
                           AGEP < 50 ~ "A45_49B",
                           AGEP < 55 ~ "A50_54B",
                           AGEP < 60 ~ "A55_59B",
                           AGEP < 65 ~ "A60_64B",
                           AGEP < 70 ~ "A65_69B",
                           AGEP < 75 ~ "A70_74B",
                           AGEP < 80 ~ "A75_79B",
                           AGEP < 85 ~ "A80_84B",
                           !is.na(AGEP) ~ "A85PLUSB"))

us_age_dist <- us_age_recat %>% 
  group_by(age_5) %>% 
  summarise(us_count = sum(PWGTP))

tot_us_popn <- sum(us_age_dist$us_count)
tot_popn <- tot_world_popn + tot_us_popn

world_age_dist <- world_age_dist_wo_us %>% filter(age != "tot_popn_2020") %>% 
  left_join(us_age_dist, by = c("age" = "age_5")) %>% 
  rowwise() %>% 
  mutate(age_popn = sum(age_dist, us_count),
         age_prop = age_popn/tot_popn) %>% 
  rename(no_us_age_dist = age_dist,
         age_dist = age_prop)

write_csv(world_age_dist, "data/world_age_dist.csv")


### NATIONAL AGE DIST ### 
national_age_dist <- gpw %>% group_by(ISOALPHA, COUNTRYNM) %>% 
  summarise(tot_popn_2020 = sum(tot_popn_2020), 
            A00_04B = sum(A00_04B)/tot_popn_2020, 
            A05_09B = sum(A05_09B)/tot_popn_2020, 
            A10_14B = sum(A10_14B)/tot_popn_2020,
            A15_19B = sum(A15_19B)/tot_popn_2020, 
            A20_24B = sum(A20_24B)/tot_popn_2020, 
            A25_29B = sum(A25_29B)/tot_popn_2020, 
            A30_34B = sum(A30_34B)/tot_popn_2020, 
            A35_39B = sum(A35_39B)/tot_popn_2020, 
            A40_44B = sum(A40_44B)/tot_popn_2020,
            A45_49B = sum(A45_49B)/tot_popn_2020, 
            A50_54B = sum(A50_54B)/tot_popn_2020, 
            A55_59B = sum(A55_59B)/tot_popn_2020,
            A60_64B = sum(A60_64B)/tot_popn_2020, 
            A65_69B = sum(A65_69B)/tot_popn_2020, 
            A70_74B = sum(A70_74B)/tot_popn_2020,
            A75_79B = sum(A75_79B)/tot_popn_2020, 
            A80_84B = sum(A80_84B)/tot_popn_2020, 
            A85PLUSB = sum(A85PLUSB)/tot_popn_2020) %>% 
  pivot_longer(cols = tot_popn_2020:A85PLUSB, values_to = "age_dist", names_to = "age") %>% 
  filter(age != "tot_popn_2020")

write_csv(national_age_dist, "data/national_age_dist.csv")

### US world and national age weights
# lose place of birth information since have to aggregate across all individuals of same age
county_totals <- pums_data %>% group_by(PUMA, ST) %>% 
  summarise(county_popn = sum(PWGTP))

# need to group us data into 5 year groups to use national and world age distributions
#   for homogeneous counterfactual, see below for motivation
# prop in each age group across whole us
us_age_dist_props <- us_age_recat %>% 
  group_by(age_5) %>% 
  summarise(count = sum(PWGTP)) %>% 
  ungroup() %>% 
  mutate(total_popn = sum(count)) %>% 
  rowwise() %>% 
  mutate(age_prop = count/total_popn)

# creating new weights for us since in 5 year age groups, adopting national and 
#   global distribution, can't use PGWTP anymore, so use total county population 
#   and proportion of that county made up of that age group to get number of 
#   people represented by individual of that age group
new_national_weights <- us_age_recat %>% select(PUMA, ST, age_5) %>% 
  distinct() %>% 
  left_join(county_totals, by = c("ST", "PUMA")) %>% 
  left_join(us_age_dist_props, by = "age_5") %>% 
  mutate(age_group_weight = county_popn * age_prop)
write_csv(new_national_weights, "data/us_national_weights_age_dist.csv")

new_world_weights <- us_age_recat %>% select(PUMA, ST, age_5) %>% 
  distinct() %>% 
  left_join(county_totals, by = c("ST", "PUMA")) %>% 
  left_join(world_age_dist, by = c("age_5" = "age")) %>% 
  mutate(age_group_weight = county_popn * age_dist)
write_csv(new_world_weights, "data/us_world_weights_age_dist.csv")

