# comparing country-level age distributions with gpw data
library(tidyverse)
library(tidylog)

# each age group comparison
# these are at admin1 level, so need to aggregate to country
gpw_data <- read_csv("data/cleaned_gpw_age_data_props.csv") 
country_gpw <- gpw_data %>% group_by(ISOALPHA, COUNTRYNM) %>% 
  summarise(A00_04B = weighted.mean(A00_04B, tot_popn_2020, na.rm = T),
            A05_09B = weighted.mean(A05_09B, tot_popn_2020, na.rm = T),
            A10_14B = weighted.mean(A10_14B, tot_popn_2020, na.rm = T),
            A15_19B = weighted.mean(A15_19B, tot_popn_2020, na.rm = T),
            A20_24B = weighted.mean(A20_24B, tot_popn_2020, na.rm = T),
            A25_29B = weighted.mean(A25_29B, tot_popn_2020, na.rm = T),
            A30_34B = weighted.mean(A30_34B, tot_popn_2020, na.rm = T),
            A35_39B = weighted.mean(A35_39B, tot_popn_2020, na.rm = T),
            A40_44B = weighted.mean(A40_44B, tot_popn_2020, na.rm = T),
            A45_49B = weighted.mean(A45_49B, tot_popn_2020, na.rm = T),
            A50_54B = weighted.mean(A50_54B, tot_popn_2020, na.rm = T),
            A55_59B = weighted.mean(A55_59B, tot_popn_2020, na.rm = T),
            A60_64B = weighted.mean(A60_64B, tot_popn_2020, na.rm = T),
            A65_69B = weighted.mean(A65_69B, tot_popn_2020, na.rm = T),
            A70_74B = weighted.mean(A70_74B, tot_popn_2020, na.rm = T),
            A75_79B = weighted.mean(A75_79B, tot_popn_2020, na.rm = T),
            A80_84B = weighted.mean(A80_84B, tot_popn_2020, na.rm = T),
            A85PLUSB = weighted.mean(A85PLUSB, tot_popn_2020, na.rm = T))

# from: https://population.un.org/wpp/Download/Standard/Population/
# had to convert to csv manually, hopefully didn't lose info?
un_data <- read_csv("data/WPP2022_POP_F02_1_POPULATION_5-YEAR_AGE_GROUPS_BOTH_SEXES copy.csv") %>% 
  filter(Year == 2020, ! is.na(`ISO3 Alpha-code`)) %>% 
  rename(ISO = `ISO3 Alpha-code`, COUNTRYNM = `Region, subregion, country or area *`) %>% 
  rowwise() %>% 
  mutate(across(`0-4`:`100+`, ~ as.numeric(gsub(" ", "", .x)) * 1000)) %>%  # because data is in thousands
  rename(A00_04B = `0-4`, A05_09B = `5-9`, A10_14B = `10-14`, A15_19B = `15-19`,
         A20_24B = `20-24`, A25_29B = `25-29`, A30_34B = `30-34`, A35_39B = `35-39`,
         A40_44B = `40-44`, A45_49B = `45-49`, A50_54B = `50-54`, A55_59B = `55-59`,
         A60_64B = `60-64`, A65_69B = `65-69`, A70_74B = `70-74`, A75_79B = `75-79`,
         A80_84B = `80-84`) %>% 
  rowwise() %>% 
  mutate(A85PLUSB = sum(`85-89`, `90-94`, `95-99`, `100+`, na.rm = T)) %>% 
  select(-c(`85-89`, `90-94`, `95-99`, `100+`)) %>% 
  # now convert to proportions
  rowwise() %>% 
  mutate(row_sum = sum(A00_04B, A05_09B, A10_14B, A15_19B, A20_24B, A25_29B, 
                       A30_34B, A35_39B,A40_44B, A45_49B, A50_54B, A55_59B, 
                       A60_64B, A65_69B, A70_74B, A75_79B, A80_84B, A85PLUSB),
         A00_04B = A00_04B/row_sum, A05_09B = A05_09B/row_sum, A10_14B = A10_14B/row_sum,
         A15_19B = A15_19B/row_sum, A20_24B = A20_24B/row_sum, A25_29B = A25_29B/row_sum, 
         A30_34B = A30_34B/row_sum, A35_39B = A35_39B/row_sum, A40_44B = A40_44B/row_sum,
         A45_49B = A45_49B/row_sum, A50_54B = A50_54B/row_sum, A55_59B = A55_59B/row_sum,
         A60_64B = A60_64B/row_sum, A65_69B = A65_69B/row_sum, A70_74B = A70_74B/row_sum,
         A75_79B = A75_79B/row_sum, A80_84B = A80_84B/row_sum, A85PLUSB = A85PLUSB/row_sum)

## time to join
gpw_long <- country_gpw %>% pivot_longer(c(A00_04B:A85PLUSB), values_to = "gpw_age_prop",
                                         names_to = "age_group")
un_long <- un_data %>% pivot_longer(c(A00_04B:A85PLUSB), values_to = "un_age_prop",
                                         names_to = "age_group")

all_data <- un_long %>% select(-COUNTRYNM) %>% 
  mutate(ISO = ifelse(ISO == "XKX", "XKO", ISO)) %>%  # fixing kosovo
  left_join(gpw_long, by = c("ISO" = "ISOALPHA", "age_group")) %>% 
  relocate(COUNTRYNM, .after = ISO) %>% 
  select(ISO, COUNTRYNM, row_sum, age_group, un_age_prop, gpw_age_prop) %>% 
  rowwise() %>% 
  mutate(diff = gpw_age_prop - un_age_prop,
         abs_diff = abs(diff)) %>% 
  filter(!is.nan(un_age_prop), !is.na(gpw_age_prop))

mean(all_data$diff, na.rm = T)
mean(all_data$abs_diff, na.rm = T)

# load in region data
region_data <-  read_csv("data/cessation_coverage_estimates.csv") %>% 
  rename(ISO = Country_ISO_Code) %>% 
  select(ISO, Region) %>% 
  mutate(grouped_region = case_when(Region == "SA" ~ "Asia",
                                    Region == "SE" ~ "Asia",
                                    Region == "AM" ~ "Americas",
                                    Region == "ME" ~ "Asia",
                                    Region == "EU" ~ "Europe",
                                    Region == "CB" ~ "Americas",
                                    Region == "SR" ~ "Europe",
                                    Region == "PA" ~ "Pacific",
                                    Region == "AF" ~ "Africa")) %>% 
  select(-Region)

all_data <- all_data %>% left_join(region_data) %>% 
  mutate(grouped_region = ifelse(!is.na(grouped_region), grouped_region,
                                 ifelse(COUNTRYNM %in% c("china, macao special administrative region", "maldives"),
                                        "Asia",
                                        ifelse(COUNTRYNM %in% c("aruba", "curacao", "sint maarten (dutch part)"),
                                               "Americas",
                                               ifelse(COUNTRYNM == "kiribati", "Pacific", NA)))))

library(MetBrewer)
all_data %>% filter(ISO != "SPM") %>% 
  ggplot(aes(x = un_age_prop * 100, y = gpw_age_prop * 100,
             col = grouped_region)) +
  geom_point(alpha = 0.7) +
  geom_abline() + 
  labs(x = "UN country-level percent in age group",
       y = "GPW admin1-aggregated country-level\npercent in age group",
       col = "Region") +
  # ylim(0, 50) +
  # xlim(0, 50) +
  scale_color_manual(values = met.brewer("Johnson", 5)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text =  element_text(size = 12),
        legend.title = element_text(size = 14)) +
  facet_wrap(~as.factor(age_group))
ggsave("figures/age-distribution-comparison-facet-nooutlier.pdf", height = 6, width = 8, dpi = 600)
#ggsave("figures/age-distribution-comparison.png", height = 6, width = 8, dpi = 600)

all_data %>% group_by(age_group) %>% 
  summarise(mean_diff = mean(diff, na.rm = T),
            mean_abs_diff = mean(abs_diff, na.rm = T))

all_data %>% filter(ISO != "SPM") %>% 
  ggplot(aes(x = age_group, y = diff)) + #, col = grouped_region)) +
  geom_boxplot() + 
  geom_hline(yintercept = 0, col = "red") +
  labs(x = "Age group",
       y = "Difference")

all_data %>% filter(ISO != "SPM") %>% 
  ggplot(aes(x = age_group, y = abs_diff)) + #, col = grouped_region)) +
  geom_boxplot() + 
  geom_hline(yintercept = 0, col = "red") +
  labs(x = "Age group",
       y = "Absolute difference")



# now need to parameterize normal distributions that i'm drawing from by coming up with one 
#   standard deviation per country such that 95% of the data is within 2 stdevs (or 99% within 3)
# age_dist_differences <- all_data %>% group_by(ISO, COUNTRYNM) %>% 
#   summarise(perc_95 = quantile(abs_diff, probs = c(0.95)),
#             country_sd = perc_95/2,
#             maximum = max(abs_diff),
#             test = perc_95 <= ((2/3) * maximum))
# write_csv(age_dist_differences, "data/age_dist_absolute_differences_stdevs.csv")

age_dist_differences <- all_data %>% group_by(ISO, COUNTRYNM) %>% 
  summarise(country_sd = max(abs_diff)/3)
write_csv(age_dist_differences, "data/age_dist_absolute_differences_stdevs.csv")


### before and after cessation comparison - OLD

gpw_data <- read_csv("data/cleaned_gpw_age_data.csv") %>% 
  group_by(ISOALPHA, COUNTRYNM) %>% 
  summarise(tot_popn_2020 = sum(tot_popn_2020), 
            A00_04B = sum(A00_04B), A05_09B = sum(A05_09B), A10_14B = sum(A10_14B),
            A15_19B = sum(A15_19B), A20_24B = sum(A20_24B), A25_29B = sum(A25_29B), 
            A30_34B = sum(A30_34B), A35_39B = sum(A35_39B), A40_44B = sum(A40_44B),
            A45_49B = sum(A45_49B), A50_54B = sum(A50_54B), A55_59B = sum(A55_59B),
            A60_64B = sum(A60_64B), A65_69B = sum(A65_69B), A70_74B = sum(A70_74B),
            A75_79B = sum(A75_79B), A80_84B = sum(A80_84B), A85PLUSB = sum(A85PLUSB)) %>% 
  ungroup()
cessation_dates <-  read_csv("data/cessation_coverage_estimates.csv") %>% 
  rename(ISO = Country_ISO_Code, 
         vax_stopped = cessdate_orig) %>% 
  select(ISO, Region, vax_stopped)

joined_data <- gpw_data %>% pivot_longer(cols = A00_04B:A85PLUSB, values_to = "popn", names_to = "age") %>% 
  rowwise() %>% 
  mutate(min_age = as.numeric(substring(age, 2, 3)),
         max_age = ifelse(age == "A85PLUSB", 100, as.numeric(substring(age, 5, 6))), # can ignore warning
         min_birth_year = 2022 - max_age,
         max_birth_year = 2022 - min_age) %>% 
  left_join(cessation_dates, by = c("ISOALPHA" = "ISO")) %>% 
  rowwise() %>% 
  mutate(is_before_cessation = ifelse(is.na(vax_stopped), NA,
                                      vax_eligible(min_birth_year, max_birth_year, vax_stopped)),
         people_eligible = is_before_cessation * popn)
  

joined_data_prop <- joined_data %>% group_by(ISOALPHA, COUNTRYNM, Region) %>% 
  summarise(frac_born_before_cessation = sum(people_eligible)/tot_popn_2020) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(grouped_region = case_when(Region == "SA" ~ "Asia",
                                    Region == "SE" ~ "Asia",
                                    Region == "AM" ~ "Americas",
                                    Region == "ME" ~ "Asia",
                                    Region == "EU" ~ "Europe",
                                    Region == "CB" ~ "Americas",
                                    Region == "SR" ~ "Europe",
                                    Region == "PA" ~ "Pacific",
                                    Region == "AF" ~ "Africa")) %>% 
  select(-Region)

# from: https://population.un.org/wpp/Download/Standard/Population/
# had to convert to csv manually, hopefully didn't lose info?
un_data <- read_csv("data/WPP2022_POP_F02_1_POPULATION_5-YEAR_AGE_GROUPS_BOTH_SEXES copy.csv") %>% 
  filter(Year == 2020, ! is.na(`ISO3 Alpha-code`)) %>% 
  rename(ISO = `ISO3 Alpha-code`, COUNTRYNM = `Region, subregion, country or area *`) %>% 
  rowwise() %>% 
  mutate(across(`0-4`:`100+`, ~ as.numeric(gsub(" ", "", .x)) * 1000), # because data is in thousands
         total_pop = sum(`0-4`, `5-9`, `10-14`, `15-19`, `20-24`, `25-29`, `30-34`,
                         `35-39`, `40-44`, `45-49`, `50-54`, `55-59`, `60-64`, `65-69`,
                         `70-74`, `75-79`, `80-84`, `85-89`, `90-94`, `95-99`, `100+`,
                         na.rm = T))
# now need proportion born before cessation date, one tricky thing is that in GPW data we 
# rescale 2010 data to 2020 and then pretend it's 2022
# so here is it appropriate to use the year 2021

# I think let's just ignore the issue for now, pretty much same steps as GPW data
un_data_long <- un_data %>% pivot_longer(cols = `0-4`:`100+`, values_to = "popn", names_to = "age") %>% 
  rowwise() %>% 
  mutate(min_age = ifelse(age == "100+", 100, as.numeric(strsplit(age, "-")[[1]][1])), # grab first number
         max_age = ifelse(age == "100+", 110, as.numeric(strsplit(age, "-")[[1]][2])), # can ignore warning
         min_birth_year = 2022 - max_age,
         max_birth_year = 2022 - min_age) %>% 
  left_join(cessation_dates) %>% 
  rowwise() %>% 
  mutate(is_before_cessation = ifelse(is.na(vax_stopped), NA,
                                      vax_eligible(min_birth_year, max_birth_year, vax_stopped)),
         people_eligible = is_before_cessation * popn)

un_data_prop <- un_data_long %>% group_by(ISO, COUNTRYNM, total_pop) %>% 
  summarise(frac_born_before_cessation = sum(people_eligible)/total_pop) %>% 
  distinct() %>% 
  ungroup()

all_data <- un_data_prop %>% rename(un_prop = frac_born_before_cessation) %>% 
  select(-COUNTRYNM) %>% 
  left_join(joined_data_prop, by = c("ISO" = "ISOALPHA")) %>% 
  relocate(un_prop, .after = COUNTRYNM) %>% 
  rowwise() %>% 
  mutate(diff = frac_born_before_cessation - un_prop,
         abs_diff = abs(diff))

mean(all_data$diff, na.rm = T)
mean(all_data$abs_diff, na.rm = T)

library(MetBrewer)
all_data %>% filter(!is.na(frac_born_before_cessation)) %>% 
  ggplot(aes(x = un_prop * 100, y = frac_born_before_cessation * 100,
             col = grouped_region, size = sqrt(total_pop))) +
  geom_point(alpha = 0.7) +
  geom_abline() + 
  labs(x = "UN country-level percent born before cessation",
       y = "GPW admin1-aggregated country-level\npercent born before cessation",
       col = "Region", size = "UN population\n2020 (sqrt)") +
  scale_color_manual(values = met.brewer("Johnson", 5)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text =  element_text(size = 12),
        legend.title = element_text(size = 14))
ggsave("figures/age-distribution-comparison.pdf", height = 6, width = 8, dpi = 600)
ggsave("figures/age-distribution-comparison.png", height = 6, width = 8, dpi = 600)
