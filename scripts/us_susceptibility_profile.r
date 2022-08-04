### CALCULATES SUSCEPTIBILITY PROFILE FOR THE US, uses place of birth to determine chance vaccinated
###   then aggregates to state level using weighted mean to get prop susceptible
### Juliana Taube

source("scripts/load_files_for_run.r")

# create is_vaxxed binary
df <- pums_data %>% left_join(cessation_coverage_data, by = "POBP") %>% 
  left_join(polio_proxy_coverage, by = c("POBP" = "fips_of_birth")) %>%
  rowwise() %>% 
  mutate(vax_stopped = ifelse(POBP < 100, 1972, vax_stopped), # set all US to 1972
         Country = ifelse(POBP < 60, "USA", Country),
         chance_vaxxed = ifelse(POBP >= 100, 
                                calc_vax_coverage_foreign(birth_year, Survey_Year,
                                                          vax_stopped, cvg_514_orig,
                                                          cvg_over14_orig, cvg_tot_orig),
                                ifelse(birth_year <= vax_stopped, spx_coverage, 0)))
                                     
df2 <- df %>% select(PUMA, ST, AGEP, POBP, PWGTP, birth_year, chance_vaxxed) %>% 
  filter(birth_year <= 2003) %>% 
  mutate(age_in_2003 = 2003 - birth_year)

# we don't care where they were born, just where they live now
# but I used where they were born to calculate their chance vaxxed
# df3 <- df2 %>% select(-POBP, -PUMA) %>% 
#   distinct()

# weighted
df4 <- df2 %>% select(-POBP, -PUMA) %>% 
  group_by(ST, age_in_2003) %>% 
  filter(ST %in% c(17, 18, 20, 29, 39, 55)) %>% 
  summarise(chance_vaxxed_wtd = weighted.mean(chance_vaxxed, PWGTP, na.rm = T))

write_csv(df4, "data/susc_profile_us_2003.csv")

# want Illinois, Indiana, Kansas, Missouri, Ohio, Wisconsin
# 17, 18, 20, 29, 39, 55






######

gpw <- read_csv("data/cleaned_gpw_age_data.csv")

# ok now trying to incorporate cessation (and eventually coverage data)
country_dates <- read_csv("data/vaccination_discontinuation.csv") %>% 
  rename(vax_stopped = Vaccine_discontinued_estimated) %>% 
  select(Country_ISO_Code, Country, vax_stopped)

data_long <- gpw %>% pivot_longer(cols = A00_04B:A85PLUSB, values_to = "popn", names_to = "age") %>% 
  mutate(min_age = as.numeric(substring(age, 2, 3)),
         max_age = ifelse(age == "A85PLUSB", 100, as.numeric(substring(age, 5, 6))), # can ignore warning
         min_birth_year = 2022 - max_age,
         max_birth_year = 2022 - min_age)

data_w_stop <- data_long %>% left_join(country_dates, by = c("ISOALPHA" = "Country_ISO_Code"))

scar_survey_coverage <- read_csv("data/scar_survey_coverage.csv")

# i think here is where coverage will get incorporated, instead of are_vaxxed = 1, we will have coverage
# for that age group
vaxxed_by_age <- data_w_stop %>% left_join(scar_survey_coverage, by = c("ISOALPHA" = "Country_ISO_Code")) %>%
  rowwise() %>%
  mutate(prop_vaxxed_by_age = calc_vax_coverage(min_birth_year, max_birth_year, 
                                                Survey_Year, vax_stopped,
                                                prop_vax_5to14, prop_vax_over14, 
                                                prop_vax_all_ages))


#### susceptibility profile --------------------------------------------------

# has to happen before ages get aggregated
susc_profile <- vaxxed_by_age %>% mutate(prop_susc = 1 - (prop_vaxxed_by_age * WANING)) %>% 
  filter(!is.na(prop_susc)) %>% 
  select(ISOALPHA:popn, prop_vaxxed_by_age:prop_susc) %>% 
  group_by(COUNTRYNM, age) %>% 
  summarise(prop_susceptible = weighted.mean(prop_susc, popn, na.rm = T)) %>% 
  mutate(age = as.factor(age))
levels(susc_profile$age) <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                              "30-34", "35-39", "40-44", "45-49", "50-54", 
                              "55-59", "60-64", "65-69", "70-74", "75-79",
                              "80-84", "85+")

pdf("figures/sus-profile-all.pdf", height = 20, width = 20)
susc_profile %>% ggplot(aes(x = age, y = prop_susceptible, group = COUNTRYNM,
                            col = COUNTRYNM)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "bottom")
facet_wrap(~COUNTRYNM)
dev.off()