### CALCULATES AND PLOTS AVERAGE ADMIN1 AGE
### Juliana Taube

source("scripts/load_files_for_run.r")

data_long <- gpw %>% pivot_longer(cols = A00_04B:A85PLUSB, values_to = "popn", 
                                  names_to = "age") 

# mapping avg age per admin1 to see if we need that granularity
avg_age <- data_long %>% 
  mutate(min_age = as.numeric(substring(age, 2, 3)), # can ignore warning
         max_age = ifelse(age == "A85PLUSB", 100, as.numeric(substring(age, 5, 6))),
         mid_age = (max_age + min_age)/2) %>% # have to do this because of age range
  group_by(ISOALPHA, COUNTRYNM, NAME1) %>% 
  summarise(mean_age = weighted.mean(mid_age, popn, na.rm = T)) 

avg_age_us <- pums_data %>% group_by(ST) %>% 
  summarise(mean_age = weighted.mean(AGEP, PWGTP, na.rm = T)) %>% 
  left_join(state_fips, by = c("ST" = "fips")) %>% 
  select(state, mean_age) %>% 
  mutate(ISOALPHA = "USA",
         COUNTRYNM = "united states") %>% 
  rename(NAME1 = state)

avg_age_all <- avg_age %>% bind_rows(avg_age_us)

map_main(estimates = avg_age_all, 
         do_plot_difference = FALSE,
         var_to_plot = "mean_age",
         legend_title = "Average age",
         lower = 9, upper = 56, palette = "Tam",
         show_legend = TRUE, wide_legend = FALSE,
         diff_comparison_path = NA)
ggsave("figures/admin1-age-dist.png", height = 4, width = 8, dpi = 600)

# global average age
global_avg_age <- data_long %>% 
  mutate(min_age = as.numeric(substring(age, 2, 3)), # can ignore warning
         max_age = ifelse(age == "A85PLUSB", 100, as.numeric(substring(age, 5, 6))),
         mid_age = (max_age + min_age)/2) %>% # have to do this because of age range
  summarise(mean_age = weighted.mean(mid_age, popn, na.rm = T)) 
