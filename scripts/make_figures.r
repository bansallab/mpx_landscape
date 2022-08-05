### CALCULATES ESTIMATES, MAKES MAIN FIGURES FOR MANUSCRIPT, AND SOME SUPPLEMENTARY FIGURES
### Juliana Taube

source("scripts/load_files_for_run.r")

### CALLING FUNCTIONS TO MAKE FIGURES ### -------------------------------------

# main_estimates <- main_function(other_age_data = NA, is_diff_age_dist = FALSE,
#                                 is_world_age_dist = FALSE, is_100_covg = FALSE,
#                                 is_1984_end = FALSE, waning = 0.807,
#                                 include_natural_immunity = FALSE, is_bootstrap = FALSE)
# write_csv(main_estimates, "estimates/world.csv")
main_estimates <- read_csv("estimates/world.csv")

# could use this for bar chart now
national_values <- main_estimates %>% group_by(ISOALPHA, COUNTRYNM) %>% 
  summarise(mean_susceptibility = weighted.mean(perc_susceptible, pop_size),
            overall_cvg = weighted.mean(overall_cvg, pop_size, na.rm = T),
            mean_age = weighted.mean(mean_age, pop_size, na.rm = T),
            perc_born_before_1980 = weighted.mean(perc_born_before_1980, pop_size, na.rm = T),
            perc_born_before_cessation = weighted.mean(perc_born_before_cessation, pop_size, na.rm = T),
            min_admin1_susc = min(perc_susceptible),
            max_admin1_susc = max(perc_susceptible),
            std_admin1_susc = sd(perc_susceptible)) %>% 
  mutate(across(c(mean_susceptibility, overall_cvg, perc_born_before_cessation),
                ~ ifelse(is.nan(.x), NA_real_, .x))) # replace NaN with NA
# write_csv(national_values, "estimates/world-by-country.csv")
# post-hoc fix to include regions without rerunning right now
state_values <- main_estimates %>% filter(COUNTRYNM == "united states") %>% 
  left_join((state_fips %>% select(state, fips)), by = c("NAME1" = "state")) %>% 
  left_join((pums_data %>% select(ST, REGION) %>% distinct()), by = c("fips" = "ST"))
# write_csv(state_values, "estimates/us-by-state.csv")

### scatters
boot_notes <- read_csv("data/data/cessation_coverage_estimates.csv") %>% 
  mutate(is_default_cvg = ifelse(is.na(`Vax Coverage Methods/Notes`), 0,
                                 ifelse(`Vax Coverage Methods/Notes` == "Default WHO target coverage (80%)",
                                        1, 0)),
         is_default_cess = ifelse(is.na(`Cessation Date Method/Notes`), 0,
                                  ifelse(`Cessation Date Method/Notes` == "Default WHO cessation date (1980)", 
                                         1, 0)),
         grouped_region = case_when(Region == "SA" ~ "Asia",
                                    Region == "SE" ~ "Asia",
                                    Region == "AM" ~ "Americas",
                                    Region == "ME" ~ "Asia",
                                    Region == "EU" ~ "Europe",
                                    Region == "CB" ~ "Americas",
                                    Region == "SR" ~ "Europe",
                                    Region == "PA" ~ "Pacific",
                                    Region == "AF" ~ "Africa")) %>% 
  rename(ISOALPHA = Country_ISO_Code) %>% 
  select(ISOALPHA, grouped_region, is_default_cvg, is_default_cess, cessdate_orig)
# GLOBAL first
# remove countries with no estimates
national_values <- national_values %>% 
  filter(!is.na(mean_susceptibility)) %>% 
  left_join(boot_notes) %>% 
  mutate(grouped_region = ifelse(COUNTRYNM == "united states", "Americas", grouped_region)) %>% 
  distinct()
# pearson correlations
c1 <- cor.test((national_values %>% filter(is_default_cvg == 0, overall_cvg >= 40))$mean_susceptibility,
               (national_values %>% filter(is_default_cvg == 0, overall_cvg >= 40))$overall_cvg)
# c2 <- cor.test(national_values$mean_susceptibility, national_values$mean_age)
c3 <- cor.test(national_values$mean_susceptibility, national_values$perc_born_before_1980)
# c4 <- cor.test((national_values %>% filter(is_default_cess == 0))$mean_susceptibility,
#                (national_values %>% filter(is_default_cess == 0))$perc_born_before_cessation)
c5 <- cor.test((national_values %>% filter(is_default_cess == 0, cessdate_orig >= 1965))$mean_susceptibility,
               (national_values %>% filter(is_default_cess == 0, cessdate_orig >= 1965))$cessdate_orig)

# 44 missing mean_susceptibility, ignore warning
# vax coverage
p1 <- national_values %>%  
  filter(is_default_cvg == 0) %>% # remove default 80% coverages
  #filter(overall_cvg > 60) %>%  # remove outliers
  ggplot(aes(x = overall_cvg, y = mean_susceptibility)) +
  geom_point(aes(col = grouped_region), size = 3, alpha = 0.5) +
  # remove outliers & defaults from smooth
  geom_smooth(data = (national_values %>% filter(is_default_cvg == 0, overall_cvg >= 40)),
              method = "lm", col = "black") +
  labs(x = "Smallpox vaccination coverage", y = "Percent susceptible", col = "Region") +
  ylim(55, 95) +
  scale_x_continuous(breaks = c(30, 50, 70, 90)) +
  scale_color_manual(values = met.brewer("Johnson", 5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 13.5),
        axis.title = element_text(size = 15.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))
# mean age
# p2 <- national_values %>% ggplot(aes(x = mean_age, y = mean_susceptibility)) +
#   geom_point(size = 2, col = "dimgrey", alpha = 0.75) +
#   geom_smooth(method = "lm", col = "black") +
#   labs(x = "Mean age", y = "Percent susceptible") +
#   ylim(55, 95) +
#   theme_bw()
# born before 1980
p3 <- national_values %>% 
  ggplot(aes(x = perc_born_before_1980, y = mean_susceptibility)) +
  geom_point(aes(col = grouped_region), size = 3, alpha = 0.5) +
  geom_smooth(method = "lm", col = "black") +
  labs(x = "Percent born before 1980", y = "Percent susceptible", col = "Region") +
  ylim(55, 95) +
  xlim(10, 58) +
  scale_color_manual(values = met.brewer("Johnson", 5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 13.5),
        axis.title = element_text(size = 15.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))
# born before cessation
# p4 <- national_values %>%  
#   filter(is_default_cess == 0) %>% # remove default cess dates
#   ggplot(aes(x = perc_born_before_cessation, y = mean_susceptibility)) +
#   geom_point(size = 2, col = "dimgrey", alpha = 0.75) +
#   geom_smooth(method = "lm", col = "black") +
#   labs(x = "Percent born before cessation", y = "Percent susceptible") +
#   ylim(55, 95) +
#   theme_bw()
# cessation date
p5 <- national_values %>% 
  filter(is_default_cess == 0) %>%  # remove default cess dates
  ggplot(aes(x = cessdate_orig, y = mean_susceptibility)) +
  geom_point(aes(col = grouped_region), size = 3, alpha = 0.5) +
  geom_smooth(data = (national_values %>% filter(is_default_cess == 0, cessdate_orig >= 1965)),
              method = "lm", col = "black") +
  labs(x = "Smallpox vaccination cessation", y = "Percent susceptible", col = "Region") +
  ylim(55, 95) +
  scale_x_continuous(breaks = c(1960, 1964, 1968, 1972, 1976, 1980, 1984),
                     labels = c("1960", "", "1968", "", "1976", "", "1984")) +
  scale_color_manual(values = met.brewer("Johnson", 5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 13.5),
        axis.title = element_text(size = 15.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

fig1_for_legend <- map_main(main_estimates,
                            do_plot_difference = FALSE, var_to_plot = "perc_susceptible",
                            legend_title = "Population\nsusceptibility\n(% of popn)", 
                            lower = 45, upper = 100, palette = "VanGogh3",
                            show_legend = TRUE, wide_legend = FALSE)

legend <- cowplot::get_legend(fig1_for_legend)
fig1_map <- map_main(main_estimates,
                     do_plot_difference = FALSE, var_to_plot = "perc_susceptible",
                     legend_title = "Population\nsusceptibility\n(% of popn)", 
                     lower = 45, upper = 100, palette = "VanGogh3",
                     show_legend = FALSE, wide_legend = FALSE)


ggarrange(fig1_map + inset_element(legend, left = 0, bottom = 0.1, right = 0.25, top = 0.5, 
                                   align_to = "panel", on_top = T),
          ggarrange(p1, p5, p3, ncol = 3, nrow = 1, labels = c("B", "C", "D"), 
                    font.label = list(size = 20), common.legend = TRUE, legend = "bottom"), 
          ncol = 1, nrow = 2, heights = c(5, 4), labels = c("A", ""),
          font.label = list(size = 20))
ggsave("figures/fig1.png", height = 8, width = 11, dpi = 600)
ggsave("figures/fig1.pdf", height = 8, width = 11, dpi = 600)
dev.off()

# us scatterplots
# pearson correlations
c1 <- cor.test((state_values %>% filter(overall_cvg >= 85))$perc_susceptible,
               (state_values %>% filter(overall_cvg >= 85))$overall_cvg)
# c2 <- cor.test(state_values$perc_susceptible, state_values$mean_age)
c3 <- cor.test(state_values$perc_susceptible, state_values$perc_born_before_1980)
# c4 <- cor.test(state_values$perc_susceptible, state_values$perc_born_before_cessation)
c5 <- cor.test(state_values$perc_susceptible, state_values$perc_foreign_born)

state_values <- state_values %>% 
  mutate(region = as.factor(case_when(REGION == 3 ~ "South",
                            REGION == 4 ~ "West",
                            REGION == 1 ~ "Northeast",
                            REGION == 2 ~ "Midwest")))
# vax coverage
p1 <- state_values %>% 
  ggplot(aes(x = overall_cvg, y = perc_susceptible)) +
  geom_point(aes(col = region), size = 3, alpha = 0.6) +
  # remove outliers from smooth
  geom_smooth(data = (state_values %>% filter(overall_cvg >= 85)),
              method = "lm", col = "black") +
  labs(x = "Smallpox vaccination coverage", y = "Percent susceptible", col = "Region") +
  ylim(55, 95) +
  scale_color_manual(values = met.brewer("Egypt", 4)) +
  theme_bw() +
  theme(axis.text = element_text(size = 13.5),
        axis.title = element_text(size = 15.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))
# mean age
# p2 <- state_values %>% ggplot(aes(x = mean_age, y = perc_susceptible)) +
#   geom_point(size = 2, col = "dimgrey", alpha = 0.75) +
#   geom_smooth(method = "lm", col = "black") +
#   labs(x = "Mean age", y = "Percent susceptible") +
#   ylim(55, 95) +
#   theme_bw()
# born before 1980
p3 <- state_values %>% ggplot(aes(x = perc_born_before_1980, y = perc_susceptible)) +
  geom_point(aes(col = region), size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", col = "black") +
  labs(x = "Percent born before 1980", y = "Percent susceptible", col = "Region") +
  ylim(55, 95) +
  scale_color_manual(values = met.brewer("Egypt", 4)) +
  theme_bw() +
  theme(axis.text = element_text(size = 13.5),
        axis.title = element_text(size = 15.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))
# born before cessation
# p4 <- state_values %>%
#   ggplot(aes(x = perc_born_before_cessation, y = perc_susceptible)) +
#   geom_point(size = 2, col = "dimgrey", alpha = 0.75) +
#   geom_smooth(method = "lm", col = "black") +
#   labs(x = "Percent born before cessation", y = "Percent susceptible") +
#   ylim(55, 95) +
#   theme_bw()
# immigrant proportion
p5 <- state_values %>% 
  ggplot(aes(x = perc_foreign_born, y = perc_susceptible)) +
  geom_point(aes(col = region), size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", col = "black") +
  labs(x = "Percent foreign-born", y = "Percent susceptible", col = "Region") +
  ylim(55, 95) +
  scale_color_manual(values = met.brewer("Egypt", 4)) +
  theme_bw() +
  theme(axis.text = element_text(size = 13.5),
        axis.title = element_text(size = 15.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

# us fine map
# us_estimates <- main_us_function(data_to_use = pums_data, is_diff_age_dist = FALSE,
#                                  for_world_map = FALSE, is_100_covg = FALSE,
#                                  is_1984_end = FALSE, waning = 0.807,
#                                  no_immigration = FALSE,
#                                  cess_cvg_data = cessation_coverage_data)
# write_csv(us_estimates, "estimates/us.csv")
us_estimates <- read_csv("estimates/us.csv")

fig2_map <- map_us(us_estimates,
                   do_plot_difference = FALSE,
                   var_to_plot = "perc_susceptible",
                   legend_title = "Population\nsusceptibility\n(% of popn)",
                   lower = 45, upper = 100, palette = "VanGogh3", 
                   show_legend = FALSE)

ggarrange(fig2_map + inset_element(legend, left = 0.85, bottom = 0.05, right = 1, top = 0.45, 
                                   align_to = "full", on_top = T),
          ggarrange(p1, p5, p3, ncol = 3, nrow = 1, labels = c("B", "C", "D"), 
                    font.label = list(size = 20), common.legend = TRUE, legend = "bottom"), 
          ncol = 1, nrow = 2, heights = c(5, 3.5), labels = c("A", ""),
          font.label = list(size = 20))
ggsave("figures/fig2.png", height = 9, width = 11, dpi = 600)
ggsave("figures/fig2.pdf", height = 9, width = 11, dpi = 600)
dev.off()

### country bar plot
bar_data <- national_values %>% 
  left_join(read_csv("data/cessation_coverage_estimates.csv") %>% select(Country_ISO_Code, Region),
            by = c("ISOALPHA" = "Country_ISO_Code")) %>% 
  mutate(Region = as.factor(ifelse(COUNTRYNM == "united states", "AM", Region))) %>% 
  distinct()
levels(bar_data$Region) # check each time that order is same
levels(bar_data$Region) <- c("South Asia", "Africa", "Europe", "Middle East",
                             "Americas", "Former Soviet Republic",
                             "Caribbean", "Pacific",  "Southeast and East Asia")
bar_data$Region <- factor(bar_data$Region, 
                          levels = c("Africa", "Americas", "Caribbean", "Europe",
                                     "Former Soviet Republic", "Middle East", "Pacific",
                                     "South Asia", "Southeast and East Asia"))

a <- bar_data %>% filter(Region %in% c("Africa", "Americas", "Caribbean")) %>% 
  ggplot(aes(x = mean_susceptibility, 
             y = fct_reorder(COUNTRYNM, mean_susceptibility), 
             fill = Region)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(xmin = min_admin1_susc, xmax = max_admin1_susc), 
                width = 0.5, col = "black") +
  labs(x = "Percent susceptible", y = "Country") +
  coord_cartesian(xlim = c(50, 100)) +
  facet_grid(rows = vars(Region), scales = "free", space = "free") +
  # have to have scales free to not show each country in every region
  # have facet_grid so each is same width even though country name length varies
  # have space free so facets with more countries are taller
  scale_fill_manual(values=met.brewer("Archambault", 9)[1:3]) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14),
        strip.text.y = element_text(size = 16)) +
  guides(fill = "none")

b <- bar_data %>% filter(! Region %in% c("Africa", "Americas", "Caribbean")) %>% 
  ggplot(aes(x = mean_susceptibility, 
             y = fct_reorder(COUNTRYNM, mean_susceptibility), 
             fill = Region)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(xmin = min_admin1_susc, xmax = max_admin1_susc), 
                width = 0.5, col = "black") +
  labs(x = "Percent susceptible", y = "Country") +
  coord_cartesian(xlim = c(50, 100)) +
  facet_grid(rows = vars(Region), scales = "free", space = "free") +
  # have to have scales free to not show each country in every region
  # have facet_grid so each is same width even though country name length varies
  # have space free so facets with more countries are taller
  scale_fill_manual(values=met.brewer("Archambault", 9)[4:9]) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14),
        strip.text.y = element_text(size = 16)) +
  guides(fill = "none")

ggarrange(a, b, ncol = 2)
ggsave("figures/grouped-barplot-twocol.pdf", height = 16, width = 16)

### COUNTERFACTUAL PLOTS
# us no immigration
us_estimates <- read_csv("estimates/us.csv")
no_immigration <- main_us_function(data_to_use = pums_data, is_diff_age_dist = FALSE,
                                   for_world_map = FALSE, is_100_covg = FALSE,
                                   is_1984_end = FALSE, waning = 0.807, 
                                   no_immigration = TRUE, 
                                   cess_cvg_data = cessation_coverage_data)
no_im_diff <- no_immigration %>% rename(perc_susceptible_no_im = perc_susceptible,
                                    pop_size_no_im = pop_size) %>% 
  left_join(us_estimates, by = c("PUMA", "ST")) %>% 
  mutate(difference = perc_susceptible_no_im - perc_susceptible)
write_csv(no_im_diff, "estimates/no-immigration-counterfactual.csv")
fig3d <- map_us(no_im_diff, do_plot_difference = TRUE, var_to_plot = "difference",
                legend_title = "Difference in population susceptibility (% of popn)",
                palette = "Hiroshige", lower = -35, upper = 35,
                show_legend = FALSE) # just took max

state_protection <- us_estimates %>% group_by(ST) %>% 
  summarise(mean_protection = mean(prop_protected))


# 1984 vax cessation
cess_1984 <- main_function(other_age_data = NA, is_diff_age_dist = FALSE, 
                           is_world_age_dist = FALSE, is_100_covg = FALSE,
                           is_1984_end = TRUE, waning = 0.807, 
                           include_natural_immunity = FALSE, 
                           is_bootstrap = FALSE) 
cess_1984_diff <- cess_1984 %>% rename(perc_susceptible_1984 = perc_susceptible) %>% 
  select(ISOALPHA, COUNTRYNM, NAME1, perc_susceptible_1984) %>% 
  left_join(main_estimates) %>% 
  mutate(diff = perc_susceptible_1984 - perc_susceptible)
write_csv(cess_1984_diff, "estimates/cessation-1984-counterfactual.csv")
fig3a <- map_main(cess_1984,
                  do_plot_difference = TRUE, 
                  var_to_plot = "difference",
                  legend_title = "Difference in population susceptibility (% of popn)", 
                  lower = -35, upper = 35, palette = "Hiroshige",
                  show_legend = TRUE, wide_legend = TRUE,
                  diff_comparison_path = "estimates/world.csv")

# 100% scar coverage
covg_100 <- main_function(other_age_data = NA, is_diff_age_dist = FALSE, 
                          is_world_age_dist = FALSE, is_100_covg = TRUE,
                          is_1984_end = FALSE, waning = 0.807, 
                          include_natural_immunity = FALSE, 
                          is_bootstrap = FALSE) 
covg_100_diff <- covg_100 %>% rename(perc_susceptible_100 = perc_susceptible) %>% 
  select(ISOALPHA, COUNTRYNM, NAME1, perc_susceptible_100) %>% 
  left_join(main_estimates) %>% 
  mutate(diff = perc_susceptible_100 - perc_susceptible)
write_csv(covg_100_diff, "estimates/coverage-100-counterfactual.csv")
fig3b <- map_main(covg_100,
                  do_plot_difference = TRUE, 
                  var_to_plot = "difference",
                  legend_title = "Difference in population susceptibility (% of popn)", 
                  lower = -35, upper = 35, palette = "Hiroshige",
                  show_legend = TRUE, wide_legend = TRUE,
                  diff_comparison_path = "estimates/world.csv")

# world age distribution 
world_same <- main_function(other_age_data = world_age_dist, 
                            is_diff_age_dist = TRUE, 
                            is_world_age_dist = TRUE, is_100_covg = FALSE,
                            is_1984_end = FALSE, waning = 0.807, 
                            include_natural_immunity = FALSE, 
                            is_bootstrap = FALSE) 
world_same_diff <- world_same %>% rename(perc_susceptible_world = perc_susceptible) %>% 
  select(ISOALPHA, COUNTRYNM, NAME1, perc_susceptible_world) %>% 
  left_join(main_estimates) %>% 
  mutate(diff = perc_susceptible_world - perc_susceptible)
write_csv(world_same_diff, "estimates/world-age-dist-counterfactual.csv")
fig3c <- map_main(world_same,
                  do_plot_difference = TRUE, 
                  var_to_plot = "difference",
                  legend_title = "Difference in population susceptibility (% of popn)", 
                  lower = -35, upper = 35, palette = "Hiroshige",
                  show_legend = TRUE, wide_legend = TRUE,
                  diff_comparison_path = "estimates/world.csv")

ggarrange(fig3a, fig3b, fig3c, fig3d, ncol = 2, nrow = 2, labels = "AUTO",
          common.legend = TRUE, legend = "bottom",
          font.label = list(size = 30))
# +
#   inset_element(fig3_legend, left = 0, bottom = 0.45, right = 0.15, top = 0.8,
#                 align_to = "full", on_top = T)
ggsave("figures/fig3.pdf", height = 8, width = 16, dpi = 600)
ggsave("figures/fig3.png", height = 8, width = 16, dpi = 600)
dev.off()


# national age distribution 
national_same <- main_function(other_age_data = national_age_dist, 
                               is_diff_age_dist = TRUE, 
                               is_world_age_dist = FALSE, is_100_covg = FALSE,
                               is_1984_end = FALSE, waning = 0.807, 
                               include_natural_immunity = FALSE, 
                               is_bootstrap = FALSE) 
national_same_diff <- national_same %>% rename(perc_susceptible_national = perc_susceptible) %>% 
  select(ISOALPHA, COUNTRYNM, NAME1, perc_susceptible_national) %>% 
  left_join(main_estimates) %>% 
  mutate(diff = perc_susceptible_national - perc_susceptible)
write_csv(national_same_diff, "estimates/national-age-dist-counterfactual.csv")
figs1 <- map_main(national_same,
                  do_plot_difference = TRUE, 
                  var_to_plot = "difference",
                  legend_title = "Difference in\npopulation\nsusceptibility\n(% of popn)", 
                  lower = -35, upper = 35, palette = "Hiroshige",
                  show_legend = TRUE, wide_legend = FALSE,
                  diff_comparison_path = "estimates/world.csv")

figs1
ggsave("figures/national-age-distribution.pdf", height = 4, width = 8, dpi = 600)
ggsave("figures/national-age-distribution.png", height = 4, width = 8, dpi = 600)
dev.off()
rm(figs1)

# natural immunity
nat_imm <- main_function(other_age_data = NA,
                         is_diff_age_dist = FALSE, is_world_age_dist = FALSE,
                         is_100_covg = FALSE, is_1984_end = FALSE, 
                         waning = 0.807, include_natural_immunity = TRUE, 
                         is_bootstrap = FALSE)
natural_immunity_diff <- nat_imm %>% rename(perc_susceptible_natimm = perc_susceptible) %>% 
  select(ISOALPHA, COUNTRYNM, NAME1, perc_susceptible_natimm) %>% 
  left_join(main_estimates) %>% 
  mutate(diff = perc_susceptible_natimm - perc_susceptible)
write_csv(natural_immunity_diff, "estimates/natural-immunity-sensitivity.csv")
figs2 <- map_main(nat_imm,
                  do_plot_difference = TRUE, 
                  var_to_plot = "difference",
                  legend_title = "Difference in\npopulation\nsusceptibility\n(% of popn)", 
                  lower = -35, upper = 35, palette = "Hiroshige",
                  show_legend = TRUE, wide_legend = FALSE,
                  diff_comparison_path = "estimates/world.csv")

figs2zoom <- map_main(nat_imm,
                      do_plot_difference = TRUE, 
                      var_to_plot = "difference",
                      legend_title = "Difference in\npopulation\nsusceptibility\n(% of popn)", 
                      lower = NA, upper = NA, palette = "Hiroshige",
                      show_legend = TRUE, wide_legend = FALSE,
                      diff_comparison_path = "estimates/world.csv")

ggarrange(figs2, figs2zoom, ncol = 1, labels = "AUTO", font.label = list(size = 30))
ggsave("figures/nat_immunity.pdf", height = 6, width = 8, dpi = 600)
ggsave("figures/nat_immunity.png", height = 6, width = 8, dpi = 600)
dev.off()
rm(figs2)
rm(figs2zoom)

# waning/cross-immunity sensitivity analyses
effectiveness <- main_function(other_age_data = NA,
                               is_diff_age_dist = FALSE, is_world_age_dist = FALSE,
                               is_100_covg = FALSE, is_1984_end = FALSE, 
                               waning = 0.756, include_natural_immunity = FALSE, 
                               is_bootstrap = FALSE)
effectiveness_diff <- effectiveness %>% rename(perc_susceptible_eff = perc_susceptible) %>% 
  select(ISOALPHA, COUNTRYNM, NAME1, perc_susceptible_eff) %>% 
  left_join(main_estimates, by = c("ISOALPHA", "COUNTRYNM", "NAME1")) %>% 
  mutate(diff = perc_susceptible_eff - perc_susceptible)
write_csv(effectiveness_diff, "estimates/effective-75.6-sensitivity.csv")
effectiveness_fig <- map_main(effectiveness,
                              do_plot_difference = TRUE, 
                              var_to_plot = "difference",
                              legend_title = "Difference in population susceptibility (% of popn)", 
                              lower = -35, upper = 35, palette = "Hiroshige",
                              show_legend = TRUE, wide_legend = TRUE,
                              diff_comparison_path = "estimates/world.csv")

no_waning <- main_function(other_age_data = NA,
                           is_diff_age_dist = FALSE, is_world_age_dist = FALSE,
                           is_100_covg = FALSE, is_1984_end = FALSE, 
                           waning = 0.85, include_natural_immunity = FALSE, 
                           is_bootstrap = FALSE)
no_diff <- no_waning %>% rename(perc_susceptible_no = perc_susceptible) %>% 
  select(ISOALPHA, COUNTRYNM, NAME1, perc_susceptible_no) %>% 
  left_join(main_estimates) %>% 
  mutate(diff = perc_susceptible_no - perc_susceptible)
write_csv(no_diff, "estimates/effective-85-sensitivity.csv")
no_waning_fig <- map_main(no_waning,
                          do_plot_difference = TRUE, 
                          var_to_plot = "difference",
                          legend_title = "Difference in population susceptibility (% of popn)", 
                          lower = -35, upper = 35, palette = "Hiroshige",
                          show_legend = TRUE, wide_legend = TRUE,
                          diff_comparison_path = "estimates/world.csv")

ggarrange(effectiveness_fig, no_waning_fig,
          ncol = 2, nrow = 1, labels = "AUTO", font.label = list(size = 30),
          common.legend = TRUE, legend = "bottom")
ggsave("figures/waning-efficacy.pdf", height = 4, width = 16, dpi = 600)
ggsave("figures/waning-efficacy.png", height = 4, width = 16, dpi = 600)
dev.off()

variola_major <- main_function(other_age_data = NA,
                               is_diff_age_dist = FALSE, is_world_age_dist = FALSE,
                               is_100_covg = FALSE, is_1984_end = FALSE, 
                               waning = 0.911, include_natural_immunity = FALSE, 
                               is_bootstrap = FALSE)
write_csv(variola_major %>% select(ISOALPHA, COUNTRYNM, NAME1, perc_susceptible), 
          "estimates/variola-major-sensitivity.csv")
major_diff <- variola_major %>% rename(perc_susceptible_maj = perc_susceptible) %>% 
  select(ISOALPHA, COUNTRYNM, NAME1, perc_susceptible_maj) %>% 
  left_join(main_estimates) %>% 
  mutate(diff = perc_susceptible_maj - perc_susceptible)
variola_major_fig <- map_main(variola_major,
                              do_plot_difference = FALSE, 
                              var_to_plot = "perc_susceptible",
                              legend_title = "Population susceptibility (% of popn)", 
                              lower = 45, upper = 100, palette = "VanGogh3",
                              show_legend = TRUE, wide_legend = TRUE,
                              diff_comparison_path = NA)

variola_minor <- main_function(other_age_data = NA,
                               is_diff_age_dist = FALSE, is_world_age_dist = FALSE,
                               is_100_covg = FALSE, is_1984_end = FALSE, 
                               waning = 0.749, include_natural_immunity = FALSE, 
                               is_bootstrap = FALSE)
write_csv(variola_minor %>% select(ISOALPHA, COUNTRYNM, NAME1, perc_susceptible), 
          "estimates/variola-minor-sensitivity.csv")
minor_diff <- variola_minor %>% rename(perc_susceptible_min = perc_susceptible) %>% 
  select(ISOALPHA, COUNTRYNM, NAME1, perc_susceptible_min) %>% 
  left_join(main_estimates) %>% 
  mutate(diff = perc_susceptible_min - perc_susceptible)
variola_minor_fig <- map_main(variola_minor,
                              do_plot_difference = FALSE, 
                              var_to_plot = "perc_susceptible",
                              legend_title = "Population susceptibility (% of popn)", 
                              lower = 45, upper = 100, palette = "VanGogh3",
                              show_legend = TRUE, wide_legend = TRUE,
                              diff_comparison_path = NA)


ggarrange(variola_major_fig, variola_minor_fig,
          ncol = 2, nrow = 1, labels = "AUTO", font.label = list(size = 30),
          common.legend = TRUE, legend = "bottom")
ggsave("figures/variolas.pdf", height = 4, width = 16, dpi = 600)
ggsave("figures/variolas.png", height = 4, width = 16, dpi = 600)
dev.off()

### uncertainty analysis
uncertainty_estimates <- read_csv("data/bootstrapped_estimates_5000_musig.csv")
avg_diff <- uncertainty_estimates %>% 
  left_join(main_estimates) %>% 
  mutate(diff = mu - perc_susceptible)
write_csv(avg_diff, "estimates/uncertainty-differences.csv")
avg <- map_main(uncertainty_estimates %>% rename(perc_susceptible = mu),
                do_plot_difference = TRUE, var_to_plot = "difference",
                legend_title = "Difference in population\nsusceptibility (% of popn)",
                lower = -35, upper = 35, palette = "Hiroshige",
                show_legend = TRUE, wide_legend = TRUE,
                diff_comparison_path = "estimates/world.csv") +
  theme(legend.position = "bottom")

stdev <- map_main(uncertainty_estimates,
                  do_plot_difference = FALSE, var_to_plot = "sigma",
                  legend_title = "Standard deviation in\npopulation susceptibility\n(% of popn)",
                  lower = 0, upper = 4, palette = "Tam",
                  show_legend = TRUE, wide_legend = TRUE,
                  diff_comparison_path = NA) +
  theme(legend.position = "bottom")

ggarrange(avg, stdev, ncol = 1, nrow = 2, labels = "AUTO", font.label = list(size = 30))
ggsave("figures/bootstrap-fig.png", dpi = 600, height = 8, width = 10)
ggsave("figures/bootstrap-fig.pdf", dpi = 600, height = 8, width = 10)
dev.off()

### checking us immigration differences

immigrant_vs_not_vax <- pums_data %>% left_join(cessation_coverage_data, by = ("POBP")) %>% 
  left_join(polio_proxy_coverage, by = c("POBP" = "fips_of_birth")) %>%
  rowwise() %>% 
  mutate(vax_stopped = ifelse(POBP < 60, 1972, vax_stopped), # set all US to 1972
         Country = ifelse(POBP < 60, "USA", Country),
         chance_vaxxed = ifelse(POBP >= 60, 
                                calc_vax_coverage_foreign(birth_year, Survey_Year,
                                                          vax_stopped, cvg_514_orig,
                                                          cvg_over14_orig, cvg_tot_orig),
                                ifelse(birth_year <= vax_stopped, spx_coverage, 0)),
                                       # ifelse(!is.na(coverage), coverage, DEFAULT_COVERAGE),
                                       # 0)),
         foreign_born = ifelse(POBP < 100, 0, 1)) %>% # including US territories rn
  group_by(foreign_born) %>% 
  summarise(perc_susceptible = (1 - (weighted.mean(chance_vaxxed, PWGTP, na.rm = T) * 0.807)) * 100,
            mean_age = weighted.mean(AGEP, PWGTP, na.rm = T))



### playing with contours
# library(akima)
# # need to make a grid, see: https://stackoverflow.com/questions/65873211/empty-contour-plot-in-ggplot
# scatter_data_grid <- scatter_data %>% # remove default 80% coverages
#   filter(is_default_cvg == 0)
# # had an issue where had duplicate (x,y) coordinates leading to same/different z (UK repeated)
# grid <- akima::interp(scatter_data_grid$perc_born_before_1980_country, 
#                       scatter_data_grid$overall_cvg_country,
#                       scatter_data_grid$perc_susceptible)
# griddf <- data.frame(x = rep(grid$x, ncol(grid$z)), 
#                      y = rep(grid$y, each = nrow(grid$z)), 
#                      z = as.numeric(grid$z))
# c1 <- ggplot(data = griddf,
#              aes(x = x,
#                  y = y,
#                  z = z)) + 
#   geom_contour_filled(alpha = 0.8) + 
#   scale_fill_viridis_d(drop = FALSE) +
#   labs(x = "Percentage born before 1980", y = "Overall scar survey coverage",
#        fill = "Percent susceptible") +
#   theme(legend.position = "bottom")
# 
# # may want to add points: https://stackoverflow.com/questions/43268692/how-to-put-the-actual-data-points-on-the-contour-plot-with-ggplot
# library(metR)
# # from: https://eliocamp.github.io/codigo-r/en/2021/09/contour-labels/
# c2 <- ggplot(data = griddf,
#              aes(x = x,
#                  y = y,
#                  z = z)) + 
#   geom_contour() +
#   metR::geom_text_contour() + 
#   geom_point(data = scatter_data_grid, aes(x = perc_born_before_1980_country,
#                                            y = overall_cvg_country,
#                                            z = perc_susceptible,
#                                            colour = Region)) +
#   labs(x = "Percentage born before 1980", y = "Overall scar survey coverage",
#        fill = "Percent susceptible") +
#   theme(legend.position = "bottom")
# #scale_color_discrete(colors=met.brewer("Tam")) #gradientn
# 
# ggarrange(c2, c1, ncol = 2, labels = "AUTO")
# ggsave("figures/contours.pdf", height = 5, width = 12)
# 
