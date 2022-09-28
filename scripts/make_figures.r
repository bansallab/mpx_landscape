### CALCULATES ESTIMATES, MAKES MAIN FIGURES FOR MANUSCRIPT, AND SOME SUPPLEMENTARY FIGURES
### Juliana Taube

source("scripts/load_files_for_run.r")

########################################################################
### GLOBAL ESTIMATES ###################################################
########################################################################
main_estimates <- calc_admin1_perc_vaxxed(vaxxed_by_age = calc_prop_vaxxed(other_age_data = NA,
                                                                                is_diff_age_dist = FALSE,
                                                                                is_world_age_dist = FALSE,
                                                                                is_100_covg = FALSE,
                                                                                is_1984_end = FALSE,
                                                                                is_bootstrap = FALSE),
                                          is_world_age_dist = FALSE,
                                          is_diff_age_dist = FALSE,
                                          is_100_covg = FALSE,
                                          is_1984_end = FALSE,
                                          is_bootstrap = FALSE) 

write_csv(main_estimates, "estimates/vaxxed-world.csv")
main_estimates <- read_csv("estimates/vaxxed-world.csv")

########################################################################
### COUNTRY LEVEL ESTIMATES FOR SCATTER AND BAR PLOTS ##################
########################################################################
# could use this for bar chart now
national_values <- main_estimates %>% group_by(ISOALPHA, COUNTRYNM) %>% 
  summarise(mean_vax = weighted.mean(perc_vaxxed, tot_popn_2020), # use 2020 population size!! weighting admin1 regions which we can do recently
            overall_cvg = weighted.mean(overall_cvg, tot_popn_2020, na.rm = T),
            mean_age = weighted.mean(mean_age, tot_popn_2020, na.rm = T),
            perc_born_before_1980 = weighted.mean(perc_born_before_1980, tot_popn_2020, na.rm = T),
            perc_born_before_cessation = weighted.mean(perc_born_before_cessation, tot_popn_2020, na.rm = T),
            min_admin1_vax = min(perc_vaxxed),
            max_admin1_vax = max(perc_vaxxed),
            std_admin1_vax = sd(perc_vaxxed)) %>% 
  ungroup() %>% 
  mutate(across(c(mean_vax, overall_cvg, perc_born_before_cessation),
                ~ ifelse(is.nan(.x), NA_real_, .x))) # replace NaN with NA
# write_csv(national_values, "estimates/world-by-country.csv")
# post-hoc fix to include regions without rerunning right now
state_values <- main_estimates %>% filter(ISOALPHA == "USA") %>%
  left_join((state_fips %>% select(state, fips)), by = c("NAME1" = "state")) %>%
  left_join((pums_data %>% select(ST, REGION) %>% distinct()), by = c("fips" = "ST"))

state_values <- state_values %>%
  mutate(region = as.factor(case_when(REGION == 3 ~ "South",
                                      REGION == 4 ~ "West",
                                      REGION == 1 ~ "Northeast",
                                      REGION == 2 ~ "Midwest")))
# write_csv(state_values, "estimates/us-by-state.csv")

### scatters
boot_notes <- read_csv("data/cessation_coverage_estimates.csv") %>% 
  mutate(is_default_cvg = ifelse(cvg_data_quality == "D", 1, 0),
         # is.na(`Vax Coverage Methods/Notes`), 0,
         #                         ifelse(grepl("Default WHO target coverage (80%)", `Vax Coverage Methods/Notes`),
         #                                1, 0)),
         is_default_cess = ifelse(cessation_data_quality == "D", 1, 0),
           # ifelse(is.na(`Cessation Date Method/Notes`), 0,
           #                        ifelse(`Cessation Date Method/Notes` == "Default WHO cessation date (1980)", 
           #                               1, 0)),
         grouped_region = case_when(Region == "SA" ~ "Asia",
                                    Region == "SE" ~ "Asia",
                                    Region == "AM" ~ "Americas",
                                    Region == "ME" ~ "Asia",
                                    Region == "EU" ~ "Europe",
                                    Region == "CB" ~ "Americas",
                                    Region == "SR" ~ "Europe",
                                    Region == "PA" ~ "Pacific",
                                    Region == "AF" ~ "Africa")) %>% 
  filter(!is.na(Country_ISO_Code)) %>% 
  rename(ISOALPHA = Country_ISO_Code) %>% 
  select(ISOALPHA, grouped_region, is_default_cvg, is_default_cess, cessdate_orig)
# GLOBAL first
# remove countries with no estimates
national_values <- national_values %>% 
  filter(!is.na(mean_vax)) %>% 
  left_join(boot_notes) %>% 
  mutate(grouped_region = ifelse(COUNTRYNM == "united states", "Americas", grouped_region)) %>% 
  distinct()

# pearson correlations
c1 <- cor.test(national_values$mean_vax, national_values$overall_cvg)
c3 <- cor.test(national_values$mean_vax, national_values$perc_born_before_1980)
c5 <- cor.test(national_values$mean_vax, national_values$cessdate_orig)

# 44 missing mean_susceptibility, ignore warning
# vax coverage
p1 <- national_values %>%  
  filter(is_default_cvg == 0) %>% # remove default 80% coverages
  #filter(overall_cvg > 60) %>%  # remove outliers
  ggplot(aes(x = overall_cvg, y = mean_vax)) +
  geom_point(aes(col = grouped_region), size = 3, alpha = 0.5) +
  # remove outliers & defaults from smooth
  geom_smooth(method = "lm", col = "black") +
  # annotate("text", x=40, y=40, 
  #          label=(paste0("slope==", round(coef(lm(national_values$mean_vax ~ national_values$overall_cvg))[2], 3))),
  #          parse=TRUE, size = 6) +
  labs(x = "Pre-eradication smallpox vacc. covg.", y = "Percent vaccinated", col = "Region") +
  ylim(5, 50) +
  scale_x_continuous(breaks = c(30, 50, 70, 90)) +
  scale_color_manual(values = met.brewer("Johnson", 5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 13.5),
        axis.title = element_text(size = 15.5),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))
# born before 1980
p3 <- national_values %>% 
  ggplot(aes(x = perc_born_before_1980, y = mean_vax)) +
  geom_point(aes(col = grouped_region), size = 3, alpha = 0.5) +
  geom_smooth(method = "lm", col = "black") +
  # annotate("text", x=20, y=40, 
  #          label=(paste0("slope==", round(coef(lm(national_values$mean_vax ~ national_values$perc_born_before_1980))[2], 3))),
  #          parse=TRUE, size = 6) +
  labs(x = "Percent born before 1980", y = "Percent vaccinated", col = "Region") +
  ylim(5, 50) +
  xlim(10, 58) +
  scale_color_manual(values = met.brewer("Johnson", 5)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13.5),
        axis.title.x = element_text(size = 15.5),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))
# cessation date
# remove default cess dates
national_values_valid_cess <- national_values %>% filter(is_default_cess == 0)
p5 <- national_values_valid_cess %>% 
  ggplot(aes(x = cessdate_orig, y = mean_vax)) +
  geom_point(aes(col = grouped_region), size = 3, alpha = 0.5) +
  geom_smooth(method = "lm", col = "black") +
  # annotate("text", x=1964, y=40,
  #          label=(paste0("slope==",
  #                        round(coef(lm(national_values_valid_cess$mean_vax ~ national_values_valid_cess$cessdate_orig))[2], 3))),
  #          parse=TRUE, size = 6) +
  labs(x = "Smallpox vaccination cessation", y = "Percent susceptible", col = "Region") +
  ylim(5, 50) +
  scale_x_continuous(breaks = c(1960, 1964, 1968, 1972, 1976, 1980, 1984),
                     labels = c("1960", "", "1968", "", "1976", "", "1984")) +
  scale_color_manual(values = met.brewer("Johnson", 5)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 13.5),
        axis.title.x = element_text(size = 15.5),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))


########################################################################
### FIGURE 1. GLOBAL SPATIAL HETEROGENEITY AND DRIVERS #################
########################################################################
fig1sf <- join_shape_data(main_estimates, 
                          do_plot_difference = FALSE,
                          diff_comparison_path = NA)
saveRDS(fig1sf, "data/world-estimates-vaxxed-sf.RDS")
fig1_for_legend <- map_ests(fig1sf,
                            do_plot_difference = FALSE, var_to_plot = "perc_vaxxed",
                            legend_title = "Population\nvaccinated\n(% of popn)", 
                            lower = 5, upper = 70, palette = "VanGogh3",
                            show_legend = TRUE, wide_legend = FALSE)

legend <- cowplot::get_legend(fig1_for_legend)
fig1_map <- map_ests(fig1sf, do_plot_difference = FALSE, var_to_plot = "perc_vaxxed",
                     legend_title = "Population\nvaccinated\n(% of popn)", 
                     lower = 5, upper = 70, palette = "VanGogh3",
                     show_legend = FALSE, wide_legend = FALSE)


ggarrange(fig1_map + inset_element(legend, left = 0, bottom = 0.1, right = 0.25, top = 0.5, 
                                   align_to = "panel", on_top = T),
          ggarrange(p1, p5, p3, ncol = 3, nrow = 1, labels = c("B", "", ""), widths = c(4, 3.5, 3.5),
                    font.label = list(size = 20), common.legend = TRUE, legend = "bottom"), 
          ncol = 1, nrow = 2, heights = c(5, 4), labels = c("A", ""),
          font.label = list(size = 20))
ggsave("figures/fig1.pdf", height = 8, width = 12, dpi = 600)
dev.off()


########################################################################
### US DATA SCALE COMPARISON ###########################################
########################################################################
# us fine map / comparison
us_state_gpw <- main_estimates %>% filter(ISOALPHA == "USA")
pumas_for_join <- data.frame(state_fips = all_pumas$ST, 
                             PUMA = all_pumas$PUMA)
# need to apply these state level predictions to each puma level
us_state_gpw_pumas_level <- us_state_gpw %>% left_join(state_fips, by = c("NAME1" = "state")) %>% 
  left_join(pumas_for_join, by = c("fips" = "state_fips")) %>% 
  rename(ST = fips)
# this mapping code works, great! now need to do difference
# map_us(us_state_gpw_pumas_level, var_to_plot = "perc_vaxxed",
#        legend_title = "Percent vaccinated", lower = 5, upper = 70, 
#        palette = "VanGogh3", show_legend = TRUE)
us_puma_no_immigration <- main_us_function(data_to_use = pums_data, is_diff_age_dist = FALSE,
                                           for_world_map = FALSE, is_100_covg = FALSE,
                                           is_1984_end = FALSE, no_immigration = TRUE, 
                                           homogeneous_vax_covg = FALSE,
                                           cess_cvg_data = cessation_coverage_data)
# this is homogeneous vax in the US, other countries can be different
us_puma_homogeneous_vax <- main_us_function(data_to_use = pums_data, is_diff_age_dist = FALSE,
                                           for_world_map = FALSE, is_100_covg = FALSE,
                                           is_1984_end = FALSE, no_immigration = FALSE, 
                                           homogeneous_vax_covg = TRUE,
                                           cess_cvg_data = cessation_coverage_data)
us_puma_all_info <- main_us_function(data_to_use = pums_data, is_diff_age_dist = FALSE,
                                     for_world_map = FALSE, is_100_covg = FALSE,
                                     is_1984_end = FALSE, no_immigration = FALSE, 
                                     homogeneous_vax_covg = FALSE,
                                     cess_cvg_data = cessation_coverage_data)
write_csv(us_puma_all_info, "estimates/vaxxed-us-pumas.csv")
us_puma_all_info <- read_csv("estimates/vaxxed-us-pumas.csv")

# calculate differences to plot
state_diff <- us_state_gpw_pumas_level %>% select(ST, PUMA, perc_vaxxed) %>% 
  rename(perc_vaxxed_state = perc_vaxxed) %>% 
  left_join(us_puma_all_info) %>% 
  mutate(diff = perc_vaxxed_state - perc_vaxxed)
no_immigration_diff <- us_puma_no_immigration %>% select(ST, PUMA, perc_vaxxed) %>% 
  rename(perc_vaxxed_no_immigration = perc_vaxxed) %>% 
  left_join(us_puma_all_info) %>% 
  mutate(diff = perc_vaxxed_no_immigration - perc_vaxxed)
homogeneous_vax_diff <- us_puma_homogeneous_vax %>% select(ST, PUMA, perc_vaxxed) %>% 
  rename(perc_vaxxed_homogeneous = perc_vaxxed) %>% 
  left_join(us_puma_all_info) %>% 
  mutate(diff = perc_vaxxed_homogeneous - perc_vaxxed)

# library(usmap)
# p1 <- plot_usmap(data = (us_state_gpw %>% rename(state = NAME1)), values = "perc_vaxxed", size = 0.2) +
#   scale_fill_gradientn(colors = met.brewer("VanGogh3"),
#                        name = "Percent vaccinated", limits = c(5, 70)) + 
#   theme(legend.position = "right")
# p2 <- map_us(us_puma_no_immigration,
#              var_to_plot = "perc_vaxxed",
#              legend_title = "Percent vaccinated",
#              lower = 5, upper = 70, palette = "VanGogh3", 
#              show_legend = FALSE)
# p3 <- map_us(us_puma_homogeneous_vax,
#              var_to_plot = "perc_vaxxed",
#              legend_title = "Percent vaccinated",
#              lower = 5, upper = 70, palette = "VanGogh3", 
#              show_legend = FALSE)
# p4 <- map_us(us_puma_all_info,
#              var_to_plot = "perc_vaxxed",
#              legend_title = "Percent vaccinated",
#              lower = 5, upper = 70, palette = "VanGogh3", 
#              show_legend = FALSE)

d1 <- map_us(state_diff, var_to_plot = "diff", legend_title = "Percentage difference",
             lower = -33, upper = 33, palette = "Hiroshige", show_legend = TRUE, wide_legend = TRUE)
d2 <- map_us(no_immigration_diff, var_to_plot = "diff", legend_title = "Percentage difference",
             lower = -22, upper = 22, palette = "Hiroshige", show_legend = TRUE, wide_legend = TRUE)
d3 <- map_us(homogeneous_vax_diff, var_to_plot = "diff", legend_title = "Percentage difference",
             lower = -7, upper = 7, palette = "Hiroshige", show_legend = TRUE, wide_legend = TRUE)

# ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, labels = "AUTO",
#           font.label = list(size = 20))
# ggsave("figures/us-map-scenarios.pdf", height = 10, width = 12)
ggarrange(ggarrange(d1, d2, ncol = 2, font.label = list(size = 20), labels = "AUTO"),
          d3, 
          ncol = 1, nrow = 2, labels = c("", "                     C"),
          font.label = list(size = 20), heights = c(1,1))
ggsave("figures/us-map-scenarios-differences.pdf", height = 10, width = 12)
dev.off()
rm(p1, p2, p3, p4, d1, d2, d3)


########################################################################
### UNCERTAINTY ANALYSIS ###############################################
########################################################################
### uncertainty analysis
uncertainty_estimates <- read_csv("data/bootstrapped_estimates_admin1_5000_musig.csv")
avg_diff <- uncertainty_estimates %>% 
  left_join(main_estimates) %>% 
  mutate(diff = mu - perc_vaxxed,
         abs_diff = abs(diff)) %>% 
  filter(!is.na(mu))
#write_csv(avg_diff, "estimates/uncertainty-differences.csv")
# admin1 difference variability
tot_admin1 <- nrow(avg_diff)
frac_admin1_lt1 <- (avg_diff %>% filter(abs_diff < 1) %>% nrow())/tot_admin1
frac_admin1_gt5 <- (avg_diff %>% filter(abs_diff > 5) %>% nrow())/tot_admin1

avg <- map_ests(join_shape_data(uncertainty_estimates %>% rename(perc_vaxxed = mu), 
                                do_plot_difference = TRUE,
                                diff_comparison_path = "estimates/vaxxed-world.csv"),
                do_plot_difference = TRUE, 
                var_to_plot = "difference",
                legend_title = "Difference in population\nvaccination (% of popn)",
                lower = -11, upper = 11, palette = "Hiroshige",
                show_legend = TRUE, wide_legend = TRUE) +
  theme(legend.position = "bottom")

stdev <- map_ests(join_shape_data(uncertainty_estimates, 
                                  do_plot_difference = FALSE,
                                  diff_comparison_path = NA),
                  do_plot_difference = FALSE, 
                  var_to_plot = "sigma",
                  legend_title = "Standard deviation in\npopulation vaccination\n(% of popn)",
                  lower = 0, upper = 6.05, palette = "Tam",
                  show_legend = TRUE, wide_legend = TRUE) +
  theme(legend.position = "bottom")

ggarrange(avg, stdev, ncol = 1, nrow = 2, labels = "AUTO", font.label = list(size = 30))
ggsave("figures/bootstrap-fig.pdf", dpi = 600, height = 8, width = 10)
# ggsave("figures/bootstrap-fig.pdf", dpi = 600, height = 8, width = 10)
dev.off()


########################################################################
### US CASE COUNTS VS SUSCEPTIBILITY ###################################
########################################################################
# NEEDS UPDATE TO SUSCEPTIBILITY
# from https://www.cdc.gov/poxvirus/monkeypox/response/2022/us-map.html on date in file name below
# us_case_data <- read_csv("data/us_case_counts_09_21_22.csv") %>% 
#   mutate(NAME1 = tolower(Location))
# library(tidycensus)
# state_popn <- get_acs(geography = "state", 
#                       variables = c(popn = "B01001_001"),
#                       output = "wide",
#                       year = 2020) %>% 
#   mutate(NAME1 = tolower(NAME)) %>% 
#   rename(population = popnE)
# 
# joined_us_data <- state_values %>% 
#   left_join(us_case_data, by = "NAME1") %>% 
#   left_join(state_popn %>% select(NAME1, population)) %>% 
#   mutate(prevalence = Cases/population)
# levels(joined_us_data$region)
# joined_us_data %>% ggplot(aes(x = perc_vaxxed, y = prevalence)) +
#   geom_point(aes(col = region), size = 3) +
#   scale_color_manual(values = met.brewer("Egypt", 4), breaks = c("Midwest", "Northeast", "South", "West")) +
#   labs(x = "Percent vaccinated", y = "Num. cases / State population") + 
#   geom_smooth(data = joined_us_data,
#               method = "lm", col = "black") +
#   scale_y_log10()
# ggsave("figures/us-cases-vs-vaccination.pdf", width = 8, height = 5, dpi = 600)
# # joined_us_data %>% ggplot(aes(x = perc_susceptible, y = Cases, col = region)) +
# #   geom_point(size = 3) +
# #   scale_color_manual(values = met.brewer("Egypt", 4)) +
# #   labs(x = "Percent susceptible", y = "Num. cases")
# cor.test(joined_us_data$perc_vaxxed,
#          joined_us_data$prevalence)
# 
# # try this with pumas level estimates
# joined_us_data_puma <- us_puma_all_info %>% group_by(ST, REGION) %>% 
#   summarise(mean_vax = mean(perc_vaxxed)) %>% 
#   ungroup() %>% 
#   left_join(state_fips, by = c("ST" = "fips")) %>% 
#   left_join(us_case_data, by = c("state" = "NAME1")) %>% 
#   left_join(state_popn %>% select(NAME1, population), by = c("state" = "NAME1")) %>% 
#   rowwise() %>% 
#   mutate(prevalence = Cases/population,
#          region = as.factor(case_when(REGION == 2 ~ "Midwest",
#                                       REGION == 1 ~ "Northeast",
#                                       REGION == 3 ~ "South",
#                                       REGION == 4 ~ "West")))
# levels(joined_us_data_puma$region) 
# joined_us_data_puma$region <- factor(joined_us_data_puma$region,
#                                              levels = c("Midwest", "Northeast", "South", "West"))
# levels(joined_us_data_puma$region) 
# joined_us_data_puma %>% ggplot(aes(x = mean_vax, y = prevalence)) +
#   geom_point(aes(col = region), size = 3) +
#   scale_color_manual(values = met.brewer("Egypt", 4)) + #, breaks = c("Midwest", "Northeast", "South", "West")) +
#   labs(x = "Percent vaccinated", y = "Num. cases / State population") + 
#   geom_smooth(data = joined_us_data_puma,
#               method = "lm", col = "black") +
#   scale_y_log10()
# ggsave("figures/us-cases-vs-vaccination-puma-agg.pdf", width = 8, height = 5, dpi = 600)
# cor.test(joined_us_data_puma$mean_vax,
#          joined_us_data_puma$prevalence)

########################################################################
### COUNTRY BAR PLOT ###################################################
########################################################################
bar_data <- national_values %>% 
  left_join(read_csv("data/cessation_coverage_estimates.csv") %>% select(Country_ISO_Code, Region),
            by = c("ISOALPHA" = "Country_ISO_Code")) %>% 
  mutate(Region = as.factor(ifelse(COUNTRYNM == "united states", "AM", Region))) %>% 
  distinct()
levels(bar_data$Region) # check each time that order is same
levels(bar_data$Region) <- c("Africa", "Americas", "Caribbean", "Europe",
                             "Middle East", "Pacific", "South Asia", 
                             "Southeast and East Asia", "Former Soviet Republic")
bar_data$Region <- factor(bar_data$Region, 
                          levels = c("Africa", "Americas", "Caribbean", "Europe",
                                     "Former Soviet Republic", "Middle East", "Pacific",
                                     "South Asia", "Southeast and East Asia"))

a <- bar_data %>% filter(Region %in% c("Africa", "Americas", "Caribbean")) %>% 
  ggplot(aes(x = mean_vax, 
             y = fct_reorder(COUNTRYNM, mean_vax), 
             fill = Region)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(xmin = min_admin1_vax, xmax = max_admin1_vax), 
                width = 0.5, col = "black") +
  labs(x = "Percent vaccinated", y = "Country") +
  coord_cartesian(xlim = c(5, 65)) +
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
  ggplot(aes(x = mean_vax, 
             y = fct_reorder(COUNTRYNM, mean_vax), 
             fill = Region)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(xmin = min_admin1_vax, xmax = max_admin1_vax), 
                width = 0.5, col = "black") +
  labs(x = "Percent vaccinated", y = "Country") +
  coord_cartesian(xlim = c(5, 65)) +
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
ggsave("figures/grouped-barplot-twocol.pdf", height = 20, width = 16)

########################################################################
### COUNTERFACTUAL MAPS ################################################
########################################################################
# 1984 vax cessation
cess_1984 <- calc_admin1_perc_vaxxed(vaxxed_by_age = calc_prop_vaxxed(other_age_data = NA,
                                                                      is_diff_age_dist = FALSE,
                                                                      is_world_age_dist = FALSE,
                                                                      is_100_covg = FALSE,
                                                                      is_1984_end = TRUE,
                                                                      is_bootstrap = FALSE),
                                     is_world_age_dist = FALSE,
                                     is_diff_age_dist = FALSE,
                                     is_100_covg = FALSE,
                                     is_1984_end = TRUE,
                                     is_bootstrap = FALSE) 
cess_1984_diff <- cess_1984 %>% rename(perc_vaxxed_1984 = perc_vaxxed) %>% 
  select(ISOALPHA, COUNTRYNM, NAME1, perc_vaxxed_1984) %>% 
  left_join(main_estimates) %>% 
  mutate(diff = perc_vaxxed_1984 - perc_vaxxed)
write_csv(cess_1984_diff, "estimates/vaxxed-cessation-1984-counterfactual.csv")
cess_1984_diff <- read_csv("estimates/vaxxed-cessation-1984-counterfactual.csv")
fig2a <- map_ests(join_shape_data(cess_1984, 
                                  do_plot_difference = TRUE,
                                  diff_comparison_path = "estimates/vaxxed-world.csv"),
                  do_plot_difference = TRUE, 
                  var_to_plot = "difference",
                  legend_title = "Difference in population vaccination coverage (% of popn)", 
                  lower = -45, upper = 45, palette = "Hiroshige",
                  show_legend = FALSE, wide_legend = TRUE)

# 100% scar coverage
covg_100 <- calc_admin1_perc_vaxxed(vaxxed_by_age = calc_prop_vaxxed(other_age_data = NA,
                                                                     is_diff_age_dist = FALSE,
                                                                     is_world_age_dist = FALSE,
                                                                     is_100_covg = TRUE,
                                                                     is_1984_end = FALSE,
                                                                     is_bootstrap = FALSE),
                                    is_world_age_dist = FALSE,
                                    is_diff_age_dist = FALSE,
                                    is_100_covg = TRUE,
                                    is_1984_end = FALSE,
                                    is_bootstrap = FALSE) 
covg_100_diff <- covg_100 %>% rename(perc_vaxxed_100 = perc_vaxxed) %>% 
  select(ISOALPHA, COUNTRYNM, NAME1, perc_vaxxed_100) %>% 
  left_join(main_estimates) %>% 
  mutate(diff = perc_vaxxed_100 - perc_vaxxed)
write_csv(covg_100_diff, "estimates/vaxxed-coverage-100-counterfactual.csv")
covg_100_diff <- read_csv("estimates/vaxxed-coverage-100-counterfactual.csv")
fig2b <- map_ests(join_shape_data(covg_100, 
                                  do_plot_difference = TRUE,
                                  diff_comparison_path = "estimates/vaxxed-world.csv"),
                  do_plot_difference = TRUE, 
                  var_to_plot = "difference",
                  legend_title = "Difference in population vaccination (% of popn)", 
                  lower = -45, upper = 45, palette = "Hiroshige",
                  show_legend = FALSE, wide_legend = TRUE)

# world age distribution 
world_same <- calc_admin1_perc_vaxxed(vaxxed_by_age = calc_prop_vaxxed(other_age_data = world_age_dist,
                                                                       is_diff_age_dist = TRUE,
                                                                       is_world_age_dist = TRUE,
                                                                       is_100_covg = FALSE,
                                                                       is_1984_end = FALSE,
                                                                       is_bootstrap = FALSE),
                                      is_world_age_dist = TRUE,
                                      is_diff_age_dist = TRUE,
                                      is_100_covg = FALSE,
                                      is_1984_end = TRUE,
                                      is_bootstrap = FALSE) 
world_same_diff <- world_same %>% rename(perc_vaxxed_world = perc_vaxxed) %>% 
  select(ISOALPHA, COUNTRYNM, NAME1, perc_vaxxed_world) %>% 
  left_join(main_estimates) %>% 
  mutate(diff = perc_vaxxed_world - perc_vaxxed)
write_csv(world_same_diff, "estimates/vaxxed-world-age-dist-counterfactual.csv")
fig2c <- map_ests(join_shape_data(world_same, 
                                  do_plot_difference = TRUE,
                                  diff_comparison_path = "estimates/vaxxed-world.csv"),
                  do_plot_difference = TRUE, 
                  var_to_plot = "difference",
                  legend_title = "Difference in population vaccination (% of popn)", 
                  lower = -45, upper = 45, palette = "Hiroshige",
                  show_legend = TRUE, wide_legend = TRUE)

ggarrange(
  ggarrange(fig2a, fig2b,
            ncol = 2, nrow = 1, labels = "AUTO", font.label = list(size = 30)), 
  fig2c, 
  ncol = 1, nrow = 2, labels = "AUTO", common.legend = TRUE, 
  legend = "bottom", font.label = list(size = 30))
# +
#   inset_element(fig3_legend, left = 0, bottom = 0.45, right = 0.15, top = 0.8,
#                 align_to = "full", on_top = T)

ggsave("figures/fig2.pdf", height = 8, width = 16, dpi = 600)
dev.off()
rm(fig2a, fig2b, fig2c)

########################################################################
### SUPPLEMENT COUNTERFACTUAL MAPS #####################################
########################################################################
# national age distribution 
# changing this to use UN age data instead of whatever aggregation I did previously

# IF USE UN DATA NEED TO REMOVE COUNTRYNM FROM JOIN IN CALC_IMMUNITY_SPLIT.R
national_same <- calc_admin1_perc_vaxxed(vaxxed_by_age = calc_prop_vaxxed(other_age_data = national_age_dist,
                                                                          is_diff_age_dist = TRUE,
                                                                          is_world_age_dist = FALSE,
                                                                          is_100_covg = FALSE,
                                                                          is_1984_end = FALSE,
                                                                          is_bootstrap = FALSE),
                                         is_world_age_dist = FALSE,
                                         is_diff_age_dist = TRUE,
                                         is_100_covg = FALSE,
                                         is_1984_end = TRUE,
                                         is_bootstrap = FALSE) 
national_same_diff <- national_same %>% rename(perc_vaxxed_national = perc_vaxxed) %>% 
  select(ISOALPHA, COUNTRYNM, NAME1, perc_vaxxed_national) %>% 
  left_join(main_estimates) %>% 
  mutate(diff = perc_vaxxed_national - perc_vaxxed)
write_csv(national_same_diff, "estimates/vaxxed-national-age-dist-counterfactual.csv")
figs1 <- map_ests(join_shape_data(national_same, 
                                  do_plot_difference = TRUE,
                                  diff_comparison_path = "estimates/vaxxed-world.csv"),
                  do_plot_difference = TRUE, 
                  var_to_plot = "difference",
                  legend_title = "Difference in\npopulation vaccination\n(% of popn)", 
                  lower = -45, upper = 45, palette = "Hiroshige",
                  show_legend = TRUE, wide_legend = FALSE)

figs1
#ggsave("figures/national-age-distribution.pdf", height = 4, width = 8, dpi = 600)
ggsave("figures/national-age-distribution.pdf", height = 4, width = 8, dpi = 600)
dev.off()
rm(figs1)



########################################################################
### DATA QUALITY #######################################################
########################################################################
# cessation data and coverage are at the national level, so use world shapefiles
World <- ne_countries(returnclass = "sf")
data_quality <- World %>% 
  left_join(cessation_coverage_data %>% # basically just losing small island nations in this join
              filter(!is.na(ISO)), # kosovo, somaliland, and northern cyprus don't have iso3 codes in World
            by = c("iso_a3" = "ISO"))
  
cessation_grade <- data_quality %>% 
  ggplot(aes(fill = cessation_data_quality)) + 
  geom_sf(size = 0.05) + 
  coord_sf() +
  scale_fill_manual(values = natparks.pals("Acadia", 4), na.value = "white") +
  labs(fill = "Data quality") +
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 15))
    
coverage_grade <- data_quality %>% 
  ggplot(aes(fill = cvg_data_quality)) + 
  geom_sf(size = 0.05) + 
  coord_sf() +
  scale_fill_manual(values = natparks.pals("Acadia", 4), na.value = "white") +
  labs(fill = "Data quality") +
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 15))

ggarrange(cessation_grade, coverage_grade, labels = "AUTO", font.label = list(size = 30),
          ncol = 1, nrow = 2, common.legend = TRUE, legend = "right")
ggsave("figures/data-quality.pdf", width = 10, height = 8)

# how many people in each grading category, versus number of countries
data_quality_df <- cessation_coverage_data %>% 
  filter(!is.na(ISO)) %>% 
  filter(!is.na(cessation_data_quality)) %>% 
  filter(!is.na(cvg_data_quality))
num_countries <- nrow(data_quality_df)
cvg_countries <- data_quality_df %>% 
  group_by(cvg_data_quality) %>% 
  summarise(count = n()/num_countries)
sum(cvg_countries$count)
cess_countries <- data_quality_df %>% 
  group_by(cessation_data_quality) %>% 
  summarise(count = n()/num_countries)
sum(cess_countries$count)

data_quality_pop_size <- main_estimates %>% group_by(ISOALPHA) %>% 
  summarise(population = sum(tot_popn_2020)) %>% 
  inner_join(data_quality_df, by = c("ISOALPHA" = "ISO")) %>% 
  ungroup()
world_pop <- sum(data_quality_pop_size$population)
cvg_pop <- data_quality_pop_size %>%
  group_by(cvg_data_quality) %>% 
  summarise(people = sum(population)/world_pop)
sum(cvg_pop$people)
cess_pop <- data_quality_pop_size %>% 
  group_by(cessation_data_quality) %>% 
  summarise(people = sum(population)/world_pop)
sum(cess_pop$people)

### checking us immigration differences

# immigrant_vs_not_vax <- pums_data %>% left_join(cessation_coverage_data, by = ("POBP")) %>% 
#   left_join(polio_proxy_coverage, by = c("POBP" = "fips_of_birth")) %>%
#   rowwise() %>% 
#   mutate(vax_stopped = ifelse(POBP < 60, 1972, vax_stopped), # set all US to 1972
#          Country = ifelse(POBP < 60, "USA", Country),
#          chance_vaxxed = ifelse(POBP >= 60, 
#                                 calc_vax_coverage_foreign(birth_year, Survey_Year,
#                                                           vax_stopped, cvg_514_orig,
#                                                           cvg_over14_orig, cvg_tot_orig),
#                                 ifelse(birth_year <= vax_stopped, spx_coverage, 0)),
#          foreign_born = ifelse(POBP < 100, 0, 1)) %>% # including US territories rn
#   group_by(foreign_born) %>% 
#   summarise(perc_vaxxed = weighted.mean(chance_vaxxed, PWGTP, na.rm = T) * 100,
#             mean_age = weighted.mean(AGEP, PWGTP, na.rm = T))
