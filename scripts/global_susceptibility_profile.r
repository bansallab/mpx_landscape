### CALCULATES SUSCEPTIBILITY PROFILE AT COUNTRY LEVEL GLOBALLY, EXCLUDING US
### PROFILES FOR SPECIFIC COUNTRIES IN SPECIFIC YEARS USED IN VALIDATION ANALYSIS
### Juliana Taube

library(tidyverse)
library(tidylog)
library(stringi)
library(MetBrewer)

WANING <- 0.807

mean_sd_country <- read_csv("data/bootstrapped_estimates_country_age_5000_musig.csv")
region_data <- read_csv("data/cessation_coverage_estimates.csv") %>% 
  rename(ISO = Country_ISO_Code) %>% 
  select(ISO, Region, UN_region)

df <- mean_sd_country %>%
  rowwise() %>% 
  mutate(min_age = as.numeric(substring(age, 2, 3)),
         max_age = ifelse(age == "A85PLUSB", 100, as.numeric(substring(age, 5, 6))), # can ignore warning
         min_birth_year = 2022 - max_age,
         max_birth_year = 2022 - min_age) %>% 
  left_join(region_data, by = c("ISOALPHA" = "ISO")) %>% 
  rowwise() %>% 
  mutate(perc_susc_by_age = (100 - (mu * WANING)),
         one_sd_above = perc_susc_by_age + sigma,
         one_sd_below = perc_susc_by_age - sigma,
         one_sd_above = ifelse(one_sd_above > 100, 100, one_sd_above),
         one_sd_below = ifelse(one_sd_below < 0, 0, one_sd_below)) %>% 
  #bind_rows(us_prof) %>% 
  mutate(age = as.factor(age),
         Region = as.factor(Region),
         UN_region = as.factor(UN_region),
         COUNTRYNM = ifelse(COUNTRYNM == "United Kingdom, Not Specified",
                            "United Kingdom", COUNTRYNM))

levels(df$age) <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                    "30-34", "35-39", "40-44", "45-49", "50-54", 
                    "55-59", "60-64", "65-69", "70-74", "75-79",
                    "80-84", "85+")
# REDO THESE LEVELS IF CESS_CVG DOCUMENT CHANGES
levels(df$Region)
levels(df$Region) <- c("South Asia", "Africa", "Europe", "Middle East", 
                       "Americas", "Former Soviet Republic",
                       "Pacific", "Caribbean", "Southeast and East Asia")
levels(df$UN_region)

regions <- unique(filter(df, !is.na(UN_region))$UN_region)
for(i in 1:length(regions)){
  doi <- df %>% filter(UN_region == regions[i])
  n_country <- doi %>% select(COUNTRYNM) %>% distinct() %>% nrow()
  out <- doi %>% ggplot(aes(x = age, y = perc_susc_by_age, col = COUNTRYNM, group = COUNTRYNM)) +
    # geom_ribbon(aes(ymax = one_sd_above, ymin = one_sd_below, fill = COUNTRYNM), alpha = 0.3,
    #             show.legend = F, color = NA) +
    geom_point(position = position_jitter(w = 0.2, h = 0), size = 4) + 
    geom_line() +
    #geom_errorbar(aes(ymax = one_sd_above, ymin = one_sd_below), width = 0.5) +
    #annotate("rect", xmin="40-44", xmax =Inf, ymin = -Inf, ymax = Inf,
    #fill = 'pink', alpha = 0.3, col = NA) + # use annotate to retain alpha control
    geom_vline(xintercept = "40-44", lty = "dashed") +
    #facet_wrap(~Region, ncol = 3) +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 60, hjust = 1, size = 16),
          axis.text.y = element_text(size = 16),
          axis.title = element_text(size = 20),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          legend.key.size = unit(1, "cm")) +
    guides(col = guide_legend(ncol = case_when(regions[i] == "Caribbean" ~ 4,
                                               regions[i] == "Middle Africa" ~ 4,
                                               regions[i] == "Oceania" ~4,
                                               regions[i] == "Eastern Asia" ~ 3,
                                               ! is.na(regions[i]) ~ 5))) +
    labs(x = "Age group", y = "Percent susceptible", col = "Country") +
    ylim(15, 100) +
    scale_color_manual(values = met.brewer("Isfahan2", n_country)) +
    scale_fill_manual(values = met.brewer("Isfahan2", n_country))
  
  out
  ggsave(paste0("figures/susceptibility-profiles/", regions[i], ".pdf"), 
         height = 8, width = 16)
  
}

### FIGURE 3 ###
cessation_data <- read_csv("data/cessation_coverage_estimates.csv") %>% 
  rename(ISO = Country_ISO_Code, vax_stopped = cessdate_orig) %>% 
  select(ISO, vax_stopped)

calc_time_since_vax <- function(current_year, min_birth_year, cessation_date){
  if(is.na(cessation_date)){# | min_birth_year > cessation_date){
    return(NA) # fix NA age
  }else if((min_birth_year + 7) > cessation_date){
    med_vax_year <- 1980
  }else{
    med_vax_year <- min_birth_year + 7
  }
  # assuming vaccinated 5 years later, then middle of age group
  # partly vaccinated but claiming some were after cessation
  
  time_since_vax <- current_year - med_vax_year
  return(time_since_vax)
}

# calc_waning <- function(vax_perc, vax_eff, t, wane_rate){
#   remaining_eff <- 1 - (t * (wane_rate/100))
#   remaining_eff <- ifelse(remaining_eff < 0, 0, remaining_eff)
#   perc_protected <- vax_perc * (vax_eff/100) * remaining_eff
#   perc_susceptible <- 100 - perc_protected
#   return(perc_susceptible)
# }

calc_waning <- function(vax_perc, vax_eff, t, wane_rate){
  # all of these are not decimals but > 1
  # vax_perc is percent of population vaccinated
  # vax_eff is initial vaccine effectiveness
  # t is number of years since vaccination
  # wane rate is percentage decrease in effectiveness per year (assuming exponential decay)
  remaining_eff <- vax_eff * exp(-(wane_rate/100) * t)
  perc_protected <- vax_perc * (remaining_eff/100)
  perc_susceptible <- 100 - perc_protected
  return(perc_susceptible)
}

df_susc <- df %>% left_join(cessation_data, by = c("ISOALPHA" = "ISO")) %>% 
  rowwise() %>% 
  mutate(time_since_vax = calc_time_since_vax(2022, min_birth_year, vax_stopped),
         susc_90 = 100 - (mu * 0.9),
         susc_80 = 100 - (mu * 0.8),
         susc_70 = 100 - (mu * 0.7),
         susc_50 = 100 - (mu * 0.5),
         susc_30 = 100 - (mu * 0.3),
         susc_85_age_wane_1.4 = calc_waning(mu, 85, time_since_vax, 1.4))
to_plot <- df_susc %>% filter(ISOALPHA %in% c("CHN", "FRA", "TCD", "BRA")) %>%
  pivot_longer(cols = c(susc_90:susc_85_age_wane_1.4),
               values_to = "perc_susc", names_to = "eff_type") %>% 
  mutate(effectiveness = ifelse(grepl("wane", eff_type), "", substr(eff_type, 6, 7)),
         include_waning = ifelse(grepl("wane", eff_type), T, F),
         COUNTRYNM = case_when(ISOALPHA == "CHN" ~ "China",
                               ISOALPHA == "FRA" ~ "France",
                               ISOALPHA == "BRA" ~ "Brazil",
                               ISOALPHA == "TCD" ~ "Chad"))

to_plot %>% 
  ggplot(aes(x = age, y = perc_susc, col = effectiveness, group = eff_type, shape = include_waning)) +
  # geom_ribbon(aes(ymin = max_eff, ymax = min_eff), fill = "grey70", alpha = 0.2,
  #             col = NA, outline.type = "both") +
  geom_point(size = 4, alpha = 0.8) +
  geom_line() +
  geom_vline(xintercept = "40-44", lty = "dashed") +
  theme_bw() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 40, hjust = 1, size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        legend.key.size = unit(1, "cm"),
        strip.text = element_text(size = 20, face = "bold")) +
  ylim(0, 100) +
  labs(x = "Age group", y = "Percent susceptible", col = "VE with population waning") +
  scale_color_manual(values = c(met.brewer("Isfahan2"), "dimgrey"), 
                     breaks = c("90", "80", "70", "50", "30"),
                     labels = c("90%", "80%", "70%", "50%", "30%")) +
  scale_shape_discrete(breaks = c(T), labels = c("85% with 1.4%/year decline"), name = "VE with age-specific waning") +
  scale_x_discrete(labels = c("0-4", "", "10-14", "", "20-24", "", "30-34", "",
                              "40-44", "", "50-54", "", "60-64", "", "70-74", "", 
                              "80-84", "")) +
  facet_wrap(~COUNTRYNM) +
  guides(shape = guide_legend(override.aes = list(colour = "dimgrey")))
ggsave("figures/fig3.pdf", height = 10, width = 20, dpi = 600)


### COUNTRY SPECIFIC PROFILES FOR CERTAIN YEARS

extract_profile <- function(iso, year){
  out <- df %>% filter(ISOALPHA == iso) %>% 
    rowwise() %>% 
    filter(min_birth_year <= year | max_birth_year < (year + 5)) %>% 
    mutate(min_age_outbreak = year - max_birth_year,
           max_age_outbreak = year - min_birth_year)
  
  return(out)
}

### Guinea Bissau (GNB), 2005 
gnb <- extract_profile("GNB", 2005)
write_csv(gnb, "data/susc_profile_gnb_2005.csv")

### Italy (ITA), 2003
ita <- extract_profile("ITA", 2003)
write_csv(ita, "data/susc_profile_ita_2003.csv")

### DRC (COD), 2006
cod <- extract_profile("COD", 2006)
write_csv(cod, "data/susc_profile_cod_2006.csv")

### Colombia (COL), 2016
col <- extract_profile("COL", 2016)
write_csv(col, "data/susc_profile_col_2016.csv")

### Brazil (BRA), 2012
bra <- extract_profile("BRA", 2012)
write_csv(bra, "data/susc_profile_bra_2012.csv")

### Denmark (DNK), 2005
dnk <- extract_profile("DNK", 2005)
write_csv(dnk, "data/susc_profile_dnk_2005.csv")

