### CALCULATES SUSCEPTIBILITY PROFILE AT COUNTRY LEVEL GLOBALLY, EXCLUDING US
### PROFILES FOR SPECIFIC COUNTRIES IN SPECIFIC YEARS USED IN VALIDATION ANALYSIS
### Juliana Taube


library(tidyverse)
library(tidylog)
library(stringi)
library(MetBrewer)

WANING <- 0.807
DEFAULT_COVERAGE <- 0.8
source("scripts/scar_survey_coverage_calcs.r")

gpw <- read_csv("data/cleaned_gpw_age_data.csv")

cessation_coverage_data_prof <- read_csv("data/cessation_coverage_estimates.csv") %>% 
  rename(ISO = Country_ISO_Code, 
         POBP = Country_PUMS_Code, # this is a country code though
         vax_stopped = cessdate_orig,
         Survey_Year = cvg_survey_yr) %>% 
  select(ISO, Country, POBP, Region, UN_region, vax_stopped, cvg_514_orig, 
         cvg_over14_orig, cvg_tot_orig, Survey_Year)

df <- gpw %>%
  pivot_longer(cols = A00_04B:A85PLUSB, values_to = "popn", names_to = "age") %>% 
  select(ISOALPHA, COUNTRYNM, age) %>% # don't think we need popn here since not weighting by it?
  distinct() %>% 
  rowwise() %>% 
  mutate(min_age = as.numeric(substring(age, 2, 3)),
         max_age = ifelse(age == "A85PLUSB", 100, as.numeric(substring(age, 5, 6))), # can ignore warning
         min_birth_year = 2022 - max_age,
         max_birth_year = 2022 - min_age) %>% 
  left_join(cessation_coverage_data_prof, by = c("ISOALPHA" = "ISO")) %>% 
  rowwise() %>% 
  mutate(prop_vaxxed_by_age = calc_vax_coverage(min_birth_year, max_birth_year, 
                                                Survey_Year, vax_stopped,
                                                cvg_514_orig, cvg_over14_orig, 
                                                cvg_tot_orig),
         perc_susc_by_age = (1 - (prop_vaxxed_by_age * WANING)) * 100,
         age = as.factor(age),
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
levels(df$Region) <- c("Americas", "Africa", "Europe", "Former Soviet Republic",
                       "Southeast and East Asia", "Middle East", "Caribbean", 
                       "Pacific", "South Asia")
levels(df$UN_region)

regions <- unique(filter(df, !is.na(UN_region))$UN_region)
for(i in 1:length(regions)){
  doi <- df %>% filter(UN_region == regions[i])
  n_country <- doi %>% select(COUNTRYNM) %>% distinct() %>% nrow()
  out <- doi %>% ggplot(aes(x = age, y = perc_susc_by_age, col = COUNTRYNM, group = COUNTRYNM)) +
    geom_point(position = position_jitter(w = 0.25, h = 0), size = 4) + 
    geom_line() +
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
                                               regions[i] == "Eastern Asia" ~ 3,
                                               ! is.na(regions[i]) ~ 5))) +
    labs(x = "Age group", y = "Percent susceptible", col = "Country") +
    ylim(15, 100) +
    scale_color_manual(values = met.brewer("Isfahan2", n_country))
  
  out
  ggsave(paste0("figures/susceptibility-profiles/", regions[i], ".pdf"), 
                height = 8, width = 16)

}

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


### old way
# susc_profile <- vaxxed_by_age %>% mutate(prop_susc = 1 - (prop_vaxxed_by_age * WANING)) %>% 
#   filter(!is.na(prop_susc)) %>% 
#   select(ISOALPHA:popn, prop_vaxxed_by_age:prop_susc) %>% 
#   group_by(COUNTRYNM, age) %>% 
#   summarise(prop_susceptible = weighted.mean(prop_susc, popn, na.rm = T)) %>% 
#   mutate(age = as.factor(age))
# levels(susc_profile$age) <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
#                               "30-34", "35-39", "40-44", "45-49", "50-54", 
#                               "55-59", "60-64", "65-69", "70-74", "75-79",
#                               "80-84", "85+")
# 
# pdf("figures/sus-profile-all.pdf", height = 20, width = 20)
# susc_profile %>% ggplot(aes(x = age, y = prop_susceptible, group = COUNTRYNM,
#                             col = COUNTRYNM)) +
#   geom_point() +
#   geom_line() +
#   theme(legend.position = "bottom")
# facet_wrap(~COUNTRYNM)
# dev.off()