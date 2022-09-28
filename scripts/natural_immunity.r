### PLOTS SMALLPOX CASE COUNTS AND POCK MARK DATA TO ASSESS IMPORTANCE OF NATURAL IMMUNITY
### Juliana Taube

library(tidyverse)
library(tidylog)
library(MetBrewer)

data <- read_csv("data/natural_immunity.csv")

data_long <- data %>% pivot_longer(`1920`:`1980`, values_to = "cases", names_to = "year") %>% 
  select(Country, cases, year, `Population in 1970 (from data world bank)`) %>% 
  rename(country = Country, population = `Population in 1970 (from data world bank)`) %>% 
  mutate(cases_prop = cases / population) %>% 
  group_by(country) %>% 
  mutate(csum_cases = cumsum(coalesce(cases, 0)),
         csum_prop = cumsum(coalesce(cases_prop, 0))) %>% 
  ungroup()

data_long %>% #filter(! country %in% c("Iran", "Iraq")) %>% 
  ggplot(aes(x = year, y = cases_prop, group = country)) + 
  geom_line(aes(col = country)) +
  scale_y_sqrt() +
  facet_wrap(~country, nrow = 4) +
  scale_x_discrete(breaks = seq(from = 1920, to = 1980, by = 5)) +
                   #labels = c("1965", "", "1969", "", "1973", "", "1977")) +
  labs(y = "Proportion of population infected\n(# cases / population)", x = "Year",
       col = "Country") +
  theme_bw() +
  guides(color = "none")
  #scale_color_manual(values = met.brewer("Archambault", 21))

# cumulative incidence
data_long %>% 
  # these are lumped into French West Africa and Ruanda-Urundi
  filter(! country %in% c("Burundi", "Rwanda", "Benin (Dahomey)",  
                          "Burkina Faso (Upper Volta)", "Niger", "Mali", "Guinea",
                          "Senegal", "Côte d'Ivoire", "Mauritania")) %>%
  mutate(country = ifelse(country == "French West Africa (Benin, Burkina Faso, Côte d'Ivoire, Guinea, Mali, Mauritania, Niger, Senegal)",
                          "French West Africa", 
                          ifelse(country == "Ruanda-Urundi (Rwanda, Burundi)", "Ruanda-Urundi",
                                 country))) %>% 
  ggplot(aes(x = year, y = csum_prop*100, group = country)) + 
  geom_line() +
  #scale_y_sqrt() +
  facet_wrap(~country, nrow = 4) +
  theme_bw() +
  scale_x_discrete(breaks = seq(from = 1920, to = 1980, by = 20)) +
  labs(y = "Cumulative incidence (percentage of population)", x = "Year") +
  theme(legend.position = "none",
        panel.spacing = unit(1.5, "lines")) # +
  # scale_color_manual(values = met.brewer("Archambault", 19))
#ggsave("figures/cases-countries-endemic-1970-later.jpeg", height = 8, width = 12, dpi = 800)
ggsave("figures/cases-countries-endemic-1967-later.pdf", height = 8, width = 16, dpi = 800)

# can we map this?
library(rnaturalearth); library(rnaturalearthdata); library(sf); library(ggplot2)
# need to change values for these countries to be the ones for Ruanda-Urundi or French West Africa
data_to_join <- data_long %>% filter(year == 1980) %>% 
  filter(! country %in% c("Burundi", "Rwanda", "Benin (Dahomey)",  
                          "Burkina Faso (Upper Volta)", "Niger", "Mali", "Guinea",
                          "Senegal", "Cote d'Ivoire", "Mauritania")) %>%
  mutate(csum_perc = csum_prop * 100) %>% 
  separate_rows(country, sep = ", ") %>% 
  mutate(country = ifelse(country == "French West Africa (Benin", "Benin",
                          ifelse(country == "Ruanda-Urundi (Rwanda", "Rwanda",
                                 ifelse(country == "Burundi)", "Burundi",
                                        ifelse(country == "Senegal)", "Senegal", country)))))
  
World <- ne_countries(returnclass = "sf") %>% 
  left_join(data_to_join, by = c("name" = "country"))

(Map <- World %>% ggplot(aes(fill = csum_perc)) + 
  geom_sf(size = 0.2) + 
  coord_sf() + 
  labs(fill = "Cumulative incidence\n(% of 1970 population)") +
  scale_fill_gradientn(colors = met.brewer("Hokusai2")))
ggsave("figures/cumulative-incidence-countries-endemic-1967-later.pdf", height = 4, width = 8, dpi = 600)


#  may use these cumulative percentages in these countries - 
#   what do we think scale of underreporting is?
# out <- data_long %>% filter(year == 1980) %>% select(country, csum_prop) %>% 
#   filter(! country %in% c("Iran", "Iraq"))
# out$ISOALPHA <- c("AFG", "BGD", "BWA", "BRA", "BDI", "COD", "ETH", "IND",
#                   "IDN", "MWI", "NPL", "NGA", "PAK", "RWA", "SOM", "ZAF", 
#                   "SDN", "TZA", "ZWE")
# write_csv(out, "data/nat_immunity_cumulative_prop_infected.csv")

### POCK MARK DATA ----------------------------------------------------------------
pock <- read_csv("data/pock_survey_coverage.csv")
# max percentages
summary(pock)
unique(pock$Country)

pock_long <- pock %>%
  select(Country, Year, UN_region, `Total percentage`, `Preschool percentage`,
         `School age percentage`, `Adult percentage`) %>% 
  filter(Country != "China, Yunnan Province") %>% # deal with the > 3.88 in this row
  mutate(`Adult percentage` = as.numeric(`Adult percentage`)) %>% 
  pivot_longer(cols = c(`Total percentage`, `Preschool percentage`,
                        `School age percentage`, `Adult percentage`), 
               values_to = "percentage",
               names_to = "age_group") %>% 
  mutate(age_group = gsub("[ ]percentage", "", age_group)) %>% 
  filter(!is.na(percentage))
pock_long$age_group <- factor(pock_long$age_group, 
                              levels = c("Preschool", "School age", "Adult", "Total"))


pock_long %>% ggplot(aes(x = percentage, fill = UN_region)) +
  geom_histogram(col = "white") + #aes(y = stat(count)/sum(count)),
  facet_wrap(~age_group) +
  #scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10), labels = c(0.001, 0.01, 0.1, 1, 10)) +
  labs(y = "Number of surveys", x = "Percentage of individuals surveyed with pock marks", 
       fill = "UN Region") +
  #guides(fill = guide_legend(ncol=6)) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 20),
        strip.text.x = element_text(size = 20)) +
  scale_fill_manual(values = met.brewer("Java", n = length(unique(pock$UN_region)))) +
  theme_bw()
ggsave("figures/pockmarks-all-countries.pdf", height = 6, width = 10, dpi = 600)

# pock_endemic <- data %>% left_join(pock, by = "Country") %>% 
#   select(Country, Year, `Last endemic spread`, `Total percentage`, `Preschool percentage`,
#          `School age percentage`, `Adult percentage`) %>% 
#   pivot_longer(cols = (`Total percentage`:`Adult percentage`), values_to = "percentage",
#                names_to = "age_group") %>% 
#   mutate(age_group = gsub("[ ]percentage", "", age_group))
# 
# pock_endemic$age_group <- factor(pock_endemic$age_group, 
#                                  levels = c("Preschool", "School age", "Adult", "Total"))
# 
# pock_endemic %>% ggplot(aes(x = percentage, fill = Country)) +
#   geom_histogram(aes(y = stat(count)/sum(count)), col = "white") +
#   facet_wrap(~age_group, scales = "free") +
#   #scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10), labels = c(0.001, 0.01, 0.1, 1, 10)) +
#   ylab("frequency") +
#   theme(legend.position = "none")
# ggsave("figures/pockmarks-endemic.jpeg", height = 6, width = 10, dpi = 600)
# ggsave("figures/pockmarks-endemic-zeros.jpeg", height = 6, width = 5, dpi = 600)


