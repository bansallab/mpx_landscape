### FUNCTION TO CALCULATE PERCENTAGE OF ADMIN1 LEVEL POPULATION PROTECTED GLOBALLY
### Juliana Taube
### last updated 07/21/22

main_function <- function(other_age_data, is_diff_age_dist, 
                          is_world_age_dist, is_100_covg, is_1984_end, 
                          waning, include_natural_immunity, is_bootstrap){
  # need cessation_coverage_data as an argument for bootstrapping
  # ASSUMING ONLY ONE COUNTERFACTUAL AT A TIME
  WANING <- waning
  us_data <- data.frame()
  # using ifelse made a list so have to spell out
  if(is_bootstrap){stop_cvg_data <- sampled_cessation_coverage_data
  }else{stop_cvg_data <- cessation_coverage_data}
  # have to provide as argument to us_function since not in global environment
  
  data_long <- gpw %>% pivot_longer(cols = A00_04B:A85PLUSB, values_to = "popn", names_to = "age") 
  
  if(is_world_age_dist){ # world age dist
    data_long <- data_long %>% left_join(other_age_data, by = c("age"))
    us_data <- main_us_function(data_to_use = new_world_weights, is_diff_age_dist = TRUE,
                                for_world_map = TRUE, is_100_covg = FALSE,
                                is_1984_end = FALSE, waning = WANING, 
                                no_immigration = FALSE, 
                                cess_cvg_data = stop_cvg_data) %>% 
      rename(NAME1 = state)
  }else if(is_diff_age_dist){ # national age dist
    data_long <- data_long %>% left_join(other_age_data, by = c("ISOALPHA", "COUNTRYNM", "age"))
    us_data <- main_us_function(data_to_use = new_national_weights, is_diff_age_dist = TRUE,
                                for_world_map = TRUE, is_100_covg = FALSE,
                                is_1984_end = FALSE, waning = WANING,
                                no_immigration = FALSE, 
                                cess_cvg_data = stop_cvg_data) %>% 
      rename(NAME1 = state)
  }
  
  data_long <- data_long %>% rowwise() %>% 
    mutate(min_age = as.numeric(substring(age, 2, 3)),
           max_age = ifelse(age == "A85PLUSB", 100, as.numeric(substring(age, 5, 6))), # can ignore warning
           mid_age = (max_age + min_age)/2,
           min_birth_year = 2022 - max_age,
           max_birth_year = 2022 - min_age,
           prop_born_before_1980 = ifelse(max_birth_year < 1980, 1,
                                    ifelse(max_birth_year == 1982, 2/5, 0))) %>%
    left_join(stop_cvg_data, by = c("ISOALPHA" = "ISO"))
  
  if(is_1984_end){ # vaccination ends in 1984
    data_long <- data_long %>% mutate(vax_stopped = 1984)
    us_data <- main_us_function(data_to_use = pums_data, is_diff_age_dist = FALSE,
                                for_world_map = TRUE, is_100_covg = FALSE,
                                is_1984_end = TRUE, waning = WANING, 
                                no_immigration = FALSE, 
                                cess_cvg_data = stop_cvg_data) %>% 
      rename(NAME1 = state)
  }
  
  if(is_100_covg){ # coverage 100%
    vaxxed_by_age <- data_long %>%
      rowwise() %>%
      mutate(prop_vaxxed_by_age = calc_vax_coverage_100(min_birth_year, max_birth_year, 
                                                    vax_stopped))
    us_data <- main_us_function(data_to_use = pums_data, is_diff_age_dist = FALSE,
                                for_world_map = TRUE, is_100_covg = TRUE,
                                is_1984_end = FALSE, waning = WANING, 
                                no_immigration = FALSE, 
                                cess_cvg_data = stop_cvg_data) %>% 
      rename(NAME1 = state)
  }else{
    vaxxed_by_age <- data_long %>% 
      rowwise() %>%
      mutate(prop_vaxxed_by_age = calc_vax_coverage(min_birth_year, max_birth_year, 
                                                    Survey_Year, vax_stopped,
                                                    cvg_514_orig, cvg_over14_orig, 
                                                    cvg_tot_orig),
             prop_born_before_cessation = ifelse(!is.na(vax_stopped), 
                                             vax_eligible(min_birth_year, max_birth_year, vax_stopped),
                                             NA_real_))
  }
  
  if(is_diff_age_dist){ # using national or world age distribution
    vaxxed_by_admin <- vaxxed_by_age %>% group_by(ISOALPHA, COUNTRYNM, NAME1) %>% 
      summarise(prop_vaxxed = weighted.mean(prop_vaxxed_by_age, age_dist, na.rm = T)) %>% 
      ungroup()
  }else if(is_100_covg | is_1984_end | include_natural_immunity){
    vaxxed_by_admin <- vaxxed_by_age %>% group_by(ISOALPHA, COUNTRYNM, NAME1) %>% 
      summarise(prop_vaxxed = weighted.mean(prop_vaxxed_by_age, popn, na.rm = T),
                pop_size = sum(popn)) %>% 
      ungroup()
  }else{ 
    vaxxed_by_admin <- vaxxed_by_age %>% group_by(ISOALPHA, COUNTRYNM, NAME1) %>% 
      summarise(prop_vaxxed = weighted.mean(prop_vaxxed_by_age, popn, na.rm = T),
                pop_size = sum(popn),
                overall_cvg = mean(c(cvg_514_orig, cvg_over14_orig, cvg_tot_orig), na.rm = T) * 100,
                mean_age = weighted.mean(mid_age, popn, na.rm = T),
                perc_born_before_1980 = weighted.mean(prop_born_before_1980, popn, na.rm = T) * 100,
                perc_born_before_cessation = weighted.mean(prop_born_before_cessation, popn, na.rm = T) * 100) %>% 
      ungroup() %>% 
      mutate(across(c(overall_cvg, mean_age, 
                      perc_born_before_1980, perc_born_before_cessation),
                    ~ ifelse(is.nan(.x), NA_real_, .x)))
  }
  
  vaxxed_by_admin <- vaxxed_by_admin %>% # applies function to multiple columns
    mutate(across(prop_vaxxed, ~ ifelse(is.nan(.x), NA_real_, .x))) # replace NaN with NA
  
  if(include_natural_immunity){
    vaxxed_by_admin <- vaxxed_by_admin %>% 
      left_join(nat_immunity, by = "ISOALPHA") %>% 
      mutate(prop_protected = WANING * (prop_vaxxed + ifelse(is.na(csum_prop), 
                                                             0, 
                                                             (csum_prop * (1 + PERC_MISSED_CASES)))))
  }else{
    vaxxed_by_admin <- vaxxed_by_admin %>% 
      mutate(prop_protected = prop_vaxxed * WANING)
  }
  
  vaxxed_by_admin <- vaxxed_by_admin %>% 
    mutate(perc_susceptible = (1-prop_protected) * 100) %>% 
    select(-prop_vaxxed, -prop_protected)
  
  if(!is_bootstrap){ # bootstrap excludes US
    #if(!exists("us_data")){
    if(nrow(us_data) == 0){
      us_data <- main_us_function(data_to_use = pums_data, is_diff_age_dist = FALSE,
                       for_world_map = TRUE, is_100_covg = FALSE,
                       is_1984_end = FALSE, waning = WANING, 
                       no_immigration = FALSE, cess_cvg_data = stop_cvg_data) %>% 
        rename(NAME1 = state) # otherwise bind_rows won't work
    }
    vaxxed_by_admin <- vaxxed_by_admin %>% 
      mutate(perc_foreign_born = NA) %>% # so US data can have this
      bind_rows(us_data)
  }else{
    vaxxed_by_admin <- vaxxed_by_admin %>% 
      select(ISOALPHA, COUNTRYNM, NAME1, perc_susceptible)
  }
  
  return(vaxxed_by_admin)
  # onyl going to have pop_size column if not a counterfactual
  
}

