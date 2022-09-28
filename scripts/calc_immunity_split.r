### FUNCTION TO CALCULATE PERCENTAGE OF ADMIN1 LEVEL POPULATION PROTECTED GLOBALLY
### trying to split this up so aggregation across ages, or across admin1s happens in separate second function
### Juliana Taube
### last updated 08/26/22 converting to prop_vaxxed, not susceptibility

###################################################
### CALCULATE PROP_VAXXED FOR EACH AGE & ADMIN1 ###
###################################################
# this is first half of two different calcs for bootstrapping
calc_prop_vaxxed <- function(other_age_data, is_diff_age_dist, 
                             is_world_age_dist, is_100_covg, is_1984_end,
                             is_bootstrap){
  # need cessation_coverage_data as an argument for bootstrapping
  # ASSUMING ONLY ONE COUNTERFACTUAL AT A TIME
  # using ifelse made a list so have to spell out
  if(is_bootstrap){
    stop_cvg_data <- sampled_cessation_coverage_data
    gpw_data <- sampled_age_data
  }else{
    stop_cvg_data <- cessation_coverage_data
    gpw_data <- gpw
    }
  # have to provide as argument to us_function since not in global environment
  
  data_long <- gpw_data %>% pivot_longer(cols = A00_04B:A85PLUSB, values_to = "popn", names_to = "age") 
  
  if(is_world_age_dist){ # world age dist
    data_long <- data_long %>% left_join(other_age_data, by = c("age"))
  }else if(is_diff_age_dist){ # national age dist
    data_long <- data_long %>% left_join(other_age_data, by = c("ISOALPHA", "COUNTRYNM", "age")) # "COUNTRYNM", no longer with UN
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
  }
  
  if(is_100_covg){ # coverage 100%
    vaxxed_by_age <- data_long %>%
      rowwise() %>%
      mutate(prop_vaxxed_by_age = calc_vax_coverage_100(min_birth_year, max_birth_year, 
                                                        vax_stopped))
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
  
  return(vaxxed_by_age)
}
# -----------------------------------------------------------------------------------

###################################################################################
### CALCULATE PROP_VAXXED ACROSS ALL AGES FOR EACH ADMIN1 (MAIN_ESTIMATES) ###
###################################################################################
calc_admin1_perc_vaxxed <- function(vaxxed_by_age, is_world_age_dist,
                                         is_diff_age_dist, is_100_covg, is_1984_end,
                                         is_bootstrap){
  
  if(is_diff_age_dist){ # using national or world age distribution
    vaxxed_by_admin <- vaxxed_by_age %>% group_by(ISOALPHA, COUNTRYNM, NAME1) %>% 
      summarise(prop_vaxxed = weighted.mean(prop_vaxxed_by_age, age_dist, na.rm = T)) %>% 
      ungroup()
  }else if(is_100_covg | is_1984_end){
    vaxxed_by_admin <- vaxxed_by_age %>% group_by(ISOALPHA, COUNTRYNM, NAME1) %>% 
      summarise(prop_vaxxed = weighted.mean(prop_vaxxed_by_age, popn, na.rm = T),
                pop_size = sum(popn)) %>% 
      ungroup()
  }else{ 
    # popn here will be proportion of individuals in each age group
    vaxxed_by_admin <- vaxxed_by_age %>% group_by(ISOALPHA, COUNTRYNM, NAME1, tot_popn_2020) %>% 
      summarise(prop_vaxxed = weighted.mean(prop_vaxxed_by_age, popn, na.rm = T),
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
    mutate(across(prop_vaxxed, ~ ifelse(is.nan(.x), NA_real_, .x)), # replace NaN with NA
           perc_vaxxed = prop_vaxxed * 100) 
  
  if(is_bootstrap){ # bootstrap excludes US
    vaxxed_by_admin <- vaxxed_by_admin %>% 
      select(ISOALPHA, COUNTRYNM, NAME1, prop_vaxxed, perc_vaxxed)
    }#else{
    # us_data <- us_for_world_map(is_diff_age_dist, is_world_age_dist, is_100_covg, 
    #                             is_1984_end, is_bootstrap)
    # 
    # vaxxed_by_admin <- vaxxed_by_admin %>% 
    #   mutate(perc_foreign_born = NA) %>% # so US data can have this
    #   bind_rows(us_data)
  #}
  
  return(vaxxed_by_admin)
}
# -----------------------------------------------------------------------------------

##################################################################
### CALCULATE PROP_VAXXED ACROSS ALL AGES FOR EACH ADMIN1 ###
##################################################################
# this is for susceptibility profile uncertainty
calc_country_age_perc_vaxxed <- function(vaxxed_by_age){
  # don't need to consider different age distributions since this calculation is age distribution agnostic
  # similarly, won't encounter other counterfactuals when using this function
  # won't need to indicate bootstrapping since that is exclusive use of this function
  
  vaxxed_by_country_age <- vaxxed_by_age %>% group_by(ISOALPHA, COUNTRYNM, age) %>% 
    summarise(prop_vaxxed_country = mean(prop_vaxxed_by_age, na.rm = T)) %>% 
    # this mean isn't weighted since the proportion of people vaccinated in an age group shouldn't
    #   differ within a country given that coverage and cessation is at the country level
    ungroup() %>% 
    mutate(across(prop_vaxxed_country, ~ ifelse(is.nan(.x), NA_real_, .x)), # replace NaN with NA
           perc_vaxxed_country = prop_vaxxed_country * 100)
 
  return(vaxxed_by_country_age)
}

