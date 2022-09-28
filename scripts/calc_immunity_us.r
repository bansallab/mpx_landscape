### FUNCTION TO CALCULATE PERCENTAGE OF PUMAS LEVEL POPULATION VACCINATED IN US FOR
###   MAIN ESTIMATES AND COUNTERFACTUALS
### AGGREGATES TO ADMIN1 (STATE) LEVEL IF USING IN WORLD MAP
### Juliana Taube

main_us_function <- function(data_to_use, is_diff_age_dist, 
                          for_world_map, is_100_covg, is_1984_end, 
                          no_immigration, homogeneous_vax_covg,
                          cess_cvg_data){
  
  #data_to_use <- data_to_use %>% left_join(cess_cvg_data, by = ("POBP"))
  if(is_100_covg){
    df <- data_to_use %>% left_join(cess_cvg_data, by = ("POBP")) %>% 
      rowwise() %>% 
      mutate(vax_stopped = ifelse(POBP < 60, 1972, vax_stopped), # was < 100
             Country = ifelse(POBP < 60, "USA", Country),
             chance_vaxxed = ifelse(birth_year <= vax_stopped, 1, 0))
  }else if(is_1984_end){
    df <- data_to_use %>% left_join(cess_cvg_data, by = ("POBP")) %>% 
      left_join(polio_proxy_coverage, by = c("POBP" = "fips_of_birth")) %>% 
      rowwise() %>% 
      mutate(vax_stopped = 1984,
             chance_vaxxed = ifelse(POBP >= 100, # foreign born
                                    calc_vax_coverage_foreign(birth_year, Survey_Year,
                                                              vax_stopped, cvg_514_orig, 
                                                              cvg_over14_orig, cvg_tot_orig),
                                    ifelse(birth_year <= vax_stopped, spx_coverage, 0)))
                                           #ifelse(!is.na(spx_coverage), spx_coverage, DEFAULT_COVERAGE),
                                           #0))) # including US territories rn
  }else if(no_immigration){ # everyone stopped vaxxing in 1972
    df <- data_to_use %>% filter(POBP < 100) %>% # including US territories rn
      left_join(polio_proxy_coverage, by = c("POBP" = "fips_of_birth")) %>% 
      rowwise() %>% 
      mutate(vax_stopped = 1972,
             chance_vaxxed = ifelse(birth_year <= vax_stopped, spx_coverage, 0))
                                    #ifelse(!is.na(spx_coverage), spx_coverage, DEFAULT_COVERAGE), 
                                    #0)) 
  }else if(homogeneous_vax_covg){ # homogeneous within US, other countries can still differ
    df <- data_to_use %>% left_join(cess_cvg_data, by = ("POBP")) %>% 
      rowwise() %>% 
      mutate(vax_stopped = ifelse(POBP < 60, 1972, vax_stopped), # was < 100
             Country = ifelse(POBP < 60, "USA", Country),
             chance_vaxxed = ifelse(POBP >= 100, # foreign born
                                    calc_vax_coverage_foreign(birth_year, Survey_Year,
                                                              vax_stopped, cvg_514_orig, 
                                                              cvg_over14_orig, cvg_tot_orig),
                                    ifelse(birth_year <= vax_stopped, US_SPX_COVG, 0)))
  }else if(is_diff_age_dist){ # data being used here will be one of the datasets with recalculated weights
    df <- data_to_use %>% rowwise() %>% 
      mutate(min_age = as.numeric(substring(age_5, 2, 3)),
                 max_age = ifelse(age_5 == "A85PLUSB", 100, as.numeric(substring(age_5, 5, 6))), # can ignore warning
                 min_birth_year = 2022 - max_age,
                 max_birth_year = 2022 - min_age) %>% 
      left_join(polio_proxy_coverage, by = c("ST" = "fips_of_birth")) %>% #  my way
      rowwise() %>% 
      mutate(chance_vaxxed = calc_vax_coverage_us_age_groups(min_birth_year, max_birth_year, 1972,
                                                             coverage))
    # can still use vax eligible function
    # but other calc_vax_coverage may not work
    # IN THIS CASE REMEMBER CAN'T TAKE INTO ACCOUNT IMMIGRATION/PLACE OF BIRTH
    # main place new weights come into play is in taking weighted mean to get prop vaxxed
    # but we need to treat diff age dists differently here because this is grouped
    # also whether national or age, still using groups, still same end date
  
  }else{
    df <- data_to_use %>% left_join(cess_cvg_data, by = ("POBP")) %>% 
      left_join(polio_proxy_coverage, by = c("POBP" = "fips_of_birth")) %>%
      rowwise() %>% 
      mutate(vax_stopped = ifelse(POBP < 100, 1972, vax_stopped), # set all US to 1972
             Country = ifelse(POBP < 60, "USA", Country),
             chance_vaxxed = ifelse(POBP >= 100,
                                    calc_vax_coverage_foreign(birth_year, Survey_Year,
                                                              vax_stopped, cvg_514_orig, 
                                                              cvg_over14_orig, cvg_tot_orig),
                                    ifelse(birth_year <= vax_stopped, spx_coverage, 0)),
                                           #ifelse(!is.na(spx_coverage), spx_coverage, DEFAULT_COVERAGE),
                                           #0)),
             prop_born_before_1980 = ifelse(birth_year < 1980, 1, 0),
             prop_born_before_cessation = ifelse(is.na(vax_stopped), NA_real_,
                                                 ifelse(birth_year <= vax_stopped,
                                                        1, 0)),
             foreign_born = ifelse(POBP >= 100, 1, 0), # including US territories as US-born
             overall_cvg = mean(c(spx_coverage, cvg_514_orig, cvg_over14_orig, 
                                  cvg_tot_orig), na.rm = T))
  }
  
  if(is_diff_age_dist){
    prop_by_puma <- df %>% group_by(PUMA, ST) %>% 
      summarise(prop_vaxxed = weighted.mean(chance_vaxxed, age_group_weight, na.rm = T)) %>% 
      ungroup()
  }else if(is_100_covg | is_1984_end | no_immigration | homogeneous_vax_covg){
    prop_by_puma <- df %>% group_by(PUMA, ST, REGION) %>% 
      summarise(prop_vaxxed = weighted.mean(chance_vaxxed, PWGTP, na.rm = T),
                pop_size = sum(PWGTP)) %>% # 
      ungroup()
  }else{
    prop_by_puma <- df %>% group_by(PUMA, ST, REGION) %>% 
      summarise(prop_vaxxed = weighted.mean(chance_vaxxed, PWGTP, na.rm = T),
                pop_size = sum(PWGTP),
                perc_born_before_1980 = weighted.mean(prop_born_before_1980, PWGTP) * 100,
                perc_born_before_cessation = weighted.mean(prop_born_before_cessation, PWGTP, na.rm = T),
                perc_foreign_born = weighted.mean(foreign_born, PWGTP) * 100,
                overall_cvg = weighted.mean(overall_cvg, PWGTP, na.rm = T)) %>% # 
      ungroup()
  }
  
  
  if(!for_world_map){
    prop_by_puma <- prop_by_puma %>% mutate(perc_vaxxed = prop_vaxxed * 100)
    return(prop_by_puma)
  }else{
    if(is_diff_age_dist){
      prop_by_state <- df %>% group_by(ST) %>% 
        summarise(prop_vaxxed = weighted.mean(chance_vaxxed, age_group_weight, na.rm = T)) %>% 
        ungroup() %>% 
        left_join(state_fips, by = c("ST" = "fips")) %>% 
        select(state, prop_vaxxed) %>% 
        mutate(ISOALPHA = "USA",
               COUNTRYNM = "united states",
               perc_vaxxed = prop_vaxxed * 100)
    }else if(is_100_covg | is_1984_end | no_immigration | homogeneous_vax_covg){
      prop_by_state <- df %>% group_by(ST, REGION) %>% 
        summarise(prop_vaxxed = weighted.mean(chance_vaxxed, PWGTP, na.rm = T),
                  pop_size = sum(PWGTP)) %>% 
        ungroup() %>% 
        left_join(state_fips, by = c("ST" = "fips")) %>% 
        select(state, prop_vaxxed, pop_size) %>% 
        mutate(ISOALPHA = "USA",
               COUNTRYNM = "united states",
               perc_vaxxed = prop_vaxxed * 100)
    }else{
      prop_by_state <- df %>% group_by(ST, REGION) %>% 
        summarise(prop_vaxxed = weighted.mean(chance_vaxxed, PWGTP, na.rm = T),
                  pop_size = sum(PWGTP),
                  perc_born_before_1980 = weighted.mean(prop_born_before_1980, PWGTP) * 100,
                  perc_born_before_cessation = weighted.mean(prop_born_before_cessation, PWGTP, na.rm = T) * 100,
                  perc_foreign_born = weighted.mean(foreign_born, PWGTP) * 100,
                  mean_age = weighted.mean(AGEP, PWGTP),
                  overall_cvg = weighted.mean(overall_cvg, PWGTP, na.rm = T) * 100) %>% 
        ungroup() %>% 
        left_join(state_fips, by = c("ST" = "fips")) %>% 
        select(state, prop_vaxxed, pop_size, overall_cvg, perc_born_before_1980,
               perc_born_before_cessation, perc_foreign_born, mean_age) %>% 
        mutate(ISOALPHA = "USA",
               COUNTRYNM = "united states",
               perc_vaxxed = prop_vaxxed * 100)
    }
    
    return(prop_by_state)
    # only includes pop_size if not different age dist
  }
  
}

