# functions processing the logic of scar survey data
# includes two different approaches, one for non-us countries, and
#   one for foreign-born individuals in the US

#DEFAULT_COVERAGE <- 0.8

####  FOR WORLD OR 5 YEAR AGE GROUPINGS
# determine proportion of age group eligible for vaccination
vax_eligible <- function(min_date, max_date, stop_date){
  if(min_date <= stop_date && stop_date < max_date){
    return((stop_date - min_date + 1)/ 5)
  }else if(max_date <= stop_date){
    return(1)
  }else{return(0)}
}

# determine proportion of age group likely actually vaccinated
calc_vax_coverage <- function(min_date, max_date, survey_date, vax_end_date, 
                              covg5to14, covg15plus, covgall){
  # first calculate prop_eligible for vaccination, if no vax_end_date then put NA
  if(!is.na(vax_end_date)){
    prop_eligible <- vax_eligible(min_date, max_date, vax_end_date)
  }else{
    return(NA_real_)
  }
  
  # if there is no survey date, need to use default 0.80 coverage
  # I think this should get changed to all original values are NA because when we do bootstrapping
  # there are a bunch where we don't have a survey date but we will have a specific coverage to use
  if(is.na(covg5to14) & is.na(covg15plus) & is.na(covgall)){
    return(prop_eligible * DEFAULT_COVERAGE)
  }else if(is.na(survey_date)){ # this is bootstrap case where survey date not available but overall covg is
    return(prop_eligible * covgall)
  }else{ # have survey date
    # convert NaNs from bootstrap to NA
    covg5to14 <- ifelse(is.nan(covg5to14), NA, covg5to14)
    covg15plus <- ifelse(is.nan(covg15plus), NA, covg15plus)
    covgall <- ifelse(is.nan(covgall), NA, covgall)
    # if have covg5to14 and covg15plus then good to go, otherwise
    if(!is.na(covg5to14) & is.na(covg15plus)){ # have 5to14 but not 15+
      covg15plus <- ifelse(is.na(covgall), covg5to14, covgall) # if have overall, use that
    }else if(is.na(covg5to14)){ # don't have 5to14, use overall
      covg5to14 <- covg15plus <- covgall
    }
    
    if(max_date > survey_date){ # any born after scar survey
      return(prop_eligible * covg5to14)
    }else{ # born before scar survey
      if(max_date <= (survey_date - 15)){ # all 15+ at time of scar survey
        return(prop_eligible * covg15plus)
      }else if(min_date > (survey_date - 15)){ # all <15 at time of scar survey
        return(prop_eligible * covg5to14)
      }else{ # some over 15 at time of scar survey
        prop_15plus <- (survey_date - 15 - min_date + 1)/5
        prop_under15 <- 1 - prop_15plus
        return((prop_under15 * covg5to14) + (prop_15plus * covg15plus))
      }
    }
  }
}



####  FOR US FOREIGN BORN NO AGE GROUPING
calc_vax_coverage_foreign <- function(birth_year, survey_date, vax_end_date, 
                                      covg5to14, covg15plus, covgall){
  vax_end_date <- ifelse(is.na(vax_end_date), 1980, vax_end_date) # use default end date if missing
  
  if(birth_year > vax_end_date){ # born after end of vaccination
    return(0)
  }else{
    # convert NaNs from bootstrap to NA
    covg5to14 <- ifelse(is.nan(covg5to14), NA, covg5to14)
    covg15plus <- ifelse(is.nan(covg15plus), NA, covg15plus)
    covgall <- ifelse(is.nan(covgall), NA, covgall)
    # if there is no survey date, need to use default 0.8 coverage
    if(is.na(survey_date)){
      return(DEFAULT_COVERAGE)
    }else{ # have survey date
      # if have covg5to14 and covg15plus then good to go, otherwise
      if(!is.na(covg5to14) & is.na(covg15plus)){ # have 5to14 but not 15+
        covg15plus <- ifelse(is.na(covgall), covg5to14, covgall) # if have overall, use that
      }else if(is.na(covg5to14)){ # don't have 5to14, use overall
        covg5to14 <- covg15plus <- covgall
      }
      
      if(birth_year > survey_date){ # any born after scar survey
        return(covg5to14)
      }else{ # born before scar survey
        if(birth_year <= (survey_date - 15)){ # all 15+ at time of scar survey
          return(covg15plus)
        }else{ # all <15 at time of scar survey
          return(covg5to14)
        }
      }
    }
  }
}


calc_vax_coverage_us_age_groups <- function(min_date, max_date, vax_end_date,
                                            coverage){
  # first calculate prop_eligible for vaccination, if no vax_end_date then put NA
  prop_eligible <- vax_eligible(min_date, max_date, vax_end_date)
  
  # if there is no coverage estimate, use default
  if(is.na(coverage)){
    return(DEFAULT_COVERAGE * prop_eligible)
  }else{
    return(coverage * prop_eligible)
  }
  
}

#### FOR SAME AVERAGE VALUE
# determine proportion of age group likely actually vaccinated
calc_vax_coverage_100 <- function(min_date, max_date, vax_end_date){
  # first calculate prop_eligible for vaccination, if no vax_end_date then put NA
  if(!is.na(vax_end_date)){
    prop_eligible <- vax_eligible(min_date, max_date, vax_end_date)
    return(prop_eligible) # since assume 100% coverage
  }else{
    return(NA_real_)
  }
}