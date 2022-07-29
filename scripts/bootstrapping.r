source("scripts/load_files_for_run.r")

# can use runif(num_samples, min_value, max_value) for real numbers
# can use rdunif(num_samples, min_value, max_value) for discrete (integers)

ITERATIONS <- 5000

# need to generate 5000 estimates of coverage and date for each country, 

date_sampler <- function(low, high, iterations, dist){
  if(is.na(low) & is.na(high)){
    return(list(rep(NA, iterations))) # I don't think there are any instances of this
  }else if(dist == "U"){ # uniform
    samps <- extraDistr::rdunif(iterations, low, high)
    return(list(samps)) # I don't think this one is low + samps because we are already choosing from the range
  }else if(dist == "N"){ # normal
    samps <- rbbinom(n = iterations, size = high - low, alpha = 1000, beta = 1000)
    return(list(low + samps))
  }else if(dist == "R"){ # right tail, left peak
    samps <- rbbinom(n = iterations, size = high - low, alpha = 0.4, beta = 1.5)
    return(list(low + samps))
  }else if(dist == "L"){ # left tail, right peak
    samps <- rbbinom(n = iterations, size = high - low, alpha = 1.5, beta = 0.4)
    return(list(low + samps))
  }
}

####### NOTES about process
# Want distribution between min_cvg and max_cvg with a mean at orig_cvg
# Beta distribution produces values between [0,1] 
# So we'll need to shift/scale and reverse to translate between these scales
# To scale from [min_cvg, max_cvg] to [0,1] we need to subtract min_cvg (to get values) from [0,max-min]), and then divide by (max_cvg-min_cvg) to get values in [0,1]
# To scale from [0,1] to [min_cvg, max_cvg] we need to multiply by (max_cvg-min_cvg) and add min_cvg 

coverage_sampler <- function(orig, low, high, iterations, dist){
  if(is.na(low) & is.na(high)){
    return(list(rep(NA, iterations)))
  }else if(dist == "U"){ # uniform
    samps <- runif(iterations, low, high)
    return(list(samps))
  }else{ # betas and normal
    mean <- (orig - low)/(high - low)
    b <- 2 # arbitrary
    a <- (2 * mean)/(1 - mean)
    samps <- (rbeta(n = iterations, shape1 = a, shape2 = b) * (high - low)) + low
    return(list(samps))
  }
}

cessation_coverage_choices <- cessation_coverage_data %>% rowwise() %>% 
  mutate(cess_date_samp = date_sampler(cessdate_low, cessdate_high, ITERATIONS, 
                                       cessdate_distrib),
         overall_coverage_samp = coverage_sampler(cvg_tot_orig, cvg_tot_low, 
                                                  cvg_tot_high, ITERATIONS, cvg_distrib),
         coverage_5to14_samp = coverage_sampler(cvg_514_orig, cvg_514_low, 
                                                cvg_514_high, ITERATIONS, cvg_distrib),
         coverage_15plus_samp = coverage_sampler(cvg_over14_orig, cvg_over14_low, 
                                                 cvg_over14_high, ITERATIONS, cvg_distrib)) %>% 
  select(ISO, POBP, Country, Survey_Year, overall_coverage_samp, coverage_5to14_samp, 
         coverage_15plus_samp, cess_date_samp)

### THIS WILL TAKE A WHILE (2.5 HRS FOR 5000 ITERATIONS)
estimates_log <- data.frame()
for(i in 1:ITERATIONS){
  print(i)
  # pull out values for this round?
  sampled_cessation_coverage_data <- cessation_coverage_choices %>% rowwise() %>% 
    # replace original columns with sampled values, names need to match cessation_coverage_data
    mutate(cvg_tot_orig = overall_coverage_samp[[i]],
           cvg_514_orig = coverage_5to14_samp[[i]], 
           cvg_over14_orig = coverage_15plus_samp[[i]],
           vax_stopped = cess_date_samp[[i]]) %>% 
    select(ISO, POBP, Country, Survey_Year, cvg_tot_orig, cvg_514_orig, 
           cvg_over14_orig, vax_stopped)
    # rename_with(.fn = ~paste0(., i), # ~ converts paste0 to a function, want names to have number i
    #             .cols = c(vax_stopped, prop_vax_all_ages, prop_vax_5to14, prop_vax_over14)) %>% 

  # then time to calculate estimates
  # need to use these coverage and cessation dates instead of default
  sample_estimate <- main_function(other_age_data = NA, is_diff_age_dist = FALSE, 
                                   is_world_age_dist = FALSE, is_100_covg = FALSE, 
                                   is_1984_end = FALSE, waning = 0.807, 
                                   include_natural_immunity = FALSE, is_bootstrap = TRUE) %>%
    rename_with(.fn = ~paste0(., i), .cols = c(perc_susceptible)) # ~ converts paste0 to a function
  # bind_rows may be faster but still need to rename
  if(i == 1){
    estimates_log <- estimates_log %>% bind_rows(sample_estimate)
  }else{
    estimates_log <- estimates_log %>% left_join(sample_estimate, by = c("ISOALPHA", "COUNTRYNM", "NAME1"))
  }
  
}

write_csv(estimates_log, "data/bootstrapped_estimates_5000.csv")
estimates_log <- read_csv("data/bootstrapped_estimates_5000.csv") # be careful bc input is bootstrap_estimates

test <- estimates_log %>% 
  pivot_longer(cols = perc_susceptible1:perc_susceptible5000,
               values_to = "perc", names_to = "rep") %>% 
  group_by(ISOALPHA, COUNTRYNM, NAME1) %>% 
  summarise(mu = mean(perc),
            sigma = sd(perc))

write_csv(test, "data/bootstrapped_estimates_5000_musig.csv")


