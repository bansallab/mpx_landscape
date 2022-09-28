### RUNS UNCERTAINTY ANALYSIS AT ADMIN1 LEVEL AGGREGATED ACROSS ALL AGE GROUPS
###   USING UPPER AND LOWER BOUNDS FOR CESSATION AND COVERAGE ESTIMATES
### Juliana Taube

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
    samps <- rbbinom(n = iterations, size = high - low, alpha = 2, beta = 2)
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

# performs sampling from normal centered at zero with appropriate standard dev for each
#   country and then adds to original value for that age group in that admin1
age_sampler <- function(orig_value, stdev, iterations){
  draws <- rnorm(n = iterations, mean = 0, sd = stdev)
  add_orig <- orig_value + draws
  add_orig[add_orig < 0] = 0 # replace negative values with zero
  return(list(add_orig))
}
# can't get negative numbers though

age_dist_choices <- gpw %>% inner_join(age_dist_differences, 
                                      by = c("ISOALPHA" = "ISO", "COUNTRYNM")) %>% 
  # gets rid of a couple small island nations not in UN dataset
  rowwise() %>% # use mutate across here?
  # mutate(across(A00_04B:A85PLUSB, ~ age_sampler(.x, country_sd, ITERATIONS)))
  mutate(A00_04B_samp = age_sampler(A00_04B, country_sd, ITERATIONS),
         A05_09B_samp = age_sampler(A05_09B, country_sd, ITERATIONS),
         A10_14B_samp = age_sampler(A10_14B, country_sd, ITERATIONS),
         A15_19B_samp = age_sampler(A15_19B, country_sd, ITERATIONS),
         A20_24B_samp = age_sampler(A20_24B, country_sd, ITERATIONS),
         A25_29B_samp = age_sampler(A25_29B, country_sd, ITERATIONS),
         A30_34B_samp = age_sampler(A30_34B, country_sd, ITERATIONS),
         A35_39B_samp = age_sampler(A35_39B, country_sd, ITERATIONS),
         A40_44B_samp = age_sampler(A40_44B, country_sd, ITERATIONS),
         A45_49B_samp = age_sampler(A45_49B, country_sd, ITERATIONS),
         A50_54B_samp = age_sampler(A50_54B, country_sd, ITERATIONS),
         A55_59B_samp = age_sampler(A55_59B, country_sd, ITERATIONS),
         A60_64B_samp = age_sampler(A60_64B, country_sd, ITERATIONS),
         A65_69B_samp = age_sampler(A65_69B, country_sd, ITERATIONS),
         A70_74B_samp = age_sampler(A70_74B, country_sd, ITERATIONS),
         A75_79B_samp = age_sampler(A75_79B, country_sd, ITERATIONS),
         A80_84B_samp = age_sampler(A80_84B, country_sd, ITERATIONS),
         A85PLUSB_samp = age_sampler(A85PLUSB, country_sd, ITERATIONS)) %>% 
  select(ISOALPHA, COUNTRYNM, NAME1, tot_popn_2020, contains("samp"))

### THIS WILL TAKE A WHILE (2.5 HRS FOR 5000 ITERATIONS)
library(beepr)
admin1_log <- data.frame()
country_age_log <- data.frame()
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

  sampled_age_data <- age_dist_choices %>% rowwise() %>% 
    mutate(across(A00_04B_samp:A85PLUSB_samp, ~ .x[[i]])) %>% 
    rename_with(~ substr(.x, 1, 7), -contains(c("PLUS", "ISO", "COUNTRY", "NAME", "tot"))) %>% 
    rename(A85PLUSB = A85PLUSB_samp) %>% 
    rowwise() %>% 
    mutate(row_sum = sum(A00_04B, A05_09B, A10_14B, A15_19B, A20_24B, A25_29B, 
                         A30_34B, A35_39B,A40_44B, A45_49B, A50_54B, A55_59B, 
                         A60_64B, A65_69B, A70_74B, A75_79B, A80_84B, A85PLUSB)) %>% 
    mutate(across(A00_04B:A85PLUSB, ~ .x/row_sum))

  # then time to calculate estimates
  # need to use these coverage and cessation dates instead of default
  shared_data <- calc_prop_vaxxed(other_age_data = NA, 
                                  is_diff_age_dist = FALSE,
                                  is_world_age_dist = FALSE, 
                                  is_100_covg = FALSE,
                                  is_1984_end = FALSE,
                                  is_bootstrap = TRUE)
  admin1_estimate <- calc_admin1_perc_vaxxed(vaxxed_by_age = shared_data,
                                                  is_world_age_dist = FALSE,
                                                  is_diff_age_dist = FALSE,
                                                  is_100_covg = FALSE,
                                                  is_1984_end = FALSE,
                                                  is_bootstrap = TRUE) %>% 
    select(-prop_vaxxed) %>% 
    rename_with(.fn = ~paste0(., i), .cols = c(perc_vaxxed)) # ~ converts paste0 to a function
  country_estimate <- calc_country_age_perc_vaxxed(vaxxed_by_age = shared_data) %>% 
    select(-prop_vaxxed_country) %>% 
    rename_with(.fn = ~paste0(., i), .cols = c(perc_vaxxed_country)) # ~ converts paste0 to a function
  # sample_estimate <- main_function(other_age_data = NA, is_diff_age_dist = FALSE, 
  #                                  is_world_age_dist = FALSE, is_100_covg = FALSE, 
  #                                  is_1984_end = FALSE, waning = 0.807, 
  #                                  include_natural_immunity = FALSE, is_bootstrap = TRUE) %>%
  #   rename_with(.fn = ~paste0(., i), .cols = c(perc_susceptible)) # ~ converts paste0 to a function
  # bind_rows may be faster but still need to rename
  if(i == 1){
    admin1_log <- admin1_log %>% bind_rows(admin1_estimate)
    country_age_log <- country_age_log %>% bind_rows(country_estimate)
  }else{
    admin1_log <- admin1_log %>% left_join(admin1_estimate, by = c("ISOALPHA", "COUNTRYNM", "NAME1"))
    country_age_log <- country_age_log %>% left_join(country_estimate, by = c("ISOALPHA", "COUNTRYNM", "age"))
  }
  
}
beep()

write_csv(admin1_log, "data/bootstrapped_admin1_estimates_5000.csv") # didn't have admin1 before
admin1_log <- read_csv("data/bootstrapped_admin1_estimates_5000.csv")

write_csv(country_age_log, "data/bootstrapped_country_age_estimates_5000.csv")
country_age_log <- read_csv("data/bootstrapped_country_age_estimates_5000.csv")

mean_sd_admin1 <- admin1_log %>% 
  pivot_longer(cols = perc_vaxxed1:perc_vaxxed5000,
               values_to = "perc", names_to = "rep") %>% 
  group_by(ISOALPHA, COUNTRYNM, NAME1) %>% 
  summarise(mu = mean(perc),
            sigma = sd(perc))

mean_sd_country <- country_age_log %>% 
  pivot_longer(cols = perc_vaxxed_country1:perc_vaxxed_country5000,
               values_to = "perc", names_to = "rep") %>% 
  group_by(ISOALPHA, COUNTRYNM, age) %>% 
  summarise(mu = mean(perc),
            sigma = sd(perc))

write_csv(mean_sd_admin1, "data/bootstrapped_estimates_admin1_5000_musig.csv")
write_csv(mean_sd_country, "data/bootstrapped_estimates_country_age_5000_musig.csv")


