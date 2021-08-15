setwd("C:\\Users\\Tomas\\Desktop\\RepTwfe")

library(tidyverse)
library(lfe)
library(fastDummies)
library(ggthemes)
library(did)

install.packages("fastDummies", "did")


iseed  = 20201221
nrep <- 100  
true_mu <- 1
set.seed(iseed)

#----------------------------------------------------------------------------
## Generate data - treated cohorts consist of 250 obs each, with the treatment effect still = true_mu on average
make_data <- function(nobs = 1000, 
                      nstates = 40) {
  
  # unit fixed effects (unobservd heterogeneity)
  unit <- tibble(
    unit = 1:nobs,
    # generate state
    state = sample(1:nstates, nobs, replace = TRUE),
    unit_fe = rnorm(nobs, state/5, 1),
    # generate instantaneous treatment effect
    #mu = rnorm(nobs, true_mu, 0.2)
    mu = true_mu
  )
  
  # year fixed effects (first part)
  year <- tibble(
    year = 1980:2010,
    year_fe = rnorm(length(year), 0, 1)
  )
  
  # Put the states into treatment groups
  treat_taus <- tibble(
    # sample the states randomly
    state = sample(1:nstates, nstates, replace = FALSE),
    # place the randomly sampled states into four treatment groups G_g
    cohort_year = sort(rep(c(1986, 1992, 1998, 2004), 10))
  )
  
  # make main dataset
  # full interaction of unit X year 
  expand_grid(unit = 1:nobs, year = 1980:2010) %>% 
    left_join(., unit) %>% 
    left_join(., year) %>% 
    left_join(., treat_taus) %>% 
    # make error term and get treatment indicators and treatment effects
    # Also get cohort specific trends (modify time FE)
    mutate(error = rnorm(nobs*31, 0, 1),
           treat = ifelse(year >= cohort_year, 1, 0),
           tau = ifelse(treat == 1, mu, 0),
           year_fe = year_fe + 0.1*(year - cohort_year)
    ) %>% 
    # calculate cumulative treatment effects
    group_by(unit) %>% 
    mutate(tau_cum = cumsum(tau)) %>% 
    ungroup() %>% 
    # calculate the dep variable
    mutate(dep_var = (2010 - cohort_year) + unit_fe + year_fe + tau_cum + error)
  
}

data <- make_data()

write.csv(data, "dataTWFE.csv")
  