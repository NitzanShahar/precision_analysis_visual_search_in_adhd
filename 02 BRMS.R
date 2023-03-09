
library(brms)
library(brms.exgaussian)
afex::set_effects_contrasts()

data_clean <- readRDS("data/data_clean.rds")


mod_spec <- bf(RespRT ~ distPresence * block_half + (block_half * distPresence | participant),
               tau ~ distPresence * block_half + (block_half * distPresence | participant))


# Priors ------------------------------------------------------------------

# get_prior(mod_spec, data = data_clean, 
#           family = exgaussian2(), 
#           stancode = exgaussian2_stancode(fixed_sigma = TRUE))

priors <- 
  # Mu
  set_prior("student_t(3, 0, 50)", class = "b") +
  # Tau
  set_prior("student_t(3, 0, 5)", class = "Intercept", dpar = "tau") +
  set_prior("student_t(3, 0, 1.1)", class = "b", dpar = "tau") +
  # Sigma
  set_prior("student_t(3, 0, 5)", class = "sigma")

validate_prior(priors, mod_spec,
               data = data_clean,
               
               family = exgaussian2(),
               stanvars = exgaussian2_stancode(fixed_sigma = TRUE))


# Fit ---------------------------------------------------------------------

mod_stepB <- brm(
  mod_spec, 
  data = data_clean, 
  prior = priors,
  
  family = exgaussian2(),
  stanvars = exgaussian2_stancode(fixed_sigma = TRUE),
  
  backend = "cmdstanr", 
  cores = 4
)


# Save --------------------------------------------------------------------

saveRDS(mod_stepB, "data/mod_stepB.Rds")
