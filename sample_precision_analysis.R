library(tidyverse)
library(brms)
library(bayestestR)

# Function that makes IV data
gen_X <- function(nsub, trials = 70, ...) {
  expand.grid(
    group = factor(1:2, labels = c("control", "ADHD")),
    participant = seq_len(nsub),
    distPresence = factor(1:2, labels = c("absent", "present")),
    block_half = factor(1:2, labels = c("First", "Second")),
    trial = seq(70)
  ) |> 
    mutate(
      participant = ifelse(group == "control", participant, participant + nsub),
      participant = factor(participant)
    )
}

f_model <- 
  RespRT ~ 
  block_half * distPresence * group + 
  (block_half * distPresence | participant)


# Make model --------------------------------------------------------------
# For data simulation


data_clean <- readRDS("data/data_clean.rds") |>
  select(participant, distPresence, block_half, RespRT) |>
  mutate(group = "control")

data_clean2g <- rbind(data_clean,
                      # We need one row with ADHD:
                      data_clean[1,] |>
                        mutate(group = "ADHD", participant = factor("g"))) |>
  mutate(group = factor(group, levels = c("control", "ADHD")))

get_prior(f_model,
          family = gaussian(),
          data = data_clean2g)

priors_for_gen <-
  set_prior("constant(600)", class = "Intercept") + 
  set_prior("constant(0)", coef = "block_halfSecond") + 
  set_prior("constant(25)", coef = "distPresencepresent") +
  set_prior("constant(100)", coef = "groupADHD") +
  set_prior("constant(-25)", coef = "block_halfSecond:distPresencepresent") +
  set_prior("constant(25)", coef = "distPresencepresent:groupADHD") +
  set_prior("constant(0)", coef = "block_halfSecond:groupADHD") +
  set_prior("constant(25)", coef = "block_halfSecond:distPresencepresent:groupADHD")

validate_prior(priors_for_gen,
               f_model,
               family = gaussian(),
               data = data_clean2g)

model_for_gen <- brm(f_model, 
                     family = gaussian(),
                     data = data_clean2g,
                     
                     prior = priors_for_gen,
                     backend = "cmdstanr", cores = 4, 
                     seed = 20230222)
# Sigma and random (co)variance are estimated from the data.


saveRDS(model_for_gen, "power/model_for_gen.rds")



# Run power simulation ----------------------------------------------------

model_for_gen <- readRDS("power/model_for_gen.rds")

n <- c(10, 20, 50)
ksims <- 10

for (ni in n) {
  
  ## For each sample size ----
  n_size_dat <- gen_X(ni)

  PPD <- posterior_predict(model_for_gen, newdata = n_size_dat,
                           allow_new_levels = TRUE,
                           sample_new_levels = "gaussian",
                           ndraw = ksims) # Make this bigger - say, 50 or 100.
  
  for (i in seq_len(ksims)) {
    print(paste0('i=',i,'ni=',ni))
    ## make a data set -------
    temp_data <- n_size_dat
    temp_data$RespRT <- PPD[i, ]

    ## Estimate sub model --------
    if (i == 1L) {
      temp_model <- brm(f_model, 
                        family = gaussian(),
                        data = temp_data,
                        backend = "cmdstanr", cores = 4)  
    } else {
      temp_model <- update(temp_model, recompile = FALSE,
                           newdata = temp_data, cores = 4)
    }
    
    
    ## Save -------
    if(i==1){results_models=list()}
    results_models[[i]]<-temp_model
    save(results_models,file = paste0('data/raw_precision_models_Ni=',ni,'.rdata'))
  }
}

# Housekeeping ------------------------------------------------------------
#here we simply translate the "results_models" object to a list with the parameters, meadian and hdi for each model
rm(list=ls())


precision_models=list()

n     <- c(10, 20, 50)
ksims <- 10
for (ni in n) {
  load(paste0('data/raw_precision_models_Ni=',ni,'.rdata'))
  
  for (i in seq_len(ksims)){
    
    model = results_models[[i]]
    
    x85 = describe_posterior(model,ci=c(0.85))
    x90 = describe_posterior(model,ci=c(0.90))
    x95 = describe_posterior(model,ci=c(0.95))
    x   = data.frame(
      Sample    = i,
      Parameters= x85$Parameter,
      Median    = x85$Median,
      CI85low   = x85$CI_low,
      CI85high  = x85$CI_high,
      CI85      = x85$CI_high - x85$CI_low,
      CI90low   = x90$CI_low,
      CI90high  = x90$CI_high,
      CI90      = x90$CI_high - x90$CI_low,
      CI95low   = x95$CI_low,
      CI95high  = x95$CI_high,
      CI95      = x95$CI_high - x95$CI_low)
    
    precision_models[[i]]=x
  }
  save(precision_models,file=paste0('data/precision_analysis_results_Ni=',ni,'.rdata'))
}
