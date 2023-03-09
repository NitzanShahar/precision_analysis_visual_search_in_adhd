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
  0 + Intercept + 
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
  set_prior("constant(600)", coef = "Intercept") + 
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

n <- c(35, 40, 50)
ksims <- 15

# # Save results template
# data.frame(
#   N = NA,
#   i = NA,
#   block_halfSecondXdistPresencepresent = NA,
#   block_halfSecondXdistPresencepresentXgroupADHD = NA
# ) |> 
#   write.table("power/power_results.csv", 
#               sep = ",", row.names = FALSE)


for (ni in n) {
  ## For each sample size ----
  n_size_dat <- gen_X(ni)

  PPD <- posterior_predict(model_for_gen, newdata = n_size_dat,
                           allow_new_levels = TRUE,
                           sample_new_levels = "gaussian",
                           ndraw = ksims) # Make this bigger - say, 50 or 100.
  
  for (i in seq_len(ksims)) {
    ## make a data set -------
    temp_data <- n_size_dat
    temp_data$RespRT <- PPD[i, ]

    ## Estimate sub model --------
    if (i == 1L) {
      temp_model <- brm(f_model, 
                        family = gaussian(),
                        data = temp_data,
                        backend = "cmdstanr", cores = 4, refresh = 0)  
    } else {
      temp_model <- update(temp_model, recompile = FALSE,
                           newdata = temp_data, cores = 4, refresh = 0)
    }
    
    
    ## Test -------
    # In this example, the test is if the 95% HDI contains 0
    temp_hdi <- hdi(temp_model)
    
    temp_hdi |> 
      # Get only parameter of interest
      filter(str_detect(Parameter, "block_halfSecond:distPresencepresent")) |> 
      # Does HDI contain 0?
      mutate(is_sig = 0 < CI_low | CI_high < 0) |> 
      # Reshape
      select(Parameter, is_sig) |> 
      pivot_wider(names_from = Parameter, values_from = is_sig) |> 
      mutate(N = ni, i = i, .before = 1) |> 
      # Save (append)
      write.table("power/power_results.csv", 
                  append = TRUE, col.names = FALSE,
                  sep = ",", row.names = FALSE)
  }
}


# Analysis ----------------------------------------------------------------


power_results <- read.csv("power/power_results.csv")

binomial_ci <- function(lgl) {
  h <- prop.test(sum(lgl), length(lgl))
  p <- parameters::model_parameters(h)
  glue::glue_data(format(p), "{Proportion}, [{`95% CI`}]")
}

power_results |> 
  drop_na(N) |> 
  group_by(N) |> 
  summarise(
    across(where(is.logical), binomial_ci, .unpack = TRUE)
  )


