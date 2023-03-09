library(tidyverse)

raw_dat <- read.csv("data/CombinedData_forMatan.csv")


# Tidy --------------------------------------------------------------------


data_clean <- raw_dat |> 
  group_by(participant,session, block) |> 
  mutate(trial = row_number()) |> 
  ungroup() |> 
  filter(
    # Remove practice trials
    Practice == "main",
    # Remove error trials
    Response.corr == 1
  ) |> 
  # Remove constant columns
  select(where(\(x) !insight::has_single_value(x))) |> 
  # Tidy some columns
  mutate(
    participant = factor(participant),
    distPresence = factor(distPresence, levels = c("absent", "present")),
    block = factor(block),
    session = factor(session),
    RespRT = as.numeric(RespRT)
  ) |> 
  mutate(
    block_half = case_when(trial <= 24 ~ "First",
                           trial > 24 ~ "Second") |> 
      factor()
  )


# Save --------------------------------------------------------------------


saveRDS(data_clean, "data/data_clean.rds")
