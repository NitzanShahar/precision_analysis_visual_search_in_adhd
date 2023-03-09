rm(list=ls())
library(tidyverse)
library(brms)
library(bayestestR)


#N=10
load(paste0('data/precision_analysis_results_Ni=10.rdata'))
load(paste0('data/precision_analysis_results_Ni=20.rdata'))
load(paste0('data/precision_analysis_results_Ni=50.rdata'))

df=do.call(rbind,precision_models)

df |> filter(Parameters == "b_block_halfSecond:distPresencepresent") |> select(!Parameters) |> summarise_all(mean)
df |> filter(Parameters == "b_block_halfSecond:distPresencepresent") |> select(!Parameters) |> summarise_all(min)
df |> filter(Parameters == "b_block_halfSecond:distPresencepresent") |> select(!Parameters) |> summarise_all(max)
df |> filter(Parameters == "b_block_halfSecond:distPresencepresent") |> select(!Parameters) |> summarise(sum(CI95high<0))


df |> filter(Parameters == "b_block_halfSecond:distPresencepresent:groupADHD") |> select(!Parameters) |> summarise_all(mean)
df |> filter(Parameters == "b_block_halfSecond:distPresencepresent:groupADHD") |> select(!Parameters) |> summarise_all(min)
df |> filter(Parameters == "b_block_halfSecond:distPresencepresent:groupADHD") |> select(!Parameters) |> summarise_all(max)
df |> filter(Parameters == "b_block_halfSecond:distPresencepresent:groupADHD") |> select(!Parameters) |> summarise(sum(CI95low>0))
