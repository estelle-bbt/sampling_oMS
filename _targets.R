#' Targets plan
#' 

## Attach required packages ----

library(targets)
library(tarchetypes)
library(ggplot2)

tar_option_set(
  packages = c("dplyr","tidyr","ggplot2")  # load dplyr in each environement
)

tar_source()

## Load Project R Functions ----

source(here::here("R", "oMS_proxy.R"))

## Analyses pipeline ----

list(
  
  ## Manage data ----
  
  tar_target(data_id,load_data_id("data/data_ABPOLL_ID_resume.txt", cols = c("co5", "co10", "co20"))),
  
  tar_target(data_obs,load_data_obs("data/obs_ABPOLL.txt")),
  
  tar_target(data_contact,get_data_contact(data_obs)),
  
  tar_target(data_session,get_data_session(data_contact)),
  
  tar_target(data_proxy,compute_oms_and_proxy(data_id,data_contact,data_session)),
  
  tar_target(pcas_10,get_pca(data_proxy$dt_proxy, cols = "co10")),
  
  tar_target(pcas_5,get_pca(data_proxy$dt_proxy, cols = "co5")),
  
  tar_target(pcas_20,get_pca(data_proxy$dt_proxy, cols = "co20")),
  
  tar_target(data_genotypes,load_data("data/all_paternities_ABPOLL.txt")),
  
  tar_target(data_true_rs_ms,get_true_rs_ms(data_genotypes,data_id)),
  
  tar_target(data_true_bateman,get_true_bateman(data_true_rs_ms, data_proxy$dt_proxy, cols = "co10")),
  
  # tar_target(sum_genotypes,sum_data_genotypes(data_genotypes)),
  
  tar_target(data_sampling,load_data("data/data_created/sampling_table_fixed_strategy.txt")),
  
  # tar_target(data_ampling_rs_ms,get_sampling_rs_ms(data_sampling,sum_genotypes,data_id)),
  
  tar_target(data_sampling_rs_ms,load_data("data/data_created/sampling_rs_ms_table_fixed_strategy.txt")),
  
  tar_target(data_sampling_bateman,get_sampling_bateman(data_sampling_rs_ms)),
  
  tar_target(data_hedges_true,get_hedges(data_sampling_bateman$dt_sampling_bateman, data_true_bateman, per_session = TRUE)),
  
  tar_target(data_hedges_false,get_hedges(data_sampling_bateman$dt_sampling_bateman, data_true_bateman, per_session = FALSE)),

  tar_target(data_var_gms,get_var_gms(data_sampling, data_id)),
  
  tar_target(data_sem_complete_sessions,get_data_sem_complete_sessions(data_true_rs_ms, data_proxy$dt_proxy, cols="co10")),
  
  tar_target(data_id_sampled_sessions,load_data("data/all_data_long_NA_0AllFemFALSE_raw.txt")),
  
  tar_target(data_sem_sampled_sessions,get_data_sem_sampled_sessions(data_id_sampled_sessions, data_proxy$dt_proxy, cols="co10")),
  
  tar_target(linear_models_oms_proxy,get_linear_models_oms_proxy(data_proxy$dt_proxy_longer)),
  
  tar_target(predictions_oms_proxy,compute_predictions_oms_proxy(linear_models_oms_proxy,data_proxy$dt_proxy_longer)),
  
  ## Quarto ----
  
  tarchetypes::tar_quarto(index, "index.qmd")
  
)



