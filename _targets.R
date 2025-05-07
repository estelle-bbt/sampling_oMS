#' Targets plan
#' 

## Attach required packages ----

library(targets)
library(tarchetypes)
library(ggplot2)

tar_option_set(
  packages = c("dplyr","tidyr")  # load dplyr in each environement
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
  
  tar_target(data_proxy,compute_oMS_and_proxy(data_id,data_contact,data_session)),
  
  tar_target(pcas_10,get_pca(data_proxy, cols = "co10")),
  
  tar_target(pcas_5,get_pca(data_proxy, cols = "co5")),
  
  tar_target(pcas_20,get_pca(data_proxy, cols = "co20")),

  ## Quarto ----
  
  tarchetypes::tar_quarto(index, "index.qmd")
  
)



