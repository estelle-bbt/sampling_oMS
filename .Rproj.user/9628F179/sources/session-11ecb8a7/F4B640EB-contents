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
  
  tar_target(data_id,load_data_id("data/data_ABPOLL_ID_resume.txt")),
  
  tar_target(data_obs,load_data_obs("data/obs_ABPOLL.txt")),
  
  tar_target(data_contact,get_data_contact(data_obs)),
  
  # RENDU LA AJOUTER DANS QUARTO
  
  ## Quarto ----
  
  tarchetypes::tar_quarto(index, "index.qmd")
  
)



