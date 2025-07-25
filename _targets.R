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

source(here::here("R", "functions.R"))

## Analyses pipeline ----

list(
  
  ## Manage data ----
  
  tar_target(data_resume_visits,get_resume_visits("data/obs_ABPOLL.txt")),
  
  tar_target(data_id,load_data_id("data/data_ABPOLL_ID_resume.txt", data_resume_visits, cols = c("co5", "co10", "co20"))),
  
  tar_target(data_flower,load_data_flower("data/data_ABPOLL_flower_resume.txt")),
  
  tar_target(oms_flower,get_ttt_effect_flower(data_flower,"import_nb_part_ID_out_co10")),
  
  tar_target(pl_flower,get_ttt_effect_flower(data_flower,"pl")),
  
  tar_target(seeds_flower,get_ttt_effect_flower(data_flower,"nb_seeds")),
  
  tar_target(data_q_by_female,load_data("data/q_by_female_ABPOLL.txt")),
  
  tar_target(data_obs,load_data_obs("data/obs_ABPOLL.txt")),
  
  tar_target(data_contact,get_data_contact(data_obs)),
  
  tar_target(data_session,get_data_session(data_contact)),
  
  tar_target(data_proxy,compute_oms_and_proxy(data_id,data_contact,data_session)),
  
  tar_target(pcas_10,get_pca(data_proxy$data_proxy, cols = "co10")),
  
  tar_target(pcas_5,get_pca(data_proxy$data_proxy, cols = "co5")),
  
  tar_target(pcas_20,get_pca(data_proxy$data_proxy, cols = "co20")),
  
  tar_target(data_genotypes,load_data("data/fix10_paternities_ABPOLL.txt")),
  
  tar_target(data_true_rs_ms,get_true_rs_ms(data_genotypes,data_id)),
  
  tar_target(data_true_bateman,get_true_bateman(data_true_rs_ms, data_proxy$data_proxy, cols = "co10")),
  
  tar_target(data_sem_complete_sessions,get_data_sem_complete_sessions(data_true_rs_ms, data_proxy$data_proxy, cols="co10")),
  
  tar_target(data_id_sampled_sessions,load_data("data/all_data_long_NA_0AllFemFALSE_raw.txt")),
  
  tar_target(oms_id,get_ttt_effect_id(data_sem_sampled_sessions,"oms")),
  
  tar_target(sr_id,get_ttt_effect_id(data_sem_sampled_sessions,"sr")),
  
  tar_target(sr_all_id,get_ttt_effect_id(data_sem_sampled_sessions,"sr_all")),
  
  tar_target(mean_ps_id,get_ttt_effect_id(data_sem_sampled_sessions,"mean_ps")),
  
  tar_target(diff_q_id,get_ttt_effect_id(data_sem_sampled_sessions,"diff_q")),
  
  tar_target(contid_id,get_ttt_effect_id(data_sem_sampled_sessions,"contact_id")),
  
  tar_target(meanpos_id,get_ttt_effect_id(data_sem_sampled_sessions,"mean_position")),
  
  tar_target(durvis_id,get_ttt_effect_id(data_sem_sampled_sessions,"dur_per_visit")),
  
  tar_target(visflo_id,get_ttt_effect_id(data_sem_sampled_sessions,"nb_visits_per_flower")),
  
  tar_target(flo_id,get_ttt_effect_id(data_sem_sampled_sessions,"nb_flower_visited")),
  
  tar_target(data_parent_share,get_data_parent_share(data_genotypes)),
  
  tar_target(data_sem_sampled_sessions,get_data_sem_sampled_sessions(data_id_sampled_sessions, data_id, data_proxy$data_proxy, data_parent_share, data_q_by_female, cols="co10")),
  
  tar_target(linear_models_oms_proxy,get_linear_models_oms_proxy(data_proxy$data_proxy_longer)),
  
  tar_target(predictions_oms_proxy,compute_predictions_oms_proxy(linear_models_oms_proxy,data_proxy$data_proxy_longer)),
  
  # tar_target(brms_pooled_data,get_brms_pooled_data(data_sem_sampled_sessions)),
  
  ## Piecewise general ----
  ## sr_all
  ## sr_all and combi1 c("r_nb_flo_open","r_height_mean")
  tar_target(piecewise_low_fem_combi1,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                    target_sr = "r_sr_all", target_traits = c("r_nb_flo_open","r_height_mean"),
                                                    x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_medium_fem_combi1,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                            target_sr = "r_sr_all", target_traits = c("r_nb_flo_open","r_height_mean"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_high_fem_combi1,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                            target_sr = "r_sr_all", target_traits = c("r_nb_flo_open","r_height_mean"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_low_mal_combi1,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                            target_sr = "r_sr_all", target_traits = c("r_nb_flo_open","r_height_mean"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_medium_mal_combi1,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                               target_sr = "r_sr_all", target_traits = c("r_nb_flo_open","r_height_mean"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_high_mal_combi1,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                             target_sr = "r_sr_all", target_traits = c("r_nb_flo_open","r_height_mean"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  ## sr_all and combi2 c("r_nb_flo_open","r_height_max")
  tar_target(piecewise_low_fem_combi2,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                            target_sr = "r_sr_all", target_traits = c("r_nb_flo_open","r_height_max"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_medium_fem_combi2,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                               target_sr = "r_sr_all", target_traits = c("r_nb_flo_open","r_height_max"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_high_fem_combi2,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                             target_sr = "r_sr_all", target_traits = c("r_nb_flo_open","r_height_max"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_low_mal_combi2,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                            target_sr = "r_sr_all", target_traits = c("r_nb_flo_open","r_height_max"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_medium_mal_combi2,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                               target_sr = "r_sr_all", target_traits = c("r_nb_flo_open","r_height_max"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_high_mal_combi2,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                             target_sr = "r_sr_all", target_traits = c("r_nb_flo_open","r_height_max"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  ## sr_all and combi3 c("r_nb_flo_all","r_height_mean")
  tar_target(piecewise_low_fem_combi3,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                            target_sr = "r_sr_all", target_traits = c("r_nb_flo_all","r_height_mean"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_medium_fem_combi3,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                               target_sr = "r_sr_all", target_traits = c("r_nb_flo_all","r_height_mean"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_high_fem_combi3,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                             target_sr = "r_sr_all", target_traits = c("r_nb_flo_all","r_height_mean"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_low_mal_combi3,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                            target_sr = "r_sr_all", target_traits = c("r_nb_flo_all","r_height_mean"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_medium_mal_combi3,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                               target_sr = "r_sr_all", target_traits = c("r_nb_flo_all","r_height_mean"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_high_mal_combi3,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                             target_sr = "r_sr_all", target_traits = c("r_nb_flo_all","r_height_mean"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  ## sr_all and combi4 c("r_nb_flo_all","r_height_max")
  tar_target(piecewise_low_fem_combi4,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                            target_sr = "r_sr_all", target_traits = c("r_nb_flo_all","r_height_max"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_medium_fem_combi4,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                               target_sr = "r_sr_all", target_traits = c("r_nb_flo_all","r_height_max"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_high_fem_combi4,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                             target_sr = "r_sr_all", target_traits = c("r_nb_flo_all","r_height_max"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_low_mal_combi4,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                            target_sr = "r_sr_all", target_traits = c("r_nb_flo_all","r_height_max"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_medium_mal_combi4,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                               target_sr = "r_sr_all", target_traits = c("r_nb_flo_all","r_height_max"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_high_mal_combi4,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                             target_sr = "r_sr_all", target_traits = c("r_nb_flo_all","r_height_max"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  ## sr_all and combi5 c("r_nb_stem","r_height_mean")
  tar_target(piecewise_low_fem_combi5,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                            target_sr = "r_sr_all", target_traits = c("r_nb_stem","r_height_mean"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_medium_fem_combi5,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                               target_sr = "r_sr_all", target_traits = c("r_nb_stem","r_height_mean"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_high_fem_combi5,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                             target_sr = "r_sr_all", target_traits = c("r_nb_stem","r_height_mean"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_low_mal_combi5,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                            target_sr = "r_sr_all", target_traits = c("r_nb_stem","r_height_mean"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_medium_mal_combi5,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                               target_sr = "r_sr_all", target_traits = c("r_nb_stem","r_height_mean"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_high_mal_combi5,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                             target_sr = "r_sr_all", target_traits = c("r_nb_stem","r_height_mean"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  ## sr_all and combi6 c("r_nb_stem","r_height_max")
  tar_target(piecewise_low_fem_combi6,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                            target_sr = "r_sr_all", target_traits = c("r_nb_stem","r_height_max"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_medium_fem_combi6,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                               target_sr = "r_sr_all", target_traits = c("r_nb_stem","r_height_max"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_high_fem_combi6,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                             target_sr = "r_sr_all", target_traits = c("r_nb_stem","r_height_max"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_low_mal_combi6,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                            target_sr = "r_sr_all", target_traits = c("r_nb_stem","r_height_max"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_medium_mal_combi6,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                               target_sr = "r_sr_all", target_traits = c("r_nb_stem","r_height_max"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_high_mal_combi6,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                             target_sr = "r_sr_all", target_traits = c("r_nb_stem","r_height_max"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  ## sr (outcross)
  ## sr and combi1 c("r_nb_flo_open","r_height_mean")
  tar_target(piecewise_sr_low_fem_combi1,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                            target_sr = "r_sr", target_traits = c("r_nb_flo_open","r_height_mean"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_medium_fem_combi1,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                               target_sr = "r_sr", target_traits = c("r_nb_flo_open","r_height_mean"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_high_fem_combi1,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                             target_sr = "r_sr", target_traits = c("r_nb_flo_open","r_height_mean"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_low_mal_combi1,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                            target_sr = "r_sr", target_traits = c("r_nb_flo_open","r_height_mean"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_medium_mal_combi1,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                               target_sr = "r_sr", target_traits = c("r_nb_flo_open","r_height_mean"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_high_mal_combi1,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                             target_sr = "r_sr", target_traits = c("r_nb_flo_open","r_height_mean"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  ## sr and combi2 c("r_nb_flo_open","r_height_max")
  tar_target(piecewise_sr_low_fem_combi2,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                            target_sr = "r_sr", target_traits = c("r_nb_flo_open","r_height_max"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_medium_fem_combi2,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                               target_sr = "r_sr", target_traits = c("r_nb_flo_open","r_height_max"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_high_fem_combi2,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                             target_sr = "r_sr", target_traits = c("r_nb_flo_open","r_height_max"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_low_mal_combi2,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                            target_sr = "r_sr", target_traits = c("r_nb_flo_open","r_height_max"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_medium_mal_combi2,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                               target_sr = "r_sr", target_traits = c("r_nb_flo_open","r_height_max"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_high_mal_combi2,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                             target_sr = "r_sr", target_traits = c("r_nb_flo_open","r_height_max"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  ## sr and combi3 c("r_nb_flo_all","r_height_mean")
  tar_target(piecewise_sr_low_fem_combi3,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                            target_sr = "r_sr", target_traits = c("r_nb_flo_all","r_height_mean"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_medium_fem_combi3,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                               target_sr = "r_sr", target_traits = c("r_nb_flo_all","r_height_mean"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_high_fem_combi3,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                             target_sr = "r_sr", target_traits = c("r_nb_flo_all","r_height_mean"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_low_mal_combi3,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                            target_sr = "r_sr", target_traits = c("r_nb_flo_all","r_height_mean"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_medium_mal_combi3,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                               target_sr = "r_sr", target_traits = c("r_nb_flo_all","r_height_mean"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_high_mal_combi3,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                             target_sr = "r_sr", target_traits = c("r_nb_flo_all","r_height_mean"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  ## sr and combi4 c("r_nb_flo_all","r_height_max")
  tar_target(piecewise_sr_low_fem_combi4,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                            target_sr = "r_sr", target_traits = c("r_nb_flo_all","r_height_max"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_medium_fem_combi4,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                               target_sr = "r_sr", target_traits = c("r_nb_flo_all","r_height_max"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_high_fem_combi4,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                             target_sr = "r_sr", target_traits = c("r_nb_flo_all","r_height_max"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_low_mal_combi4,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                            target_sr = "r_sr", target_traits = c("r_nb_flo_all","r_height_max"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_medium_mal_combi4,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                               target_sr = "r_sr", target_traits = c("r_nb_flo_all","r_height_max"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_high_mal_combi4,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                             target_sr = "r_sr", target_traits = c("r_nb_flo_all","r_height_max"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  ## sr and combi5 c("r_nb_stem","r_height_mean")
  tar_target(piecewise_sr_low_fem_combi5,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                            target_sr = "r_sr", target_traits = c("r_nb_stem","r_height_mean"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_medium_fem_combi5,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                               target_sr = "r_sr", target_traits = c("r_nb_stem","r_height_mean"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_high_fem_combi5,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                             target_sr = "r_sr", target_traits = c("r_nb_stem","r_height_mean"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_low_mal_combi5,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                            target_sr = "r_sr", target_traits = c("r_nb_stem","r_height_mean"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_medium_mal_combi5,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                               target_sr = "r_sr", target_traits = c("r_nb_stem","r_height_mean"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_high_mal_combi5,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                             target_sr = "r_sr", target_traits = c("r_nb_stem","r_height_mean"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  ## sr and combi6 c("r_nb_stem","r_height_max")
  tar_target(piecewise_sr_low_fem_combi6,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                            target_sr = "r_sr", target_traits = c("r_nb_stem","r_height_max"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_medium_fem_combi6,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                               target_sr = "r_sr", target_traits = c("r_nb_stem","r_height_max"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_high_fem_combi6,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                             target_sr = "r_sr", target_traits = c("r_nb_stem","r_height_max"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_low_mal_combi6,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                            target_sr = "r_sr", target_traits = c("r_nb_stem","r_height_max"),
                                                            x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_medium_mal_combi6,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                               target_sr = "r_sr", target_traits = c("r_nb_stem","r_height_max"),
                                                               x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  tar_target(piecewise_sr_high_mal_combi6,get_piecewise_general(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                             target_sr = "r_sr", target_traits = c("r_nb_stem","r_height_max"),
                                                             x_coord = c(2,2,1,3), y_coord = c(2,3,1,1))),
  
  ## Piecewise males ----
  
  ## sr_all and combi1 c("r_nb_flo_open","r_height_mean")
  tar_target(piecewisemales_low_combi1,get_piecewise_males(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                            target_sr = "r_sr_all", target_ps = "r_mean_ps",
                                                            target_traits = c("r_nb_flo_open","r_height_mean"),
                                                            x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  tar_target(piecewisemales_medium_combi1,get_piecewise_males(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                           target_sr = "r_sr_all", target_ps = "r_mean_ps",
                                                           target_traits = c("r_nb_flo_open","r_height_mean"),
                                                           x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  tar_target(piecewisemales_high_combi1,get_piecewise_males(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                           target_sr = "r_sr_all", target_ps = "r_mean_ps",
                                                           target_traits = c("r_nb_flo_open","r_height_mean"),
                                                           x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  ## sr_all and combi3 c("r_nb_flo_all","r_height_mean")
  tar_target(piecewisemales_low_combi3,get_piecewise_males(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                           target_sr = "r_sr_all", target_ps = "r_mean_ps",
                                                           target_traits = c("r_nb_flo_all","r_height_mean"),
                                                           x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  tar_target(piecewisemales_medium_combi3,get_piecewise_males(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                              target_sr = "r_sr_all", target_ps = "r_mean_ps",
                                                              target_traits = c("r_nb_flo_all","r_height_mean"),
                                                              x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  tar_target(piecewisemales_high_combi3,get_piecewise_males(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                            target_sr = "r_sr_all", target_ps = "r_mean_ps",
                                                            target_traits = c("r_nb_flo_all","r_height_mean"),
                                                            x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  ## sr_all and combi7 c("r_nb_flo","r_height_mean")
  tar_target(piecewisemales_low_combi7,get_piecewise_males(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                           target_sr = "r_sr_all", target_ps = "r_mean_ps",
                                                           target_traits = c("r_nb_flo","r_height_mean"),
                                                           x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  tar_target(piecewisemales_medium_combi7,get_piecewise_males(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                              target_sr = "r_sr_all", target_ps = "r_mean_ps",
                                                              target_traits = c("r_nb_flo","r_height_mean"),
                                                              x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  tar_target(piecewisemales_high_combi7,get_piecewise_males(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                            target_sr = "r_sr_all", target_ps = "r_mean_ps",
                                                            target_traits = c("r_nb_flo","r_height_mean"),
                                                            x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  ## sr and combi1 c("r_nb_flo_open","r_height_mean")
  tar_target(piecewisemales_sr_low_combi1,get_piecewise_males(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                           target_sr = "r_sr", target_ps = "r_mean_ps",
                                                           target_traits = c("r_nb_flo_open","r_height_mean"),
                                                           x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  tar_target(piecewisemales_sr_medium_combi1,get_piecewise_males(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                              target_sr = "r_sr", target_ps = "r_mean_ps",
                                                              target_traits = c("r_nb_flo_open","r_height_mean"),
                                                              x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  tar_target(piecewisemales_sr_high_combi1,get_piecewise_males(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                            target_sr = "r_sr", target_ps = "r_mean_ps",
                                                            target_traits = c("r_nb_flo_open","r_height_mean"),
                                                            x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  ## Piecewise females ----
  
  ## sr_all and combi1 c("r_nb_flo_open","r_height_mean")
  tar_target(piecewisefemales_low_combi1,get_piecewise_females(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                           target_sr = "r_sr_all", target_ps = "r_ratio_q",
                                                           target_traits = c("r_nb_flo_open","r_height_mean"),
                                                           x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  tar_target(piecewisefemales_medium_combi1,get_piecewise_females(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                              target_sr = "r_sr_all", target_ps = "r_ratio_q",
                                                              target_traits = c("r_nb_flo_open","r_height_mean"),
                                                              x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  tar_target(piecewisefemales_high_combi1,get_piecewise_females(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                            target_sr = "r_sr_all", target_ps = "r_ratio_q",
                                                            target_traits = c("r_nb_flo_open","r_height_mean"),
                                                            x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  ## sr_all and combi3 c("r_nb_flo_all","r_height_mean")
  tar_target(piecewisefemales_low_combi3,get_piecewise_females(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                           target_sr = "r_sr_all", target_ps = "r_ratio_q",
                                                           target_traits = c("r_nb_flo_all","r_height_mean"),
                                                           x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  tar_target(piecewisefemales_medium_combi3,get_piecewise_females(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                              target_sr = "r_sr_all", target_ps = "r_ratio_q",
                                                              target_traits = c("r_nb_flo_all","r_height_mean"),
                                                              x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  tar_target(piecewisefemales_high_combi3,get_piecewise_females(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                            target_sr = "r_sr_all", target_ps = "r_ratio_q",
                                                            target_traits = c("r_nb_flo_all","r_height_mean"),
                                                            x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  ## sr_all and combi7 c("r_nb_flo","r_height_mean")
  tar_target(piecewisefemales_low_combi7,get_piecewise_females(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                               target_sr = "r_sr_all", target_ps = "r_ratio_q",
                                                               target_traits = c("r_nb_flo","r_height_mean"),
                                                               x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  tar_target(piecewisefemales_medium_combi7,get_piecewise_females(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                                  target_sr = "r_sr_all", target_ps = "r_ratio_q",
                                                                  target_traits = c("r_nb_flo","r_height_mean"),
                                                                  x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  tar_target(piecewisefemales_high_combi7,get_piecewise_females(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                                target_sr = "r_sr_all", target_ps = "r_ratio_q",
                                                                target_traits = c("r_nb_flo","r_height_mean"),
                                                                x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  ## sr and combi1 c("r_nb_flo_open","r_height_mean")
  tar_target(piecewisefemales_sr_low_combi1,get_piecewise_females(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                               target_sr = "r_sr", target_ps = "r_ratio_q",
                                                               target_traits = c("r_nb_flo_open","r_height_mean"),
                                                               x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  tar_target(piecewisefemales_sr_medium_combi1,get_piecewise_females(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                                  target_sr = "r_sr", target_ps = "r_ratio_q",
                                                                  target_traits = c("r_nb_flo_open","r_height_mean"),
                                                                  x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  tar_target(piecewisefemales_sr_high_combi1,get_piecewise_females(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                                target_sr = "r_sr", target_ps = "r_ratio_q",
                                                                target_traits = c("r_nb_flo_open","r_height_mean"),
                                                                x_coord = c(1.5,2,2.5,1,3), y_coord = c(2,4,2,1,1))),
  
  ## Piecewise males complete with visits ----
  
  # tar_target(piecewisemales_low_combi1_visits,get_piecewise_males_visits(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
  #                                                                 target_sr = "r_sr_all", target_ps = "r_mean_ps",
  #                                                                 target_traits = c("r_nb_flo_open","r_height_mean"),
  #                                                                 x_coord = c(3,1,2,1.5,2,2.5,1,3), y_coord = c(2,2,2,3,4,3,1,1))),
  
  
  ## Linear models for visits ----
  
  tar_target(analyses_visits,get_analyses_visits(data_id)),
  
  ## Piecewise for visits ----
  
  tar_target(piecewise_visits_male_low,get_piecewise_visits(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                       target_traits = c("r_nb_flo_open","r_height_mean"))),
  
  tar_target(piecewise_visits_male_medium,get_piecewise_visits(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                            target_traits = c("r_nb_flo_open","r_height_mean"))),
  
  tar_target(piecewise_visits_male_high,get_piecewise_visits(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                            target_traits = c("r_nb_flo_open","r_height_mean"))),
  
  tar_target(piecewise_visits_female_low,get_piecewise_visits(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                            target_traits = c("r_nb_flo_open","r_height_mean"))),
  
  tar_target(piecewise_visits_female_medium,get_piecewise_visits(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                               target_traits = c("r_nb_flo_open","r_height_mean"))),
  
  tar_target(piecewise_visits_female_high,get_piecewise_visits(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                             target_traits = c("r_nb_flo_open","r_height_mean"))),
  
  
  ## Piecewise for flower scale ----
  
  tar_target(piecewise_flower_low,get_piecewise_flower(data_flower, target_ttt = "1_low",
                                                            x_coord = c(3,2,2,1), y_coord = c(2,1.5,2.5,2))),
  
  tar_target(piecewise_flower_medium,get_piecewise_flower(data_flower, target_ttt = "2_medium",
                                                       x_coord = c(3,2,2,1), y_coord = c(2,1.5,2.5,2))),
  
  tar_target(piecewise_flower_high,get_piecewise_flower(data_flower, target_ttt = "3_high",
                                                       x_coord = c(3,2,2,1), y_coord = c(2,1.5,2.5,2))),
  
  
  ## Quarto ----
  
  tarchetypes::tar_quarto(index, "index.qmd")
  
)



