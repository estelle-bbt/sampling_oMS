#' Targets plan
#' 

## Attach required packages ----

library(targets)
library(tarchetypes)
library(ggplot2)

tar_option_set(
  packages = c("dplyr","tidyr","ggplot2","piecewiseSEM")  # load dplyr in each environement
)

tar_source()

## Load Project R Functions ----

source(here::here("R", "functions.R"))

## Analyses pipeline ----

list(
  
  ## Manage data ----
  
  # TO DO : clean these targets
  
  tar_target(data_resume_visits,get_resume_visits("data/obs_ABPOLL.txt")),
  
  tar_target(data_id,load_data_id("data/data_ABPOLL_ID_resume.txt", data_resume_visits, cols = c("co5", "co10", "co20"))),
  
  tar_target(data_flower,load_data_flower("data/data_ABPOLL_flower_resume.txt")),
  
  # tar_target(oms_flower,get_ttt_effect_flower(data_flower,"import_nb_part_ID_out_co10")),
  # 
  # tar_target(visit_flower,get_ttt_effect_flower(data_flower,"nb_visit")),
  # 
  # tar_target(pl_flower,get_ttt_effect_flower(data_flower,"pl")),
  # 
  # tar_target(seeds_flower,get_ttt_effect_flower(data_flower,"nb_seeds")),
  
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
  
  tar_target(data_previous_study,load_data("data/all_data_long_NA_0AllFemFALSE_raw.txt")),
  
  tar_target(data_from_genotypes,get_data_from_genotypes(data_genotypes, data_id, "data/data_ABPOLL_ID_level_detID.txt")),
  
  tar_target(data_sem_sampled_sessions,get_data_sem_sampled_sessions(data_id, data_previous_study, data_from_genotypes, cols="co10")),
  
  tar_target(linear_models_oms_proxy,get_linear_models_oms_proxy(data_proxy$data_proxy_longer)),
  
  tar_target(predictions_oms_proxy,compute_predictions_oms_proxy(linear_models_oms_proxy,data_proxy$data_proxy_longer)),
  
  ## Effect on the measured variables ----
  
  tar_target(sr_all_mal,get_ttt_effect_id(data_sem_sampled_sessions,sex = "mal",variable = "sr_all")),
  
  tar_target(sr_all_fem,get_ttt_effect_id(data_sem_sampled_sessions,sex = "fem",variable = "sr_all")),
  
  tar_target(sr_out_mal,get_ttt_effect_id(data_sem_sampled_sessions,sex = "mal",variable = "sr_out")),
  
  tar_target(sr_out_fem,get_ttt_effect_id(data_sem_sampled_sessions,sex = "fem",variable = "sr_out")),
  
  tar_target(oms_mal,get_ttt_effect_id(data_sem_sampled_sessions,sex = "mal",variable = "oms")),
  
  tar_target(oms_fem,get_ttt_effect_id(data_sem_sampled_sessions,sex = "fem",variable = "oms")),
  
  tar_target(mean_ps_mal,get_ttt_effect_id(data_sem_sampled_sessions, sex = "mal", variable = "mean_ps")),
  
  tar_target(diff_q_fem,get_ttt_effect_id(data_sem_sampled_sessions, sex = "fem", variable = "diff_q")),
  
  tar_target(contid_id,get_ttt_effect_id(data_sem_sampled_sessions, sex = "id", variable = "contact_id")),
  
  tar_target(meanpos_id,get_ttt_effect_id(data_sem_sampled_sessions, sex = "id", variable = "mean_position", text_size = 18)),
  
  tar_target(visflo_id,get_ttt_effect_id(data_sem_sampled_sessions, sex = "id", variable = "nb_visits_per_flower", text_size = 18)),
  
  tar_target(flo_id,get_ttt_effect_id(data_sem_sampled_sessions, sex = "id", variable = "nb_flower_visited", text_size = 18)),
  
  ## Basic SEM models / Wtot / open flower versus mean height (combi 1) ----
  
  ## Males 
  
  tar_target(piecewise_males_low_combi1_wtot_basic,get_piecewise_males_visits(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                                         target_sr = "W", target_ps = "PS",
                                                                         target_traits = c("F","H"))),
  
  tar_target(piecewise_males_medium_combi1_wtot_basic,get_piecewise_males_visits(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                                            target_sr = "W", target_ps = "PS",
                                                                            target_traits = c("F","H"))),
  
  tar_target(piecewise_males_high_combi1_wtot_basic,get_piecewise_males_visits(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                                          target_sr = "W", target_ps = "PS",
                                                                          target_traits = c("F","H"))),
  
  ## Females
  
  tar_target(piecewise_females_low_combi1_wtot_basic,get_piecewise_females_visits(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                                             target_sr = "W", target_ps = "ME",
                                                                             target_traits = c("F","H"))),
  
  tar_target(piecewise_females_medium_combi1_wtot_basic,get_piecewise_females_visits(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                                                target_sr = "W", target_ps = "ME",
                                                                                target_traits = c("F","H"))),
  
  tar_target(piecewise_females_high_combi1_wtot_basic,get_piecewise_females_visits(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                                              target_sr = "W", target_ps = "ME",
                                                                              target_traits = c("F","H"))),
  
  ## Final SEM models with Wtot and open flower versus mean height (combi 1) ----
  
  ## Males 
  
  tar_target(piecewise_males_low_combi1_wtot_final,get_piecewise_males_visits_low(data_sem_sampled_sessions, target_sr = "W")),
  
  tar_target(piecewise_males_medium_combi1_wtot_final,get_piecewise_males_visits_medium(data_sem_sampled_sessions, target_sr = "W")),
  
  tar_target(piecewise_males_high_combi1_wtot_final,get_piecewise_males_visits_high(data_sem_sampled_sessions, target_sr = "W")),
             
  ## Females
  
  tar_target(piecewise_females_low_combi1_wtot_final,get_piecewise_females_visits_low(data_sem_sampled_sessions, target_sr = "W")),
  
  tar_target(piecewise_females_medium_combi1_wtot_final,get_piecewise_females_visits_medium(data_sem_sampled_sessions, target_sr = "W")),
  
  tar_target(piecewise_females_high_combi1_wtot_final,get_piecewise_females_visits_high(data_sem_sampled_sessions, target_sr = "W")),
  
  
  ## Z-tests for SEM comparison ----
  
  tar_target(z_tests,get_z_tests(piecewise_males_low_combi1_wtot_final,piecewise_males_medium_combi1_wtot_final,piecewise_males_high_combi1_wtot_final,
                                 piecewise_females_low_combi1_wtot_final,piecewise_females_medium_combi1_wtot_final,piecewise_females_high_combi1_wtot_final)),
  
  ## Check result robustness with Wout ----
  
  ## Males 
  
  tar_target(piecewise_males_low_combi1_wout_final,get_piecewise_males_visits_low(data_sem_sampled_sessions, target_sr = "Wout")),
  
  tar_target(piecewise_males_medium_combi1_wout_final,get_piecewise_males_visits_medium(data_sem_sampled_sessions, target_sr = "Wout")),
  
  tar_target(piecewise_males_high_combi1_wout_final,get_piecewise_males_visits_high(data_sem_sampled_sessions, target_sr = "Wout")),
  
  ## Females
  
  tar_target(piecewise_females_low_combi1_wout_final,get_piecewise_females_visits_low(data_sem_sampled_sessions, target_sr = "Wout")),
  
  tar_target(piecewise_females_medium_combi1_wout_final,get_piecewise_females_visits_medium(data_sem_sampled_sessions, target_sr = "Wout")),
  
  tar_target(piecewise_females_high_combi1_wout_final,get_piecewise_females_visits_high(data_sem_sampled_sessions, target_sr = "Wout")),
  
  ## Check result robustness with complete visit proxies ----
  
  ## Males 
  
  tar_target(piecewise_males_complete_low_combi1_wtot_final,get_piecewise_males_visits_complete_low(data_sem_sampled_sessions, target_sr = "W")),
  
  tar_target(piecewise_males_complete_medium_combi1_wtot_final,get_piecewise_males_visits_complete_medium(data_sem_sampled_sessions, target_sr = "W")),
  
  tar_target(piecewise_males_complete_high_combi1_wtot_final,get_piecewise_males_visits_complete_high(data_sem_sampled_sessions, target_sr = "W")),
  
  ## Females
  
  tar_target(piecewise_females_complete_low_combi1_wtot_final,get_piecewise_females_visits_complete_low(data_sem_sampled_sessions, target_sr = "W")),
  
  tar_target(piecewise_females_complete_medium_combi1_wtot_final,get_piecewise_females_visits_complete_medium(data_sem_sampled_sessions, target_sr = "W")),
  
  tar_target(piecewise_females_complete_high_combi1_wtot_final,get_piecewise_females_visits_complete_high(data_sem_sampled_sessions, target_sr = "W")),
  
  
  ## Check result robustness with the different combination of traits ----
  
  ## Males 
  
  # tar_target(piecewise_males_low_combi1_wtot_final,get_piecewise_males_visits_low(data_sem_sampled_sessions, target_sr = "W")),
  tar_target(piecewise_males_low_combi2_wtot_final,get_piecewise_males_visits_low(data_sem_sampled_sessions, target_sr = "W", target_trait = c("F","r_height_max"))),
  tar_target(piecewise_males_low_combi3_wtot_final,get_piecewise_males_visits_low(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_flo_all","H"))), 
  tar_target(piecewise_males_low_combi4_wtot_final,get_piecewise_males_visits_low(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_flo_all","r_height_max"))),
  tar_target(piecewise_males_low_combi5_wtot_final,get_piecewise_males_visits_low(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_stem","H"))), 
  tar_target(piecewise_males_low_combi6_wtot_final,get_piecewise_males_visits_low(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_stem","r_height_max"))), 
  
  # tar_target(piecewise_males_medium_combi1_wtot_final,get_piecewise_males_visits_medium(data_sem_sampled_sessions, target_sr = "W")),
  tar_target(piecewise_males_medium_combi2_wtot_final,get_piecewise_males_visits_medium(data_sem_sampled_sessions, target_sr = "W", target_trait = c("F","r_height_max"))),
  tar_target(piecewise_males_medium_combi3_wtot_final,get_piecewise_males_visits_medium(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_flo_all","H"))), 
  tar_target(piecewise_males_medium_combi4_wtot_final,get_piecewise_males_visits_medium(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_flo_all","r_height_max"))),
  tar_target(piecewise_males_medium_combi5_wtot_final,get_piecewise_males_visits_medium(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_stem","H"))), 
  tar_target(piecewise_males_medium_combi6_wtot_final,get_piecewise_males_visits_medium(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_stem","r_height_max"))), 
  
  
  # tar_target(piecewise_males_high_combi1_wtot_final,get_piecewise_males_visits_high(data_sem_sampled_sessions, target_sr = "W")),
  tar_target(piecewise_males_high_combi2_wtot_final,get_piecewise_males_visits_high(data_sem_sampled_sessions, target_sr = "W", target_trait = c("F","r_height_max"))),
  tar_target(piecewise_males_high_combi3_wtot_final,get_piecewise_males_visits_high(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_flo_all","H"))), 
  tar_target(piecewise_males_high_combi4_wtot_final,get_piecewise_males_visits_high(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_flo_all","r_height_max"))),
  tar_target(piecewise_males_high_combi5_wtot_final,get_piecewise_males_visits_high(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_stem","H"))), 
  tar_target(piecewise_males_high_combi6_wtot_final,get_piecewise_males_visits_high(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_stem","r_height_max"))), 
  
  
  ## Females
  
  # tar_target(piecewise_females_low_combi1_wtot_final,get_piecewise_females_visits_low(data_sem_sampled_sessions, target_sr = "W")),
  tar_target(piecewise_females_low_combi2_wtot_final,get_piecewise_females_visits_low(data_sem_sampled_sessions, target_sr = "W", target_trait = c("F","r_height_max"))),
  tar_target(piecewise_females_low_combi3_wtot_final,get_piecewise_females_visits_low(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_flo_all","H"))), 
  tar_target(piecewise_females_low_combi4_wtot_final,get_piecewise_females_visits_low(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_flo_all","r_height_max"))),
  tar_target(piecewise_females_low_combi5_wtot_final,get_piecewise_females_visits_low(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_stem","H"))), 
  tar_target(piecewise_females_low_combi6_wtot_final,get_piecewise_females_visits_low(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_stem","r_height_max"))), 
  
  # tar_target(piecewise_females_medium_combi1_wtot_final,get_piecewise_females_visits_medium(data_sem_sampled_sessions, target_sr = "W")),
  tar_target(piecewise_females_medium_combi2_wtot_final,get_piecewise_females_visits_medium(data_sem_sampled_sessions, target_sr = "W", target_trait = c("F","r_height_max"))),
  tar_target(piecewise_females_medium_combi3_wtot_final,get_piecewise_females_visits_medium(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_flo_all","H"))), 
  tar_target(piecewise_females_medium_combi4_wtot_final,get_piecewise_females_visits_medium(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_flo_all","r_height_max"))),
  tar_target(piecewise_females_medium_combi5_wtot_final,get_piecewise_females_visits_medium(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_stem","H"))), 
  tar_target(piecewise_females_medium_combi6_wtot_final,get_piecewise_females_visits_medium(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_stem","r_height_max"))), 
  
  
  # tar_target(piecewise_females_high_combi1_wtot_final,get_piecewise_females_visits_high(data_sem_sampled_sessions, target_sr = "W")),
  tar_target(piecewise_females_high_combi2_wtot_final,get_piecewise_females_visits_high(data_sem_sampled_sessions, target_sr = "W", target_trait = c("F","r_height_max"))),
  tar_target(piecewise_females_high_combi3_wtot_final,get_piecewise_females_visits_high(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_flo_all","H"))), 
  tar_target(piecewise_females_high_combi4_wtot_final,get_piecewise_females_visits_high(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_flo_all","r_height_max"))),
  tar_target(piecewise_females_high_combi5_wtot_final,get_piecewise_females_visits_high(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_stem","H"))), 
  tar_target(piecewise_females_high_combi6_wtot_final,get_piecewise_females_visits_high(data_sem_sampled_sessions, target_sr = "W", target_trait = c("r_nb_stem","r_height_max"))), 
  
  
  ## Quarto ----
  
  tarchetypes::tar_quarto(index, "index.qmd", quiet = FALSE)
  
)
