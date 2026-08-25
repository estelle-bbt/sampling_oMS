## Treatment effect on the measured variables ----

targets::tar_load(sr_all_mal)
sr_all_mal$plot
targets::tar_load(sr_out_mal)
sr_out_mal$plot
targets::tar_load(oms_mal)
oms_mal$plot
targets::tar_load(mean_ps_mal)
mean_ps_mal$plot
targets::tar_load(diff_q_fem)
diff_q_fem$plot
targets::tar_load(contid_id)
contid_id$plot
targets::tar_load(meanpos_id)
meanpos_id$plot
targets::tar_load(visflo_id)
visflo_id$plot
targets::tar_load(flo_id)
flo_id$plot

# Results -----------------------------------------------------------------

### Males ----
targets::tar_load(piecewise_males_low_combi1_wtot_final)
targets::tar_load(piecewise_males_medium_combi1_wtot_final)
targets::tar_load(piecewise_males_high_combi1_wtot_final)

### Females ----
targets::tar_load(piecewise_females_low_combi1_wtot_final)
targets::tar_load(piecewise_females_medium_combi1_wtot_final)
targets::tar_load(piecewise_females_high_combi1_wtot_final)

## Result robustness using the different combination of floral traits ----

### Males ----

targets::tar_read(piecewise_males_low_combi1_wtot_final)$coefs
targets::tar_read(piecewise_males_low_combi2_wtot_final)$coefs
targets::tar_read(piecewise_males_low_combi3_wtot_final)$coefs
targets::tar_read(piecewise_males_low_combi4_wtot_final)$coefs
targets::tar_read(piecewise_males_low_combi5_wtot_final)$coefs
targets::tar_read(piecewise_males_low_combi6_wtot_final)$coefs

targets::tar_read(piecewise_males_medium_combi1_wtot_final)$coefs
targets::tar_read(piecewise_males_medium_combi2_wtot_final)$coefs
targets::tar_read(piecewise_males_medium_combi3_wtot_final)$coefs
targets::tar_read(piecewise_males_medium_combi4_wtot_final)$coefs
targets::tar_read(piecewise_males_medium_combi5_wtot_final)$coefs
targets::tar_read(piecewise_males_medium_combi6_wtot_final)$coefs

targets::tar_read(piecewise_males_high_combi1_wtot_final)$coefs
targets::tar_read(piecewise_males_high_combi2_wtot_final)$coefs
targets::tar_read(piecewise_males_high_combi3_wtot_final)$coefs
targets::tar_read(piecewise_males_high_combi4_wtot_final)$coefs
targets::tar_read(piecewise_males_high_combi5_wtot_final)$coefs
targets::tar_read(piecewise_males_high_combi6_wtot_final)$coefs

### Females ----

targets::tar_read(piecewise_females_low_combi1_wtot_final)$coefs
targets::tar_read(piecewise_females_low_combi2_wtot_final)$coefs
targets::tar_read(piecewise_females_low_combi3_wtot_final)$coefs
targets::tar_read(piecewise_females_low_combi4_wtot_final)$coefs
targets::tar_read(piecewise_females_low_combi5_wtot_final)$coefs
targets::tar_read(piecewise_females_low_combi6_wtot_final)$coefs

targets::tar_read(piecewise_females_medium_combi1_wtot_final)$coefs
targets::tar_read(piecewise_females_medium_combi2_wtot_final)$coefs
targets::tar_read(piecewise_females_medium_combi3_wtot_final)$coefs
targets::tar_read(piecewise_females_medium_combi4_wtot_final)$coefs
targets::tar_read(piecewise_females_medium_combi5_wtot_final)$coefs
targets::tar_read(piecewise_females_medium_combi6_wtot_final)$coefs

targets::tar_read(piecewise_females_high_combi1_wtot_final)$coefs
targets::tar_read(piecewise_females_high_combi2_wtot_final)$coefs
targets::tar_read(piecewise_females_high_combi3_wtot_final)$coefs
targets::tar_read(piecewise_females_high_combi4_wtot_final)$coefs
targets::tar_read(piecewise_females_high_combi5_wtot_final)$coefs
targets::tar_read(piecewise_females_high_combi6_wtot_final)$coefs

## Result robustness using complete visit proxies ----

### Males ----

targets::tar_read(piecewise_males_complete_low_combi1_wtot_final)$coefs
targets::tar_read(piecewise_males_complete_medium_combi1_wtot_final)$coefs
targets::tar_read(piecewise_males_complete_high_combi1_wtot_final)$coefs |> select(-clean_est)

### Females ----

targets::tar_read(piecewise_females_complete_low_combi1_wtot_final)$coefs
targets::tar_read(piecewise_females_complete_medium_combi1_wtot_final)$coefs
targets::tar_read(piecewise_females_complete_high_combi1_wtot_final)$coefs

## To do : add Z-tests and everything in the supplementary information files ----
