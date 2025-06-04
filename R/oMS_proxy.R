#' Read data about plant ids
#'
#' @description 
#' This function reads the data about plant ids and format the table.
#'
#' @param file a character of length 1. The path to the .txt file.
#'
#' @return A `table` containing data. 
#' 
#' @import dplyr
#' 
#' @export

load_data_id <- function(file_path, cols = c("co5", "co10", "co20")){
  # data_id <- read.table(file_path,head=T) |>
  #   mutate(poll_treat_factor=as.factor(case_when(poll_treat==1~"low",
  #                                                poll_treat==2~"medium",
  #                                                TRUE~"high"))) |>
  #   mutate(poll_treat_factor=forcats::fct_relevel(poll_treat_factor, c("low","medium","high"))) |>
  #   mutate(prop_self=sr_self/(sr_self+sr_fem_out+sr_mal_out),
  #          gam_ov_proxy=nb_flo*nbOv_mean,
  #          sr_out=sr_fem_out+sr_mal_out) |>
  #   mutate(mean_nb_visit_per_flower=nb_visit/nb_dist_vis)
  
  data_id <- read.table(file_path,head=T) |>
    rename(id = ID_full) |>
    select(session, id, nbGr_SR_sum,
           nb_flo, nb_flo_open, nb_flo_all, height_max, height_mean, nb_stem, nb_poll_focal,
           !!!syms(paste0("import_nb_part_ID_out_", cols)),
           !!!syms(paste0("export_nb_part_ID_out_", cols))) |>
    rename(sr_fem_total=nbGr_SR_sum) |>
    rename_with(~ gsub("import_nb_part_ID_out_", "oms_fem_", .x), starts_with("import_")) |>
    rename_with(~ gsub("export_nb_part_ID_out_", "oms_mal_", .x), starts_with("export_"))
  
  return(data_id)
}

#' Read data about pollinator visit observations
#'
#' @description 
#' This function reads the data about visits and format the table.
#'
#' @param file a character of length 1. The path to the .txt file.
#'
#' @return A `table` containing data. 
#'
#' @import dplyr
#' 
#' @export

load_data_obs <- function(file_path){
  data_obs <- read.table(file_path,head=T) |>
  mutate(ttt=as.factor(case_when(grepl("FA",session)~"1_low",
                                 grepl("MO",session)~"2_medium",
                                 TRUE~"3_high")))
return(data_obs)
}

#' Summarize the observational data at the level of contacts with each id.
#'
#' @description 
#' This function summarize the observational data at the id level with each (maybe independant, maybe not) contact
#' Each contact is then characterized by the number of visited flowers as well as its total duration.
#'
#' @param data about pollinator observations
#'
#' @return A `table` containing summarized data. 
#'
#' @import dplyr
#' @import tidyr
#' 
#' @export

get_data_contact <- function(data_obs){
  data_contact <- data_obs |>
    mutate(ID_full=paste0(session,".",ifelse(ID==10,10,paste0("0",ID)))) |>
    group_by(session,people) |>
    mutate(consecutive_contact=consecutive_id(ID_full)) |>
    group_by(session,people,consecutive_contact) |>
    summarise(start=min(time),
              name_ID=unique(ID_full),
              nb_visit=n(),
              duration=sum(duration,na.rm=T)) |> # to calculate flower age (visits needed to be sequentially arrange, independently of the pollinators)
    mutate(id=name_ID) |>
    pivot_wider(values_from=name_ID,names_from=name_ID,names_prefix="newID_",values_fn = list(name_ID = ~ 1), values_fill = list(name_ID = 0)) |>
    mutate(across(starts_with("newID"),~ifelse(duplicated(cumsum(.)),NA,cumsum(.)))) |>
    rowwise() |>
    mutate(no_contact_with_id=sum(across(starts_with("newID")),na.rm=T)) |>
    select(!starts_with("newID")) |>
    relocate(session,people,id) |>
    arrange(session,start)
  # to do : remove people (we don't know the id of the poll, only arrival time) + consecutive contact unless it is order by time only
  # check if no_contact_with_id is independant of the poll
  
  data_contact_id <- data_contact |>
    group_by(session,id) |>
    summarize(nb_contact = max(no_contact_with_id),
              nb_visit = sum(nb_visit),
              dur_visit = sum(duration,na.rm=T)) 
  
  return(list(data_contact = data_contact,
              data_contact_id = data_contact_id))
}

#' Summarize the observational data at the level of the pollinator observation session
#'
#' @description 
#' This function summarize the observational data at the pollinator observation session level.
#'
#' @param data about pollinator observations
#'
#' @return A `table` containing summarized data. 
#'
#' @import dplyr
#' @import tidyr
#' 
#' @export

get_data_session <- function(data_contact){

  data_session <- data_contact$data_contact_id |>
                group_by(session) |>
                summarize(max_oms = n_distinct(id),
                          total_nb_contact = sum(nb_contact),
                          total_nb_visit = sum(nb_visit),
                          total_duration = sum(dur_visit)) |>
    left_join(data_contact$data_contact |>
                group_by(session) |>
                summarize(total_nb_poll = n_distinct(people)))
  
  return(data_session)
}

#' Computing the different proxies for oms
#'
#' @description 
#' Compute the different proxies for oms (see quarto doc)
#'
#' @param data summarized at the id and session level
#'
#' @return A `table` containing the proxies
#'
#' @import dplyr
#' @import tidyr
#' 
#' @export

compute_oms_and_proxy <- function(data_id,data_contact,data_session, cols = c("co5", "co10", "co20")){
  
  data_proxy <- data_id |>
    left_join(data_contact$data_contact_id) |>
    left_join(data_session) |>
    mutate(
      index_A1 = nb_contact,
      index_A2 = nb_contact/total_nb_contact,
      index_A3 = nb_contact*max_oms,
      index_A4 = (nb_contact*max_oms)/total_nb_contact,
      index_A5 = index_A4/total_nb_poll,
      index_B1 = nb_visit,
      index_B2 = nb_visit/total_nb_visit,
      index_B3 = nb_visit*max_oms,
      index_B4 = (nb_visit*max_oms)/total_nb_visit,
      index_B5 = index_B4/total_nb_poll,
      index_C1 = dur_visit,
      index_C2 = dur_visit/total_duration,
      index_C3 = dur_visit*max_oms,
      index_C4 = (dur_visit*max_oms)/total_duration,
      index_C5 = index_C4/total_nb_poll
      )
  
  cols_name <- paste(cols, collapse = "|")
  
  data_proxy_longer <- data_proxy |>
    pivot_longer(
      cols = starts_with("oms"),
      names_to = c("sex", "co"),
      names_pattern = paste0("oms_(fem|mal)_(", cols_name, ")"),
      values_to = "value"
    ) |>
    pivot_wider(
      names_from = "co",
      values_from = "value"
    )  |>
    rename_with(
      ~ paste0("oms_", .),
      .cols = all_of(cols)  # seulement les colonnes de ton vecteur `cols`
    ) |>
    mutate(
      ttt = case_when(substr(session,3,4) == "FA" ~ "low",
                      substr(session,3,4) == "MO" ~ "medium",
                      TRUE ~ "high")
    ) 
    
  
  return(list(data_proxy = data_proxy,
              data_proxy_longer = data_proxy_longer))
}


#' PCA
#'
#' @description 
#' Compute the different proxies for oms (see quarto doc)
#'
#' @param data summarized at the id and session level
#'
#' @return A `table` containing the proxies
#'
#' @import dplyr
#' @import tidyr
#' 
#' @export

get_pca <- function(data_proxy, cols = "co10"){
  
  fem_proxy <- data_proxy |>
    dplyr::select(!!sym(paste0("oms_fem_",cols)),
           index_A1,index_A2,index_A3,index_A4,index_A5,
           index_B1,index_B2,index_B3,index_B4,index_B5,
           index_C1,index_C2,index_C3,index_C4,index_C5)
  
  pca_fem <- FactoMineR::PCA(fem_proxy)
  
  
  mal_proxy <- data_proxy |>
    dplyr::select(!!sym(paste0("oms_mal_",cols)),
           index_A1,index_A2,index_A3,index_A4,index_A5,
           index_B1,index_B2,index_B3,index_B4,index_B5,
           index_C1,index_C2,index_C3,index_C4,index_C5)
  
  pca_mal <- FactoMineR::PCA(mal_proxy)
  
  sex_proxy <- data_proxy |>
    dplyr::select(!!sym(paste0("oms_fem_",cols)), !!sym(paste0("oms_mal_",cols)),
           index_A1,index_A2,index_A3,index_A4,index_A5,
           index_B1,index_B2,index_B3,index_B4,index_B5,
           index_C1,index_C2,index_C3,index_C4,index_C5
           ) |>
    pivot_longer(
      cols = starts_with("oms"),
      names_to = "sex",
      values_to = paste0("oms_",cols),
      names_pattern = "oms_(fem|mal)_.*"
    )
  
  pca_sex <- FactoMineR::PCA(sex_proxy %>% select(-sex))
  
  return(list(pca_fem = plot(pca_fem, choix="var"),
              pca_mal = plot(pca_mal, choix="var"),
              pca_sex = plot(pca_sex, choix="var")))
}

#' Generic function to just read dataset in .txt form
#'
#' @description 
#' This function allow to directly load the .txt dataset previously obtained.
#' Note that the sampling of genotypes, the script is available on dryad. We here 
#' directly provide our sampling dataset in a purpose of reproducible results.
#'
#' @param file a character of length 1. The path to the .txt file.
#'
#' @return A `table` containing data. 
#' 
#' @import dplyr
#' 
#' @export

load_data <- function(file_path){
  
  data <- read.table(file_path,head=T) 
  
  return(data)
}

#' Get true reproductive success on all genotypes
#'
#' @description 
#' For reproductive success, two options: 1- total or 2- outcrossed reproductive success
#' 1- for total reproductive success, for females: total seeds or total genotypes
#' and for males: share on total seeds or counted genotypes
#' 2- for outcrossed reproductive success, for females: share on total seeds or counted genotypes
#' and for males: share on total seeds or counted genotypes
#' NB: for Bateman gradient we stay will consistent with the manuscript and 
#' only explore outcrossed reproductive success, on total seeds for both sexes
#' 
#'
#' @param data sampling dataset
#'
#' @return A `table` containing data. 
#' 
#' @import dplyr
#' 
#' @export

get_true_rs_ms <- function(data_genotypes,data_id){
  
  data_true_rs_ms <- tibble()
  
  data_clean_genotypes <- data_genotypes |>
    mutate(session = substr(known_id, 1, 5)) |>
    # Keep only complete sessions, excluding 4.FA2 due to insufficient seeds
    filter(session %in% c("1.MO2", "3.MO1", "1.MO1", "3.FO2", "4.MO1"))
  
  data_self_fem <- data_clean_genotypes |>
    mutate(self=ifelse(candidate_id==known_id,"T","F")) |>
    group_by(known_id) |>
    summarise(all_genot_fem=n(),
              sr_self_count=sum(self=="T"),
              sr_fem_out_count=sum(self=="F"),
              gms_self=n_distinct(candidate_id[self=="T"]),
              gms_fem_out=n_distinct(candidate_id[self=="F"]),
              gms_fem_total=n_distinct(candidate_id)) |>
    left_join(data_id |>
                select(id,sr_fem_total),by=join_by(known_id==id)) |>
    mutate(sr_self_share_seed=(sr_self_count/all_genot_fem)*sr_fem_total,
           sr_fem_out_share_seed=(sr_fem_out_count/all_genot_fem)*sr_fem_total) |>
    rename(id=known_id) |>
    mutate(session=substr(id,1,5),.before=1)
  
  data_mal <- data_clean_genotypes |>
    mutate(self=ifelse(candidate_id==known_id,"T","F"),
           id_flow=substr(gsub("_run3","",offspring_id),1,nchar(offspring_id)-3)) |>
    group_by(candidate_id) |>
    summarise(sr_mal_total_count=n(),
              sr_mal_out_count=sum(self=="F"),
              gms_mal_out=n_distinct(known_id[self=="F"]),
              gms_mal_total=n_distinct(known_id),
              gmsflo_mal_out=n_distinct(id_flow[self=="F"])
    ) |>
    rename(id=candidate_id) |>
    mutate(session=substr(id,1,5),.before=1) 
  
  data_mal_share <- data_clean_genotypes |>
    mutate(self=ifelse(candidate_id==known_id,"T","F")) |>
    filter(self=="F") |>
    group_by(candidate_id,known_id) |>
    summarise(sr_couple=n()) |>
    left_join(data_self_fem |>
                select(id,sr_fem_total,all_genot_fem),by=join_by(known_id==id)) |>
    mutate(sr_mal_out_share_seed_couple=(sr_couple/all_genot_fem)*sr_fem_total,
           sr_mal_out_share_genot_couple=sr_couple) |>
    group_by(candidate_id) |>
    summarize(sr_mal_out_share_seed=sum(sr_mal_out_share_seed_couple),
              sr_mal_out_share_genot=sum(sr_mal_out_share_genot_couple)) |>
    rename(id=candidate_id) |>
    mutate(session=substr(id,1,5),.before=1) 
  
  sr_all <- data_self_fem |>
    full_join(data_mal) |>
    left_join(data_mal_share) |>
    mutate(
      sr_mal_total_share_seed = rowSums(across(c(sr_mal_out_share_seed, sr_self_share_seed)), na.rm = TRUE)
    ) |>
    arrange(id)
  
  # CONSTRUCT TABLE
  data_true_rs_ms <- data_true_rs_ms |> 
    rbind(sr_all)
  
  return(data_true_rs_ms)
}

#' Estimating the diverse metrics for sampling simulations
#'
#' @description 
#' This function estimate reproductive and mating success for each set of parameters
#' each population and each simulation
#'
#' @param data sampling dataset
#'
#' @return A `table` containing data. 
#' 
#' @import dplyr
#' 
#' @export

get_sampling_rs_ms <- function(data_sampling,data_true_rs_ms,data_id){
  
  data_sampling_rs_ms <- tibble()

  for(sim in unique(data_sampling$no_sim)){
    for(per in unique(data_sampling$percent)){
      for(met_nb in unique(data_sampling$method_number)){
        for(met_frt in unique(data_sampling$method_fruit)){

          target_tab <- data_sampling |>
            filter(no_sim==sim,percent==per,method_number==met_nb,method_fruit==met_frt)

          # CLASSIC PROXIES

          data_self_fem <- target_tab |>
            mutate(self=ifelse(candidate_id==known_id,"T","F")) |>
            group_by(known_id) |>
            summarise(n_genot=n(),
                      sr_self_count=sum(self=="T"),
                      sr_fem_out_count=sum(self=="F"),
                      gms_self=n_distinct(candidate_id[self=="T"]),
                      gms_fem_out=n_distinct(candidate_id[self=="F"]),
                      gms_fem_total=n_distinct(candidate_id)) |>
            left_join(data_true_rs_ms |>
                        select(known_id,all_genot_fem)) |> # total number of genotypes per female
            left_join(data_id |>
                        select(id,sr_fem_total),by=join_by(known_id==id)) |> # total number of seeds per female
            mutate(sr_self_share_seed=(sr_self_count/n_genot)*sr_fem_total,
                   sr_fem_out_share_seed=(sr_fem_out_count/n_genot)*sr_fem_total,
                   sr_self_share_genot=(sr_self_count/n_genot)*all_genot_fem,
                   sr_fem_out_share_genot=(sr_fem_out_count/n_genot)*all_genot_fem) |>
            rename(id=known_id) |>
            mutate(session=substr(id,1,5),.before=1)

          data_mal <- target_tab |>
            mutate(self=ifelse(candidate_id==known_id,"T","F"),
                   id_flow=substr(gsub("_run3","",offspring_id),1,nchar(offspring_id)-3)) |>
            group_by(candidate_id) |>
            summarise(sr_mal_total_count=n(),
                      sr_mal_out_count=sum(self=="F"),
                      gms_mal_out=n_distinct(known_id[self=="F"]),
                      gms_mal_total=n_distinct(known_id),
                      gmsflo_mal_out=n_distinct(id_flow[self=="F"])
                      ) |>
            rename(id=candidate_id) |>
            mutate(session=substr(id,1,5),.before=1) 

          data_mal_share <- target_tab |>
            mutate(self=ifelse(candidate_id==known_id,"T","F")) |>
            filter(self=="F") |>
            group_by(candidate_id,known_id) |>
            summarise(sr_couple=n()) |>
            left_join(data_self_fem |>
                        select(id,sr_fem_total,all_genot_fem,n_genot),by=join_by(known_id==id)) |>
            mutate(sr_mal_out_share_seed_couple=(sr_couple/n_genot)*sr_fem_total,
                   sr_mal_out_share_genot_couple=(sr_couple/n_genot)*all_genot_fem) |>
            group_by(candidate_id) |>
            summarize(sr_mal_out_share_seed=sum(sr_mal_out_share_seed_couple),
                      sr_mal_out_share_genot=sum(sr_mal_out_share_genot_couple)) |>
            rename(id=candidate_id) |>
            mutate(session=substr(id,1,5),.before=1) 

          sr_all <- data_self_fem |>
            full_join(data_mal) |>
            left_join(data_mal_share) |>
            mutate(
              sr_mal_total_share_seed = rowSums(across(c(sr_mal_out_share_seed, sr_self_share_seed)), na.rm = TRUE),
              sr_mal_total_share_genot = rowSums(across(c(sr_mal_out_share_genot, sr_self_share_genot)), na.rm = TRUE)
            ) |>
            arrange(id) |>
            mutate(no_sim=sim,
                   percent=per,
                   method_number=met_nb,
                   method_fruit=met_frt,.before=1)

          # CONSTRUCT TABLE
          data_sampling_rs_ms <- data_sampling_rs_ms |> 
            rbind(sr_all)
        }
      }
    }
  }
  
  return(data_sampling_rs_ms)
}

#' Get Bateman's true gradients based on observations
#'
#' @description 
#' For mating success, two options: 1- observational mating success based on carry-over 10
#' or 2- number of contacts, which best correlates with oms
#' NB: we stay consistent with the manuscript and only explore outcrossed reproductive 
#' success, on total seeds for both sexes
#'
#' @param data sampling dataset
#'
#' @return A `table` containing data. 
#' 
#' @import dplyr
#' 
#' @export

get_true_bateman <- function(data_true_rs_ms, data_proxy, cols = "co10"){
  
  data_merged <- data_true_rs_ms |>
    select(session, id, sr_fem_out_share_seed, sr_mal_out_share_seed) |>
    left_join(
      data_proxy |>
        select(id,
               index_A1,
               !!sym(paste0("oms_fem_", cols)),
               !!sym(paste0("oms_mal_", cols)))
    ) |>
    group_by(session) |>
    mutate(across(where(is.numeric), ~ . / mean(., na.rm = TRUE), .names = "r_{.col}")) |>
    ungroup() |>
    select(session, id, starts_with("r_")) |>
    pivot_longer(
      cols = starts_with(c("r_sr","r_oms")),
      names_to = c(".value", "sex"),
      names_pattern = "r_(sr|oms)_(fem|mal)_.*"
    ) |>
    rename(r_sr=sr,
           r_oms=oms)
  
    data_true_bateman <- tibble()
    
    for(s in unique(data_merged$session)){
      
      data_foc <- data_merged |>
        filter(session==s) |>
        filter(!is.na(r_sr))
      
      n_id = data_foc |>
        summarize(n()) |> pull()
      
      # FEMALES
      data_foc <- data_foc |>
        mutate(sex=relevel(as.factor(sex), ref = "fem"))
      mod_bat_oms <- lmerTest::lmer(data=data_foc,r_sr~r_oms*sex+(1|id))
      est_bat_oms <- summary(mod_bat_oms)$coefficients[2,"Estimate"]
      se_bat_oms <- summary(mod_bat_oms)$coefficients[2,"Std. Error"]
      pval_bat_oms <-  summary(mod_bat_oms)$coefficients[2,"Pr(>|t|)"] 
      
      mod_bat_contact <- lmerTest::lmer(data=data_foc,r_sr~r_index_A1*sex+(1|id))
      est_bat_contact <- summary(mod_bat_contact)$coefficients[2,"Estimate"]
      se_bat_contact <- summary(mod_bat_contact)$coefficients[2,"Std. Error"]
      pval_bat_contact <-  summary(mod_bat_contact)$coefficients[2,"Pr(>|t|)"] 
      
      data_true_bateman <- data_true_bateman |>
        bind_rows(tibble(session=s,n_id=n_id,sex="fem",
                         est_bat_oms=est_bat_oms,se_bat_oms=se_bat_oms,pval_bat_oms=pval_bat_oms,
                         est_bat_contact=est_bat_contact,se_bat_contact=se_bat_contact,pval_bat_contact=pval_bat_contact))
      
      # MALES
      data_foc <- data_foc |>
        mutate(sex=relevel(as.factor(sex), ref = "mal"))
      mod_bat_oms <- lmerTest::lmer(data=data_foc,r_sr~r_oms*sex+(1|id))
      est_bat_oms <- summary(mod_bat_oms)$coefficients[2,"Estimate"]
      se_bat_oms <- summary(mod_bat_oms)$coefficients[2,"Std. Error"]
      pval_bat_oms <-  summary(mod_bat_oms)$coefficients[2,"Pr(>|t|)"] 
      
      mod_bat_contact <- lmerTest::lmer(data=data_foc,r_sr~r_index_A1*sex+(1|id))
      est_bat_contact <- summary(mod_bat_contact)$coefficients[2,"Estimate"]
      se_bat_contact <- summary(mod_bat_contact)$coefficients[2,"Std. Error"]
      pval_bat_contact <-  summary(mod_bat_contact)$coefficients[2,"Pr(>|t|)"] 
      
      data_true_bateman <- data_true_bateman |>
        bind_rows(tibble(session=s,n_id=n_id,sex="mal",
                         est_bat_oms=est_bat_oms,se_bat_oms=se_bat_oms,pval_bat_oms=pval_bat_oms,
                         est_bat_contact=est_bat_contact,se_bat_contact=se_bat_contact,pval_bat_contact=pval_bat_contact))
      # INTER
      mod_bat_oms_full <- lmerTest::lmer(data=data_foc,r_sr~r_oms*sex+(1|id))
      mod_bat_oms_null <- lmerTest::lmer(data=data_foc,r_sr~r_oms+sex+(1|id))
      anova_inter_oms <- anova(mod_bat_oms_full,mod_bat_oms_null)
      chisq_inter_oms <- as.data.frame(anova_inter_oms)["mod_bat_oms_full","Chisq"]
      pval_inter_oms <- as.data.frame(anova_inter_oms)["mod_bat_oms_full","Pr(>Chisq)"]
      
      mod_bat_contact_full <- lmerTest::lmer(data=data_foc,r_sr~r_index_A1*sex+(1|id))
      mod_bat_contact_null <- lmerTest::lmer(data=data_foc,r_sr~r_index_A1+sex+(1|id))
      anova_inter_contact <- anova(mod_bat_contact_full,mod_bat_contact_null)
      chisq_inter_contact <- as.data.frame(anova_inter_contact)["mod_bat_contact_full","Chisq"]
      pval_inter_contact <- as.data.frame(anova_inter_contact)["mod_bat_contact_full","Pr(>Chisq)"]
      
      data_true_bateman <- data_true_bateman |>
        bind_rows(tibble(session = s, n_id = n_id, sex="inter",
                         chisq_inter_oms = chisq_inter_oms, pval_inter_oms = pval_inter_oms,
                         chisq_inter_contact = chisq_inter_contact, pval_inter_contact = pval_inter_contact))
    }
    
    data_true_bateman_all <- tibble()
    
    ## ALL SESSIONS 
    
    n_id = data_merged |>
      summarize(n()) |> pull()
    
    # FEMALES
    data_merged <- data_merged |>
      mutate(sex=relevel(as.factor(sex), ref = "fem"))
    mod_bat_oms <- lmerTest::lmer(data=data_merged,r_sr~r_oms*sex+(1|session)+(1|session:id))
    est_bat_oms <- summary(mod_bat_oms)$coefficients[2,"Estimate"]
    se_bat_oms <- summary(mod_bat_oms)$coefficients[2,"Std. Error"]
    pval_bat_oms <-  summary(mod_bat_oms)$coefficients[2,"Pr(>|t|)"] 
    
    mod_bat_contact <- lmerTest::lmer(data=data_merged,r_sr~r_index_A1*sex+(1|session)+(1|session:id))
    est_bat_contact <- summary(mod_bat_contact)$coefficients[2,"Estimate"]
    se_bat_contact <- summary(mod_bat_contact)$coefficients[2,"Std. Error"]
    pval_bat_contact <-  summary(mod_bat_contact)$coefficients[2,"Pr(>|t|)"] 
    
    data_true_bateman_all <- data_true_bateman_all |>
      bind_rows(tibble(sex="fem", n_id = n_id, 
                       est_bat_oms=est_bat_oms,se_bat_oms=se_bat_oms,pval_bat_oms=pval_bat_oms,
                       est_bat_contact=est_bat_contact,se_bat_contact=se_bat_contact,pval_bat_contact=pval_bat_contact))
    
    # MALES
    data_merged <- data_merged |>
      mutate(sex=relevel(as.factor(sex), ref = "mal"))
    mod_bat_oms <- lmerTest::lmer(data=data_merged,r_sr~r_oms*sex+(1|session)+(1|session:id))
    est_bat_oms <- summary(mod_bat_oms)$coefficients[2,"Estimate"]
    se_bat_oms <- summary(mod_bat_oms)$coefficients[2,"Std. Error"]
    pval_bat_oms <-  summary(mod_bat_oms)$coefficients[2,"Pr(>|t|)"] 
    
    mod_bat_contact <- lmerTest::lmer(data=data_merged,r_sr~r_index_A1*sex+(1|session)+(1|session:id))
    est_bat_contact <- summary(mod_bat_contact)$coefficients[2,"Estimate"]
    se_bat_contact <- summary(mod_bat_contact)$coefficients[2,"Std. Error"]
    pval_bat_contact <-  summary(mod_bat_contact)$coefficients[2,"Pr(>|t|)"] 
    
    data_true_bateman_all <- data_true_bateman_all |>
      bind_rows(tibble(sex="mal", n_id = n_id, 
                       est_bat_oms=est_bat_oms,se_bat_oms=se_bat_oms,pval_bat_oms=pval_bat_oms,
                       est_bat_contact=est_bat_contact,se_bat_contact=se_bat_contact,pval_bat_contact=pval_bat_contact))
    
    # INTER
    mod_bat_oms_full <- lmerTest::lmer(data=data_merged,r_sr~r_oms*sex+(1|session)+(1|session:id))
    mod_bat_oms_null <- lmerTest::lmer(data=data_merged,r_sr~r_oms+sex+(1|session)+(1|session:id))
    anova_inter_oms <- anova(mod_bat_oms_full,mod_bat_oms_null)
    chisq_inter_oms <- as.data.frame(anova_inter_oms)["mod_bat_oms_full","Chisq"]
    pval_inter_oms <- as.data.frame(anova_inter_oms)["mod_bat_oms_full","Pr(>Chisq)"]
    
    mod_bat_contact_full <- lmerTest::lmer(data=data_merged,r_sr~r_index_A1*sex+(1|session)+(1|session:id))
    mod_bat_contact_null <- lmerTest::lmer(data=data_merged,r_sr~r_index_A1+sex+(1|session)+(1|session:id))
    anova_inter_contact <- anova(mod_bat_contact_full,mod_bat_contact_null)
    chisq_inter_contact <- as.data.frame(anova_inter_contact)["mod_bat_contact_full","Chisq"]
    pval_inter_contact <- as.data.frame(anova_inter_contact)["mod_bat_contact_full","Pr(>Chisq)"]
    
    data_true_bateman_all <- data_true_bateman_all |>
      bind_rows(tibble(session = s, n_id = n_id, sex="inter",
                       chisq_inter_oms = chisq_inter_oms, pval_inter_oms = pval_inter_oms,
                       chisq_inter_contact = chisq_inter_contact, pval_inter_contact = pval_inter_contact))

    return(list(data_merged = data_merged,
                data_true_bateman = data_true_bateman,
                data_true_bateman_all = data_true_bateman_all))
}


#' Get Bateman's sampling gradients based on genetic
#'
#' @description 
#' For mating success, we focus as for true Bateman on outcrossed partners only.
#' For outcrossed reproductive success, we stay consistent with the manuscript 
#' and only explore outcrossed reproductive success, 
#' share on total seeds for both sexes
#'
#' @param data sampling dataset
#'
#' @return A `table` containing data. 
#' 
#' @import dplyr
#' 
#' @export

get_sampling_bateman <- function(data_sampling_rs_ms){
  
  data_merged <- data_sampling_rs_ms |>
    select(no_sim, percent, method_number ,method_fruit , session, id, 
           sr_fem_out_share_seed, sr_mal_out_share_seed,gms_fem_out, gms_mal_out) |>
    group_by(no_sim, percent, method_number ,method_fruit , session) |>
    mutate(across(where(is.numeric), ~ . / mean(., na.rm = TRUE), .names = "r_{.col}")) |>
    ungroup() |>
    select(no_sim, percent, method_number ,method_fruit , session, id, starts_with("r_")) |>
    pivot_longer(
      cols = starts_with(c("r_sr","r_gms")),
      names_to = c(".value", "sex"),
      names_pattern = "r_(sr|gms)_(fem|mal)_.*"
    ) |>
    rename(r_sr=sr,
           r_gms=gms)
  
  data_sampling_bateman <- tibble()
  
  # for(sim in unique(data_merged$no_sim)){
    for(sim in c(1:100)){
    for(per in unique(data_merged$percent)){
      for(met_nb in unique(data_merged$method_number)){
        for(met_frt in unique(data_merged$method_fruit)){
          for(s in unique(data_merged$session)){
            
            data_foc <- data_merged |>
              filter(no_sim == sim, percent == per, method_number == met_nb, method_fruit == met_frt, session==s) |>
              filter(!(is.na(r_sr)))
            
            n_id = data_foc |>
              summarize(n()) |> pull()
            
            # FEMALES
            data_foc <- data_foc |>
              mutate(sex=relevel(as.factor(sex), ref = "fem"))
            mod_bat_gms <- lmerTest::lmer(data=data_foc,r_sr~r_gms*sex+(1|id))
            est_bat_gms <- as.data.frame(summary(mod_bat_gms)$coefficients)["r_gms","Estimate"]
            se_bat_gms <- as.data.frame(summary(mod_bat_gms)$coefficients)["r_gms","Std. Error"]
            pval_bat_gms <-  as.data.frame(summary(mod_bat_gms)$coefficients)["r_gms","Pr(>|t|)"]
            
            data_sampling_bateman <- data_sampling_bateman |>
              bind_rows(tibble(no_sim = sim, percent = per, method_number = met_nb, method_fruit = met_frt, session = s,
                               n_id = n_id, sex="fem",
                               est_bat_gms=est_bat_gms,se_bat_gms=se_bat_gms,pval_bat_gms=pval_bat_gms))
            
            # MALES
            data_foc <- data_foc |>
              mutate(sex=relevel(as.factor(sex), ref = "mal"))
            mod_bat_gms <- lmerTest::lmer(data=data_foc,r_sr~r_gms*sex+(1|id))
            est_bat_gms <- as.data.frame(summary(mod_bat_gms)$coefficients)["r_gms","Estimate"]
            se_bat_gms <- as.data.frame(summary(mod_bat_gms)$coefficients)["r_gms","Std. Error"]
            pval_bat_gms <-  as.data.frame(summary(mod_bat_gms)$coefficients)["r_gms","Pr(>|t|)"]
            
            data_sampling_bateman <- data_sampling_bateman |>
              bind_rows(tibble(no_sim = sim, percent = per, method_number = met_nb, method_fruit = met_frt, session = s,
                               n_id = n_id, sex="mal",
                               est_bat_gms=est_bat_gms,se_bat_gms=se_bat_gms,pval_bat_gms=pval_bat_gms))
            # INTER
            mod_bat_gms_full <- lmerTest::lmer(data=data_foc,r_sr~r_gms*sex+(1|id))
            mod_bat_gms_null <- lmerTest::lmer(data=data_foc,r_sr~r_gms+sex+(1|id))
            anova_inter <- anova(mod_bat_gms_full,mod_bat_gms_null)
            chisq_inter <- as.data.frame(anova_inter)["mod_bat_gms_full","Chisq"]
            pval_inter <- as.data.frame(anova_inter)["mod_bat_gms_full","Pr(>Chisq)"]
            
            data_sampling_bateman <- data_sampling_bateman |>
              bind_rows(tibble(no_sim = sim, percent = per, method_number = met_nb, method_fruit = met_frt, session = s,
                               n_id = n_id, sex="inter",
                               chisq_inter = chisq_inter, pval_inter = pval_inter))
          }
        }
      }
    }
  }
  
  return(list(data_merged = data_merged,
              data_sampling_bateman = data_sampling_bateman))
}

#' Get Hedges' g Bateman
#'
#' @description 
#' Apply Tim's methods (to be developped)
#'
#' @param data Bateman's true oms and sampling gms
#'
#' @return A `table` containing data. 
#' 
#' @import dplyr
#' 
#' @export

get_hedges <- function(data_sampling_bateman, data_true_bateman_raw, per_session = TRUE){
  
  data_true_bateman <- data_true_bateman_raw$data_true_bateman_all
  group_vars <- c("percent", "method_number", "method_fruit", "sex")
  if (per_session) {
    group_vars <- c(group_vars, "session")
    data_true_bateman <- data_true_bateman_raw$data_true_bateman
  }
  
  data_hedges <- data_sampling_bateman %>%
    filter(sex != "inter") |>
    group_by(!!!syms(group_vars)) %>%
    summarise(mean_est = mean(est_bat_gms,na.rm=T),
              med_est = median(est_bat_gms,na.rm=T),
              n_sim_est = n(),
              n_id_est = max(n_id), # we take the maximum nb of ind used ? the mean ? the min ?
              sd_est = sd(est_bat_gms,na.rm=T)) %>%
    left_join(data_true_bateman |>
                filter(sex != "inter") |>
                rename(n_id_true = n_id) |>
                mutate(sd_true = se_bat_oms*sqrt(n_id_true))) %>%
    mutate(sd_comb = sqrt(((n_id_est - 1) * sd_est^2 + (n_id_true - 1) * sd_true^2) / (n_id_est + n_id_true - 2)),
           cohen_d = (med_est - est_bat_oms) / sd_comb,
           var_cohen_d = (n_id_est + n_id_true) / (n_id_est * n_id_true) + (cohen_d^2 / (2 * (n_id_est + n_id_true))),
           sd_cohen_d = sqrt(var_cohen_d),
           cohen_d_lower = cohen_d-1.96*sd_cohen_d,
           cohen_d_upper = cohen_d+1.96*sd_cohen_d) 
  
  plot_hedges <- ggplot(data=data_hedges, aes(x=method_fruit, y=cohen_d, ymin=cohen_d_lower, ymax=cohen_d_upper,color=sex)) +
    {
      if (per_session) {
        facet_grid(percent ~ session)
      } else {
        facet_grid(percent ~ .)
      }
    } +
    geom_hline(yintercept=0, lty=2,color="gray50") +  # add a dotted line at x=1 after flip
    scale_shape_manual(values=21)+
    geom_pointrange(position=position_dodge(0.75),linewidth=2,lineend="round",stroke=0.8,fill="white") +
    coord_flip() +  # flip coordinates (puts labels on y axis)
    ylab("Hedges (95% CI)") +
    scale_color_manual(values=c("#397367","#b47355")) +
    theme_classic()  + # use a white background
    theme(axis.text=element_text(size=8),plot.title=element_text(size=6),
          strip.background = element_rect(colour="black", fill="gray30",
                                          size=1.5, linetype="solid"),
          strip.text = element_text(size=8, color="white",
                                    face="bold"))
  
  return(list(data_hedges = data_hedges,
              plot_hedges = plot_hedges))
}


#' Bootstrap function to estimate mean and 95% CI
#'
#' @description 
#' Needed for variance partition
#'
#' @param data with any values
#'
#' @return mean and 95% CI
#' 
#' @import dplyr
#' 
#' @export

bootstrap_mean_ci <- function(values, n_bootstrap = 100000, conf_level = 0.95) {
  boot_func <- function(data, indices) mean(data[indices])
  boot_results <- boot(data = values, statistic = boot_func, R = n_bootstrap)
  
  ci <- boot.ci(boot_results, type = "norm", conf = conf_level)$normal[2:3]
  
  # get mean and ci
  c(mean = mean(boot_results$t), lower_ci = ci[1], upper_ci = ci[2])
}

#' Variance partition with gms 
#'
#' @description 
#'  Variance partition with gms 
#'
#' @param data table with sampling genotypes and data on id
#'
#' @return variance partition values 
#' 
#' @import dplyr
#' 
#' @export

get_var_gms <- function(data_sampling,data_id) {
  
  # add individual that did not reproduced
  put_0 <- TRUE
  
  # table with results for males
  var_dec_gms_mal_final <- tibble()
  
  # table with results for females
  var_dec_gms_fem_final <- tibble()
 
  # for(sim in unique(data_merged$no_sim)){
  for(sim in c(1:10)){
    for(per in unique(data_sampling$percent)){
      for(met_nb in unique(data_sampling$method_number)){
        for(met_frt in unique(data_sampling$method_fruit)){
          for(s in unique(data_sampling$session)){
            
            data_id_foc <- data_id |>
              filter(session == s)
            
            data_foc <- data_sampling |>
              filter(no_sim == sim, percent == per, method_number == met_nb, method_fruit == met_frt, session==s) 
            
            data_foc_out <- data_foc |>
              filter(known_id != candidate_id) # on outcrossed rs only
            
            # MALES 
            
            rs_gms_mal <- data_foc_out %>%
              group_by(candidate_id,known_id) %>%
              summarise(seed_couple=n()) %>%
              left_join(data_foc %>% # all genotypes per mother
                          group_by(known_id) %>%
                          summarise(n_genot=n())) %>%
              left_join(data_id_foc %>%
                          select(id,sr_fem_total),by=join_by(known_id==id)) %>%
              mutate(rs_couple=(seed_couple/n_genot)*sr_fem_total) %>%
              group_by(candidate_id) %>%
              summarise(rs=sum(rs_couple,na.rm=T))
            
            gms_gms_mal <- data_foc_out %>%
              group_by(candidate_id) %>%
              summarise(gms=n_distinct(known_id))
            
            fec_gms_mal <- data_foc_out %>%
              group_by(candidate_id,known_id) %>%
              summarise(seed_couple=n()) %>%
              left_join(data_id_foc %>%
                          select(id,sr_fem_total),by=join_by(known_id==id)) %>%
              summarise(fec=mean(sr_fem_total,na.rm=T))
            
            ps_gms_mal <- data_foc_out %>%
              group_by(candidate_id,known_id) %>%
              summarise(seed_couple=n()) %>%
              left_join(data_foc %>% # all genotypes per mother
                          group_by(known_id) %>%
                          summarise(n_genot=n())) %>%
              left_join(data_id_foc %>%
                          select(id,sr_fem_total),by=join_by(known_id==id)) %>%
              mutate(rs_couple=(seed_couple/n_genot)*sr_fem_total) %>%
              group_by(candidate_id) %>%
              summarise(ps=sum(rs_couple,na.rm=T)/sum(sr_fem_total,na.rm=T))
            
            # merged table and verification
            var_dec_gms_mal <- rs_gms_mal %>%
              left_join(gms_gms_mal) %>%
              left_join(fec_gms_mal) %>%
              left_join(ps_gms_mal) %>%
              replace(is.na(.),0) %>%
              mutate(mult=gms*ps*fec,
                     verif=ifelse(rs==mult,1,0))  # ok
            
            # including 0
            if(put_0==TRUE){
              var_dec_gms_mal <- var_dec_gms_mal %>%
                bind_rows(data_id_foc %>%
                            filter(!id %in% var_dec_gms_mal$candidate_id) %>%
                            select(id) %>%
                            rename(candidate_id=id) %>%
                            mutate(rs=0,gms=0))
            }
            
            # relativization
            var_dec_gms_mal <- var_dec_gms_mal %>%
              mutate(r_rs=rs/mean(rs,na.rm=T),
                     r_gms=gms/mean(gms,na.rm=T),
                     r_fec=fec/mean(fec,na.rm=T),
                     r_ps=ps/mean(ps,na.rm=T))
            
            # final table for males
            var_dec_gms_mal_anal <- var_dec_gms_mal %>%
              summarise(rs = var(r_rs,na.rm=T),
                        gms = var(r_gms,na.rm=T),
                        fec = var(r_fec,na.rm=T),
                        ps = var(r_ps,na.rm=T),
                        cov_gms_fecC = 2*cov(r_gms,r_fec,use="complete"),
                        cov_gms_ps = 2*cov(r_gms,r_ps,use="complete"),
                        cov_fec_ps = 2*cov(r_fec,r_ps,use="complete")) |>
              mutate(no_sim = sim, percent = per, method_number = met_nb, 
                     method_fruit = met_frt, session = s,.before=1)
            
            var_dec_gms_mal_final <- var_dec_gms_mal_final |>
              bind_rows(var_dec_gms_mal_anal)
            
            # FEMALES
            
            rs_gms_fem <- data_foc %>%
              group_by(known_id) %>%
              summarise(seed_self=sum(known_id==candidate_id),
                        seed_out=sum(known_id!=candidate_id)) %>%
              left_join(data_id_foc %>%
                          select(id,sr_fem_total),by=join_by(known_id==id)) %>%
              mutate(rs=(seed_out/(seed_self+seed_out))*sr_fem_total) %>%
              filter(rs!=0) # remove female that only selfed
            
            gms_gms_fem <- data_foc_out %>%
              group_by(known_id) %>%
              summarise(gms=n_distinct(candidate_id))
            
            fec_gms_fem <- data_foc_out %>%
              group_by(known_id,candidate_id) %>%
              summarise(seed_couple=n()) %>%
              left_join(rs_gms_fem) %>%
              mutate(rs_couple=(seed_couple/(seed_self+seed_out))*sr_fem_total) %>%
              group_by(known_id) %>%
              summarise(fec=mean(rs_couple))
            
            # merge tables and verification
            var_dec_gms_fem <- rs_gms_fem %>%
              left_join(gms_gms_fem) %>%
              left_join(fec_gms_fem) %>%
              replace(is.na(.),0) %>%
              mutate(mult=gms*fec,
                     verif=ifelse(rs==mult,1,0)) # ok
            
            # including 0
            if(put_0==TRUE){
              var_dec_gms_fem <- var_dec_gms_fem %>%
                bind_rows(data_id_foc %>%
                            filter(!id %in% var_dec_gms_fem$known_id) %>%
                            select(id) %>%
                            rename(known_id=id) %>%
                            mutate(rs=0,gms=0))
            }
            
            # relativization
            var_dec_gms_fem <- var_dec_gms_fem %>%
              mutate(r_rs=rs/mean(rs,na.rm=T),
                     r_gms=gms/mean(gms,na.rm=T),
                     r_fec=fec/mean(fec,na.rm=T))
            
            # final table for females
            var_dec_gms_fem_anal <- var_dec_gms_fem %>%
              summarise(rs = var(r_rs,na.rm=T),
                        gms = var(r_gms,na.rm=T),
                        fec = var(r_fec,na.rm=T),
                        cov_gms_fec = 2*cov(r_gms,r_fec,use="complete")) |>
              mutate(no_sim = sim, percent = per, method_number = met_nb, 
                     method_fruit = met_frt, session = s,.before=1)
            
            var_dec_gms_fem_final <- var_dec_gms_fem_final |>
              bind_rows(var_dec_gms_fem_anal)
            
          }
        }
      }
    }
  }
  return(list(var_dec_gms_mal = var_dec_gms_mal_final,
         var_dec_gms_fem = var_dec_gms_fem_final))
}

#' Variance partition with gms 
#'
#' @description 
#'  Variance partition with gms 
#'
#' @param data table with sampling genotypes and data on id
#'
#' @return variance partition values 
#' 
#' @import dplyr
#' 
#' @export

get_plot_gms <- function(data_sampling,data_id) {
  
  # add individual that did not reproduced
  put_0 <- TRUE
  
  # table with results for males
  var_dec_gms_mal_final <- tibble()
  
  # table with results for females
  var_dec_gms_fem_final <- tibble()
  
  # for(sim in unique(data_merged$no_sim)){
  for(sim in c(1:10)){
    for(per in unique(data_sampling$percent)){
      for(met_nb in unique(data_sampling$method_number)){
        for(met_frt in unique(data_sampling$method_fruit)){
          for(s in unique(data_sampling$session)){
            
            data_id_foc <- data_id |>
              filter(session == s)
            
            data_foc <- data_sampling |>
              filter(no_sim == sim, percent == per, method_number == met_nb, method_fruit == met_frt, session==s) 
            
            data_foc_out <- data_foc |>
              filter(known_id != candidate_id) # on outcrossed rs only
            
            # MALES 
            
            rs_gms_mal <- data_foc_out %>%
              group_by(candidate_id,known_id) %>%
              summarise(seed_couple=n()) %>%
              left_join(data_foc %>% # all genotypes per mother
                          group_by(known_id) %>%
                          summarise(n_genot=n())) %>%
              left_join(data_id_foc %>%
                          select(id,sr_fem_total),by=join_by(known_id==id)) %>%
              mutate(rs_couple=(seed_couple/n_genot)*sr_fem_total) %>%
              group_by(candidate_id) %>%
              summarise(rs=sum(rs_couple,na.rm=T))
            
            gms_gms_mal <- data_foc_out %>%
              group_by(candidate_id) %>%
              summarise(gms=n_distinct(known_id))
            
            fec_gms_mal <- data_foc_out %>%
              group_by(candidate_id,known_id) %>%
              summarise(seed_couple=n()) %>%
              left_join(data_id_foc %>%
                          select(id,sr_fem_total),by=join_by(known_id==id)) %>%
              summarise(fec=mean(sr_fem_total,na.rm=T))
            
            ps_gms_mal <- data_foc_out %>%
              group_by(candidate_id,known_id) %>%
              summarise(seed_couple=n()) %>%
              left_join(data_foc %>% # all genotypes per mother
                          group_by(known_id) %>%
                          summarise(n_genot=n())) %>%
              left_join(data_id_foc %>%
                          select(id,sr_fem_total),by=join_by(known_id==id)) %>%
              mutate(rs_couple=(seed_couple/n_genot)*sr_fem_total) %>%
              group_by(candidate_id) %>%
              summarise(ps=sum(rs_couple,na.rm=T)/sum(sr_fem_total,na.rm=T))
            
            # merged table and verification
            var_dec_gms_mal <- rs_gms_mal %>%
              left_join(gms_gms_mal) %>%
              left_join(fec_gms_mal) %>%
              left_join(ps_gms_mal) %>%
              replace(is.na(.),0) %>%
              mutate(mult=gms*ps*fec,
                     verif=ifelse(rs==mult,1,0))  # ok
            
            # including 0
            if(put_0==TRUE){
              var_dec_gms_mal <- var_dec_gms_mal %>%
                bind_rows(data_id_foc %>%
                            filter(!id %in% var_dec_gms_mal$candidate_id) %>%
                            select(id) %>%
                            rename(candidate_id=id) %>%
                            mutate(rs=0,gms=0))
            }
            
            # relativization
            var_dec_gms_mal <- var_dec_gms_mal %>%
              mutate(r_rs=rs/mean(rs,na.rm=T),
                     r_gms=gms/mean(gms,na.rm=T),
                     r_fec=fec/mean(fec,na.rm=T),
                     r_ps=ps/mean(ps,na.rm=T))
            
            # final table for males
            var_dec_gms_mal_anal <- var_dec_gms_mal %>%
              summarise(rs = var(r_rs,na.rm=T),
                        gms = var(r_gms,na.rm=T),
                        fec = var(r_fec,na.rm=T),
                        ps = var(r_ps,na.rm=T),
                        cov_gms_fecC = 2*cov(r_gms,r_fec,use="complete"),
                        cov_gms_ps = 2*cov(r_gms,r_ps,use="complete"),
                        cov_fec_ps = 2*cov(r_fec,r_ps,use="complete")) |>
              mutate(no_sim = sim, percent = per, method_number = met_nb, 
                     method_fruit = met_frt, session = s,.before=1)
            
            var_dec_gms_mal_final <- var_dec_gms_mal_final |>
              bind_rows(var_dec_gms_mal_anal)
            
            # FEMALES
            
            rs_gms_fem <- data_foc %>%
              group_by(known_id) %>%
              summarise(seed_self=sum(known_id==candidate_id),
                        seed_out=sum(known_id!=candidate_id)) %>%
              left_join(data_id_foc %>%
                          select(id,sr_fem_total),by=join_by(known_id==id)) %>%
              mutate(rs=(seed_out/(seed_self+seed_out))*sr_fem_total) %>%
              filter(rs!=0) # remove female that only selfed
            
            gms_gms_fem <- data_foc_out %>%
              group_by(known_id) %>%
              summarise(gms=n_distinct(candidate_id))
            
            fec_gms_fem <- data_foc_out %>%
              group_by(known_id,candidate_id) %>%
              summarise(seed_couple=n()) %>%
              left_join(rs_gms_fem) %>%
              mutate(rs_couple=(seed_couple/(seed_self+seed_out))*sr_fem_total) %>%
              group_by(known_id) %>%
              summarise(fec=mean(rs_couple))
            
            # merge tables and verification
            var_dec_gms_fem <- rs_gms_fem %>%
              left_join(gms_gms_fem) %>%
              left_join(fec_gms_fem) %>%
              replace(is.na(.),0) %>%
              mutate(mult=gms*fec,
                     verif=ifelse(rs==mult,1,0)) # ok
            
            # including 0
            if(put_0==TRUE){
              var_dec_gms_fem <- var_dec_gms_fem %>%
                bind_rows(data_id_foc %>%
                            filter(!id %in% var_dec_gms_fem$known_id) %>%
                            select(id) %>%
                            rename(known_id=id) %>%
                            mutate(rs=0,gms=0))
            }
            
            # relativization
            var_dec_gms_fem <- var_dec_gms_fem %>%
              mutate(r_rs=rs/mean(rs,na.rm=T),
                     r_gms=gms/mean(gms,na.rm=T),
                     r_fec=fec/mean(fec,na.rm=T))
            
            # final table for females
            var_dec_gms_fem_anal <- var_dec_gms_fem %>%
              summarise(rs = var(r_rs,na.rm=T),
                        gms = var(r_gms,na.rm=T),
                        fec = var(r_fec,na.rm=T),
                        cov_gms_fec = 2*cov(r_gms,r_fec,use="complete")) |>
              mutate(no_sim = sim, percent = per, method_number = met_nb, 
                     method_fruit = met_frt, session = s,.before=1)
            
            var_dec_gms_fem_final <- var_dec_gms_fem_final |>
              bind_rows(var_dec_gms_fem_anal)
            
          }
        }
      }
    }
  }
  return(list(var_dec_gms_mal = var_dec_gms_mal_final,
         var_dec_gms_fem = var_dec_gms_fem_final))
}

#' Get data of paternity share for sem analyses
#'
#' @description Function to get paternity share data
#'
#' @param data table with genotypes
#'
#' @return table paternity share data
#'
#' @import dplyr
#'
#' @export

get_data_paternity_share <- function(data_genotypes) {
  
  data_paternity_share <- data_genotypes |>
    filter(candidate_id != known_id) |>
    group_by(candidate_id,known_id) |>
    summarise(genot_couple = n()) |>
    left_join(data_genotypes |>
                group_by(known_id) |>
                summarise(genot_mother = n())) |>
    mutate(paternity_share = genot_couple / genot_mother) |>
    group_by(candidate_id) |>
    summarise(mean_ps = mean(paternity_share)) |>
    rename(id = candidate_id) |>
    left_join(data_genotypes |>
                group_by(candidate_id,known_id) |>
                summarise(genot_couple = n()) |>
                left_join(data_genotypes |>
                            group_by(known_id) |>
                            summarise(genot_mother = n())) |>
                mutate(paternity_share = genot_couple / genot_mother) |>
                group_by(candidate_id) |>
                summarise(mean_ps_all = mean(paternity_share)) |>
                rename(id = candidate_id)) |>
    mutate(sex = "mal",.before = 2)
  
  return(data_paternity_share)
}

#' Get data for sem analyses - sessions with all genotypes
#'
#' @description Function to gett data for sem analyses
#'
#' @param data table with true rs ms and data on id
#'
#' @return table data for sem analyses
#' 
#' @import dplyr
#' 
#' @export

get_data_sem_complete_sessions <- function(data_true_rs_ms, data_proxy, cols="co10") {
  
  data_sem <- data_true_rs_ms |>
    select(session, id, sr_fem_out_share_seed, sr_mal_out_share_seed) |>
    left_join(
      data_proxy |>
        select(id,
               nb_flo_open,height_max,
               !!sym(paste0("oms_fem_", cols)),
               !!sym(paste0("oms_mal_", cols)))
    ) |>
    group_by(session) |>
    mutate(across(where(is.numeric), ~ . / mean(., na.rm = TRUE), .names = "r_{.col}")) |>
    ungroup() |>
    select(session, id, starts_with("r_")) |>
    pivot_longer(
      cols = starts_with(c("r_sr","r_oms")),
      names_to = c(".value", "sex"),
      names_pattern = "r_(sr|oms)_(fem|mal)_.*"
    ) |>
    rename(r_sr=sr,
           r_oms=oms) |>
    mutate(
      ttt = case_when(substr(session,3,4) == "FA" ~ "low",
                      substr(session,3,4) == "MO" ~ "medium",
                      TRUE ~ "high")
    ) 
  
  return(data_sem)
}

#' Get data for sem analyses - all sessions with sampled genotypes
#'
#' @description Function to gett data for sem analyses
#'
#' @param data table with true rs ms and data on id
#'
#' @return table data for sem analyses
#' 
#' @import dplyr
#' 
#' @export

get_data_sem_sampled_sessions <- function(data_id_sampled_sessions, data_proxy, data_paternity_share, cols="co10") {
  
  data_sem <- data_id_sampled_sessions |>
    select(session, ID_full, type, r_SR_out, r_SR_all, poll_treat_factor) |>
    rename(id = ID_full,
           sex = type,
           r_sr = r_SR_out,
           r_sr_all = r_SR_all,
           ttt = poll_treat_factor) |>
    left_join(data_paternity_share) |>
    group_by(session) |>
    mutate(r_mean_ps = mean_ps / mean(mean_ps,na.rm = T),
           r_mean_ps_all = mean_ps_all / mean(mean_ps_all,na.rm = T)) |>
    ungroup() |>
    select(!c(mean_ps,mean_ps_all)) |>
    left_join(
      data_proxy |>
        select(id, session,
               nb_flo_open,nb_flo_all,height_max,height_mean,nb_stem,
               !!sym(paste0("oms_fem_", cols)),
               !!sym(paste0("oms_mal_", cols))) |>
        group_by(session) |>
        mutate(across(where(is.numeric), ~ . / mean(., na.rm = TRUE), .names = "r_{.col}")) |>
        ungroup() |>
        select(session, id, starts_with("r_")) |>
        pivot_longer(
          cols = starts_with(c("r_oms")),
          names_to = "sex",
          names_pattern = "r_oms_(fem|mal)_.*",
          values_to = "r_oms"
        ) 
    ) 
    
  
  return(data_sem)
}


#' Get linear models for correlation oms vs proxy
#'
#' @description ...
#'
#' @param data ...
#'
#' @return ...
#' 
#' @import dplyr
#' 
#' @export

get_linear_models_oms_proxy <- function(data_proxy_longer) {
  
  mod_A3 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_A3*sex+(1|session)+(1|session:id))
  mod_A3_null_1 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_A3+sex+(1|session)+(1|session:id))
  mod_A3_null_2 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_A3+(1|session)+(1|session:id))
  
  mod_A1 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_A1*sex+(1|session)+(1|session:id))
  mod_A1_null_1 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_A1+sex+(1|session)+(1|session:id))
  mod_A1_null_2 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_A1+(1|session)+(1|session:id))
  
  mod_B3 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_B3*sex+(1|session)+(1|session:id))
  mod_B3_null_1 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_B3+sex+(1|session)+(1|session:id))
  mod_B3_null_2 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_B3+(1|session)+(1|session:id))
  
  mod_B1 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_B1*sex+(1|session)+(1|session:id))
  mod_B1_null_1 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_B1+sex+(1|session)+(1|session:id))
  mod_B1_null_2 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_B1+(1|session)+(1|session:id))
  
  mod_C3 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_C3*sex+(1|session)+(1|session:id))
  mod_C3_null_1 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_C3+sex+(1|session)+(1|session:id))
  mod_C3_null_2 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_C3+(1|session)+(1|session:id))
  
  mod_C1 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_C1*sex+(1|session)+(1|session:id))
  mod_C1_null_1 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_C1+sex+(1|session)+(1|session:id))
  mod_C1_null_2 <- lmerTest::lmer(data=data_proxy_longer,oms_co10~index_C1+(1|session)+(1|session:id))
  
  return(list(mod_A3 = mod_A3,
         mod_A3_null_1 = mod_A3_null_1,
         mod_A3_null_2 = mod_A3_null_2,
         mod_A1 = mod_A1,
         mod_A1_null_1 = mod_A1_null_1,
         mod_A1_null_2 = mod_A1_null_2,
         mod_B3 = mod_B3,
         mod_B3_null_1 = mod_B3_null_1,
         mod_B3_null_2 = mod_B3_null_2,
         mod_B1 = mod_B1,
         mod_B1_null_1 = mod_B1_null_1,
         mod_B1_null_2 = mod_B1_null_2,
         mod_C3 = mod_C3,
         mod_C3_null_1 = mod_C3_null_1,
         mod_C3_null_2 = mod_C3_null_2,
         mod_C1 = mod_C1,
         mod_C1_null_1 = mod_C1_null_1,
         mod_C1_null_2 = mod_C1_null_2))
}


#' Get linear models for correlation oms vs proxy
#'
#' @description ...
#'
#' @param data ...
#'
#' @return ...
#' 
#' @import dplyr 
#' 
#' @export

compute_predictions_oms_proxy <- function(linear_models_oms_proxy,data_proxy_longer) {
  
  
  # A1
  x_seq <- seq(
    min(data_proxy_longer[["index_A1"]], na.rm = TRUE),
    max(data_proxy_longer[["index_A1"]], na.rm = TRUE),
    length.out = 100
  )
  
  # add dummy random variables
  newdata <- data.frame(
    session = rep(data_proxy_longer$session[1], length(x_seq)),
    id = rep(data_proxy_longer$id[1], length(x_seq))
  )
  newdata[["index_A1"]] <- x_seq
  
  # predictions (without random effect)
  ci_A1 <- merTools::predictInterval(
    linear_models_oms_proxy$mod_A1_null_2,
    newdata = newdata,
    level = 0.95,
    n.sims = 1000,
    stat = "mean",
    type = "linear.prediction",
    include.resid.var = FALSE
  ) %>%
    cbind(x_seq) %>%
    mutate(var = "raw",
           cat = "id level")
  
  # A3
  x_seq <- seq(
    min(data_proxy_longer[["index_A3"]], na.rm = TRUE),
    max(data_proxy_longer[["index_A3"]], na.rm = TRUE),
    length.out = 100
  )
  
  # add dummy random variables
  newdata <- data.frame(
    session = rep(data_proxy_longer$session[1], length(x_seq)),
    id = rep(data_proxy_longer$id[1], length(x_seq))
  )
  newdata[["index_A3"]] <- x_seq
  
  # predictions (without random effect)
  ci_A3 <- merTools::predictInterval(
    linear_models_oms_proxy$mod_A3_null_2,
    newdata = newdata,
    level = 0.95,
    n.sims = 1000,
    stat = "mean",
    type = "linear.prediction",
    include.resid.var = FALSE
  ) %>%
    cbind(x_seq) %>%
    mutate(var = "raw * oms",
           cat = "id level")
  
  # B1
  x_seq <- seq(
    min(data_proxy_longer[["index_B1"]], na.rm = TRUE),
    max(data_proxy_longer[["index_B1"]], na.rm = TRUE),
    length.out = 100
  )
  
  # add dummy random variables
  newdata <- data.frame(
    session = rep(data_proxy_longer$session[1], length(x_seq)),
    id = rep(data_proxy_longer$id[1], length(x_seq))
  )
  newdata[["index_B1"]] <- x_seq
  
  # predictions (without random effect)
  ci_B1 <- merTools::predictInterval(
    linear_models_oms_proxy$mod_B1_null_2,
    newdata = newdata,
    level = 0.95,
    n.sims = 1000,
    stat = "mean",
    type = "linear.prediction",
    include.resid.var = FALSE
  ) %>%
    cbind(x_seq) %>%
    mutate(var = "raw",
           cat = "flower level")
  
  # B3
  x_seq <- seq(
    min(data_proxy_longer[["index_B3"]], na.rm = TRUE),
    max(data_proxy_longer[["index_B3"]], na.rm = TRUE),
    length.out = 100
  )
  
  # add dummy random variables
  newdata <- data.frame(
    session = rep(data_proxy_longer$session[1], length(x_seq)),
    id = rep(data_proxy_longer$id[1], length(x_seq))
  )
  newdata[["index_B3"]] <- x_seq
  
  # predictions (without random effect)
  ci_B3 <- merTools::predictInterval(
    linear_models_oms_proxy$mod_B3_null_2,
    newdata = newdata,
    level = 0.95,
    n.sims = 1000,
    stat = "mean",
    type = "linear.prediction",
    include.resid.var = FALSE
  ) %>%
    cbind(x_seq) %>%
    mutate(var = "raw * oms",
           cat = "flower level")
  
  # C1
  x_seq <- seq(
    min(data_proxy_longer[["index_C1"]], na.rm = TRUE),
    max(data_proxy_longer[["index_C1"]], na.rm = TRUE),
    length.out = 100
  )
  
  # add dummy random variables
  newdata <- data.frame(
    session = rep(data_proxy_longer$session[1], length(x_seq)),
    id = rep(data_proxy_longer$id[1], length(x_seq))
  )
  newdata[["index_C1"]] <- x_seq
  
  # predictions (without random effect)
  ci_C1 <- merTools::predictInterval(
    linear_models_oms_proxy$mod_C1_null_2,
    newdata = newdata,
    level = 0.95,
    n.sims = 1000,
    stat = "mean",
    type = "linear.prediction",
    include.resid.var = FALSE
  ) %>%
    cbind(x_seq) %>%
    mutate(var = "raw",
           cat = "duration")
  
  # C3
  x_seq <- seq(
    min(data_proxy_longer[["index_C3"]], na.rm = TRUE),
    max(data_proxy_longer[["index_C3"]], na.rm = TRUE),
    length.out = 100
  )
  
  # add dummy random variables
  newdata <- data.frame(
    session = rep(data_proxy_longer$session[1], length(x_seq)),
    id = rep(data_proxy_longer$id[1], length(x_seq))
  )
  newdata[["index_C3"]] <- x_seq
  
  # predictions (without random effect)
  ci_C3 <- merTools::predictInterval(
    linear_models_oms_proxy$mod_C3_null_2,
    newdata = newdata,
    level = 0.95,
    n.sims = 1000,
    stat = "mean",
    type = "linear.prediction",
    include.resid.var = FALSE
  ) %>%
    cbind(x_seq) %>%
    mutate(var = "raw * oms",
           cat = "duration")
  
  pred_df <- ci_A1 %>%
    bind_rows(ci_A3) %>%
    bind_rows(ci_B1) %>%
    bind_rows(ci_B3) %>%
    bind_rows(ci_C1) %>%
    bind_rows(ci_C3)
  
  return(pred_df)
}

#' SEM analyses with brms - total reproductive success
#'
#' @description to be adapted to run also with outcrossed rs + change traits
#'
#' @param data 
#'
#' @return 
#' 
#' @import dplyr 
#' 
#' @export

get_brms_pooled_data <- function(data_sem_sampled_sessions) {
  
  mv_model_all <- brms::brm(
    brms::bf(r_oms ~ r_nb_flo_all * ttt * sex + r_height_mean * ttt * sex + (1 | session) + (1 | session:id)) +
      brms::bf(r_sr_all  ~ r_oms * ttt * sex + r_nb_flo_all * ttt * sex + r_height_mean * ttt * sex + (1 | session) + (1 | session:id)),
    data = data_sem_sampled_sessions,
    family = gaussian(),
    chains = 4, cores = 4, iter = 4000,
    prior = brms::set_prior("normal(0, 1)", class = "b")
  )
  
  summary <- summary(mv_model_all)
  # global_plot <- plot(mv_model_all)
  oms <- brms::conditional_effects(mv_model_all, effects = "r_oms:ttt", conditions = data.frame(sex = c("mal","fem")))
  nb_flo <- brms::conditional_effects(mv_model_all, effects = "r_nb_flo_all:ttt", conditions = data.frame(sex = c("mal","fem")))
  height <- brms::conditional_effects(mv_model_all, effects = "r_height_mean:ttt", conditions = data.frame(sex = c("mal","fem")))
  
  
  return(list(summary = summary,
              #global_plot = global_plot,
              oms = oms,
              nb_flo = nb_flo,
              height = height))
}

#' SEM analyses with piecewise - separated dataset
#'
#' @description general piecewise adapted to both sexes
#'
#' @param data 
#'
#' @return 
#' 
#' @import dplyr 
#' 
#' @export

get_piecewise_general <- function(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                  target_sr = "r_sr_all", target_traits = c("r_nb_flo_open","r_height_mean"),
                                  x_coord = c(2,2,1,3), y_coord = c(2,3,1,1), color = "bisque4") {
  
  data_target <- data_sem_sampled_sessions %>% 
    filter(ttt == target_ttt & sex == target_sex)
  
  formula1 <- reformulate(target_traits, response = "r_oms")
  formula1 <- update(formula1, . ~ . + (1|session))
  
  formula2 <- reformulate(c("r_oms", target_traits), response = target_sr)
  formula2 <- update(formula2, . ~ . + (1|session))
  
  psem_proxy <- piecewiseSEM::psem(lme4::lmer(data=data_target,formula1),
                                   
                                   lme4::lmer(data=data_target,formula2))
  
  plot <- plot(psem_proxy,
               
               node_attrs = data.frame(fillcolor = color,
                                       
                                       x = x_coord, y = y_coord,
                                       
                                       fontsize=6),
               
               edge_attrs = data.frame(fontsize=6))
  
  summary <- summary(psem_proxy)
  
  return(list(summary = summary,
              plot = plot))
}

 
  
