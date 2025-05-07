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
  # dt_id <- read.table(file_path,head=T) |>
  #   mutate(poll_treat_factor=as.factor(case_when(poll_treat==1~"low",
  #                                                poll_treat==2~"medium",
  #                                                TRUE~"high"))) |>
  #   mutate(poll_treat_factor=forcats::fct_relevel(poll_treat_factor, c("low","medium","high"))) |>
  #   mutate(prop_self=SR_self/(SR_self+SR_fem_out+SR_mal_out),
  #          gam_ov_proxy=nb_flo*nbOv_mean,
  #          SR_out=SR_fem_out+SR_mal_out) |>
  #   mutate(mean_nb_visit_per_flower=nb_visit/nb_dist_vis)
  
  dt_id <- read.table(file_path,head=T) |>
    rename(id = ID_full) |>
    select(session, id, nb_flo_open, nb_poll_focal,
           !!!syms(paste0("import_nb_part_ID_out_", cols)),
           !!!syms(paste0("export_nb_part_ID_out_", cols))) %>%
    rename_with(~ gsub("import_nb_part_ID_out_", "oMS_fem_", .x), starts_with("import_")) %>%
    rename_with(~ gsub("export_nb_part_ID_out_", "oMS_mal_", .x), starts_with("export_"))
  
  return(dt_id)
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
  dt_obs <- read.table(file_path,head=T) |>
  mutate(ttt=as.factor(case_when(grepl("FA",session)~"1_low",
                                 grepl("MO",session)~"2_medium",
                                 TRUE~"3_high")))
return(dt_obs)
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

get_data_contact <- function(dt_obs){
  dt_contact <- dt_obs |>
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
  
  dt_contact_id <- dt_contact |>
    group_by(session,id) |>
    summarize(nb_contact = max(no_contact_with_id),
              nb_visit = sum(nb_visit),
              dur_visit = sum(duration,na.rm=T)) 
  
  return(list(dt_contact = dt_contact,
              dt_contact_id = dt_contact_id))
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

  dt_session <- data_contact$dt_contact_id |>
                group_by(session) |>
                summarize(max_oMS = n_distinct(id),
                          total_nb_contact = sum(nb_contact),
                          total_nb_visit = sum(nb_visit),
                          total_duration = sum(dur_visit)) |>
    left_join(data_contact$dt_contact |>
                group_by(session) |>
                summarize(total_nb_poll = n_distinct(people)))
  
  return(dt_session)
}

#' Computing the different proxies for oMS
#'
#' @description 
#' Compute the different proxies for oMS (see quarto doc)
#'
#' @param data summarized at the id and session level
#'
#' @return A `table` containing the proxies
#'
#' @import dplyr
#' @import tidyr
#' 
#' @export

compute_oMS_and_proxy <- function(data_id,data_contact,data_session){
  
  dt_proxy <- data_id |>
    left_join(data_contact$dt_contact_id) |>
    left_join(data_session) |>
    mutate(
      index_A1 = nb_contact,
      index_A2 = nb_contact/total_nb_contact,
      index_A3 = nb_contact*max_oMS,
      index_A4 = (nb_contact*max_oMS)/total_nb_contact,
      index_A5 = index_A4/total_nb_poll,
      index_B1 = nb_visit,
      index_B2 = nb_visit/total_nb_visit,
      index_B3 = nb_visit*max_oMS,
      index_B4 = (nb_visit*max_oMS)/total_nb_visit,
      index_B5 = index_A4/total_nb_poll
      )
  
  return(dt_proxy)
}

#' PCA
#'
#' @description 
#' Compute the different proxies for oMS (see quarto doc)
#'
#' @param data summarized at the id and session level
#'
#' @return A `table` containing the proxies
#'
#' @import dplyr
#' @import tidyr
#' 
#' @export

get_pca <- function(dt_proxy, cols = "co10"){
  
  fem_proxy <- dt_proxy |>
    select(!!sym(paste0("oMS_fem_",cols)),
           index_A1,index_A2,index_A3,index_A4,index_A5,
           index_B1,index_B2,index_B3,index_B4,index_B5)
  
  pca_fem <- FactoMineR::PCA(fem_proxy)
  
  
  mal_proxy <- dt_proxy |>
    select(!!sym(paste0("oMS_mal_",cols)),
           index_A1,index_A2,index_A3,index_A4,index_A5,
           index_B1,index_B2,index_B3,index_B4,index_B5)
  
  pca_mal <- FactoMineR::PCA(mal_proxy)
  
  return(list(pca_fem = plot(pca_fem, choix="var"),
              pca_mal = plot(pca_mal, choix="var")))
}
