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

load_data_id <- function(file_path){
  dt_id <- read.table(file_path,head=T) |>
    mutate(poll_treat_factor=as.factor(case_when(poll_treat==1~"low",
                                                 poll_treat==2~"medium",
                                                 TRUE~"high"))) |>
    mutate(poll_treat_factor=forcats::fct_relevel(poll_treat_factor, c("low","medium","high"))) |>
    mutate(prop_self=SR_self/(SR_self+SR_fem_out+SR_mal_out),
           gam_ov_proxy=nb_flo*nbOv_mean,
           SR_out=SR_fem_out+SR_mal_out) |>
    mutate(mean_nb_visit_per_flower=nb_visit/nb_dist_vis)
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
  
  dt_contact_id <- dt_contact |>
    group_by(session,id) |>
    summarize(nb_contact = max(no_contact_with_id),
              nb_visit = sum(nb_visit),
              dur_visit = sum(duration,na.rm=T)) |>
    left_join(dt_contact |>
                group_by(session) |>
                summarize(max_oMS = n_distinct(id)))
  
  return(list(dt_contact,dt_contact_id))
}