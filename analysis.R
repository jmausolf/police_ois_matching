#Load Libraries
library(tidyverse)
library(lubridate)
library(forcats)


pd <- "dfw"
ois_type <- "all"
cs_types <- c("wp", "gd", "ds")



#Load Requested Data Frames
for (cs in cs_types) {
  analysis_glob <- paste0('data/', pd, '_', cs, '_', ois_type, "*.csv")
  assign(paste0(cs, '_df' ), read_csv(Sys.glob(analysis_glob)))
}

#General Pre-QC Processing for each DF

df <- wp_df %>% 
  mutate(police = if_else(police == TRUE, TRUE, FALSE, FALSE)) %>% 
  mutate(crowd = if_else(crowd == TRUE, TRUE, FALSE, FALSE)) %>% 
  mutate(match_qc = if_else(police == TRUE & crowd == TRUE, "yes_match", "no_match")) %>%
  mutate(match_qc = if_else(match_qc == "no_match" & police == TRUE & crowd == FALSE, "no_match_crowd_missing", match_qc)) %>%
  mutate(match_qc = if_else(match_qc == "no_match" & crowd == TRUE & police == FALSE, "no_match_police_missing", match_qc)) %>%
  select(date, name, police, crowd, match_qc, everything()) %>% 
  unique() %>% 
  filter(outcome == "deceased")

refine_matches <- function(mdf, ois_type) {
  df <- mdf %>% 
    mutate(police = if_else(police == TRUE, TRUE, FALSE, FALSE)) %>% 
    mutate(crowd = if_else(crowd == TRUE, TRUE, FALSE, FALSE)) %>% 
    mutate(match_qc = if_else(police == TRUE & crowd == TRUE, "yes_match", "no_match")) %>%
    mutate(match_qc = if_else(match_qc == "no_match" & police == TRUE & crowd == FALSE, "no_match_crowd_missing", match_qc)) %>%
    mutate(match_qc = if_else(match_qc == "no_match" & crowd == TRUE & police == FALSE, "no_match_police_missing", match_qc)) %>%
    select(date, name, police, crowd, match_qc, everything()) %>% 
    unique() %>% 
    filter(outcome == ois_type)
}

refine_matches <- function(mdf, ois_type) {
  if(ois_type=='all'){
    df <- mdf
  } else if (ois_type=='non_fatal'){
    df <- mdf %>% filter(outcome !='deceased')
  } else {
    df <- mdf %>% filter(outcome == ois_type)
  }
  
  df <- df %>% 
    mutate(police = if_else(police == TRUE, TRUE, FALSE, FALSE)) %>% 
    mutate(crowd = if_else(crowd == TRUE, TRUE, FALSE, FALSE)) %>% 
    mutate(match_qc = if_else(police == TRUE & crowd == TRUE, "yes_match", "no_match")) %>%
    mutate(match_qc = if_else(match_qc == "no_match" & police == TRUE & crowd == FALSE, "no_match_crowd_missing", match_qc)) %>%
    mutate(match_qc = if_else(match_qc == "no_match" & crowd == TRUE & police == FALSE, "no_match_police_missing", match_qc)) %>%
    select(date, name, police, crowd, match_qc, everything()) %>% 
    unique() 
  
  return(df)
}

#outcome needs to be fixed

df <- refine_matches(wp_df, "deceased")
df <- refine_matches(gd_df, "deceased")
df <- refine_matches(ds_df, "deceased")
df <- refine_matches(ds_df, "non_fatal")
df <- refine_matches(ds_df, "all")
