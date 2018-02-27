#Load Libraries
library(tidyverse)
library(lubridate)
library(forcats)
library(tibble)

pd <- "dfw"
ois_type <- "all"
cs_types <- c("wp", "gd", "ds")



#Load Requested Data Frames
for (cs in cs_types) {
  analysis_glob <- paste0('data/', pd, '_', cs, '_', ois_type, "*.csv")
  assign(paste0(cs, '_df' ), read_csv(Sys.glob(analysis_glob)))
}

#General Pre-QC Processing for each DF

# df <- wp_df %>% 
#   mutate(police = if_else(police == TRUE, TRUE, FALSE, FALSE)) %>% 
#   mutate(crowd = if_else(crowd == TRUE, TRUE, FALSE, FALSE)) %>% 
#   mutate(match = if_else(police == TRUE & crowd == TRUE, "yes_match", "no_match")) %>%
#   mutate(match = if_else(match == "no_match" & police == TRUE & crowd == FALSE, "no_match_crowd_missing", match)) %>%
#   mutate(match = if_else(match == "no_match" & crowd == TRUE & police == FALSE, "no_match_police_missing", match)) %>%
#   mutate(outcome = if_else(police == TRUE, outcome, "unknown_police_missing", "unknown_police_missing")) %>% 
#   select(date, name, police, crowd, match, outcome, everything()) %>% 
#   unique() %>% 
#   arrange(match) %>% 
#   filter(outcome == "deceased" | outcome == "unknown_police_missing")



refine_matches <- function(mdf, ois_type) {
  
  df <- mdf %>% 
    mutate(police = if_else(police == TRUE, TRUE, FALSE, FALSE)) %>% 
    mutate(crowd = if_else(crowd == TRUE, TRUE, FALSE, FALSE)) %>% 
    mutate(match = if_else(police == TRUE & crowd == TRUE, "yes_match", "no_match")) %>%
    mutate(match = if_else(match == "no_match" & police == TRUE & crowd == FALSE, "no_match_crowd_missing", match)) %>%
    mutate(match = if_else(match == "no_match" & crowd == TRUE & police == FALSE, "no_match_police_missing", match)) %>%
    mutate(outcome = if_else(police == TRUE, outcome, "unknown_police_missing", "unknown_police_missing")) %>% 
    select(date, name, police, crowd, match, outcome, everything()) %>% 
    unique() %>% 
    arrange(match) 
    #rowid_to_column("id")
  
  if(ois_type=='all'){
    df <- df
  } else if (ois_type=='non_fatal'){
    df <- df %>% filter(outcome !='deceased' | outcome == "unknown_police_missing")
  } else {
    df <- df %>% filter(outcome == ois_type | outcome == "unknown_police_missing")
  }
  
  return(df)
}

#wp_df_all <- refine_matches(wp_df, "all")
#gd_df_all <- refine_matches(gd_df, "all")
#ds_df_all <- refine_matches(ds_df, "all")
#ds_df_all <- refine_matches(ds_df, "all")

#WP Cleaning
wp_df_deceased <- refine_matches(wp_df, "deceased")


#GD Cleaning
gd_df_deceased <- refine_matches(gd_df, "deceased") %>%
  mutate(date_qc = date, 
         name_qc = name,
         match_qc = match) %>% 
  rename(pd = lawenforcementagency) %>% 
  mutate(pd = if_else(is.na(pd) & police == TRUE, "Dallas Police Department", pd)) %>% 
  mutate(uof = if_else(is.na(classification) & police == TRUE, "Gunshot", classification)) %>% 
  select(date, date_qc, name, name_qc, police, crowd, match, match_qc, outcome, pd, uof, everything()) %>% 
  #Remove Cases That Are Not Dallas PD
  filter(pd == "Dallas Police Department") %>%
  #Remove Cases That Are Not OIS
  filter(!(uof !=  "Gunshot" & match != "yes_match")) %>% 
  #Alter qc details for elias portillo case
  mutate(date_qc = if_else(date == "2016-08-24" & name =="elias portillo", as.Date("2016-08-25"), date_qc),
         name_qc = if_else(date == "2016-08-25" & name =="unknown", "elias portillo", name_qc),
         match_qc = if_else(date_qc == "2016-08-25" & name_qc =="elias portillo", "yes_match", match_qc)) %>% 
  distinct(date_qc, name_qc, match_qc)





#DS Cleaning
ds_df_deceased <- refine_matches(ds_df, "deceased")
ds_df_nonfatal <- refine_matches(ds_df, "non_fatal")
