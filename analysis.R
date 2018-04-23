#Load Libraries
library(tidyverse)
library(lubridate)
library(forcats)
library(tibble)
library(stargazer)
library(ggpubr)

##Make directories
system('mkdir -p images')
system('mkdir -p tables')

#pd <- "dfw"
pd_types <- c("dfw", "den", "jax", "mco")
ois_type <- "all"
cs_types <- c("wp", "gd", "ds")



#Load Requested Data Frames
for (pd in pd_types) {

  for (cs in cs_types) {
    analysis_glob <- paste0('data/', pd, '_', cs, '_', ois_type, "*.csv")
    assign(paste(pd, cs, 'df', sep = "_" ), read_csv(Sys.glob(analysis_glob)))
  }
    
}


#General Pre-QC Processing for each DF

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


#Custom Custom Table Functions

#Tables
save_stargazer <- function(output.file, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append = FALSE)
}


make_pd_df <- function(){
  N <- 10
  Years <- "Example"
  Matches = 0.80
  No_Match_Crowd = 0.20
  No_Match_Police = 0.20
  departments <- "Example"
  x <- data.frame(Years, N, Matches, No_Match_Crowd, No_Match_Police)
  
  x <- x %>% mutate(Years = as.character(Years))
  
  row.names(x) <- departments
  
  return(x)
}


make_var_df <- function(){
  N <- 10
  Matches = 0.80
  No_Match_Crowd = 0.20
  No_Match_Police = 0.20
  departments <- "Example"
  x <- data.frame(N, Matches, No_Match_Crowd, No_Match_Police)
  
  row.names(x) <- departments
  
  return(x)
}

add_pd_case <- function(df, dept, rowvector){
  idx <- nrow(df) + 1
  df[idx,] = rowvector
  rownames(df)[idx]<- dept
  return(df)
}

summary_row <- function(df) {
  
  minyr <- year(min(df[["date_qc"]]))
  maxyr <- year(max(df[["date_qc"]]))
  yrs <- paste(minyr, maxyr, sep="-")
  
  n <- nrow(df)
  avgmat = sprintf(mean(df[["matches"]]), fmt = '%#.2f')
  avgcmis = sprintf(mean(df[["no_match_crowd"]]), fmt = '%#.2f')
  avgpmis = sprintf(mean(df[["no_match_police"]]), fmt = '%#.2f')  
  
  nmat = sum(df[["matches"]] == 1)
  ncmis = sum(df[["no_match_crowd"]] == 1)
  npmis = sum(df[["no_match_police"]] == 1)
  
  avg_matches = paste0(avgmat, " (", nmat, ")")
  avg_crowd_missing = paste0(avgcmis, " (", ncmis, ")")
  avg_police_missing = paste0(avgpmis, " (", npmis, ")")
  
  rv <- c(yrs, n, avg_matches, avg_crowd_missing, avg_police_missing)
  return(rv)
}


summary_row_var_val <- function(df, col_name, value, negate) {
  #variablen <- as.name(variablen)
  require("dplyr")
  require("lazyeval")

  
  if(missing(col_name) && missing(value)){
    df <- df
  } else {
    if(missing(negate)){
      filter_criteria <- interp(~y == x, .values=list(y = as.name(col_name), x = value))
    } else {
      filter_criteria <- interp(~y != x, .values=list(y = as.name(col_name), x = value))
    }  
    df <- df %>% filter_(filter_criteria)
    
  }  

  
  minyr <- year(min(df[["date_qc"]]))
  maxyr <- year(max(df[["date_qc"]]))
  yrs <- paste(minyr, maxyr, sep="-")
  
  n <- nrow(df)
  if(n > 0){
    avgmat = sprintf(mean(df[["matches"]]), fmt = '%#.2f')
    avgcmis = sprintf(mean(df[["no_match_crowd"]]), fmt = '%#.2f')
    avgpmis = sprintf(mean(df[["no_match_police"]]), fmt = '%#.2f')  
    
    nmat = sum(df[["matches"]] == 1)
    ncmis = sum(df[["no_match_crowd"]] == 1)
    npmis = sum(df[["no_match_police"]] == 1)    

    avg_matches = paste0(avgmat, " (", nmat, ")")
    avg_crowd_missing = paste0(avgcmis, " (", ncmis, ")")
    avg_police_missing = paste0(avgpmis, " (", npmis, ")")
    
    
  } else {
    message(paste("[*] no observations matching criteria", col_name, value, sep = " "))
    avg_matches = "0.00 (0)"
    avg_crowd_missing = "0.00 (0)"
    avg_police_missing = "0.00 (0)"
  }

  rv <- c(n, avg_matches, avg_crowd_missing, avg_police_missing)
  return(rv)
}



#WP Cleaning - Dallas PD
dfw_wp_df_deceased <- refine_matches(dfw_wp_df, "deceased") %>% 
  mutate(date_qc = date, 
         name_qc = name,
         match_qc = match,
         race_eth_qc = race_p,
         armed_qc = subjectweapon) %>% 
    mutate(uof = if_else(is.na(mannerofdeath) & police == TRUE, "shot", mannerofdeath)) %>% 
  #Clean up race_eth_qc
  mutate(race_eth_qc = if_else(is.na(race_eth_qc) & crowd == TRUE, race_c, race_eth_qc)) %>%
  mutate(race_eth_qc = fct_collapse(race_eth_qc,
                                    "B" = c("B", "Dwayne B"),
                                    "W" = c("W", "Jr._Alton Anthony W"))) %>% 
  #Clean up armed_qc
  mutate(armed_qc = if_else(is.na(armed_qc) & crowd == TRUE, armed, armed_qc)) %>%
  select(date, date_qc, name, name_qc, police, crowd, match, match_qc, outcome, uof, everything()) %>% 
  filter(!(uof !=  "shot" & match != "yes_match")) %>% 
  #Alter qc details for elias portillo case
  mutate(name_qc = if_else(date == "2016-08-25" & name =="unknown", "elias portillo", name_qc)) %>%
  #Alter qc details for alton folmar case
  mutate(name_qc = if_else(date == "2017-06-19" & name =="folmar", "alton folmar", name_qc),
         match_qc = if_else(date_qc == "2017-06-19" & name_qc =="alton folmar", "yes_match", match_qc),
         armed_qc = if_else(date_qc == "2017-06-19" & name_qc =="alton folmar", "Handgun", armed_qc)) %>% 
  select(date_qc, name_qc, match_qc, race_eth_qc, race_p, race_c, armed_qc, armed, subjectweapon) %>% 
  distinct(date_qc, name_qc, match_qc, race_eth_qc, armed_qc) %>% 
    mutate(matches = (if_else(match_qc == "yes_match", 1, 0)),
           no_match_police = if_else(match_qc == "no_match_police_missing", 1, 0),
           no_match_crowd = if_else(match_qc == "no_match_crowd_missing", 1, 0))



#WP Cleaning - Denver PD
den_wp_df_deceased <- refine_matches(den_wp_df, "deceased") %>% 
  mutate(date_qc = date, 
         name_qc = name,
         match_qc = match,
         race_eth_qc = race_p,
         armed_qc = armedwith) %>% 
  #Clean up race_eth_qc
  mutate(race_eth_qc = if_else(ethnicity_p == "H", ethnicity_p, race_eth_qc)) %>%  
  mutate(race_eth_qc = if_else(is.na(race_eth_qc) & crowd == TRUE, race_c, race_eth_qc)) %>%
  mutate(race_eth_qc = fct_collapse(race_eth_qc,
                                    "L" = c("H"))) %>% 
  #Clean up armed_qc
  mutate(armed_qc = if_else(is.na(armed_qc) & crowd == TRUE, armed, armed_qc)) %>%
  mutate(armed_qc = fct_collapse(armed_qc,
                                    "Firearm" = c("Firearm", "gun"),
                                    "Motor Vehicle" = c("Motor Vehicle", "vehicle"))) %>% 
  #Alter qc details for jessica hernandez case (wp used nick-name versus full name)
  mutate(name_qc = if_else(date == "2015-01-26" & name =="jessie hernandez", "jessica hernandez", name_qc),
         match_qc = if_else(date_qc == "2015-01-26" & name_qc =="jessica hernandez", "yes_match", match_qc)) %>% 
  #Alter qc details for gerardino cayetano-gonzalez case (police mispelled name)
  mutate(name_qc = if_else(date == "2016-02-22" & name =="garardino cayetano-gonzalez", "gerardino gonzalez", name_qc),
         match_qc = if_else(date_qc == "2016-02-22" & name_qc =="gerardino gonzalez", "yes_match", match_qc)) %>% 
  #Alter qc details for dion damon case (wp mispelled name)
  mutate(name_qc = if_else(date == "2016-04-12" & name =="dion daman", "dion damon", name_qc),
         match_qc = if_else(date_qc == "2016-04-12" & name_qc =="dion damon", "yes_match", match_qc), 
         #defer to police for race and weapon
         race_eth_qc = if_else(date_qc == "2016-04-12" & name_qc =="dion damon", "L", as.character(race_eth_qc)),
         armed_qc = if_else(date_qc == "2016-04-12" & name_qc =="dion damon", "Simulated Weapon", as.character(armed_qc))) %>% 
  #Alter qc details for alexander duran case (wp used nickname)
  mutate(name_qc = if_else(date == "2018-02-06" & name =="alex duran", "alexander duran", name_qc),
         match_qc = if_else(date_qc == "2018-02-06" & name_qc =="alexander duran", "yes_match", match_qc),
         armed_qc = if_else(date_qc == "2018-02-06" & name_qc =="alexander duran", "Knife", as.character(armed_qc))) %>% 
  #Remove Non Denver PD Cases
  filter(name_qc != "austin dunsmore") %>%  #jefferson county pd %>% 
  filter(name_qc != "kyler grabbingbear") %>% #adams county pd %>% 
  select(date_qc, name_qc, match_qc, race_eth_qc, race_p, ethnicity_p, race_c, armed_qc, armed, armedwith) %>% 
  distinct(date_qc, name_qc, match_qc, race_eth_qc, armed_qc) %>%
  mutate(matches = (if_else(match_qc == "yes_match", 1, 0)),
         no_match_police = if_else(match_qc == "no_match_police_missing", 1, 0),
         no_match_crowd = if_else(match_qc == "no_match_crowd_missing", 1, 0))



#WP Cleaning - Jacksonville PD
jax_wp_df_deceased <- refine_matches(jax_wp_df, "deceased") %>% 
  mutate(date_qc = date, 
         name_qc = name,
         match_qc = match,
         race_eth_qc = race_p,
         armed_qc = suspectweapon) %>% 
  #Clean up race_eth_qc
  mutate(race_eth_qc = if_else(is.na(race_eth_qc) & crowd == TRUE, race_c, race_eth_qc)) %>%
  #Clean up armed_qc
  mutate(armed_qc = if_else(is.na(armed_qc) & crowd == TRUE, armed, armed_qc)) %>%
  select(date, date_qc, name, name_qc, police, crowd, match, match_qc, outcome, everything()) %>% 
  #Alter qc details for victor larosa case (crowd mispelled name)
  mutate(name_qc = if_else(date == "2015-07-02" & name =="victo larosa", "victor larosa", name_qc),
         armed_qc = if_else(date_qc == "2015-07-02" & name_qc =="victor larosa", "Vehicle", armed_qc),
         match_qc = if_else(date_qc == "2015-07-02" & name_qc =="victor larosa", "yes_match", match_qc)) %>% 
  select(date_qc, name_qc, match_qc, race_eth_qc, race_p, race_c, armed_qc, armed) %>% 
  distinct(date_qc, name_qc, match_qc, race_eth_qc, armed_qc) %>% 
  mutate(matches = (if_else(match_qc == "yes_match", 1, 0)),
         no_match_police = if_else(match_qc == "no_match_police_missing", 1, 0),
         no_match_crowd = if_else(match_qc == "no_match_crowd_missing", 1, 0))



#WP Cleaning - Orlando PD
mco_wp_df_deceased <- refine_matches(mco_wp_df, "deceased") %>% 
  mutate(date_qc = date, 
         name_qc = name,
         match_qc = match,
         race_eth_qc = race_p,
         armed_qc = suspectsweapon) %>% 
  #Clean up race_eth_qc
  mutate(race_eth_qc = if_else(is.na(race_eth_qc) & crowd == TRUE, race_c, race_eth_qc)) %>%
  mutate(race_eth_qc = fct_collapse(race_eth_qc,
                                    "B" = c("Black"),
                                    "L" = c("H"),
                                    "W" = c("White"),
                                    "O" = c("Other"))) %>% 
  #Clean up armed_qc
  mutate(armed_qc = if_else(is.na(armed_qc) & crowd == TRUE, armed, armed_qc)) %>%
  #Alter qc details for charlin charles case (police reversed, mispelled name)
  mutate(name_qc = if_else(date == "2016-05-01" & name =="charles carlin", "charlin charles", name_qc),
         armed_qc = if_else(date_qc == "2016-05-01" & name_qc =="charlin charles", "Toy Gun", armed_qc),
         match_qc = if_else(date_qc == "2016-05-01" & name_qc =="charlin charles", "yes_match", match_qc)) %>%
  #Remove Unmatched Cases Not Orlando PD (Part of Orange County Sheriff per GD data)
  filter(name_qc != c("deresha armstrong", "william charbonneau")) %>% 
  select(date_qc, name_qc, match_qc, race_eth_qc, race_p, race_c, armed_qc, armed) %>% 
  distinct(date_qc, name_qc, match_qc, race_eth_qc, armed_qc) %>% 
  mutate(matches = (if_else(match_qc == "yes_match", 1, 0)),
         no_match_police = if_else(match_qc == "no_match_police_missing", 1, 0),
         no_match_crowd = if_else(match_qc == "no_match_crowd_missing", 1, 0))







#Combined Data, All Departments
all_wp_df_deceased <- rbind(dfw_wp_df_deceased, den_wp_df_deceased, jax_wp_df_deceased, mco_wp_df_deceased)


#Create Desired DF Table
wpdf <- make_pd_df()[-1,] %>% 
  add_pd_case("Dallas Police Department", summary_row(dfw_wp_df_deceased)) %>% 
  add_pd_case("Denver Police Department", summary_row(den_wp_df_deceased)) %>%
  add_pd_case("Jacksonville Sheriff's Office", summary_row(jax_wp_df_deceased)) %>%
  add_pd_case("Orlando Police Department", summary_row(mco_wp_df_deceased)) %>%
  add_pd_case("All Police Departments  ", summary_row(all_wp_df_deceased)) %>% 
  rename("Matched" = Matches,
         "Not Matched by Crowd" = No_Match_Crowd,
         "Not Matched by Police" = No_Match_Police)

#Save Table
save_stargazer("tables/table1.tex", as.data.frame(wpdf), header=FALSE, type='latex',
               font.size = "footnotesize",
               title = "Matched Reports of Fatal Officer Involved Shootings - Washington Post",
               notes  = "Notes: Match Metrics by Police Department, Matches and Mismatches by Source Displayed as Proportions",
               summary = FALSE)




#GD Cleaning
dfw_gd_df_deceased <- refine_matches(dfw_gd_df, "deceased") %>%
  mutate(date_qc = date, 
         name_qc = name,
         match_qc = match,
         race_eth_qc = race_p,
         armed_qc = subjectweapon) %>% 
  rename(pd = lawenforcementagency) %>% 
  mutate(pd = if_else(is.na(pd) & police == TRUE, "Dallas Police Department", pd)) %>% 
  mutate(uof = if_else(is.na(classification) & police == TRUE, "Gunshot", classification)) %>%
  #Clean up race_eth_qc
  mutate(race_eth_qc = if_else(is.na(race_eth_qc) & crowd == TRUE, race_c, race_eth_qc)) %>%
  mutate(race_eth_qc = fct_collapse(race_eth_qc,
                                    "B" = c("B", "Black", "Dwayne B"),
                                    "L" = c("L", "Hispanic/Latino"))) %>% 
  #Clean up armed_qc
  mutate(armed_qc = if_else(is.na(armed_qc) & crowd == TRUE, armed, armed_qc)) %>%  
  select(date, date_qc, name, name_qc, police, crowd, match, match_qc, outcome, pd, uof, everything()) %>% 
  #Remove Cases That Are Not Dallas PD
  filter(pd == "Dallas Police Department") %>%
  #Remove Cases That Are Not OIS
  filter(!(uof !=  "Gunshot" & match != "yes_match")) %>% 
  #Alter qc details for elias portillo case
  mutate(date_qc = if_else(date == "2016-08-24" & name =="elias portillo", as.Date("2016-08-25"), date_qc),
         name_qc = if_else(date == "2016-08-25" & name =="unknown", "elias portillo", name_qc),
         match_qc = if_else(date_qc == "2016-08-25" & name_qc =="elias portillo", "yes_match", match_qc),
         #defer to police for race and weapon
         armed_qc = if_else(date_qc == "2016-08-25" & name_qc =="elias portillo", "Handgun", armed_qc)) %>%
  select(date_qc, name_qc, match_qc, race_eth_qc, race_p, race_c, armed_qc, armed, subjectweapon) %>% 
  distinct(date_qc, name_qc, match_qc, race_eth_qc, armed_qc) %>% 
  mutate(matches = (if_else(match_qc == "yes_match", 1, 0)),
         no_match_police = if_else(match_qc == "no_match_police_missing", 1, 0),
         no_match_crowd = if_else(match_qc == "no_match_crowd_missing", 1, 0))


den_gd_df_deceased <- refine_matches(den_gd_df, "deceased") %>%
  mutate(date_qc = date, 
         name_qc = name,
         match_qc = match,
         race_eth_qc = race_p,
         armed_qc = armedwith) %>% 
  rename(pd = lawenforcementagency) %>% 
  mutate(pd = if_else(is.na(pd) & police == TRUE, "Denver Police Department", pd)) %>% 
  mutate(uof = if_else(is.na(classification) & police == TRUE, "Gunshot", classification)) %>% 
  #Clean up race_eth_qc
  mutate(race_eth_qc = if_else(ethnicity_p == "H", ethnicity_p, race_eth_qc)) %>%  
  mutate(race_eth_qc = if_else(is.na(race_eth_qc) & crowd == TRUE, race_c, race_eth_qc)) %>%
  mutate(race_eth_qc = fct_collapse(race_eth_qc,
                                    "B" = c("Black"),
                                    "L" = c("Hispanic/Latino", "H"),
                                    "N" = c("Native American"))) %>% 
  #Clean up armed_qc
  mutate(armed_qc = if_else(is.na(armed_qc) & crowd == TRUE, armed, armed_qc)) %>%  
  select(date, date_qc, name, name_qc, police, crowd, match, match_qc, outcome, pd, uof, everything()) %>%
  #Remove Cases That Are Not Denver PD
  filter(!(pd !=  "Denver Police Department" & match != "yes_match")) %>% 
  #Remove Cases That Are Not OIS
  filter(!(uof !=  "Gunshot" & match != "yes_match")) %>% 
  #Alter qc details for jessica hernandez case (gd used nickname)
  mutate(name_qc = if_else(date == "2015-01-26" & name =="jesse hernandez", "jessica hernandez", name_qc),
         match_qc = if_else(date_qc == "2015-01-26" & name_qc =="jessica hernandez", "yes_match", match_qc),
         armed_qc = if_else(date_qc == "2015-01-26" & name_qc =="jessica hernandez", "Motor Vehicle", armed_qc)) %>% 
  #Alter qc details for gerardino cayetano-gonzalez case (police mispelled name, gd used hyphen)
  mutate(name_qc = if_else(date == "2016-02-22" & name =="garardino cayetano-gonzalez", "gerardino gonzalez", name_qc),
         name_qc = if_else(date == "2016-02-22" & name =="gerardino cayetano-gonzalez", "gerardino gonzalez", name_qc),
         match_qc = if_else(date_qc == "2016-02-22" & name_qc =="gerardino gonzalez", "yes_match", match_qc)) %>% 
  select(date_qc, name_qc, match_qc, race_eth_qc, race_p, ethnicity_p, race_c, armed_qc, armed, armedwith) %>% 
  distinct(date_qc, name_qc, match_qc, race_eth_qc, armed_qc) %>%
  mutate(matches = (if_else(match_qc == "yes_match", 1, 0)),
         no_match_police = if_else(match_qc == "no_match_police_missing", 1, 0),
         no_match_crowd = if_else(match_qc == "no_match_crowd_missing", 1, 0))


#GD Cleaning - Jacksonville PD
jax_gd_df_deceased <- refine_matches(jax_gd_df, "deceased") %>% 
  mutate(date_qc = date, 
         name_qc = name,
         match_qc = match,
         race_eth_qc = race_p,
         armed_qc = suspectweapon) %>%
  rename(pd = lawenforcementagency) %>% 
  mutate(pd = if_else(is.na(pd) & police == TRUE, "Jacksonville Sheriff's Office", pd)) %>%
  mutate(uof = if_else(is.na(classification) & police == TRUE, "Gunshot", classification)) %>% 
  #Clean up race_eth_qc
  mutate(race_eth_qc = if_else(is.na(race_eth_qc) & crowd == TRUE, race_c, race_eth_qc)) %>%
  mutate(race_eth_qc = fct_collapse(race_eth_qc, "B" = c("Black"))) %>% 
  #Clean up armed_qc
  mutate(armed_qc = if_else(is.na(armed_qc) & crowd == TRUE, armed, armed_qc)) %>%
  #Remove Cases That Are Not PD
  filter(!(pd !=  "Jacksonville Sheriff's Office" & match != "yes_match")) %>% 
  #Remove Cases That Are Not OIS
  filter(!(uof !=  "Gunshot" & match != "yes_match")) %>% 
  select(date, date_qc, name, name_qc, police, crowd, match, match_qc, outcome, everything()) %>% 
  #Alter qc details for victor larosa case (crowd mispelled name)
  mutate(name_qc = if_else(date == "2015-07-02" & name =="victo larosa", "victor larosa", name_qc),
         armed_qc = if_else(date_qc == "2015-07-02" & name_qc =="victor larosa", "Vehicle", armed_qc),
         match_qc = if_else(date_qc == "2015-07-02" & name_qc =="victor larosa", "yes_match", match_qc)) %>% 
  select(date_qc, name_qc, match_qc, race_eth_qc, race_p, race_c, armed_qc, armed) %>% 
  distinct(date_qc, name_qc, match_qc, race_eth_qc, armed_qc) %>% 
  mutate(matches = (if_else(match_qc == "yes_match", 1, 0)),
         no_match_police = if_else(match_qc == "no_match_police_missing", 1, 0),
         no_match_crowd = if_else(match_qc == "no_match_crowd_missing", 1, 0))


#GD Cleaning - Orlando FL
mco_gd_df_deceased <- refine_matches(mco_gd_df, "deceased") %>% 
  mutate(date_qc = date, 
         name_qc = name,
         match_qc = match,
         race_eth_qc = race_p,
         armed_qc = suspectsweapon) %>%
  rename(pd = lawenforcementagency) %>% 
  mutate(pd = if_else(is.na(pd) & police == TRUE, "Orlando Police Department", pd)) %>%
  mutate(uof = if_else(is.na(classification) & police == TRUE, "Gunshot", classification)) %>% 
  #Clean up race_eth_qc
  mutate(race_eth_qc = if_else(is.na(race_eth_qc) & crowd == TRUE, race_c, race_eth_qc)) %>%
  mutate(race_eth_qc = fct_collapse(race_eth_qc,
                                    "B" = c("Black"),
                                    "W" = c("White"),
                                    "O" = c("Other"))) %>% 
  #Clean up armed_qc
  mutate(armed_qc = if_else(is.na(armed_qc) & crowd == TRUE, armed, armed_qc)) %>%
  #Remove Cases That Are Not PD
  filter(!(pd !=  "Orlando Police Department" & match != "yes_match")) %>% 
  #Remove Cases That Are Not OIS
  filter(!(uof !=  "Gunshot" & match != "yes_match")) %>% 
  select(date, date_qc, name, name_qc, police, crowd, match, match_qc, outcome, everything()) %>% 
  #Alter qc details for charlin charles case (police reversed, mispelled name)
  mutate(name_qc = if_else(date == "2016-05-01" & name =="charles carlin", "charlin charles", name_qc),
         armed_qc = if_else(date_qc == "2016-05-01" & name_qc =="charlin charles", "Toy Gun", armed_qc),
         match_qc = if_else(date_qc == "2016-05-01" & name_qc =="charlin charles", "yes_match", match_qc)) %>%
  select(date_qc, name_qc, match_qc, race_eth_qc, race_p, race_c, armed_qc, armed) %>% 
  distinct(date_qc, name_qc, match_qc, race_eth_qc, armed_qc) %>% 
  mutate(matches = (if_else(match_qc == "yes_match", 1, 0)),
         no_match_police = if_else(match_qc == "no_match_police_missing", 1, 0),
         no_match_crowd = if_else(match_qc == "no_match_crowd_missing", 1, 0))



  
#Combined Data, All Departments
all_gd_df_deceased <- rbind(dfw_gd_df_deceased, den_gd_df_deceased, jax_gd_df_deceased, mco_gd_df_deceased)

#Create GD Matches Table
gddf <- make_pd_df()[-1,] %>% 
  add_pd_case("Dallas Police Department", summary_row(dfw_gd_df_deceased)) %>% 
  add_pd_case("Denver Police Department", summary_row(den_gd_df_deceased)) %>% 
  add_pd_case("Jacksonville Sheriff's Office", summary_row(jax_gd_df_deceased)) %>% 
  add_pd_case("Orlando Police Department", summary_row(mco_gd_df_deceased)) %>% 
  add_pd_case("All Police Departments  ", summary_row(all_gd_df_deceased)) %>% 
  rename("Matched" = Matches,
         "Not Matched by Crowd" = No_Match_Crowd,
         "Not Matched by Police" = No_Match_Police)


#Save Table
save_stargazer("tables/table2.tex", as.data.frame(gddf), header=FALSE, type='latex',
               font.size = "footnotesize",
               title = "Matched Reports of Fatal Officer Involved Shootings - The Guardian",
               notes  = "Notes: Match Metrics by Police Department, Matches and Mismatches by Source Displayed as Proportions",
               summary = FALSE)







#DS Cleaning - Dallas PD
dfw_ds_df_all <- refine_matches(dfw_ds_df, "all") %>% 
  mutate(name = if_else(is.na(name), "error_unknown", name)) %>% 
  mutate(date_qc = date, 
         name_qc = name,
         match_qc = match,
         outcome_qc = outcome,
         race_eth_qc = race_p,
         armed_qc = subjectweapon) %>%
  rename(pd = agencyname) %>% 
  mutate(pd = if_else(is.na(pd) & police == TRUE, "Dallas Police Department", pd),
         pd = if_else(pd == "Dallas Police", "Dallas Police Department", pd),
         pd = if_else(pd == "Dallas PD", "Dallas Police Department", pd)) %>% 
  #Clean up race_eth_qc
  unite(race_eth_c, c(race_c, hispanicorlatinoorigin), remove = FALSE) %>% 
  mutate(race_eth_qc = if_else(is.na(race_eth_qc) & crowd == TRUE, race_eth_c, race_eth_qc)) %>%
  mutate(race_eth_qc = fct_collapse(race_eth_qc,
                                    "B" = c("B", "Black", "Dwayne B", "Ellis_Lenny B", "Jr. B", 
                                            "Black or African American_Unknown",
                                            "Black or African American_Not of Hispanic or Latino origin"),
                                    "L" = c("L", "Hispanic/Latino", "H", "Jr. L", "Gabaldon_Robeto L"),
                                    "O" = c("Native American"),
                                    "W" = c("W", "White", "White_Not of Hispanic or Latino origin", "Jr. W"),
                                    "U" = c("Summary", "Unknown_Unknown"))) %>% 
  #Clean up armed_qc
  mutate(armed_qc = if_else(is.na(armed_qc) & crowd == TRUE, weapon, armed_qc)) %>%
  select(date, date_qc, name, name_qc, match, match_qc, outcome, outcome_qc, everything()) %>% 
  #Remove Cases That Are Not Dallas PD
  filter(pd == "Dallas Police Department") %>%
  #Alter qc details for james harper case
  mutate(date_qc = if_else(date == "2012-08-24" & name =="james harper", as.Date("2012-07-24"), date_qc),
         date_qc = if_else(date == "2012-12-24" & name =="james harper", as.Date("2012-07-24"), date_qc),
         name_qc = if_else(date == "2012-07-24" & name =="james jarper", "james harper", name_qc),
         match_qc = if_else(date_qc == "2012-07-24" & name_qc =="james harper", "yes_match", match_qc),
         outcome_qc = if_else(name_qc == "james harper", "deceased", outcome_qc)) %>% 
  #Alter qc details for name parse errors dpd
  #mutate(name_qc = if_else(date == "2012-08-03" & "error_unknown", "roberto gabaldon", name_qc),
  #       name_qc = if_else(date == "2012-12-14" & "error_unknown", "lenny ellis", name_qc)) %>% 
  #Alter qc details for roberto gabaldon case
  mutate(match_qc = if_else(date_qc == "2012-08-03" & name_qc =="roberto gabaldon", "yes_match", match_qc),
         outcome_qc = if_else(name_qc == "roberto gabaldon", "injured", outcome_qc)) %>% 
  #Alter qc details for stoney eugene rawlinson case
  mutate(date_qc = if_else(date == "2013-02-09" & name =="stoney rawlinson", as.Date("2013-02-08"), date_qc),
         name_qc = if_else(date == "2013-02-08" & name =="rawlison stoney", "stoney rawlinson", name_qc),
         #name_qc = if_else(date == "2013-02-09" & name =="stoney eugene rawlinson", "stoney rawlinson", name_qc),
         match_qc = if_else(date_qc == "2013-02-08" & name_qc =="stoney rawlinson", "yes_match", match_qc),
         outcome_qc = if_else(name_qc == "stoney rawlinson", "deceased", outcome_qc)) %>% 
  select(date_qc, name_qc, match_qc, race_eth_qc, race_p, race_c, race_eth_c, armed_qc, subjectweapon, weapon, outcome_qc) %>% 
  distinct(date_qc, name_qc, match_qc, race_eth_qc, armed_qc, outcome_qc) %>%
  arrange(match_qc) %>% 
  mutate(matches = (if_else(match_qc == "yes_match", 1, 0)),
         no_match_police = if_else(match_qc == "no_match_police_missing", 1, 0),
         no_match_crowd = if_else(match_qc == "no_match_crowd_missing", 1, 0))


dfw_ds_df_deceased <- dfw_ds_df_all %>% 
  filter(outcome_qc == "deceased")

dfw_ds_df_nonfatal <- dfw_ds_df_all %>% 
  filter(outcome_qc != "deceased")




#DS Cleaning - Orlando PD
mco_ds_df_all <- refine_matches(mco_ds_df, "all") %>% 
  mutate(name = if_else(is.na(name), "error_unknown", name)) %>% 
  mutate(date_qc = date, 
         name_qc = name,
         match_qc = match,
         outcome_qc = outcome,
         race_eth_qc = race_p,
         armed_qc = suspectsweapon) %>%
  rename(pd = agencyname) %>% 
  mutate(pd = if_else(is.na(pd) & police == TRUE, "Orlando Police Department", pd),
         pd = if_else(pd == "Orlando Police", "Orlando Police Department", pd),
         pd = if_else(pd == "orlando police", "Orlando Police Department", pd),
         pd = if_else(pd == "Orlando PD", "Orlando Police Department", pd, missing = "Unknown")) %>% 
  #Clean up race_eth_qc
  unite(race_eth_c, c(race_c, hispanicorlatinoorigin), remove = FALSE) %>% 
  mutate(race_eth_qc = if_else(is.na(race_eth_qc) & crowd == TRUE, race_eth_c, race_eth_qc)) %>%
  mutate(race_eth_qc = if_else(is.na(race_eth_qc), "Unknown", race_eth_qc)) %>% 
  mutate(race_eth_qc = fct_collapse(race_eth_qc,
                                    "B" = c("B", "Black", 
                                            "Black or African American_Unknown",
                                            "Black or African American_Not of Hispanic or Latino origin"),
                                    "L" = c("L", "Hispanic/Latino", "H"),
                                    "O" = c("Native American"),
                                    "W" = c("W", "White", "White_Not of Hispanic or Latino origin"),
                                    "U" = c("Summary", "Unknown_Unknown"))) %>% 
  #Clean up armed_qc
  mutate(armed_qc = if_else(is.na(armed_qc) & crowd == TRUE, weapon, armed_qc)) %>%
  select(date, date_qc, name, name_qc, match, match_qc, outcome, outcome_qc, race_eth_qc, armed_qc, everything()) %>% 
  #Remove Cases That Are Not Dallas PD
  filter(pd == "Orlando Police Department") %>%
  #Alter qc details for karvas gamble case (cs spelling and wrong date)
  mutate(date_qc = if_else(date == "2013-01-17" & name =="karvis gamble", as.Date("2013-01-16"), date_qc),
         date_qc = if_else(date == "2013-01-30" & name =="karvis gamble", as.Date("2013-01-16"), date_qc),
         name_qc = if_else(date_qc == "2013-01-16" & name =="karvis gamble", "karvas gamble", name_qc),
         match_qc = if_else(date_qc == "2013-01-16" & name_qc =="karvas gamble", "yes_match", match_qc),
         outcome_qc = if_else(date_qc == "2013-01-16" & name_qc =="karvas gamble", "deceased", outcome_qc),
         race_eth_qc = if_else(date_qc == "2013-01-16" & name_qc =="karvas gamble", "B", as.character(race_eth_qc))) %>% 
  #Alter qc details for sadiki allwood case (unknown name by crowd)
  mutate(name_qc = if_else(date_qc == "2013-01-15" & name =="unknown", "sadiki allwood", name_qc),
         match_qc = if_else(date_qc == "2013-01-15" & name_qc =="sadiki allwood", "yes_match", match_qc),
         outcome_qc = if_else(date_qc == "2013-01-15" & name_qc =="sadiki allwood", "shot_alive", outcome_qc),
         race_eth_qc = if_else(date_qc == "2013-01-15" & name_qc =="sadiki allwood", "B", as.character(race_eth_qc))) %>% 
  #Alter qc details for marcus cull case (unknown name by crowd)
  mutate(name_qc = if_else(date_qc == "2011-05-06" & name =="unknown", "marcus cull", name_qc),
         match_qc = if_else(date_qc == "2011-05-06" & name_qc =="marcus cull", "yes_match", match_qc),
         outcome_qc = if_else(date_qc == "2011-05-06" & name_qc =="marcus cull", "shot_alive", outcome_qc),
         race_eth_qc = if_else(date_qc == "2011-05-06" & name_qc =="marcus cull", "B", as.character(race_eth_qc)),
         armed_qc = if_else(date_qc == "2011-05-06" & name_qc=="marcus cull", "Motor Vehicle", armed_qc)) %>% 
  #Alter qc details for cordaryl wilson (police missing)
  mutate(outcome_qc = if_else(date_qc == "2013-01-16" & name == "cordaryl wilson", "shot_alive", outcome_qc)) %>% 
  distinct(date_qc, name_qc, match_qc, race_eth_qc, armed_qc, outcome_qc) %>%
  arrange(match_qc) %>% 
  mutate(matches = (if_else(match_qc == "yes_match", 1, 0)),
         no_match_police = if_else(match_qc == "no_match_police_missing", 1, 0),
         no_match_crowd = if_else(match_qc == "no_match_crowd_missing", 1, 0))


mco_ds_df_deceased <- mco_ds_df_all %>% 
  filter(outcome_qc == "deceased")

mco_ds_df_nonfatal <- mco_ds_df_all %>% 
  filter(outcome_qc != "deceased")


#Create Desired DF Table
dsdf <- make_pd_df()[-1,] %>% 
  add_pd_case("Dallas Police Department - Fatal", summary_row(dfw_ds_df_deceased)) %>% 
  add_pd_case("Orlando Police Department - Fatal", summary_row(mco_ds_df_deceased)) %>% 
  add_pd_case("Dallas Police Department - Non Fatal", summary_row(dfw_ds_df_nonfatal)) %>% 
  add_pd_case("Orlando Police Department - Non Fatal", summary_row(mco_ds_df_nonfatal)) %>% 
  rename("Matched" = Matches,
         "Not Matched by Crowd" = No_Match_Crowd,
         "Not Matched by Police" = No_Match_Police)




#Save Table
save_stargazer("tables/table3.tex", as.data.frame(dsdf), header=FALSE, type='latex',
               font.size = "footnotesize",
               title = "Matched Reports of Fatal and Non-Fatal Officer Involved Shootings - Deadspin",
               notes  = "Notes: Match Metrics by Police Department, Matches and Mismatches by Source Displayed as Proportions",
               summary = FALSE)


#Analysis of Matches, Mismatches by Race and Armed Status
all_ds_df_deceased <- dfw_ds_df_all %>% 
  filter(outcome_qc == "deceased") %>% 
  select(-outcome_qc) 

all_ds_df_deceased <- rbind(dfw_ds_df_deceased, mco_ds_df_deceased) %>% 
  select(-outcome_qc) 




#Create Variable Error Table
#Create Desired DF Table
make_errors_table <- function(df) {
  errors_table <- make_var_df()[-1,] %>% 
    #Race/Ethnicity
    add_pd_case("White", summary_row_var_val(df, "race_eth_qc", "W")) %>%
    add_pd_case("Black", summary_row_var_val(df, "race_eth_qc", "B")) %>% 
    add_pd_case("Hispanic/Latino", summary_row_var_val(df, "race_eth_qc", "L")) %>%
    add_pd_case("Asian", summary_row_var_val(df, "race_eth_qc", "A")) %>%
    add_pd_case("Native American", summary_row_var_val(df, "race_eth_qc", "N")) %>% 
    #Race Missing
    add_pd_case("Missing Race/Ethnicity", summary_row_var_val(df, "race_eth_qc", "MISSING")) %>%   
    #Armed/Unarmed-Undetermined Bin
    add_pd_case("Unarmed or Undetermined", summary_row_var_val(df, "armed_bin", "Unarmed/Undetermined"))  %>%
    add_pd_case("Armed", summary_row_var_val(df, "armed_bin", "Armed"))  %>%
    #Armed Types
    add_pd_case("Firearm", summary_row_var_val(df, "armed_qc", "Firearm"))  %>% 
    add_pd_case("Air Gun", summary_row_var_val(df, "armed_qc", "Air Gun"))  %>% 
    add_pd_case("Toy Gun", summary_row_var_val(df, "armed_qc", "Toy Gun"))  %>% 
    add_pd_case("Taser", summary_row_var_val(df, "armed_qc", "Taser"))  %>%
    add_pd_case("Knife", summary_row_var_val(df, "armed_qc", "Knife"))  %>%
    add_pd_case("Vehicle", summary_row_var_val(df, "armed_qc", "Vehicle"))  %>% 
    add_pd_case("Other", summary_row_var_val(df, "armed_qc", "Other"))  %>% 
    #Armed Missing
    add_pd_case("Missing Weapon Type", summary_row_var_val(df, "armed_qc", "MISSING")) %>%   
    add_pd_case("Total", summary_row_var_val(df))  %>% 
    rename("Matched" = Matches,
           "Not Matched by Crowd" = No_Match_Crowd,
           "Not Matched by Police" = No_Match_Police)
  
  return(errors_table)
}



clean_error_data <- function(df) {
  error_data <- df %>%
    mutate(race_eth_qc = as.factor(if_else(is.na(race_eth_qc), "MISSING", as.character(race_eth_qc)))) %>%
    mutate(armed_qc = fct_collapse(armed_qc,
                                   "Firearm" = c("Firearm", "gun", "Assault Rifle", 
                                                 "Rifle", "Handgun", "Shotgun"),
                                   "Air Gun" = c("BB Gun", "Pellet Gun"),
                                   "Taser" = c("Taser"),
                                   "Toy Gun" =c("Toy Gun", "Replica Handgun"),
                                   "Other" = c("Screwdriver", "Blunt Object"),
                                   "Vehicle" = c("Vehicle", "Motor Vehicle"),
                                   "Unarmed" = c("Simulated Weapon", "Hands", "unarmed", "Hands/ASP"),
                                   "Undetermined" = c("undetermined"))) %>% 
    mutate(armed_bin = if_else(armed_qc != "Unarmed" & armed_qc != "Undetermined", "Armed", "Unarmed/Undetermined"))
  
  return(error_data)
}





error_all_deceased <- rbind(all_wp_df_deceased, all_gd_df_deceased, all_ds_df_deceased) %>%
  clean_error_data() %>% 
  make_errors_table()

#Save Table
save_stargazer("tables/table4.tex", as.data.frame(error_all_deceased), header=FALSE, type='latex',
               font.size = "footnotesize",
               title = "Matches and Mismatches of Fatal Shootings by Subject Race and Weapon Type",
               notes  = "Notes: Crowdsources: Washington Post, The Guardian, Deadspin. Police Departments: Dallas, Denver, Jacksonville, Orlando.",
               summary = FALSE)



error_wp_gd_deceased <- rbind(all_wp_df_deceased, all_gd_df_deceased) %>%
  clean_error_data() %>% 
  make_errors_table()

#Save Table
save_stargazer("tables/table5.tex", as.data.frame(error_wp_gd_deceased), header=FALSE, type='latex',
               font.size = "footnotesize",
               title = "Matches and Mismatches of Fatal Shootings by Subject Race and Weapon Type",
               notes  = "Notes: Crowdsources: Washington Post, The Guardian. Police Departments: Dallas, Denver, Jacksonville, Orlando.",
               summary = FALSE)






#Graph Data - Dallas
wp <- dfw_wp_df_deceased %>%
  mutate(outcome = "Fatal") %>% 
  mutate(source = "Washington Post")

gd <- dfw_gd_df_deceased %>%
  mutate(outcome = "Fatal") %>% 
  mutate(source = "The Guardian")
  
ds <- dfw_ds_df_all %>% 
  mutate(outcome = if_else(outcome_qc == "deceased", "Fatal", "Non-Fatal")) %>% 
  mutate(source = "Deadspin") %>% 
  select(-outcome_qc) 

gdf_dfw <- rbind(wp, gd, ds) %>% 
  mutate(outcome = factor(outcome),
         source = factor(source)) %>% 
  mutate(match_qc = factor(match_qc, 
      levels = c("no_match_police_missing", "no_match_crowd_missing", "yes_match"),
      labels = c( "Not Matched by Police", "Not Matched by Crowdsource", "Matched"))) %>% 
  mutate(pd = "Dallas Police Department")


#Graph - Dallas
ggplot(gdf_dfw) +
  geom_bar(aes(source, fill = match_qc), alpha=1, position = "fill") +
  facet_grid(.~outcome) +
  scale_fill_manual(values=c("#2174B0", "#093E63", "#BF1200")) +
  xlab("Crowd Source") +
  ylab("Proportion of Matches and Non Matches by Type") +
  labs(title = "Matched Reports of Officer Involved Shootings",
       subtitle = "Dallas Police Department, 2011 - 2018") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
ggsave("images/plt1.png", width = 8, height = 4.8)


#Graph Data - Denver
wp <- den_wp_df_deceased %>%
  mutate(outcome = "Fatal") %>% 
  mutate(source = "Washington Post")

gd <- den_gd_df_deceased %>%
  mutate(outcome = "Fatal") %>% 
  mutate(source = "The Guardian")


gdf_den <- rbind(wp, gd) %>% 
  mutate(outcome = factor(outcome),
         source = factor(source)) %>% 
  mutate(match_qc = factor(match_qc, 
                           levels = c("no_match_police_missing", "no_match_crowd_missing", "yes_match"),
                           labels = c( "Not Matched by Police", "Not Matched by Crowdsource", "Matched"))) %>% 
  mutate(pd = "Denver Police Department")


#Graph - Denver
ggplot(gdf_den) +
  geom_bar(aes(source, fill = match_qc), alpha=1, position = "fill") +
  facet_grid(.~outcome) +
  scale_fill_manual(values=c("#2174B0", "#093E63", "#BF1200")) +
  xlab("Crowd Source") +
  ylab("Proportion of Matches and Non Matches by Type") +
  labs(title = "Matched Reports of Officer Involved Shootings",
       subtitle = "Denver Police Department, 2015 - 2018") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("images/plt2.png", width = 8, height = 4.8)


#Graph Data - Jacksonville
wp <- jax_wp_df_deceased %>%
  mutate(outcome = "Fatal") %>% 
  mutate(source = "Washington Post")

gd <- jax_gd_df_deceased %>%
  mutate(outcome = "Fatal") %>% 
  mutate(source = "The Guardian")


gdf_jax <- rbind(wp, gd) %>% 
  mutate(outcome = factor(outcome),
         source = factor(source)) %>% 
  mutate(match_qc = factor(match_qc, 
                           levels = c("no_match_police_missing", "no_match_crowd_missing", "yes_match"),
                           labels = c( "Not Matched by Police", "Not Matched by Crowdsource", "Matched"))) %>% 
  mutate(pd = "Jacksonville Sheriff's Office")


#Graph - Jacksonville Sheriff's Office
ggplot(gdf_jax) +
  geom_bar(aes(source, fill = match_qc), alpha=1, position = "fill") +
  facet_grid(.~outcome) +
  scale_fill_manual(values=c("#2174B0", "#093E63", "#BF1200")) +
  xlab("Crowd Source") +
  ylab("Proportion of Matches and Non Matches by Type") +
  labs(title = "Matched Reports of Officer Involved Shootings",
       subtitle = "Jacksonville Sheriff's Office, 2015 - 2016") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("images/plt3.png", width = 8, height = 4.8)




#Graph Data - Orlando
wp <- mco_wp_df_deceased %>%
  mutate(outcome = "Fatal") %>% 
  mutate(source = "Washington Post")

gd <- mco_gd_df_deceased %>%
  mutate(outcome = "Fatal") %>% 
  mutate(source = "The Guardian")

ds <- mco_ds_df_all %>% 
  mutate(outcome = if_else(outcome_qc == "deceased", "Fatal", "Non-Fatal")) %>% 
  mutate(source = "Deadspin") %>% 
  select(-outcome_qc) 

gdf_mco <- rbind(wp, gd, ds) %>% 
  mutate(outcome = factor(outcome),
         source = factor(source)) %>% 
  mutate(match_qc = factor(match_qc, 
                           levels = c("no_match_police_missing", "no_match_crowd_missing", "yes_match"),
                           labels = c( "Not Matched by Police", "Not Matched by Crowdsource", "Matched"))) %>% 
  mutate(pd = "Orlando Police Department")


#Graph - Orlando
ggplot(gdf_mco) +
  geom_bar(aes(source, fill = match_qc), alpha=1, position = "fill") +
  facet_grid(.~outcome) +
  scale_fill_manual(values=c("#2174B0", "#093E63", "#BF1200")) +
  xlab("Crowd Source") +
  ylab("Proportion of Matches and Non Matches by Type") +
  labs(title = "Matched Reports of Officer Involved Shootings",
       subtitle = "Orlando Police Department, 2011 - 2018") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggsave("images/plt4.png", width = 8, height = 4.8)





#Combined Graphs
base_combine <- rbind(gdf_dfw, gdf_den, gdf_jax, gdf_mco)
duplicate_all <- base_combine %>%
  mutate(pd = "All Police Departments")
gdf_combined <- rbind(base_combine, duplicate_all) %>% 
  filter(outcome == "Fatal")


by_pd <- ggplot(gdf_combined) +
  geom_bar(aes(source, fill = match_qc), alpha=1, position = "fill") +
  facet_grid(.~pd) +
  scale_fill_manual(values=c("#2174B0", "#093E63", "#BF1200")) +
  xlab("Crowd Source") +
  ylab("Proportion of Matches and Non Matches by Type") +
  labs(title = "Matched Reports of Officer Involved Shootings",
       caption = "Dallas Police Department, 2011 - 2018\nDenver Police Department, 2015 - 2016\nJacksonville Sheriff's Office 2015-2016\nOrlando Police Department, 2011-2018") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("images/plt_combined.png", width = 8, height = 4.8)



#Additional way to combine graphs
# ggarrange(by_pd, combined_pd,
#           common.legend = TRUE, legend = "bottom") 
# ggsave("images/combined.png", width = 8, height = 4.8)
