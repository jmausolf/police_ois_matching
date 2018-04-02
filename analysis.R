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
pd_types <- c("dfw", "den")
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
  avgmat = sprintf(mean(df[["matches"]]), fmt = '%#.2f')
  avgcmis = sprintf(mean(df[["no_match_crowd"]]), fmt = '%#.2f')
  avgpmis = sprintf(mean(df[["no_match_police"]]), fmt = '%#.2f')  
  
  nmat = sum(df[["matches"]] == 1)
  ncmis = sum(df[["no_match_crowd"]] == 1)
  npmis = sum(df[["no_match_police"]] == 1)
  
  avg_matches = paste0(avgmat, " (", nmat, ")")
  avg_crowd_missing = paste0(avgcmis, " (", ncmis, ")")
  avg_police_missing = paste0(avgpmis, " (", npmis, ")")
  
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
                                    "B" = c("B", "Black", "Dwayne B"),
                                    "L" = c("L", "Hispanic/Latino"))) %>% 
  #Clean up armed_qc
  mutate(armed_qc = if_else(is.na(armed_qc) & crowd == TRUE, armed, armed_qc)) %>%
    select(date, date_qc, name, name_qc, police, crowd, match, match_qc, outcome, uof, everything()) %>% 
    #Missing police files could be out of jurisdiction, but WP does not have the pd responsible
    filter(!(uof !=  "shot" & match != "yes_match")) %>% 
    #Alter qc details for elias portillo case
    mutate(name_qc = if_else(date == "2016-08-25" & name =="unknown", "elias portillo", name_qc)) %>% 
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
                                    "B" = c("B", "Black", "Dwayne B"),
                                    "L" = c("L", "Hispanic/Latino", "H"),
                                    "O" = c("Native American"))) %>% 
  #Clean up armed_qc
  mutate(armed_qc = if_else(is.na(armed_qc) & crowd == TRUE, armed, armed_qc)) %>%
  mutate(armed_qc = fct_collapse(armed_qc,
                                    "Firearm" = c("Firearm", "Gun", "gun"),
                                    "Motor Vehicle" = c("Motor Vehicle", "Vehicle", "vehicle"))) %>% 
  #Alter qc details for jessica hernandez case (wp used nick-name versus full name)
  mutate(name_qc = if_else(date == "2015-01-26" & name =="jessie hernandez", "jessica hernandez", name_qc),
         match_qc = if_else(date_qc == "2015-01-26" & name_qc =="jessica hernandez", "yes_match", match_qc)) %>% 
  #Alter qc details for gerardino cayetano-gonzalez case (police mispelled name)
  mutate(name_qc = if_else(date == "2016-02-22" & name =="garardino cayetano-gonzalez", "gerardino cayetano gonzalez", name_qc),
         match_qc = if_else(date_qc == "2016-02-22" & name_qc =="gerardino cayetano gonzalez", "yes_match", match_qc)) %>% 
  #Alter qc details for dion damon case (wp mispelled name)
  mutate(name_qc = if_else(date == "2016-04-12" & name =="dion daman", "dion damon", name_qc),
         match_qc = if_else(date_qc == "2016-04-12" & name_qc =="dion damon", "yes_match", match_qc), 
         #defer to police for race and weapon
         race_eth_qc = if_else(date_qc == "2016-04-12" & name_qc =="dion damon", "L", as.character(race_eth_qc)),
         armed_qc = if_else(date_qc == "2016-04-12" & name_qc =="dion damon", "Simulated Weapon", as.character(armed_qc))) %>% 
  #Alter qc details for miguel angel martinez case (wp uses middle name, den pd does not)
  mutate(name_qc = if_else(date == "2015-11-22" & name =="miguel martinez", "miguel angel martinez", name_qc),
         match_qc = if_else(date_qc == "2015-11-22" & name_qc =="miguel angel martinez", "yes_match", match_qc)) %>% 
  select(date_qc, name_qc, match_qc, race_eth_qc, race_p, ethnicity_p, race_c, armed_qc, armed, armedwith) %>% 
  distinct(date_qc, name_qc, match_qc, race_eth_qc, armed_qc) %>%
  mutate(matches = (if_else(match_qc == "yes_match", 1, 0)),
         no_match_police = if_else(match_qc == "no_match_police_missing", 1, 0),
         no_match_crowd = if_else(match_qc == "no_match_crowd_missing", 1, 0))


#Combined Data, All Departments
all_wp_df_deceased <- rbind(dfw_wp_df_deceased, den_wp_df_deceased)


#Create Desired DF Table
wpdf <- make_pd_df()[-1,] %>% 
  add_pd_case("Dallas Police Department", summary_row(dfw_wp_df_deceased)) %>% 
  add_pd_case("Denver Police Department", summary_row(den_wp_df_deceased)) %>% 
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
                                    "B" = c("B", "Black", "Dwayne B"),
                                    "L" = c("L", "Hispanic/Latino", "H"),
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
  mutate(name_qc = if_else(date == "2016-02-22" & name =="garardino cayetano-gonzalez", "gerardino cayetano gonzalez", name_qc),
         name_qc = if_else(date == "2016-02-22" & name =="gerardino cayetano-gonzalez", "gerardino cayetano gonzalez", name_qc),
         match_qc = if_else(date_qc == "2016-02-22" & name_qc =="gerardino cayetano gonzalez", "yes_match", match_qc)) %>% 
  select(date_qc, name_qc, match_qc, race_eth_qc, race_p, ethnicity_p, race_c, armed_qc, armed, armedwith) %>% 
  distinct(date_qc, name_qc, match_qc, race_eth_qc, armed_qc) %>%
  mutate(matches = (if_else(match_qc == "yes_match", 1, 0)),
         no_match_police = if_else(match_qc == "no_match_police_missing", 1, 0),
         no_match_crowd = if_else(match_qc == "no_match_crowd_missing", 1, 0))
  
#Combined Data, All Departments
all_gd_df_deceased <- rbind(dfw_gd_df_deceased, den_gd_df_deceased)

#Create GD Matches Table
gddf <- make_pd_df()[-1,] %>% 
  add_pd_case("Dallas Police Department", summary_row(dfw_gd_df_deceased)) %>% 
  add_pd_case("Denver Police Department", summary_row(den_gd_df_deceased)) %>% 
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







#DS Cleaning
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
                                    "B" = c("B", "Black", "Dwayne B", "Ellis_Lenny B", 
                                            "Black or African American_Unknown",
                                            "Black or African American_Not of Hispanic or Latino origin"),
                                    "L" = c("L", "Hispanic/Latino", "H", "Jr. L"),
                                    "O" = c("Native American"),
                                    "W" = c("W", "White", "White_Not of Hispanic or Latino origin"))) %>% 
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
  mutate(name_qc = if_else(date == "2012-08-03" & is.na(name), "roberto gabaldon", name_qc),
         name_qc = if_else(date == "2012-12-14" & is.na(name), "lenny ellis", name_qc)) %>% 
  #Alter qc details for roberto gabaldon case
  mutate(match_qc = if_else(date_qc == "2012-08-03" & name_qc =="roberto gabaldon", "yes_match", match_qc),
         outcome_qc = if_else(name_qc == "roberto gabaldon", "injured", outcome_qc)) %>% 
  #Alter qc details for stoney eugene rawlinson case
  mutate(date_qc = if_else(date == "2013-02-09" & name =="stoney eugene rawlinson", as.Date("2013-02-08"), date_qc),
         name_qc = if_else(date == "2013-02-08" & name =="rawlison stoney", "stoney rawlinson", name_qc),
         name_qc = if_else(date == "2013-02-09" & name =="stoney eugene rawlinson", "stoney rawlinson", name_qc),
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

#Create Desired DF Table
dsdf <- make_pd_df()[-1,] %>% 
  add_pd_case("Dallas Police Department - Fatal", summary_row(dfw_ds_df_deceased)) %>% 
  add_pd_case("Dallas Police Department - Non Fatal", summary_row(dfw_ds_df_nonfatal)) %>% 
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

#Pretable Cleaning
dem_errors <- rbind(all_wp_df_deceased, all_gd_df_deceased, all_ds_df_deceased) %>% 
  mutate(armed_qc = fct_collapse(armed_qc,
                                    "Firearm" = c("Firearm", "gun", "Assault Rifle", 
                                                  "Rifle", "Handgun", "Shotgun"),
                                    "Air Gun" = c("BB Gun", "Pellet Gun"),
                                    "Taser" = c("Taser"),
                                    "Other" = c("Screwdriver"),
                                    "Vehicle" = c("Vehicle", "Motor Vehicle"),
                                    "Unarmed" = c("Simulated Weapon", "physical attack", "Hands"))) 


#Create Variable Error Table
#Create Desired DF Table
errors_table <- make_var_df()[-1,] %>% 
  add_pd_case("White", summary_row_var_val(dem_errors, "race_eth_qc", "W")) %>%
  add_pd_case("Black", summary_row_var_val(dem_errors, "race_eth_qc", "B")) %>% 
  add_pd_case("Hispanic/Latino", summary_row_var_val(dem_errors, "race_eth_qc", "L")) %>%
  add_pd_case("Native American", summary_row_var_val(dem_errors, "race_eth_qc", "N")) %>% 
  add_pd_case("Unarmed", summary_row_var_val(dem_errors, "armed_qc", "Unarmed"))  %>%
  add_pd_case("Armed", summary_row_var_val(dem_errors, "armed_qc", "Unarmed", negate = TRUE))  %>% 
  add_pd_case("Firearm", summary_row_var_val(dem_errors, "armed_qc", "Firearm"))  %>% 
  add_pd_case("Air Gun", summary_row_var_val(dem_errors, "armed_qc", "Air Gun"))  %>% 
  add_pd_case("Taser", summary_row_var_val(dem_errors, "armed_qc", "Taser"))  %>%
  add_pd_case("Knife", summary_row_var_val(dem_errors, "armed_qc", "Knife"))  %>%
  add_pd_case("Vehicle", summary_row_var_val(dem_errors, "armed_qc", "Vehicle"))  %>% 
  add_pd_case("Other", summary_row_var_val(dem_errors, "armed_qc", "Other"))  %>% 
  add_pd_case("Total", summary_row_var_val(dem_errors))  %>% 
  rename("Matched" = Matches,
         "Not Matched by Crowd" = No_Match_Crowd,
         "Not Matched by Police" = No_Match_Police)


#Save Table
save_stargazer("tables/table4.tex", as.data.frame(errors_table), header=FALSE, type='latex',
               font.size = "footnotesize",
               title = "Matches and Mismatches of Fatal Shootings by Subject Race and Weapon Type",
               notes  = "Notes: Crowdsources: Washington Post, The Guardian, Deadspin",
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
       subtitle = "Dallas Police Department, 2011 - 2017") +
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
  scale_fill_manual(values=c("#BF1200")) +
  xlab("Crowd Source") +
  ylab("Proportion of Matches and Non Matches by Type") +
  labs(title = "Matched Reports of Officer Involved Shootings",
       subtitle = "Denver Police Department, 2015 - 2016") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))
ggsave("images/plt2.png", width = 8, height = 4.8)


#Combined Graphs
base_combine <- rbind(gdf_dfw, gdf_den)
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
       caption = "Dallas Police Department, 2011 - 2017\nDenver Police Department, 2015 - 2016") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("images/plt3.png", width = 8, height = 4.8)



#Additional way to combine graphs
# ggarrange(by_pd, combined_pd,
#           common.legend = TRUE, legend = "bottom") 
# ggsave("images/combined.png", width = 8, height = 4.8)
