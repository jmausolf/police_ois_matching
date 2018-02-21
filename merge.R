#Load Libraries
library(tidyverse)
library(lubridate)
library(forcats)

#Load merge sources
source("merge_profile.R")

#Select files
police_glob <- paste0('downloads/', pd, "*police_ois_report*cleaned.csv")
crowd_glob <- paste0('downloads/', cs, "*crowdsource_ois_report*cleaned.csv")

#Load DF's
pdf <- read_csv(Sys.glob(police_glob))
cdf <- read_csv(Sys.glob(crowd_glob))


#################################################
#Functions
#Load Police Departments Functions
#################################################

dallas_pd <- function(pdf, start_date, ois_type){
  
  #clean types
  df <- pdf %>% 
    mutate(det_outcome = outcome,
           outcome = fct_recode(outcome,
           "deceased" = "1 deceased 1 injured",
           "deceased" = "deceased injured",
           "deceased" = "deceased",
           "injured"  = "injured",
           "injured"  = "2 injured",
           "shootmiss" = "shoot and miss",
           "other" = "other"))
  
  if(missing(start_date)) start_date = '2015-01-01'
  if(missing(ois_type) || ois_type=='all'){
    df <- pdf
    type <- "All types of OIS"
  } else {
    df <- pdf %>% filter(outcome == ois_type)
    type <- ois_type
  }
  
  df <- df %>%
    mutate(police = TRUE,
           date = mdy(date)) %>%
    filter(date >=  start_date) %>%
    select(case, date, name, everything()) %>%
    rename(race_p = race,
           gender_p = gender)
    
    print(paste("Dallas Police:", start_date, type, sep = " "))
    return(df)
}




#################################################
#Functions
#Load Crowd Source Data
#################################################

washington_post_cs <- function(cdf, start_date, .city, .state){
  if(missing(start_date)) start_date = '2015-01-01'
  if(missing(.city) && missing(.state)){ 
    df <- cdf
    location <- "All cities, states"
  } else if(missing(.city) || missing(.state)){
    warning("[*] warning, please specify a city and state")
  } else {
    location <- paste(.city, .state, sep = " ")
    df <- cdf %>% filter(city == .city,
                         state == .state)
  }
  
  df <- df %>%
    mutate(crowd = TRUE) %>%
    mutate(date = as.Date(date)) %>% 
    filter(date >=  start_date) %>% 
    select(date, name, city, everything()) %>%
    rename(race_c = race,
           gender_c = gender)
  
  print(paste("Washington Post:", start_date, location, sep = " "))
  return(df)
}


deadspin_cs <- function(cdf, start_date, end_date, .city, .state){
  
  df <- cdf %>% rename(state = state_abv)
  
  if(missing(start_date)) start_date = '2011-01-01'
  if(missing(end_date)) end_date = '2015-01-01'
  if(missing(.city) && missing(.state)){ 
    df <- df
    location <- "All cities, states"
  } else if(missing(.city) || missing(.state)){
    warning("[*] warning, please specify a city and state")
  } else {
    location <- paste(.city, .state, sep = " ")
    df <- df %>% filter(city == .city,
                         state == .state)
  }
  

  df <- df %>%
    mutate(crowd = TRUE) %>%
    mutate(date = mdy(dateofincident)) %>% 
    filter(date >=  start_date,
           date < end_date) %>% 
    select(date, name, city, everything()) %>%
    rename(race_c = race,
           gender_c = victimsgender)
  
  print(paste("Deadspin:", start_date, end_date, location, sep = " "))
  return(df)
}


gun_violence_cs <- function(cdf, start_date, .city, .state){
  
  #need to preprocess city, state before if else
  df <- cdf %>%
    #TODO consider better address parsing
    separate(address, into = c('city', 'state', 'country', 'other'), 
             sep = ", ", remove = FALSE, extra = 'merge', fill = 'right') %>% 
    separate(state, into = c('.state', 'zip'), 
             sep = " ", remove = FALSE, extra = 'merge', fill = 'right') %>%
    rename(org_state = state,
           state = .state) 
  
  #if(missing(start_date)) start_date = '2015-01-01'
  if(missing(.city) && missing(.state)){ 
    df <- df
    location <- "All cities, states"
  } else if(missing(.city) || missing(.state)){
    warning("[*] warning, please specify a city and state")
  } else {
    location <- paste(.city, .state, sep = " ")
    df <- df %>% filter(city == .city,
                         state == .state)
  }
  
  df <- df %>%
    mutate(crowd = TRUE) %>%
    mutate(date = as.Date(date)) %>% 
    filter(date >=  start_date) %>% 
    select(date, name, city, everything()) %>%
    rename(race_c = race,
           gender_c = gender)
  
  print(paste("Gun Violence:", start_date, location, sep = " "))
  return(df)
}



#################################################
#Functions
#Select Police Department and Crowdsource
#################################################

police_select <- function(pd, outcome='all'){
  if(pd == 'dfw') police <- dallas_pd(pdf, '2011-01-01', outcome)
  if(pd == 'oha') police <- "chicago"  #insert police <- clean_chicago()
  
  return(police)  
}


crowd_select <- function(cs){
  if(cs == 'wp') crowd <- washington_post_cs(cdf, '2015-01-01', 'Dallas', 'TX')
  if(cs == 'gv') crowd <- gun_violence_cs(cdf, '2015-01-01', 'Dallas', 'TX')
  if(cs == 'ds') crowd <- deadspin_cs(cdf, '2011-01-01', '2015-01-01', 'Dallas', 'TX')
  
  return(crowd)  
}



#################################################
#Load and Join
#################################################

#Load Requested Police and Crowd Sources
police <- police_select(pd, ois_type)
crowd <- crowd_select(cs)

#Merge DF's
print("[*] merging ois reports...")
joined <- full_join(police, crowd, by = c('name', 'date')) %>%
  select(date, name, police, crowd, everything()) %>%
  arrange(police, date)

print(joined)
#Write Results
outfile <- paste0('data/', paste(pd, cs, ois_type, 'matched_police_crowdsource_files.csv', sep = '_'))
write_csv(joined, outfile)
#system(paste("open", outfile, ";", sep = " "))

