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

dallas_pd <- function(pdf, start_date, end_date, ois_type){
  
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
  
  if(missing(start_date)) start_date = '2011-01-01'
  if(missing(end_date)) end_date = '2015-01-01'
  if(missing(ois_type) || ois_type=='all'){
    df <- pdf
    type <- "All types of OIS"
  } else if (ois_type=='non_fatal'){
    df <- pdf %>% filter(outcome !='deceased')
    type <- "All non-fatal types of OIS"
  } else {
    df <- pdf %>% filter(outcome == ois_type)
    type <- ois_type
  }
  
  df <- df %>%
    mutate(police = TRUE,
           date = mdy(date)) %>%
    filter(date >=  start_date,
           date < end_date) %>% 
    select(case, date, name, everything()) %>%
    rename(race_p = race,
           gender_p = gender)
    
    print(paste("Dallas Police:", start_date, "-", end_date, type, sep = " "))
    return(df)
}




#################################################
#Functions
#Load Crowd Source Data
#################################################

washington_post_cs <- function(cdf, start_date, end_date, .city, .state){
  if(missing(start_date)) start_date = '2011-01-01'
  if(missing(end_date)) end_date = '2015-01-01'
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
    filter(date >=  start_date,
           date < end_date) %>% 
    select(date, name, city, everything()) %>%
    rename(race_c = race,
           gender_c = gender)
  
  print(paste("Washington Post:", start_date, "-", end_date, location, sep = " "))
  return(df)
}


guardian_cs <- function(cdf, start_date, end_date, .city, .state){
  
  #make date column from month, day, year
  df <- cdf %>% unite(date, c(month, day, year), sep = "-", remove = FALSE)
  
  if(missing(start_date)) start_date = '2015-01-01'
  if(missing(end_date)) end_date = '2017-01-01'
  if(missing(.city) && missing(.state)){ 
    df <- cdf
    location <- "All cities, states"
  } else if(missing(.city) || missing(.state)){
    warning("[*] warning, please specify a city and state")
  } else {
    location <- paste(.city, .state, sep = " ")
    .city = "Dallas"
    .state = "TX"
    df <- df %>% filter(city == .city,
                         state == .state)
  }
  
  df <- df %>%
    mutate(crowd = TRUE) %>%
    mutate(date = mdy(date)) %>% 
    filter(date >=  start_date,
           date < end_date) %>% 
    select(date, name, city, everything()) %>%
    rename(race_c = race,
           gender_c = gender)
  
  print(paste("Guardian:", start_date, "-", end_date, location, sep = " "))
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
  
  print(paste("Deadspin:", start_date, "-", end_date, location, sep = " "))
  return(df)
}


gun_violence_cs <- function(cdf, start_date, end_date, .city, .state){
  
  #need to preprocess city, state before if else
  df <- cdf %>%
    #TODO consider better address parsing
    separate(address, into = c('city', 'state', 'country', 'other'), 
             sep = ", ", remove = FALSE, extra = 'merge', fill = 'right') %>% 
    separate(state, into = c('.state', 'zip'), 
             sep = " ", remove = FALSE, extra = 'merge', fill = 'right') %>%
    rename(org_state = state,
           state = .state) 
  
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
    mutate(date = as.Date(date)) %>% 
    filter(date >=  start_date,
           date < end_date) %>% 
    select(date, name, city, everything()) %>%
    rename(race_c = race,
           gender_c = gender)
  
  print(paste("Gun Violence:", start_date, "-", end_date, location, sep = " "))
  return(df)
}



#################################################
#Functions
#Select Police Department and Crowdsource
#################################################

police_select <- function(pd, start_date, end_date, outcome='all'){
  if(pd == 'dfw') police <- dallas_pd(pdf, start_date, end_date, outcome)
  
  return(police)  
}

police_citystate <- function(pd){
  if(pd == 'dfw') citystate <- list("Dallas", "TX")
  
  return(citystate)  
}


crowd_select <- function(cs, citystate){
  if(cs == 'wp') {
    start_date <- '2015-01-01' #earliest wp data
    end_date <- '2017-05-01' #latest dfw is 04/2017
    crowd <- washington_post_cs(cdf, start_date, end_date, citystate[[1]], citystate[[2]])
  }
  if(cs == 'gd') {
    start_date <- '2015-01-01' #earliest gd data
    end_date <- '2017-01-01' #latest gd data is in 2016
    crowd <- guardian_cs(cdf, start_date, end_date, citystate[[1]], citystate[[2]])
  }
  if(cs == 'gv') {
    start_date <- '2015-01-01'
    end_date <- '2017-05-01'
    crowd <- gun_violence_cs(cdf, start_date, end_date, citystate[[1]], citystate[[2]])
  } 
  if(cs == 'ds') {
    start_date <- '2011-01-01' #dallas data good, ds data many events
    end_date <- '2015-01-01' #dallas data good, ds data many events
    crowd <- deadspin_cs(cdf, start_date, end_date, citystate[[1]], citystate[[2]])
  }
  
  return(list(crowd, start_date, end_date))  
}



#################################################
#Load and Join
#################################################

#Load Requested Police and Crowd Sources

#Police Citystate
citystate <- police_citystate(pd)


#Crowd Profile
crowd_profile <- crowd_select(cs, citystate)
crowd <- crowd_profile[[1]]
start_date <- crowd_profile[[2]]
end_date <- crowd_profile[[3]]

#Police Profile
police <- police_select(pd, start_date, end_date, ois_type)

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

