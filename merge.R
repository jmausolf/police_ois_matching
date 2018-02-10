#Load Libraries
library(tidyverse)
library(lubridate)

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
  if(missing(start_date)) start_date = '2015-01-01'
  if(missing(ois_type)){
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




#################################################
#Functions
#Select Police Department and Crowdsource
#################################################

police_select <- function(pd){
  if(pd == 'dfw') police <- dallas_pd(pdf, '2015-01-01', 'deceased')
  if(pd == 'oha') police <- "chicago"  #insert police <- clean_chicago()
  
  return(police)  
}


crowd_select <- function(cs){
  if(cs == 'wp') crowd <- washington_post_cs(cdf, '2015-01-01', 'Dallas', 'TX')
  if(cs == 'gv') crowd <- "gun violence"  #crowd function
  
  return(crowd)  
}



#################################################
#Load and Join
#################################################

#Load Requested Police and Crowd Sources
police <- police_select(pd)
crowd <- crowd_select(cs)

#Merge DF's
print("[*] merging ois reports...")
joined <- full_join(police, crowd, by = c('name', 'date')) %>%
  select(date, name, police, crowd, original, everything()) %>%
  arrange(date)

print(joined)
#Write Results
outfile <- paste0('data/', paste(pd, cs, 'matched_police_crowdsource_files.csv', sep = '_'))
write_csv(joined, outfile)
system(paste("open", outfile, ";", sep = " "))


