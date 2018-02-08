print("[*] merging ois reports...")

#Load Libraries
library(tidyverse)
library(lubridate)

#Load merge sources
source("merge_profile.R")

#pd <- 'oha'
police_glob <- paste0(pd, "*police_ois_report*cleaned.csv")

#cs <- 'wp'
crowd_glob <- paste0(cs, "*crowdsource_ois_report*cleaned.csv")


#Load DF's
pdf <- read_csv(Sys.glob(police_glob))
cdf <- read_csv(Sys.glob(crowd_glob))

#TODO
#need to define the cleaning scripts as functions
#call only certain functions based on merge profile

#####################
#make functions

police_test <- function(pd){
  if(pd == 'dfw') police <- "dallas"
  if(pd == 'oha') police <- "chicago"  #insert police <- clean_chicago()

  return(police)  
}

police_test(pd)

crowd_test <- function(cs){
  if(cs == 'wp') crowd <- "washington post"
  if(cs == 'gv') crowd <- "gun violence"  #crowd function
  
  return(crowd)  
}

crowd_test(cs)


#Filter DF's
police <- pdf %>%
    mutate(police = TRUE,
           date = mdy(date)) %>%
    filter(outcome == 'deceased',
           date >=  '2015-01-01') %>%
    select(case, date, name, everything()) %>%
    rename(race_p = race,
           gender_p = gender)

crowd <- cdf %>%
  mutate(crowd = TRUE) %>%
  mutate(date = as.Date(date)) %>% 
  filter(date >=  '2015-01-01') %>% 
  filter(city == 'Dallas',
         state == 'TX') %>%
  select(date, name, everything())
  #select(date, name, city, everything()) %>%
  # rename(race_c = race,
  #        gender_c = gender)

#Merge DF's
joined <- full_join(police, crowd, by = c('name', 'date')) %>%
  select(date, name, police, crowd, original, everything()) %>%
  arrange(date)

print(joined)
#Write Results
outfile <- paste(pd, cs, 'matched_police_crowdsource_files.csv', sep = '_')
write_csv(joined, outfile)
system(paste("open", outfile, ";", sep = " "))


