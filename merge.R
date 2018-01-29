print("[*] merging ois reports...")

#Load Libraries
library(tidyverse)
library(lubridate)

#Load DF's
pdf <- read_csv("dfw_police_ois_report_2018-01-28_cleaned.csv")
cdf <- read_csv("wp_crowdsource_ois_report_2018-01-28_cleaned.csv")

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
  filter(date >=  '2015-01-01',
         city == 'Dallas',
         state == 'TX') %>% 
  select(date, name, city, everything()) %>% 
  rename(race_c = race,
         gender_c = gender)

#Merge DF's
joined <- full_join(police, crowd, by = c('name', 'date')) %>% 
  select(date, name, police, crowd, original, everything()) %>% 
  arrange(date)

print(joined)
#Write Results
outfile <- "dfw_wp_matched_police_crowdsource_files.csv"
write_csv(joined, outfile)
system(paste("open", outfile, ";", sep = " "))


