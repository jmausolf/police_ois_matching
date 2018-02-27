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

