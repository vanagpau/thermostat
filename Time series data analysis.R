#Thermostat control time series data

library(tidyverse)
library(data.table)
library(psych)

#Big data - with unit interactions
setwd("/home/vanagpau/Documents/PSYC 7007 Dissertation/DATA/Raw thermostat data 24th March/Data for Paul Vanags")
sample_big_crescent <- fread ("FilteredLog_CRESCENT.csv", fill = TRUE, nrows = 2000, 
                              select = c(1:6), col.names = c(
                                "Time stamp", "Node", "Unix time", "Status flag", "Set point", "Air temp"))
sample_big_warneford <- fread ("FilteredLog_WARNEFORD.csv", fill = TRUE, nrows = 2000, 
                               select = c(1:6), col.names = c(
                                 "Time stamp", "Node", "Unix time", "Status flag", "Set point", "Air temp"))
#as_tibble (crescent)
#as_tibble (warneford)
#rbind (crescent, warneford)


#Irus data (hourly, no interaction)
sample_irus_crescent <- fread("IrusData - Energy data Crescent and Warneford.csv", 
                         header = TRUE, nrows = 1000, select = c(1:5, 10, 11, 25, 26))
sample_irus_warneford <- fread("IrusData - Energy data Crescent and Warneford.csv", 
                         header = FALSE, skip = "WARNEFORD", nrows = 1000, select = c(1:5, 10, 11, 25, 26))
sample_irus <- rbind(sample_irus_crescent, sample_irus_warneford, use.names = FALSE)
