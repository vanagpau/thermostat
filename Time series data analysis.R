#Thermostat control time series data

library(tidyverse)
library(data.table)
library(psych)
library(vroom)

#Big data - with unit interactions
setwd("/home/vanagpau/Documents/PSYC 7007 Dissertation/DATA/Raw thermostat data 24th March/Data for Paul Vanags")
sample_big_crescent <- vroom ("FilteredLog_CRESCENT.csv", n_max = 50000, 
                              col_select = c(1:6), col_names = c(
                                "Time stamp", "Node", "Unix time", "Status flag", "Set point", "Air temp"))
sample_big_warneford <- vroom ("FilteredLog_WARNEFORD.csv", n_max = 50000, 
                               col_select = c(1:6), col_names = c(
                                 "Time stamp", "Node", "Unix time", "Status flag", "Set point", "Air temp"))

#Irus data (hourly, no interaction)
irus_data <- vroom ("IrusData - Energy data Crescent and Warneford.csv", 
                              col_names = TRUE, col_select = c(1:5, 10, 11, 25, 26))
