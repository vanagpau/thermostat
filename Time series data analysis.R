#Thermostat control time series data

library(tidyverse)
library(data.table)
library(psych)
library(gridExtra)
library(lubridate)
library(vroom)

#Big data - with unit interactions
setwd("E:/R files")
#Get a sample of the raw interaction data
big_crescent <- vroom ("FilteredLog_CRESCENT.csv", 
                              col_select = c(1:6), col_names = c(
                                "Time stamp", "Node", "Unix time", "Status flag", "Set point", "Air temp"))
big_warneford <- vroom ("FilteredLog_WARNEFORD.csv", 
                               col_select = c(1:6), col_names = c(
                                 "Time stamp", "Node", "Unix time", "Status flag", "Set point", "Air temp"))

#Read in the Irus data (hourly, no interaction)
irus_data <- vroom ("IrusData - Energy data Crescent and Warneford.csv", 
                    col_names = TRUE, col_select = c(1:5, 7, 10, 11, 25, 26))
#Filter for room heaters only (takes out water heaters, kitchens etc)
irus_data <- irus_data %>% filter(Type == "Room Heater") %>% arrange(Name)

as.factor(irus_data$Name)
as.factor(irus_data$Site)

#Merge the date and time fields
irus_data$date_time <- as.POSIXct(paste(irus_data$Date, irus_data$Time))

#Merge the Site and Room Name fields
irus_data$site_room <- paste(irus_data$Site, irus_data$Name)
#Remove any spaces (to ensure matching)
irus_data$site_room <- gsub('\\s+', '', irus_data$site_room)

#Controversially ignore all the default settings (19 and 21 defaults)
# <- irus_data %>% filter(Setpoint != 21 & hour(date_time) >= 7 & hour(date_time) <= 10)

#Calc mean room temp before and after posters on 14/2 for each room, append to irus_data
irus_data <- irus_data %>% mutate(avg_setpoint_before = ifelse(Date > as.Date(
  "2020-02-01") & Date < as.Date("2020-02-14"), mean(Setpoint), NA))
irus_data <- irus_data %>% mutate(avg_setpoint_after = ifelse(Date > as.Date(
  "2020-02-14") & Date < as.Date("2020-02-28"), mean(Setpoint), NA))

irus_data <- irus_data %>% mutate(sub19_before = ifelse(Date > as.Date(
  "2020-02-01") & Date < as.Date("2020-02-14") & Setpoint <19, Setpoint, NA))
irus_data <- irus_data %>% mutate(sub19_after = ifelse(Date > as.Date(
  "2020-02-14") & Date < as.Date("2020-02-28") & Setpoint <19, Setpoint, NA))

#Calculate number of settings below 19 degrees, before and after 14th Feb (posters date)

irus_data %>% filter(Date > as.Date("2020-02-01") & Date < as.Date(
  "2020-02-14")) %>% filter(Setpoint<19) %>% group_by(site_room) %>% count(
    name = "sub19_before")
irus_data %>% filter(Date > as.Date("2020-02-14") & Date < as.Date(
  "2020-02-28")) %>% filter(Setpoint<19) %>% group_by(site_room) %>% count(
    name = "sub19_after")


#PLOTS

#Histograms of each room setpoints (v air temp)
irus_data %>% group_by(Name, Setpoint) %>% count() %>% filter(Name == "A01A") %>% ggplot(
  mapping = aes(x = Setpoint, y = n)) + geom_col() + xlim(18, 24) + ggtitle("A01A")
irus_data %>% group_by(Name, Setpoint) %>% count() %>% filter(Name == "A01B") %>% ggplot(
  mapping = aes(x = Setpoint, y = n)) + geom_col() + xlim(18, 24) + ggtitle("A01B")
irus_data %>% group_by(Name, Setpoint) %>% count() %>% filter(Name == "A01C") %>% ggplot(
  mapping = aes(x = Setpoint, y = n)) + geom_col() + xlim(18, 24) + ggtitle("A01C")

#Plot of Setpoint
plotA01A <- irus_data %>% filter(Name == "A01A") %>% ggplot () + geom_point(
  mapping = aes(x = date_time, y = `Temp Air`, colour = "Air")) + ylim(16,24) + geom_point(
    mapping = aes(x = date_time, y = Setpoint, colour = "Setpoint"))

plotA01B <- irus_data %>% filter(Name == "A01B") %>% ggplot () + geom_point(
  mapping = aes(x = date_time, y = `Temp Air`, colour = "Air")) + ylim(16,24) + geom_point(
    mapping = aes(x = date_time, y = Setpoint, colour = "Setpoint"))

plotA01C <- irus_data %>% filter(Name == "A01C") %>% ggplot () + geom_point(
  mapping = aes(x = date_time, y = `Temp Air`, colour = "Air")) + ylim(16,24) + geom_point(
    mapping = aes(x = date_time, y = Setpoint, colour = "Setpoint"))
grid.arrange(plotA01A, plotA01B, plotA01C, nrow = 3)



