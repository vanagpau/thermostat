library(tidyverse)
library(data.table)
library(psych)
library(standardize)
library(gridExtra)
library(corrplot)
library(GPArotation)
library(RColorBrewer)
library(lubridate)
library(vroom)
library(fasttime)
library(sjPlot)
library(naniar)
library(nFactors)
library(TTR)
library(tidyquant)
library(lme4)
library(apaTables)


setwd("E:/R files")
#For Laptop work setwd("/home/vanagpau/R/thermostat")
#Load Values and Environmental Behaviours (VEB) Questionnaire
VEB <- fread ("VEB.csv")
as_tibble (VEB)


#Create filter for all completed and non-preview surveys: cs ('completed surveys')
cs <- VEB %>% filter(Status == "IP Address", Progress == "100")
  
#Replace Likert strings with numbers as characters
cs <- cs %>% lapply(gsub, pattern = "Strongly agree", replacement = 3)
cs <- cs %>% lapply(gsub, pattern = "Agree", replacement = 2)
cs <- cs %>% lapply(gsub, pattern = "Somewhat agree", replacement = 1)
cs <- cs %>% lapply(gsub, pattern = "Neither agree nor disagree", replacement = 0)
cs <- cs %>% lapply(gsub, pattern = "Somewhat disagree", replacement = -1)
cs <- cs %>% lapply(gsub, pattern = "Disagree", replacement = -2)
cs <- cs %>% lapply(gsub, pattern = "Strongly disagree", replacement = -3)
cs <- cs %>% lapply(gsub, pattern = "7 = Very likely", replacement = 7)
cs <- cs %>% lapply(gsub, pattern = "4 = Neither likely nor unlikely", replacement = 4)
cs <- cs %>% lapply(gsub, pattern = "1 = Not at all likely", replacement = 1)
cs <- cs %>% lapply(gsub, pattern = "5 = Very much", replacement = 5)
cs <- cs %>% lapply(gsub, pattern = "1 = Not at all", replacement = 1)
cs <- cs %>% lapply(gsub, pattern = "1 = Very conservative", replacement = 1)
cs <- cs %>% lapply(gsub, pattern = "7 = Very liberal", replacement = 7)

#Convert Crescent & Warneford to characters
cs$Q3 <- as.character(cs$Q3)

#Convert Age data to numeric
cs$Q34 <- as.numeric(cs$Q34)

#Coerce list back to tibble
cs <- as_tibble(cs)

#Rename halls to match with thermostat data
cs[cs$Q3 == "Crescent Hall", "Q3"] <- as.character("CRESCENT")
cs[cs$Q3 == "Warneford Hall", "Q3"] <- as.character("WARNEFORD")
#Combine columns to produce matchable room string
cs$site_room <- paste(cs$Q3, cs$Q4, cs$Q21)
#Remove spaces for matching
cs$site_room <- gsub('\\s+', '', cs$site_room)
#Convert to factor
cs$site_room <- as_factor(cs$site_room)

#Convert char strings to numerics for col 15-111 (survey main body)
cs [,15:111] <- as_tibble(
  sapply(cs[,15:111], as.numeric, USE.NAMES = FALSE))
cs [,115] <- as_tibble(
  sapply(cs[,115], as.numeric, USE.NAMES = FALSE))

#Define function to reverse scored items
reverse5 <- function (x) recode(x,'5' = 1, '4' = 2, `2` = 4, '1'=5) 
reverse7 <- function (x) recode(x, '3'=-3, `2` = -2, `1` = -1, `-1` = 1, `-2` = 2, '-3'=3) 

#Apply reverse scoring to NEP
cs[c(16, 18, 20, 22, 24, 26, 28)] <- lapply (
  cs[c(16, 18, 20, 22, 24, 26, 28)], reverse7)

#Apply reverse scoring to EAI
#Original: cs[c(31,33,35,37,38,41,42,44,47,49,51,53)] <- lapply (cs[c(31,33,35,37,38,41,42,44,47,49,51,53)], reverse7)
cs[c(31,33,35,36,39,41,43,44,46,48,51,53)] <- lapply (
  cs[c(31,33,35,36,39,41,43,44,46,48,51,53)], reverse7)

#Apply reverse scoring to BSCS
cs[c(100,101,102,103,105,107,108,110,111)] <- lapply (
  cs[c(100,101,102,103,105,107,108,110,111)], reverse5)

#Compute EAI mean
cs <- cs %>% rowwise() %>% mutate(EAI_mean = mean(c(
  Q6_1, Q6_2, Q6_3, Q6_4, Q6_5, Q6_7, Q6_8, Q6_9, Q6_10, Q6_11, Q6_12, Q6_13,
  Q6_14, Q6_15, Q6_16, Q6_17, Q6_18, Q6_19, Q6_20, Q6_21, Q6_22, Q6_23, Q6_24), na.rm = TRUE)) %>% ungroup()

#Create scales
NEP <- cs %>% select(Q5_1:Q5_15)
EAI <- cs %>% select(Q6_1:Q6_24)
BSCS <- cs %>% select(Q11_1:Q11_13)
likelyPEB <- cs %>% select(Q7_1:Q7_22)
thermo_moral <- cs %>% select(Q8_7:Q8_9)
moral_ascoop <- cs %>% select(Q9_1:Q9_7)
moral_found <- cs %>% select(Q10_1:Q10_7)
moralisation <- cs %>% select(c(Q8_7, Q8_9))

#Create Activist and Pragmatist variables from likely PEB scale
cs <- cs %>% rowwise() %>% mutate(
  PEB_activist = mean(c(Q7_2, Q7_3, Q7_4, Q7_5, Q7_6, Q7_8, Q7_9, Q7_10, Q7_11), na.rm = TRUE))
cs <- cs %>% rowwise() %>% mutate(
  PEB_pragmatist = mean(c(
    Q7_12, Q7_13, Q7_14, Q7_15, Q7_16, Q7_17, Q7_18, Q7_19, Q7_20, Q7_21, Q7_22), na.rm = TRUE))

#Calculate overall scores for NEP, BSCS, likelyPEB
#creates vector of means..
cs <- cbind(cs, NEP_mean = rowMeans(NEP, na.rm = TRUE))
cs <- cbind(cs, likelyPEB_mean = rowMeans(likelyPEB, na.rm = TRUE))
cs <- cbind(cs, BSCS_mean = rowMeans(BSCS, na.rm = TRUE))
cs <- cbind(cs, thermo_moral_mean = rowMeans(thermo_moral, na.rm = TRUE))
cs <- cbind(cs, MAC_mean = rowMeans(moral_ascoop, na.rm = TRUE))
cs <- cbind(cs, MFT_mean = rowMeans(moral_found, na.rm = TRUE))

#Rename column names to descriptives: cs %>% rename(new = old)
cs <- cs %>% rename('Political orientation' = Q35_1)
cs <- cs %>% rename('Habit' = Q8_2)
cs <- cs %>% rename('SocialNorm' = Q8_3)
cs <- cs %>% rename('PBC' = Q8_5)
cs <- cs %>% rename('AwarenessConsequences' = Q8_1)
cs <- cs %>% rename('Intention' = Q8_6)
cs <- cs %>% rename('AscriptionResponsibility' = Q8_4)

#Replace 0's with NA in Age
cs$Q34[cs$Q34 == 0] <- NA

#Centre Age
cs$Q34 <- cs$Q34 - mean(cs$Q34, na.rm = TRUE)

#LOAD IN THE THERMOSTAT DATA

#Get a sample of the raw interaction data
# big_crescent <- vroom ("FilteredLog_CRESCENT.csv",
#                                col_select = c(1:6), col_names = c(
#                                 "Time stamp", "Node", "Unix time", "Status flag", "Set point", "Air temp"))
# big_warneford <- vroom ("FilteredLog_WARNEFORD.csv",
                                # col_select = c(1:6), col_names = c(
                                #   "Time stamp", "Node", "Unix time", "Status flag", "Set point", "Air temp"))
#extract a sample for testing/manipulation
# sample1000 <- sample_n(big_warneford, 1000)
# sample1000$`Time stamp` <- as.POSIXct(bigWsample$`Time stamp`, format = '%d/%m/%Y-%H:%M:%S')
# sample1000

# big_warneford$`Time stamp` <- as.POSIXct(big_warneford$`Time stamp`, format = '%d/%m/%Y-%H:%M:%S')
# big_warneford
# #extract a specific set for comparison with IRUS
# test29feb <- big_warneford %>% filter(`Time stamp` > "2020-02-29 17:00:00" & `Time stamp` < 
#                                         "2020-02-29 20:00:00", Node == 111)
# test29feb

#Read in the Irus data (hourly, without interactions)
irus_data <- vroom("IrusData - Energy data Crescent and Warneford.csv",
  col_names = TRUE, col_select = c(1:5, 7, 10, 11, 25, 26))

#Filter for student room heaters only (takes out water heaters, kitchens, offices etc)
irus_data <- irus_data %>%
  filter(Type == "Room Heater", Name != "Office", nchar(Name) <= 6) %>%
  arrange(Name)

irus_data$Site <- as.factor(irus_data$Site)

#Load external temperature data (source: Weather Online)
external_temp <- tibble(fread("Oxford temp data.csv") %>%
  rename(Ext_temp_celsius = `Temp 2m [C]`) %>%
  select(Date, Ext_temp_celsius))
external_temp$Date <- as.Date(external_temp$Date, "%d/%m/%Y")

irus_data <- left_join(irus_data, external_temp, by = "Date")

#Merge the Site and Room Name fields
irus_data$site_room <- paste(irus_data$Site, irus_data$Name)

#Remove any spaces (to ensure matching)
irus_data$site_room <- gsub('\\s+', '', irus_data$site_room)

#Merge the date and time fields
irus_data$date_time <- fastPOSIXct(paste(irus_data$Date, irus_data$Time))

#Convert rooms to factors (for graphing)
irus_data$site_room <- as.factor(irus_data$site_room)

#Add daily mean setpoint per room and daily average per room into dataframe

irus_data <- irus_data %>% group_by(site_room, Date) %>% 
  mutate(daily_mean_sp = mean(Setpoint, na.rm = TRUE),
         daily_airtemp = mean(`Temp Air`, na.rm = TRUE))

irus_data <- left_join(irus_data, irus_data %>% 
                         group_by(site_room, Date) %>% 
                         filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
                         filter(Setpoint != 19) %>% mutate(daily_mean_sp_excdflt = mean(
                           Setpoint, na.rm = TRUE)))

irus_data <- left_join(irus_data, irus_data %>% 
                         group_by(site_room) %>% 
                         filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
                         filter(Setpoint != 19) %>% mutate(mean_sp_excdflt = mean(
                           Setpoint, na.rm = TRUE)))

#HYPOTHESIS 4 - add Before & After tags

irus_data$intervention <- NA
attach(irus_data)
irus_data$intervention[Date < as.Date("2020-02-14")] <- "Before"
irus_data$intervention[Date > as.Date("2020-02-14")] <- "After"
detach(irus_data)

#Add EXCLUDE filter for peaks
exclude <- irus_data %>% filter((Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) |
(Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01"))) %>%
  group_by(date_time ,site_room ) %>%
  summarise(temp = Setpoint) %>%  summarise(temp = mean(temp, na.rm = TRUE)) %>% 
  arrange(desc(temp)) %>% filter (temp > 20) %>% pull(date_time)

# Add EXCLUDE flag to Irus dataframe - 0 = ones to ignore; 1 = ones to keep
irus_data <- irus_data %>% ungroup() %>% mutate(exclude = (ifelse(irus_data$date_time %in% exclude, 0, 1)))



#Calc mean room temp before and after posters (14th Feb) for each room & append to irus_data
irus_data <- left_join (irus_data, irus_data %>% 
  filter (Date > as.Date("2020-01-30") & Date < as.Date(  "2020-02-14")) %>% 
  group_by(site_room) %>% 
  summarise(avg_setpoint_before = mean(Setpoint)), by = "site_room")

irus_data <- left_join (irus_data, irus_data %>%
 filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01")) %>%
   group_by(site_room) %>%
   summarise(avg_setpoint_after = mean(Setpoint)), by = "site_room")

#Calculate with cleaned data - exclude peaks
irus_data <- left_join (irus_data, irus_data %>% 
  filter (Date > as.Date("2020-01-30") & Date < as.Date(  "2020-02-14")) %>% filter (exclude == 1) %>%
  group_by(site_room) %>% 
  summarise(avg_setpoint_beforeCL = (mean(Setpoint) - 19)), by = "site_room")

irus_data <- left_join (irus_data, irus_data %>%
 filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01")) %>% filter (exclude == 1) %>%
   group_by(site_room) %>%
   summarise(avg_setpoint_afterCL = (mean(Setpoint) - 19)), by = "site_room")

irus_data <- left_join (irus_data, irus_data %>%
 filter(Date > as.Date("2020-01-30") & Date < as.Date("2020-03-01")) %>% filter (exclude == 1) %>%
   group_by(site_room) %>%
   summarise(avg_setpointCL = (mean(Setpoint) - 19)), by = "site_room")

 
# #add sub19 settings to Irus data
# irus_data <- irus_data %>% mutate (sub19_before = ifelse(Date > as.Date(
#   "2020-01-30") & Date < as.Date("2020-02-14") & Setpoint <19, Setpoint, NA))
# 
# irus_data <- irus_data %>% mutate (sub19_after = ifelse(Date > as.Date(
#   "2020-02-14") & Date < as.Date("2020-03-01") & Setpoint <19, Setpoint, NA))
# 
# #Add before and after settings +/- 3 days and +/- 7 days INC defaults
# 
# irus_data <- left_join (irus_data, irus_data %>% 
#   filter (Date > as.Date("2020-02-10") & Date < as.Date(  "2020-02-14")) %>% 
#   group_by(site_room) %>% 
#   summarise(avg_setpoint_before3 = mean(Setpoint)), by = "site_room")
# 
# irus_data <- left_join (irus_data, irus_data %>%
#  filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-02-18")) %>%
#    group_by(site_room) %>%
#    summarise(avg_setpoint_after3 = mean(Setpoint)), by = "site_room")
# 
# irus_data <- left_join (irus_data, irus_data %>% 
#   filter (Date > as.Date("2020-02-06") & Date < as.Date(  "2020-02-14")) %>% 
#   group_by(site_room) %>% 
#   summarise(avg_setpoint_before7 = mean(Setpoint)), by = "site_room")
# 
# irus_data <- left_join (irus_data, irus_data %>%
#  filter (Date > as.Date("2020-02-14") & Date < as.Date("2020-02-22")) %>%
#    group_by(site_room) %>%
#    summarise(avg_setpoint_after7 = mean(Setpoint)), by = "site_room")
# 



#Calculate before and after average thermostat set points EXC. default settings + add to irus_data
irus_data <- left_join (irus_data, irus_data %>% 
  filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>% 
    filter(Setpoint != 19) %>% 
  filter(Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) %>%
  group_by(site_room) %>% 
  summarise(avg_sp_before_excdflt = mean(Setpoint)), by = "site_room")

irus_data <- left_join (irus_data, irus_data %>% 
  filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
filter(Setpoint != 19) %>% filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01")) %>%
  group_by(site_room) %>% 
  summarise(avg_sp_after_excdflt = mean(Setpoint)), by = "site_room")


#Add EXC defaults, EXC peaks data to Irus
irus_data <- left_join (irus_data, irus_data %>% 
  filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>% 
    filter(Setpoint != 19) %>% 
  filter(Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) %>% filter(exclude == 1) %>%
  group_by(site_room) %>% 
  summarise(avg_sp_before_excdfltCL = mean(Setpoint)), by = "site_room")

irus_data <- left_join (irus_data, irus_data %>% 
  filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
filter(Setpoint != 19) %>% filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01")) %>% 
  filter(exclude == 1) %>%
  group_by(site_room) %>% 
  summarise(avg_sp_after_excdfltCL = (mean(Setpoint)-19)), by = "site_room")

#MEANS
irus_data %>% 
  filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>% 
    filter(Setpoint != 19) %>% 
  filter(Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) %>% filter(exclude == 1) %>% 
  summarise(avg_sp_before_excdfltCL = (mean(Setpoint)-19)) %>% as.data.frame()

irus_data %>% 
  filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>% 
    filter(Setpoint != 19) %>% 
  filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01")) %>% filter(exclude == 1) %>% 
  summarise(avg_sp_before_excdfltCL = (mean(Setpoint)-19)) %>% as.data.frame()


# 
# 
# #Add 3 day exc defaults measure
# 
# irus_data <- left_join (irus_data, irus_data %>% 
#   filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>% 
#     filter(Setpoint != 19) %>% 
#   filter(Date > as.Date("2020-02-10") & Date < as.Date("2020-02-14")) %>%
#   group_by(site_room) %>% 
#   summarise(avg_sp_before_excdflt3 = mean(Setpoint)), by = "site_room")
# 
# irus_data <- left_join (irus_data, irus_data %>% 
#   filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
# filter(Setpoint != 19) %>% filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-02-18")) %>%
#   group_by(site_room) %>% 
#   summarise(avg_sp_after_excdflt3 = mean(Setpoint)), by = "site_room")
# 
# #Add 7 day exc defaults measure
# 
# irus_data <- left_join (irus_data, irus_data %>% 
#   filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>% 
#     filter(Setpoint != 19) %>% 
#   filter(Date > as.Date("2020-02-06") & Date < as.Date("2020-02-14")) %>%
#   group_by(site_room) %>% 
#   summarise(avg_sp_before_excdflt7 = mean(Setpoint)), by = "site_room")
# 
# irus_data <- left_join (irus_data, irus_data %>% 
#   filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
# filter(Setpoint != 19) %>% filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-02-22")) %>%
#   group_by(site_room) %>% 
#   summarise(avg_sp_after_excdflt7 = mean(Setpoint)), by = "site_room")
# 
# 
# 


# 
# irus_data <- irus_data %>% mutate (thermo_change = avg_setpoint_after - avg_setpoint_before)
# irus_data <- irus_data %>% mutate (sub19_change = sub19_after - sub19_before)

irus_data <- irus_data %>% mutate (thermo_change_excdflt = avg_sp_after_excdflt - avg_sp_before_excdflt)

#Calculate number of settings below 19 degrees, before and after 14th Feb (posters date)

#Add data from irus_data tibble to data-frame 'cs' (questionnaire data)

# cs <- left_join(cs, irus_data %>% filter(Date > as.Date("2020-01-30") & Date < as.Date(
#   "2020-02-14")) %>% filter(Setpoint<19) %>% group_by(site_room) %>% count(
#     name = "sub19_before"), by = "site_room") %>% mutate_all(~replace(., is.na(.), 0))
# 
# cs <- left_join(cs, irus_data %>% filter(Date > as.Date("2020-02-14") & Date < as.Date(
#   "2020-03-01")) %>% filter(Setpoint<19) %>% group_by(site_room) %>% count(
#     name = "sub19_after"), by = "site_room") %>% mutate_all(~replace(., is.na(.), 0))

#Add thermostat data inc defaults to CS
cs <- left_join(cs, irus_data %>% 
    filter(Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) %>% 
    group_by(site_room) %>% 
    summarise(avg_setpoint_before = mean(Setpoint)), by = "site_room")

cs <- left_join(cs, irus_data %>% 
                  filter(Date > as.Date("2020-02-14") & Date < as.Date(  "2020-03-01")) %>%
                  group_by(site_room) %>%
                  summarise(avg_setpoint_after = mean(Setpoint)), by = "site_room")


cs <- left_join(cs, irus_data %>% 
    filter(Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) %>% filter(exclude == 1) %>%
    group_by(site_room) %>% 
    summarise(avg_setpoint_beforeCL = (mean(Setpoint)-19)), by = "site_room")

cs <- left_join(cs, irus_data %>% 
                  filter(Date > as.Date("2020-02-14") & Date < as.Date(  "2020-03-01")) %>% filter(exclude == 1) %>%
                  group_by(site_room) %>%
                  summarise(avg_setpoint_afterCL = (mean(Setpoint)-19)), by = "site_room")

cs <- left_join(cs, irus_data %>% 
                  filter(Date > as.Date("2020-01-30") & Date < as.Date(  "2020-03-01")) %>% filter(exclude == 1) %>%
                  group_by(site_room) %>%
                  summarise(avg_setpointCL = (mean(Setpoint)-19)), by = "site_room")


#Add thermostat data exc defaults

cs <- left_join (cs, irus_data %>% 
  filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
filter(Setpoint != 19) %>% filter(Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) %>%
  group_by(site_room) %>% 
  summarise(avg_sp_before_excdflt = mean(Setpoint)), by = "site_room")

cs <- left_join (cs, irus_data %>% 
  filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
filter(Setpoint != 19) %>% filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01")) %>%
  group_by(site_room) %>% 
  summarise(avg_sp_after_excdflt = mean(Setpoint)), by = "site_room")

cs <- cs %>% mutate(avg_sp_before_excdflt19 = avg_sp_before_excdflt - 19,
                    avg_sp_after_excdflt19 = avg_sp_after_excdflt - 19)

#Add data exc dflt & exc peaks (cleaned)
cs <- left_join (cs, irus_data %>% 
  filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
filter(Setpoint != 19) %>% filter(Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) %>%
  filter(exclude == 1) %>%
  group_by(site_room) %>% 
  summarise(avg_sp_before_excdfltCL = (mean(Setpoint)-19)), by = "site_room")

cs <- left_join (cs, irus_data %>% 
  filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
filter(Setpoint != 19) %>% filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01")) %>%
  filter(exclude ==1) %>%
  group_by(site_room) %>% 
  summarise(avg_sp_after_excdfltCL = (mean(Setpoint)-19)), by = "site_room")

cs <- left_join (cs, irus_data %>% 
  filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
filter(Setpoint != 19) %>% filter(Date > as.Date("2020-01-30") & Date < as.Date("2020-03-01")) %>%
  filter(exclude ==1) %>%
  group_by(site_room) %>% 
  summarise(avg_sp_overall_excdfltCL = (mean(Setpoint)-19)), by = "site_room")



#Add data at 3 days and 7 days

# cs <- left_join (cs, irus_data %>% 
#   filter (Date > as.Date("2020-02-10") & Date < as.Date(  "2020-02-14")) %>% 
#   group_by(site_room) %>% 
#   summarise(avg_setpoint_before3 = mean(Setpoint)), by = "site_room")
# 
# cs <- left_join (cs, irus_data %>%
#  filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-02-18")) %>%
#    group_by(site_room) %>%
#    summarise(avg_setpoint_after3 = mean(Setpoint)), by = "site_room")
# 
# cs <- left_join (cs, irus_data %>% 
#   filter (Date > as.Date("2020-02-06") & Date < as.Date(  "2020-02-14")) %>% 
#   group_by(site_room) %>% 
#   summarise(avg_setpoint_before7 = mean(Setpoint)), by = "site_room")
# 
# cs <- left_join (cs, irus_data %>%
#  filter (Date > as.Date("2020-02-14") & Date < as.Date("2020-02-22")) %>%
#    group_by(site_room) %>%
#    summarise(avg_setpoint_after7 = mean(Setpoint)), by = "site_room")
# 
# mean(cs$avg_setpoint_before3, na.rm = TRUE)
# mean(cs$avg_setpoint_after3, na.rm = TRUE)
# mean(cs$avg_setpoint_before7, na.rm = TRUE)
# mean(cs$avg_setpoint_after7, na.rm = TRUE)




#Add 3 day exc defaults measure to df cs

# cs <- left_join (cs, irus_data %>% 
#   filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>% 
#     filter(Setpoint != 19) %>% 
#   filter(Date > as.Date("2020-02-10") & Date < as.Date("2020-02-14")) %>%
#   group_by(site_room) %>% 
#   summarise(avg_sp_before_excdflt3 = mean(Setpoint)), by = "site_room")
# 
# cs <- left_join (cs, irus_data %>% 
#   filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
# filter(Setpoint != 19) %>% filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-02-18")) %>%
#   group_by(site_room) %>% 
#   summarise(avg_sp_after_excdflt3 = mean(Setpoint)), by = "site_room")
# 
# 
# #Add 7 day exc defaults measure to df cs
# 
# cs <- left_join (cs, irus_data %>% 
#   filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>% 
#     filter(Setpoint != 19) %>% 
#   filter(Date > as.Date("2020-02-06") & Date < as.Date("2020-02-14")) %>%
#   group_by(site_room) %>% 
#   summarise(avg_sp_before_excdflt7 = mean(Setpoint)), by = "site_room")
# 
# cs <- left_join (cs, irus_data %>% 
#   filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
# filter(Setpoint != 19) %>% filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-02-22")) %>%
#   group_by(site_room) %>% 
#   summarise(avg_sp_after_excdflt7 = mean(Setpoint)), by = "site_room")
# 



#Add change variable
cs <- cs %>% mutate(thermo_changeCL = avg_setpoint_afterCL - avg_setpoint_beforeCL)
cs <- cs %>% mutate (thermo_change_excdfltCL = avg_sp_after_excdfltCL - avg_sp_before_excdfltCL)





#Remove actual (Irus) data from duplicate room L04F - replace with NAs
#cs[(which(grepl("L04F", cs$Q4))),(match("sub19_before",names(cs))):(
#  match("daily_airtemp_2020-03-13",names(cs)))] <- NA

#Standardise key variables
cs <- cs %>% mutate(likelyPEB_mean_sz = as.numeric(scale(likelyPEB_mean)))
cs <- cs %>% mutate(EAI_mean_sz = as.numeric(scale(EAI_mean)))
cs <- cs %>% mutate(NEP_mean_sz = as.numeric(scale(NEP_mean)))
cs <- cs %>% mutate(PEB_activist_sz = as.numeric(scale(PEB_activist)))
cs <- cs %>% mutate(PEB_pragmatist_sz = as.numeric(scale(PEB_pragmatist)))
cs <- cs %>% mutate(thermo_moral_mean_sz = as.numeric(scale(thermo_moral_mean)))
cs <- cs %>% mutate(MAC_mean_sz = as.numeric(scale(MAC_mean)))
cs <- cs %>% mutate(MFT_mean_sz = as.numeric(scale(MFT_mean)))
# cs <- cs %>% mutate(sub19_before_sz = as.numeric(scale(sub19_before)))
# cs <- cs %>% mutate(sub19_after_sz = as.numeric(scale(sub19_after)))
cs <- cs %>% mutate(avg_setpoint_before_sz = as.numeric(scale(avg_setpoint_before)))
cs <- cs %>% mutate(avg_setpoint_after_sz = as.numeric(scale(avg_setpoint_after)))
cs <- cs %>% mutate(avg_sp_before_excdflt_sz = as.numeric(scale(avg_sp_before_excdflt)))
cs <- cs %>% mutate(avg_sp_after_excdflt_sz = as.numeric(scale(avg_sp_after_excdflt)))
cs <- cs %>% mutate(thermo_changeCL_sz = as.numeric(scale(thermo_changeCL)))
# cs <- cs %>% mutate(sub19_change_sz = as.numeric(scale(sub19_change)))
cs <- cs %>% mutate(thermo_change_excdfltCL_sz = as.numeric(scale(thermo_change_excdfltCL)))
cs <- cs %>% mutate(AwarenessConsequences_sz = as.numeric(scale(AwarenessConsequences)))
cs <- cs %>% mutate(Habit_sz = as.numeric(scale(Habit)))
cs <- cs %>% mutate(Intention_sz = as.numeric(scale(Intention)))
cs <- cs %>% mutate(SocialNorm_sz = as.numeric(scale(SocialNorm)))
cs <- cs %>% mutate(AscriptionResponsibility_sz = as.numeric(scale(AscriptionResponsibility)))
cs <- cs %>% mutate(PBC_sz = as.numeric(scale(PBC)))
cs <- cs %>% mutate(BSCS_mean_sz = as.numeric(scale(BSCS_mean)))
cs <- cs %>% mutate(PoliticalOrientation_sz = as.numeric(scale(`Political orientation`)))









#END OF DATA CLEAN and SETUP














####EXPORT FOR MODELLING

# main data file for longitudinal modelling (LARGE; 700k + rows)
write.csv(irus_data, file = "AdamIrusdata.csv")

# data file for matching effect to rooms (Small; 88 rows)
adam_data_cleaned <-left_join(cs, irus_data %>% filter(site_room %in% c(cs$site_room)) %>% filter (exclude == 1) %>%
    group_by(site_room, Date) %>% summarise(daily_mean_sp = mean(Setpoint, na.rm = TRUE), daily_airtemp = mean(
       `Temp Air`, na.rm = TRUE), daily_mean_sp_excdflt = mean(daily_mean_sp_excdflt, na.rm = TRUE),
       external = mean(Ext_temp_celsius)),
    by = "site_room")

write.csv(adam_data_cleaned, file = "Adam4nopeaks.csv")






#STATISTICS

#Calculate Cronbach's Alpha for questionnaire scales

alpha(NEP)
alpha(EAI)
alpha(BSCS)
alpha(likelyPEB)
alpha(thermo_moral)
alpha(moral_ascoop)
alpha(moral_found)
alpha(moralisation)

#Activist
cs %>% select(Q7_2, Q7_3, Q7_4, Q7_5, Q7_6, Q7_8, Q7_9, Q7_10, Q7_11) %>% alpha()
#Pragmatist
cs %>% select(Q7_12, Q7_13, Q7_14, Q7_15, Q7_16, Q7_17, Q7_18, Q7_19, Q7_20, Q7_21, Q7_22) %>% alpha()
#Descriptives
summary(cs$PEB_activist)
summary(cs$PEB_pragmatist)


#Shapiro-Wilk test on all scales
lapply(cs[132:138], shapiro.test)

#Descriptive stats on participants

mean(cs$Q34, na.rm = TRUE)
describe(cs$Q34)






#FACTOR ANALYSIS







#FA of Attitudes scale
cs %>% select(Q8_7:Q8_9) %>% cor() %>% fa.parallel(
  n.obs=88, main = "Parallel Analysis scree plot - Attitude scale")

#FA of likely PEB scale

#Assumption tests: KMO and Barlett's sphericity
cs %>% select(Q7_1:Q7_22) %>% cortest()
cs %>% select(Q7_1:Q7_22) %>% KMO()
#Parallel analysis suggests 2 factors
cs %>% select(Q7_1:Q7_22) %>% cor(
  use = "complete.obs") %>% fa.parallel(
    n.obs=88, main = "Parallel Analysis scree plot - likely PEB")
cs %>% select(Q7_1:Q7_22) %>% cor(
  use = "complete.obs") %>% fa(nfactors=2)

#FA of MAC scale
#Assumption tests: KMO and Barlett's sphericity
cs %>% select(Q9_1:Q9_7) %>% cortest()
cs %>% select(Q9_1:Q9_7) %>% KMO()

mMAC <- cs %>% select(Q9_1:Q9_7)

#Code from https://www.statmethods.net/advstats/factor.html
ev <- eigen(cor(mMAC)) # get eigenvalues
ap <- parallel(subject=nrow(mMAC),var=ncol(mMAC), rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

cs %>% select(Q9_1:Q9_7) %>% cor(use = "complete.obs") %>% fa()

#FA of MFT scale
#Assumption tests: KMO and Barlett's sphericity
cs %>% select(Q10_1:Q10_7) %>% cortest()
cs %>% select(Q10_1:Q10_7) %>% KMO()
#Parallel analysis suggests 1 factors
mMFT <- cs %>% select(Q10_1:Q10_7)

#Code from https://www.statmethods.net/advstats/factor.html
ev <- eigen(cor(mMFT)) # get eigenvalues
ap <- parallel(subject=nrow(mMFT),var=ncol(mMFT), rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

#FA of BSCS scale
#Assumption tests: KMO and Barlett's sphericity
cs %>% select(Q11_1:Q11_13) %>% cortest()
cs %>% select(Q11_1:Q11_13) %>% KMO()
#Parallel analysis suggests 1 factors
cs %>% select(Q11_1:Q11_13) %>% cor(
  use = "complete.obs") %>% fa.parallel(
    n.obs=88, main = "Parallel Analysis scree plot - Brief Self-Control Scale")




#Correlations EAI vs NEP
with(cs, cor.test(NEP_mean, likelyPEB_mean))
with(cs, cor.test(EAI_mean, likelyPEB_mean))
with(cs, cor.test(NEP_mean, thermo_moral_mean))
with(cs, cor.test(EAI_mean, thermo_moral_mean))

#Significance test of EAI vs NEP correlations vs likelyPEB
r.test(r12=(with(cs, cor(EAI_mean, likelyPEB_mean))), n=88, r34=(
  with(cs, cor(NEP_mean, likelyPEB_mean))), n2=88)

#Significance test of EAI vs NEP correlations vs actual thermostat control
r.test(r12=(with(cs, cor(EAI_mean, thermo_change_excdflt))), n=88, r34=(
  with(cs, cor(NEP_mean, thermo_change_excdflt))), n2=88)

#Correlations and significance compared to CADM

#Correlation of behavioural intention scales with one another

with(cs, cor.test(PEB_pragmatist, PEB_activist))




#Correlation of political orientation w. likely_PEB & thermo_change
with(cs, cor.test(cs$'Political orientation', -avg_sp_overall_excdfltCL))
with(cs, cor.test(cs$'Political orientation', PEB_activist))
with(cs, cor.test(cs$'Political orientation', PEB_pragmatist))
with(cs, cor.test(cs$'Political orientation', thermo_change))
with(cs, cor.test(cs$'Political orientation', sub19_change))

#Correlation Awareness of Consequences w. likely PEB
with(cs, cor.test(cs$'AwarenessConsequences', -avg_sp_overall_excdfltCL))
with(cs, cor.test(cs$'AwarenessConsequences', PEB_activist))
with(cs, cor.test(cs$'AwarenessConsequences', PEB_pragmatist))
#comparison with CADM Behaviour
r.test(n = 88, r12 = with(cs, cor(cs$'AwarenessConsequences', -avg_sp_overall_excdfltCL, use="complete.obs")), n2 = 13215, r34 = .22)
r.test(n = 88, r12 = (with(cs, cor(cs$'AwarenessConsequences', PEB_activist))), n2 = 13215, r34 = .22)
r.test(n = 88, r12 = (with(cs, cor(cs$'AwarenessConsequences', PEB_pragmatist))), n2 = 13215, r34 = .22)
#comparison with CADM Intention

r.test(n = 88, r12 = (with(cs, cor(cs$'AwarenessConsequences', PEB_activist))), n2 = 12464, r34 = .33)
r.test(n = 88, r12 = (with(cs, cor(cs$'AwarenessConsequences', PEB_pragmatist))), n2 = 12464, r34 = .33)




#Correlation Ascription of Responsibility w. likely PEB
with(cs, cor.test(cs$'AscriptionResponsibility', -avg_sp_overall_excdfltCL))
with(cs, cor.test(cs$'AscriptionResponsibility', PEB_activist))
with(cs, cor.test(cs$'AscriptionResponsibility', PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(cs$'AscriptionResponsibility', -avg_sp_overall_excdfltCL, use="complete.obs"))), n2 = 4217, r34 = .10)
r.test(n = 88, r12 = (with(cs, cor(cs$'AscriptionResponsibility', PEB_activist))), n2 = 4217, r34 = .10)
r.test(n = 88, r12 = (with(cs, cor(cs$'AscriptionResponsibility', PEB_pragmatist))), n2 = 4217, r34 = .10)
#comparison with CADM Intention

r.test(n = 88, r12 = (with(cs, cor(cs$'AscriptionResponsibility', PEB_activist))), n2 = 4784, r34 = .34)
r.test(n = 88, r12 = (with(cs, cor(cs$'AscriptionResponsibility', PEB_pragmatist))), n2 = 4784, r34 = .34)


#Correlation NEP w. likely PEB
with(cs, cor.test(NEP_mean, -avg_sp_overall_excdfltCL))
with(cs, cor.test(NEP_mean, PEB_activist))
with(cs, cor.test(NEP_mean, PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(NEP_mean, -avg_sp_overall_excdfltCL, use="complete.obs"))), n2 = 3499, r34 = .09)
r.test(n = 88, r12 = (with(cs, cor(NEP_mean, PEB_activist))), n2 = 3499, r34 = .09)
r.test(n = 88, r12 = (with(cs, cor(NEP_mean, PEB_pragmatist))), n2 = 3499, r34 = .09)
#comparison with CADM Intention

r.test(n = 88, r12 = (with(cs, cor(NEP_mean, PEB_activist))), n2 = 4077, r34 = .32)
r.test(n = 88, r12 = (with(cs, cor(NEP_mean, PEB_pragmatist))), n2 = 4077, r34 = .32)


#Correlation SocialNorm w. likely PEB
with(cs, cor.test(cs$'SocialNorm', -avg_sp_overall_excdfltCL))
with(cs, cor.test(cs$'SocialNorm', PEB_activist))
with(cs, cor.test(cs$'SocialNorm', PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(cs$'SocialNorm', -avg_sp_overall_excdfltCL, use="complete.obs"))), n2 = 14170, r34 = .24)
r.test(n = 88, r12 = (with(cs, cor(cs$'SocialNorm', PEB_activist))), n2 = 14170, r34 = .24)
r.test(n = 88, r12 = (with(cs, cor(cs$'SocialNorm', PEB_pragmatist))), n2 = 14170, r34 = .24)
#comparison with CADM Intention

r.test(n = 88, r12 = (with(cs, cor(cs$'SocialNorm', PEB_activist))), n2 = 16768, r34 = .47)
r.test(n = 88, r12 = (with(cs, cor(cs$'SocialNorm', PEB_pragmatist))), n2 = 16768, r34 = .47)


#Correlation Perceived Behavioural Control w. likely PEB
with(cs, cor.test(cs$'PBC', -avg_sp_overall_excdfltCL))
with(cs, cor.test(cs$'PBC', PEB_activist))
with(cs, cor.test(cs$'PBC', PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(cs$'PBC', -avg_sp_overall_excdfltCL, use="complete.obs"))), n2 = 15020, r34 = .40)
r.test(n = 88, r12 = (with(cs, cor(cs$'PBC', PEB_activist))), n2 = 15020, r34 = .40)
r.test(n = 88, r12 = (with(cs, cor(cs$'PBC', PEB_pragmatist))), n2 = 15020, r34 = .40)
#comparison with CADM Intention

r.test(n = 88, r12 = (with(cs, cor(cs$'PBC', PEB_activist))), n2 = 17489, r34 = .54)
r.test(n = 88, r12 = (with(cs, cor(cs$'PBC', PEB_pragmatist))), n2 = 17489, r34 = .54)


#Correlation Habits w. likely PEB
with(cs, cor.test(cs$'Habit', -avg_sp_overall_excdfltCL))
with(cs, cor.test(cs$'Habit', PEB_activist))
with(cs, cor.test(cs$'Habit', PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(cs$'Habit', -avg_sp_overall_excdfltCL, use="complete.obs"))), n2 = 7747, r34 = .46)
r.test(n = 88, r12 = (with(cs, cor(cs$'Habit', PEB_activist))), n2 = 7747, r34 = .46)
r.test(n = 88, r12 = (with(cs, cor(cs$'Habit', PEB_pragmatist))), n2 = 7747, r34 = .46)
#comparison with CADM Intention

r.test(n = 88, r12 = (with(cs, cor(cs$'Habit', PEB_activist))), n2 = 7319, r34 = .47)
r.test(n = 88, r12 = (with(cs, cor(cs$'Habit', PEB_pragmatist))), n2 = 7319, r34 = .47)



#Correlation Intentions w. likely PEB
with(cs, cor.test(cs$'Intention', -avg_sp_overall_excdfltCL))
with(cs, cor.test(cs$'Intention', PEB_activist))
with(cs, cor.test(cs$'Intention', PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(cs$'Intention', -avg_sp_overall_excdfltCL, use="complete.obs"
                                   ))), n2 = 12945, r34 = .55)
r.test(n = 88, r12 = (with(cs, cor(cs$'Intention', PEB_activist))), n2 = 12945, r34 = .55)
r.test(n = 88, r12 = (with(cs, cor(cs$'Intention', PEB_pragmatist))), n2 = 12945, r34 = .55)

#Correlation Attitudes w. likely PEB
with(cs, cor.test(thermo_moral_mean, -avg_sp_overall_excdfltCL))
with(cs, cor.test(thermo_moral_mean, PEB_activist))
with(cs, cor.test(thermo_moral_mean, PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(thermo_moral_mean, -avg_sp_overall_excdfltCL, use="complete.obs"))), n2 = 14053, r34 = .36)
r.test(n = 88, r12 = (with(cs, cor(thermo_moral_mean, PEB_activist))), n2 = 14053, r34 = .36)
r.test(n = 88, r12 = (with(cs, cor(thermo_moral_mean, PEB_pragmatist))), n2 = 14053, r34 = .36)
#comparison with CADM Intention

r.test(n = 88, r12 = (with(cs, cor(thermo_moral_mean, PEB_activist))), n2 = 16949, r34 = .62)
r.test(n = 88, r12 = (with(cs, cor(thermo_moral_mean, PEB_pragmatist))), n2 = 16949, r34 = .62)


#Correlation BSCS with likelyPEB
with(cs, cor.test(BSCS_mean, -avg_sp_overall_excdfltCL))
with(cs, cor.test(BSCS_mean, PEB_activist))
with(cs, cor.test(BSCS_mean, PEB_pragmatist))

#Correlation MAC w. likely PEB
with(cs, cor.test(MAC_mean, -avg_sp_overall_excdfltCL))
with(cs, cor.test(MAC_mean, PEB_activist))
with(cs, cor.test(MAC_mean, PEB_pragmatist))
#Correlation MFT w. likely PEB
with(cs, cor.test(MFT_mean, -avg_sp_overall_excdfltCL))
with(cs, cor.test(MFT_mean, PEB_activist))
with(cs, cor.test(MFT_mean, PEB_pragmatist))

#Do the whole thing as one big correlation matrix - ACTUAL behaviour
#M is only correlation matrix, M_full (from corr.test) includes p-values and conf. intervals

M <- cs %>% select(avg_sp_overall_excdfltCL, avg_setpointCL, thermo_changeCL, thermo_change_excdfltCL, 'Political orientation', PEB_activist, PEB_pragmatist, 'AwarenessConsequences', 'AscriptionResponsibility', 
              NEP_mean, EAI_mean, 'SocialNorm', 'PBC', 'Habit', 'Intention', thermo_moral_mean, BSCS_mean,
              MAC_mean, MFT_mean) %>% cor(use = "pairwise.complete.obs")

M_full <- cs %>% select(avg_sp_overall_excdfltCL, avg_setpointCL, thermo_changeCL, thermo_change_excdfltCL, 'Political orientation', PEB_activist, PEB_pragmatist, 'AwarenessConsequences', 'AscriptionResponsibility', 
              NEP_mean, EAI_mean, 'SocialNorm', 'PBC', 'Habit', 'Intention', thermo_moral_mean, BSCS_mean,
              MAC_mean, MFT_mean) %>% corr.test()

print(M_full, short = FALSE)
print(M)
write.csv(M_full$ci, file = "main correlations.csv")
corrplot(M, method = "number", type = "lower", order = "AOE")

colcode <- c("firebrick2", 
             "firebrick2",  
             "firebrick2", 
              "firebrick2", 
              "firebrick2", 
              "firebrick2", 
              "firebrick2", 
              "firebrick2", 
              "firebrick2", 
              "firebrick2", 
              "firebrick2", 
              "firebrick2", 
              "firebrick2", 
              "firebrick2", 
              "olivedrab", 
              "olivedrab", 
              "firebrick2", 
              "olivedrab",
             "olivedrab")

colnames(M) <- c("Mean setpoint (exc defaults)",
                 "Mean setpoint", 
                 "Thermostat change",
                 "Thermostat change (exc defaults)", 
                 "Political orientation", 
                 "PEB Activist", 
                 "PEB Pragmatist", 
                 "Awareness of consequences", 
                 "Acsription of responsibility", 
                 "NEP",
                 "EAI", 
                 "Social norm", 
                 "Perceived behavioural control", 
                 "Habit",
                 "Intention",
                 "Attitude (specific)",
                 "Self-control",
                 "MAC",
                 "MFT")

rownames(M) <- c("Mean setpoint (exc defaults)",
                 "Mean setpoint", 
                 "Thermostat change",
                 "Thermostat change (exc defaults)", 
                 "Political orientation", 
                 "PEB Activist", 
                 "PEB Pragmatist", 
                 "Awareness of consequences", 
                 "Acsription of responsibility", 
                 "NEP",
                 "EAI", 
                 "Social norm", 
                 "Perceived behavioural control", 
                 "Habit",
                 "Intention",
                 "Attitude (specific)",
                 "Self-control",
                 "MAC",
                 "MFT")

res1 <- corr.p(M, 86) # Note: 86 observations for thermo_exc_dfltCL as 2 taken out as duplicated (Room L04F)
res1$ci



write.table(res1$ci, file = "corr_actuals.txt", sep = ",", quote = FALSE, row.names = T)

corrplot.mixed(M, order = "hclust",lower.col = "white", number.cex = .5, tl.pos = "lt", tl.cex = .7, tl.col = colcode)

#Statistics showing understanding thermostat control = environmental behaviour

cs %>% filter((Q8_7 > 0 | Q8_8 > 0) | (Q8_7 > 0 | Q8_9 > 0) | (Q8_8 > 0 | Q8_9 > 0)) 
# = 81/88 agree with 2 or more of the 3 thermo_moral_mean statements

cs %>% filter(Q8_7 < 0, Q8_8 < 0, Q8_9 < 0)
#only 2 of 88 disagree with all 3 statements

cs %>% filter(Q8_7 > 0, Q8_8 > 0, Q8_9 > 0)
#41 / 88  (48%) agree to some extent with all 3 statements

hist(cs$thermo_moral_mean)

#Correlations with Actual PEB

#Correlations With AFTER v BEFORE thermo setting (thermo_change_excdflt)

with(cs, cor.test(cs$'Political orientation', thermo_change_excdflt))
with(cs, cor.test(cs$'AwarenessConsequences', thermo_change_excdflt))
with(cs, corr.test(cs$'AwarenessConsequences', thermo_change_excdflt, adjust = "none"))
with(cs, cor.test(cs$'AscriptionResponsibility', thermo_change_excdflt))
with(cs, cor.test(NEP_mean, thermo_change_excdflt))
with(cs, cor.test(EAI_mean, thermo_change_excdflt))
with(cs, cor.test(cs$'SocialNorm', thermo_change_excdflt))
with(cs, cor.test(cs$'PBC', thermo_change_excdflt))
with(cs, cor.test(cs$'Habit', thermo_change_excdflt))
with(cs, cor.test(cs$'Intention', thermo_change_excdflt))
with(cs, cor.test(cs$thermo_moral_mean, thermo_change_excdflt))
with(cs, cor.test(BSCS_mean, thermo_change_excdflt))
with(cs, cor.test(MAC_mean, thermo_change_excdflt))
with(cs, cor.test(MFT_mean, thermo_change_excdflt))

#Calculate p-value differences between CADM and thermostat study for ACTUAL PEB
#Note: r values are changed to negative from CADM to stay consistent
#As thermo_change_excdflt = AFTER - BEFORE therefore the PEB is negative

#Correlation Awareness of Consequences w. thermo_change_excdflt
print(r.test(n = 86, r12 = (with(cs, cor(cs$'AwarenessConsequences', cs$thermo_change_excdflt, 
                                   use = "complete.obs"))), n2 = 13215, r34 = -.22), digits = 3)

#Correlation Ascription of Responsibility w. thermo_change_excdflt
print(r.test(n = 86, r12 = (with(cs, cor(cs$'AscriptionResponsibility', thermo_change_excdflt, 
                                   use = "complete.obs"))), n2 = 4217, r34 = -.10), digits = 3)

#Correlation NEP w. thermo_change_excdflt
print(r.test(n = 86, r12 = (with(cs, cor(NEP_mean, thermo_change_excdflt, 
                                   use = "complete.obs"))), n2 = 3499, r34 = -.09), digits = 3)

#Correlation SocialNorm w. thermo_change_excdflt
print(r.test(n = 86, r12 = (with(cs, cor(cs$'SocialNorm', thermo_change_excdflt, 
                                   use = "complete.obs"))), n2 = 14170, r34 = -.24), digits = 3)

#Correlation Perceived Behavioural Control w. thermo_change_excdflt
print(r.test(n = 86, r12 = (with(cs, cor(cs$'PBC', thermo_change_excdflt, 
                                   use = "complete.obs"))), n2 = 15020, r34 = -.40), digits = 3)

#Correlation Habits w. thermo_change_excdflt
print(r.test(n = 86, r12 = (with(cs, cor(cs$'Habit', thermo_change_excdflt, 
                                   use = "complete.obs"))), n2 = 7747, r34 = -.46), digits = 3)

#Correlation Intentions w. thermo_change_excdflt
print(r.test(n = 86, r12 = (with(cs, cor(cs$'Intention', thermo_change_excdflt, 
                                   use = "complete.obs"))), n2 = 12945, r34 = -.55), digits = 3)

#Correlation Attitudes w. thermo_change_excdflt
print(r.test(n = 86, r12 = (with(cs, cor(thermo_moral_mean, thermo_change_excdflt, 
                                   use = "complete.obs"))), n2 = 14053, r34 = -.36), digits = 3)



#Correlation plot of morality scales with thermo attitude
cs %>% select(MAC_mean, MFT_mean, thermo_moral_mean, likelyPEB_mean) %>% cor(
  ) %>% corrplot(method = "number", type = "lower")












#T TESTS



#Paired T-tests for before and after thermostat set temp

#averages of thermo setting before and after INC defaults

t1 <- irus_data %>% group_by(site_room) %>%
  select(avg_setpoint_before, avg_setpoint_after) %>%
  distinct(site_room, avg_setpoint_before, avg_setpoint_after) 

t.test(t1$avg_setpoint_after,t1$avg_setpoint_before,paired=TRUE, na.rm = TRUE, alternative = "less")

#averages of thermo setting before and after EXC defaults
t3 <- irus_data %>% group_by(site_room) %>%
  distinct(site_room, avg_sp_before_excdflt, avg_sp_after_excdflt)
t.test(t3$avg_sp_after_excdflt,t3$avg_sp_before_excdflt, paired=TRUE, alternative = "less")

#IRUS t tests for clean data - NO PEAKS
t2 <- irus_data %>% group_by(site_room) %>%
  distinct(site_room, avg_setpoint_beforeCL, avg_setpoint_afterCL)
t.test(t2$avg_setpoint_afterCL, t2$avg_setpoint_beforeCL, paired=TRUE, alternative = "less")

#IRUS t tests for clean data - NO PEAKS - EXC DEFAULTS
t4 <- irus_data %>% group_by(site_room) %>%
  distinct(site_room, avg_sp_before_excdfltCL, avg_sp_after_excdfltCL)
t.test(t4$avg_sp_after_excdfltCL, t4$avg_sp_before_excdfltCL, paired=TRUE, alternative = "less")

#Equivalent calculations for SURVEY dataset
shapiro.test(cs$thermo_change)
shapiro.test(cs$thermo_change_excdflt)

#MEANS and t tests for CS survey data
#grand averages
 irus_data %>% filter (Date > as.Date("2020-01-30") & Date < as.Date(  "2020-02-14")) %>%  
 summarise(avg_setpoint_before = mean(Setpoint, na.rm = TRUE)) %>% as.data.frame()
 
  irus_data %>% filter (Date > as.Date("2020-02-14") & Date < as.Date(  "2020-03-01")) %>%  
 summarise(avg_setpoint_before = mean(Setpoint, na.rm = TRUE)) %>% as.data.frame()

t.test(cs$avg_setpoint_after, cs$avg_setpoint_before, paired = TRUE, alternative = "less")

#cleaned grand averages (no peaks)
irus_data %>% 
  filter (Date > as.Date("2020-01-30") & Date < as.Date(  "2020-02-14")) %>% filter (exclude == 1) %>%
  summarise(avg_setpoint_beforeCL = mean(Setpoint, na.rm = TRUE)) %>% as.data.frame()

irus_data %>% 
  filter (Date > as.Date("2020-02-14") & Date < as.Date(  "2020-03-01")) %>% filter (exclude == 1) %>%
  summarise(avg_setpoint_afterCL = mean(Setpoint, na.rm = TRUE)) %>% as.data.frame()

t.test(cs$avg_setpoint_afterCL, cs$avg_setpoint_beforeCL, paired = TRUE, alternative = "less")

#excluding defaults
irus_data %>% 
  filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
filter(Setpoint != 19) %>% filter(Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) %>% 
  summarise(avg_sp_before_excdflt = mean(Setpoint, na.rm = TRUE)) %>% as.data.frame()


irus_data %>% 
  filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
filter(Setpoint != 19) %>% filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01")) %>% 
  summarise(avg_sp_after_excdflt = mean(Setpoint, na.rm = TRUE)) %>% as.data.frame()

t.test(cs$avg_sp_after_excdflt, cs$avg_sp_before_excdflt, paired = TRUE, alternative = "less")

#cleaned and excluding defaults
irus_data %>% 
  filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
filter(Setpoint != 19) %>% filter(Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) %>% 
  filter(exclude == 1) %>% 
  summarise(avg_sp_before_excdfltCL = mean(Setpoint, na.rm = TRUE)) %>% as.data.frame


irus_data %>% 
  filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
filter(Setpoint != 19) %>% filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01")) %>% 
  filter(exclude == 1) %>% 
  summarise(avg_sp_after_excdfltCL = mean(Setpoint, na.rm = TRUE)) %>% as.data.frame()

t.test(cs$avg_sp_after_excdfltCL, cs$avg_sp_before_excdfltCL, paired = TRUE, alternative = "less")

#Plot of survey data setpoints before and after

cs %>% ggplot(aes(x = avg_setpoint_before, y = avg_setpoint_after)) + geom_point() + geom_smooth(method = "lm")

#No link between activist setpoint after, some link between pragmatist
cs %>% ggplot(aes(x = PEB_activist, y = avg_setpoint_after)) + geom_point() + geom_smooth(method = "lm")
cs %>% ggplot(aes(x = PEB_pragmatist, y = avg_setpoint_after)) + geom_point() + geom_smooth(method = "lm")
cs %>% ggplot(aes(x = PEB_pragmatist, y = thermo_change)) + geom_point() + geom_smooth(method = "lm")
cs %>% ggplot(aes(x = PEB_activist, y = thermo_change)) + geom_point() + geom_smooth(method = "lm")



#Link between attitudes and thermo setting actual before AND after ie. no change
cs %>% ggplot(aes(x = thermo_moral_mean, y = avg_setpoint_before)) + geom_point() + geom_smooth(method = "lm")
cs %>% ggplot(aes(x = thermo_moral_mean, y = avg_setpoint_after)) + geom_point() + geom_smooth(method = "lm")
corr.test(cs$thermo_moral_mean, cs$avg_setpoint_before, use = "pairwise") # =-0.34
corr.test(cs$thermo_moral_mean, cs$avg_setpoint_after, use = "pairwise") # =-0.35


#Mean external temp before (6.9 celsius) and after (7.0) comms
irus_data %>% ungroup() %>%
  filter(Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) %>%
  summarise(average_before = mean(Ext_temp_celsius))

irus_data %>% ungroup() %>%
  filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01")) %>%
  summarise(average_after = mean(Ext_temp_celsius))


#correlation between thermostat setting (exc dflt) and external temp = -.56; p = .001  
t_external <- irus_data %>% 
  group_by(Date) %>% 
  filter((Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) |
(Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01"))) %>%
  summarise(ext = mean(Ext_temp_celsius), daily = mean(daily_mean_sp_excdflt, na.rm = TRUE))

cor.test(t_external$ext, t_external$daily)


#For each room filter to include only those which include defaults 19 and 21
#Sort by proportion of observations which are default (low propn = high interaction)

irus_data %>% filter((Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) | (
    Date > as.Date("2020-02-14") & Date < as.Date("2020-02-28"))) %>% 
  filter(Setpoint == 19 | Setpoint == 21) %>%
  group_by(site_room) %>% 
  summarise(propn = length(Setpoint)/672*100) %>%
  arrange(., desc(propn)) %>%
  ggplot () + geom_histogram(aes(x = propn), binwidth = 5) + 
  labs(title = "Level of deference to default settings by room", x =
            "Proportion of observations at default", y = "Number of rooms")

#Code to show that only 2 rooms stuck exclusively to default
irus_data %>%   filter((Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) | (
 Date > as.Date("2020-02-14") & Date < as.Date("2020-02-28"))) %>% 
group_by(site_room) %>% 
summarise(default = sum(Setpoint == 19 | Setpoint == 21), adjusted = sum(
Setpoint != 19 & Setpoint != 21), total = sum(Setpoint > 0),
'Below 19' = sum(Setpoint < 19 & Setpoint != 21), 'Above 19' = sum(Setpoint > 19  & Setpoint != 21)) %>%
  filter(adjusted > 0)

#Plot of %age of observations at default by room

irus_data %>%   filter((Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) | (
    Date > as.Date("2020-02-14") & Date < as.Date("2020-02-28"))) %>% 
  group_by(site_room) %>% 
  summarise(default = sum(Setpoint == 19 | Setpoint == 21), adjusted = sum(
    Setpoint != 19 & Setpoint != 21), total = sum(Setpoint > 0),
    'Below 19' = sum(Setpoint < 19 & Setpoint != 21), 'Above 19' = sum(Setpoint > 19  & Setpoint != 21)) %>%
  pivot_longer(., 'Below 19':'Above 19', names_to = "Setting", values_to = "Count") %>%
  mutate(propn = default / total) %>%
  mutate(site_room = fct_reorder(site_room, propn)) %>%
  ggplot () + geom_point(aes(x = site_room, y = propn), size = 0.75) + labs(
    title = "Thermostat settings 30th Jan 2020 - 1st March", 
    y = "Proportion of non-default observations") +
  geom_point(aes(x = site_room, y = Count/total, colour = Setting), size  = 2, alpha = 1/2) +
  scale_colour_manual(values=c("firebrick2", "royalblue3")) + theme(
    axis.text.x = element_blank(), axis.ticks.x=element_blank()) + xlab("Individual rooms") +
  labs(subtitle = "Overall proportion of settings at default, split by set level") 




#Number of observations in the analysis date range NOT default = 105,643
irus_data %>%   
  filter((Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) | (
     Date > as.Date("2020-02-14") & Date < as.Date("2020-02-28"))) %>%
  filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
  filter(Setpoint != 19)

#Number of observations in the analysis date range @ default = 260,045
irus_data %>% filter(Setpoint == 19 | Setpoint == 21) %>% filter((Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) | (
+     Date > as.Date("2020-02-14") & Date < as.Date("2020-02-28")))
#Out of 355k = 73% @ default setting
irus_data %>% filter((Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) | (
+     Date > as.Date("2020-02-14") & Date < as.Date("2020-02-28")))

#Hypothesis 5: Differences between Halls

cs %>% filter(Q3 == "CRESCENT") %>%
  summarise(before = mean(avg_sp_before_excdflt), after = mean(avg_sp_after_excdflt))
cs %>% filter(Q3 == "CRESCENT") %>%
  summarise(before = mean(avg_setpoint_before), after = mean(avg_setpoint_after))
cs %>% filter(Q3 == "WARNEFORD") %>%
  summarise(before = mean(avg_sp_before_excdflt, na.rm = TRUE), after = mean(
    avg_sp_after_excdflt, na.rm = TRUE))
cs %>% filter(Q3 == "WARNEFORD") %>%
  summarise(before = mean(avg_setpoint_before, na.rm = TRUE), after = mean(
    avg_setpoint_after, na.rm = TRUE))

t_warneford <-  cs %>% filter(Q3 == "WARNEFORD") %>%
  select(avg_setpoint_before, avg_setpoint_after, avg_sp_before_excdflt, avg_sp_after_excdflt)
t_crescent <-  cs %>% filter(Q3 == "CRESCENT") %>%
  select(avg_setpoint_before, avg_setpoint_after, avg_sp_before_excdflt, avg_sp_after_excdflt)

t.test(x = t_warneford$avg_setpoint_after, y = t_warneford$avg_setpoint_before,
       alternative = "l", mu = 0, paired = TRUE)
t.test(x = t_crescent$avg_setpoint_after, y = t_crescent$avg_setpoint_before,
       alternative = "l", mu = 0, paired = TRUE)

t.test(x = t_warneford$avg_sp_after_excdflt, y = t_warneford$avg_sp_before_excdflt,
       alternative = "l", mu = 0, paired = TRUE)
t.test(x = t_crescent$avg_sp_after_excdflt, y = t_crescent$avg_sp_before_excdflt,
       alternative = "l", mu = 0, paired = TRUE)



#Boxplot of temps before and after by Hall (note: averages are MEDIAN!)

boxplot1 <- cs %>% group_by(Q3) %>%
  select(avg_setpoint_before, avg_setpoint_after, avg_sp_before_excdflt,
         avg_sp_after_excdflt)  %>% 
  pivot_longer(., cols = 2:5, names_to = "condition", values_to = "setting")
boxplot1$condition <- as.factor(boxplot$condition) %>% factor(.,
    levels = c('avg_setpoint_before', 'avg_setpoint_after', 
               'avg_sp_before_excdflt', 'avg_sp_after_excdflt'), 
    labels = c('Before - inc defaults', 'After - inc defaults', 
               'Before - excl defaults', 'After - excl defaults'), ordered = TRUE)
boxplot1 %>% ggplot() + geom_boxplot(aes(x = Q3, y = setting, colour = condition,)) + 
  theme(axis.text.x= element_text(size = 15), axis.title.x = element_blank()) +
  scale_colour_manual(values = c("firebrick2","royalblue3","firebrick3","royalblue3")) +
  labs(title = "Median/IQR of thermostat settings by experimental condition") +
  scale_x_discrete(labels=c("CRESCENT" = "FAMILY", "WARNEFORD" = "GENERIC"))



















##############            REGRESSION MODELLING





#Model H1a (CADM validation)
model_H1a <- lm(likelyPEB_mean_sz ~ (AwarenessConsequences_sz + Habit_sz + SocialNorm_sz + 
  AscriptionResponsibility_sz + PBC_sz + Intention_sz + NEP_mean_sz + 
    thermo_moral_mean_sz), cs)
summary(model_H1a)


model_H1a_2way <- lm(likelyPEB_mean_sz ~ (AwarenessConsequences_sz + Habit_sz + SocialNorm_sz + 
  AscriptionResponsibility_sz + PBC_sz + Intention_sz + NEP_mean_sz + 
    thermo_moral_mean_sz)^2, cs)
summary(model_H1a_2way)

plot(model_H1a)
plot_model(model_H1a, type = c("std"))
plot_model(model_H1a_2way)

#Model H1a_act (Activist sub-score)
model_H1a_act <- lm(PEB_activist_sz ~ AwarenessConsequences_sz + Habit_sz + SocialNorm_sz + 
  AscriptionResponsibility_sz + PBC_sz + Intention_sz + NEP_mean_sz + thermo_moral_mean_sz, cs)
summary(model_H1a_act)

model_H1a_act_2way <- lm(PEB_activist_sz ~ (AwarenessConsequences_sz + Habit_sz + SocialNorm_sz + 
  AscriptionResponsibility_sz + PBC_sz + Intention_sz + NEP_mean_sz + thermo_moral_mean_sz)^2, cs)
summary(model_H1a_act_2way)

plot_model(model_H1a_act)
plot_model(model_H1a_act_2way)

#Model H1a_prag (Pragmatist sub-score)
model_H1a_prag <- lm(PEB_pragmatist_sz ~ AwarenessConsequences_sz + Habit_sz + SocialNorm_sz + 
  AscriptionResponsibility_sz + PBC_sz + Intention_sz + NEP_mean_sz + thermo_moral_mean_sz, cs)
summary(model_H1a_prag)

model_H1a_prag_2way <- lm(PEB_pragmatist_sz ~ (
  AwarenessConsequences_sz + Habit_sz + SocialNorm_sz +   AscriptionResponsibility_sz +
    PBC_sz + Intention_sz + NEP_mean_sz + thermo_moral_mean_sz)^2, cs)
summary(model_H1a_prag_2way)

tab_model(model_H1a, model_H1a_prag, model_H1a_act)
tab_model(model_H1a_2way, model_H1a_act_2way, model_H1a_prag_2way)




#Model H1b
model_H1b <- lm(avg_sp_after_excdflt19 ~ 1 + avg_sp_before_excdflt19 + AwarenessConsequences_sz + Habit_sz + SocialNorm_sz + 
  AscriptionResponsibility_sz + PBC_sz + Intention_sz + NEP_mean_sz  + thermo_moral_mean_sz, cs)
summary(model_H1b)

tab_model(model_H1b)
tab_model(model_H1a, model_H1a_act, model_H1a_prag, model_H1b)

#Model H1bB = all predictors predicting actual temp BEFORE
model_H1bB <- lm(avg_sp_before_excdflt19 ~ 1 + AwarenessConsequences_sz + Habit_sz + SocialNorm_sz + 
  AscriptionResponsibility_sz + PBC_sz + Intention_sz + NEP_mean_sz  + thermo_moral_mean_sz, cs)
summary(model_H1bB)

tab_model(model_H1bB)

#Model H1bA = all predictors predicting actual temp AFTER
model_H1bA <- lm(avg_sp_after_excdflt19 ~ 1 + AwarenessConsequences_sz + Habit_sz + SocialNorm_sz + 
  AscriptionResponsibility_sz + PBC_sz + Intention_sz + NEP_mean_sz  + thermo_moral_mean_sz, cs)
summary(model_H1bA)

tab_model(model_H1bA, model_H1bB)

#Model H1b_diff = all predictors predicting DIFFERENCE in temp
model_H1b_diff <- lm(thermo_change_excdflt ~ 1+ AwarenessConsequences_sz + Habit_sz + SocialNorm_sz + 
  AscriptionResponsibility_sz + PBC_sz + Intention_sz + NEP_mean_sz  + thermo_moral_mean_sz, cs)
summary(model_H1b_diff)

tab_model(model_H1b_diff)

tab_model(model_H1bA, model_H1bB, model_H1b_diff)


#Model H1b_diff_2way = all predictors predicting DIFFERENCE in temp
model_H1b_diff_2way <- lm(thermo_change_excdflt ~ 1 + (AwarenessConsequences_sz + Habit_sz + SocialNorm_sz + 
  AscriptionResponsibility_sz + PBC_sz + Intention_sz + NEP_mean_sz  + thermo_moral_mean_sz)^2, cs)
summary(model_H1b_diff_2way)

tab_model(model_H1b_diff_2way)

#Model H1aD (CADM validation) likely_PEB w. DEMOGRAPHICS
cs$Q13 <- na_if(cs$Q13, "Different Identity") # exclude single observation
cs$Q15 <- na_if(cs$Q15, "Other ethnic group") # exclude single observation
model_H1aD <- lm(avg_sp_overall_excdfltCL ~ Q13 + Q34 +
                   PoliticalOrientation_sz + Q17 + Q15, cs)
summary(model_H1aD)
tab_model(model_H1aD)


model1 <- (lm(avg_sp_overall_excdfltCL ~ 1 + Q13, cs))
model2 <- (lm(avg_sp_overall_excdfltCL ~ 1 + Q34, cs))
model3 <- (lm(avg_sp_overall_excdfltCL ~ 1 + PoliticalOrientation_sz , cs))
model4 <- (lm(avg_sp_overall_excdfltCL ~ 1 + Q17, cs))
model5 <- (lm(avg_sp_overall_excdfltCL ~ 1 + Q15, cs))
tab_model(model1, model2, model3, model4, model5)

summary(model3)
summary(model4)
confint(model4, "Q17Yes", level = 0.95)

#Model H1bD (CADM validation) thermo_change_excdflt w. DEMOGRAPHICS

model_H1bD <- lm(avg_sp_after_excdflt19 ~ 1 + avg_sp_before_excdflt19 + Q13 + Q34 + 
  PoliticalOrientation_sz + Q17 + Q15, cs)
summary(model_H1bD)

plot_model(lm(avg_sp_after_excdflt19 ~ 1 + avg_sp_before_excdflt19 + Q13 + Q34 + 
  PoliticalOrientation_sz + Q17 + Q15, cs))

plot_model(lm(avg_sp_after_excdflt19 ~ 1 + avg_sp_before_excdflt19 + Q13 + Q34 + 
  PoliticalOrientation_sz + Q17 + Q15, cs))








plot_model(model_H1bD, type = "pred")
tab_model(model_H1aD, model_H1bD)

#HYPOTHESIS 2

#Model 2a: EAI v NEP - claimed PEB
model_H2a <- lm(thermo_moral_mean_sz ~ EAI_mean_sz + NEP_mean_sz, cs)
summary(model_H2a)

tab_model(model_H2a)

#Model 2b: EAI v NEP - actual PEB

#models with overall temp PEB
model_H2b1 <- lm(avg_setpointCL ~ 1 + NEP_mean_sz + EAI_mean_sz, cs)
summary(model_H2b1)

model_H2b2 <- lm(avg_sp_overall_excdfltCL ~ 1 + NEP_mean_sz + EAI_mean_sz, cs)
summary(model_H2b2)


#models with AFTER controlling for BEFORE temp
model_H2b3 <- lm(avg_setpoint_afterCL ~ 1 + avg_setpoint_beforeCL + EAI_mean_sz + NEP_mean_sz, cs)
summary(model_H2b3)

model_H2b4 <- lm(avg_sp_after_excdfltCL ~ 1 + avg_sp_before_excdfltCL + EAI_mean_sz + NEP_mean_sz, cs)
summary(model_H2b4)

tab_model(model_H2b1, model_H2b2, model_H2b3, model_H2b4)
plot_model(model_H2b1, type = "pred")

#HYPOTHESIS 3
model_H3 <- lm((avg_setpointCL-19) ~ 1 + BSCS_mean_sz, cs)
summary(model_H3)

model_H3a <- lm(avg_sp_after_excdflt19 ~ 1 + avg_sp_before_excdflt19 + BSCS_mean_sz, cs)
summary(model_H3a)

tab_model(model_H3, model_H3a)




#HYPOTHESIS 4

H4 <- irus_data %>% 
  group_by(Name, Site) %>% 
  summarise(room_before = mean(avg_setpoint_before - 19, na.rm = TRUE), 
            room_after = mean(avg_setpoint_after - 19), external = mean(Ext_temp_celsius)) %>%  ungroup()
H4 <- H4 %>% mutate(external_sz = as.numeric(scale(external)))

model_H4 <- lm(room_after ~ 1 + room_before + Site + room_before*Site + external_sz, H4)


summary(model_H4)


tab_model(model_H4)
plot_model(model_H4, type = c("int"))

H4cs <- irus_data %>% filter(site_room %in% c(cs$site_room)) %>% filter(exclude == 1)
H4cs <- H4cs %>% mutate(external_sz = as.numeric(scale(Ext_temp_celsius)))
model_H4cs <- lm(Setpoint ~ 1 + Setpoint + intervention + external_sz, H4cs)


#HYPOTHESIS 6

#MAC

model_H6a <- lm(avg_sp_after_excdflt19 ~ 1 + avg_sp_before_excdflt19 + 
                 Q3, cs)
model_H6b <- lm(avg_sp_after_excdflt19 ~ 1 + avg_sp_before_excdflt19 + 
                 MAC_mean_sz + Q3, cs)
model_H6c <- lm(avg_sp_after_excdflt19 ~ 1 + avg_sp_before_excdflt19 + 
                 MAC_mean_sz + MAC_mean_sz*Q3, cs)

tab_model(model_H6a, model_H6b, model_H6c)

#MFT

model_H6d <- lm(avg_sp_after_excdflt19 ~ 1 + avg_sp_before_excdflt19 + 
                 Q3, cs)
model_H6e <- lm(avg_sp_after_excdflt19 ~ 1 + avg_sp_before_excdflt19 + 
                 MFT_mean_sz + Q3, cs)
model_H6f <- lm(avg_sp_after_excdflt19 ~ 1 + avg_sp_before_excdflt19 + 
                 MFT_mean_sz + MFT_mean_sz*Q3, cs)

tab_model(model_H6d, model_H6e, model_H6f)



#Match Room numbers to Time Series data
#Show any duplicate room numbers - there is one duplicate in Crescent - Room L04F
cs %>% group_by(Q4) %>% filter(n()>1) %>% select(Q4)
cs %>% group_by(Q21) %>% filter(n()>1) %>% select(Q21)




#Linear Mixed Models


#1. Visualise

d %>% ggplot(aes(x = DAY, y = TEMPEXCL)) + geom_path()





#PLOTS

#Plot histogram of EAI_mean per subject
cs %>% ggplot(aes(x=EAI_mean)) + geom_histogram(binwidth = 0.1)

#Scatter plot of NEP vs EAI
ggplot(cs) + geom_smooth(mapping = aes(x = EAI_mean, y = NEP_mean))

#Smoothed line plots of NEP, EAI and BSCS scales vs likelyPEB
plot1 <- ggplot(cs) + geom_smooth(mapping = aes(x = EAI_mean, y = likelyPEB_mean))
plot2 <- ggplot(cs) + geom_smooth(mapping = aes(x = NEP_mean, y = likelyPEB_mean))
plot3 <-  ggplot(cs, mapping = aes(x = BSCS_mean, y = likelyPEB_mean)) + geom_smooth(
  ) + geom_point() + geom_smooth(method = "lm", colour = "black", size = 0.5) + ggtitle(
    "Brief Self-Control Scale comparison to claimed PEB")
grid.arrange(plot1, plot2, plot3, ncol = 3)
ggplot(cs) + geom_smooth(mapping = aes(x = thermo_moral_mean, y = EAI_mean))


#Plot of attitude to likelyPEB
ggplot (cs) + geom_smooth(mapping = aes(x = thermo_moral_mean, y = likelyPEB_mean))
ggplot (cs) + geom_smooth(method = "lm", se = TRUE, mapping = aes(
  x = thermo_moral_mean, y = EAI_mean), colour = "darkgoldenrod3", fill = "darkgoldenrod3") + geom_point(
    mapping = aes(x = thermo_moral_mean, y = EAI_mean), colour = "darkgoldenrod3") +
  geom_smooth(method = "lm", se = TRUE, mapping = aes(x = thermo_moral_mean, y = NEP_mean),
              colour = "springgreen4", fill = "springgreen4") +   geom_point(mapping = aes(
                x = thermo_moral_mean, y = NEP_mean), colour = "springgreen4", 
                shape = "triangle") +  labs(x = "Pro-environmental Attitude to Thermostat",
                                            y = "General Environmental Attitude Scale (Mean)") +
  annotate(geom = "point", x = -1, y = 2.8, colour = "springgreen4", 
           shape = "triangle", size = 3) + 
  annotate(geom = "text", x = -1, y = 2.8, label = "  NEP", hjust = "left", size = 4) +
  annotate(geom = "point", x = -1, y = 2.5, colour = "darkgoldenrod3", 
           shape = "circle", size = 3) + 
  annotate(geom = "text", x = -1, y = 2.5, label = "  EAI", hjust = "left", size = 4)

#Normal distribution tests
#Q-Q plots
qqnorm(cs$likelyPEB_mean, ylab = "likelyPEB")
qqline(cs$likelyPEB_mean, ylab = "likelyPEB")
qqnorm(cs$BSCS_mean, ylab = "BSCS_mean")
qqline(cs$BSCS_mean, ylab = "BSCS_mean")
qqnorm(cs$NEP_mean, ylab = "NEP_mean")
qqline(cs$NEP_mean, ylab = "NEP_mean")
qqnorm(cs$EAI_mean, ylab = "EAI_mean")
qqline(cs$EAI_mean, ylab = "EAI_mean")
qqnorm(cs$likelyPEB_mean, ylab = "thermo_moral")
qqline(cs$likelyPEB_mean, ylab = "thermo_moral")

hist(cs$PEB_activist, breaks = 7)
hist(cs$PEB_pragmatist, breaks = 7, xlim = c(0,7), main = NULL, xlab = "Mean score on PEB Pragmatist")


cs %>% ggplot() + geom_point(mapping = aes(
  x = site_room, y = avg_setpoint_before), colour = "blue") + geom_point(mapping = aes(
    x = site_room, y = avg_setpoint_after), colour = "red") + theme(
      axis.text.x = element_text(angle = 90, size = 8))

plotA01A <- irus_data %>% filter(Name == "A01A") %>% ggplot () + ylim(16,24) + geom_point(
    mapping = aes(x = date_time, y = Setpoint))
plotA01B <- irus_data %>% filter(Name == "A01B") %>% ggplot () + ylim(16,24) + geom_point(
    mapping = aes(x = date_time, y = Setpoint))
plotA01C <- irus_data %>% filter(Name == "A01C") %>% ggplot () + ylim(16,24) + geom_point(
    mapping = aes(x = date_time, y = Setpoint)) + ggtitle("Room A01C - thermostat set point over time") +
  xlab("Date/time")
plotA01C
plotA01D <- irus_data %>% filter(Name == "A01D") %>% ggplot () + ylim(16,24) + geom_point(
    mapping = aes(x = date_time, y = Setpoint))
plotA01E <- irus_data %>% filter(Name == "A01E") %>% ggplot () + ylim(16,24) + geom_point(
    mapping = aes(x = date_time, y = Setpoint))
plotA01F <- irus_data %>% filter(Name == "A01F") %>% ggplot () + ylim(16,24) + geom_point(
    mapping = aes(x = date_time, y = Setpoint))

grid.arrange(plotA01A, plotA01B, plotA01C, plotA01D, plotA01E, plotA01F, nrow = 3)



#Scatter plot of temp change before and after posters
cs %>% ggplot() + geom_point(mapping = aes(x = site_room, y = thermo_change),
  colour=ifelse((cs$thermo_change>0), "red", "blue"), size = 2) + theme(
    axis.text.x = element_text(angle = 90, size = 8))

#Before and After Sub19 bar chart
pivot_longer(cs, sub19_before:sub19_after, names_to = "Sub19", values_to = "count") %>% 
  select(site_room, count, Sub19) %>% arrange(desc(count))

pivot_longer(cs, sub19_before:sub19_after, names_to = "Sub19", values_to = "count") %>%
  ggplot () + geom_col(mapping = aes(x = site_room, y = count, fill = Sub19), position = "dodge")  +  theme(
      axis.text.x = element_text(angle = 90, size = 8))



#Plot of Setpoint and Air Temp
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

#Plot of irus_data before and after intervention
irus_data %>% group_by(site_room) %>% summarise(thermo_change = mean(
  thermo_change)) %>% ggplot() + geom_point(mapping = aes(x = site_room, y = thermo_change))

irus_data %>% group_by(site_room) %>% 
  summarise(thermo_change = mean(thermo_change)) %>%
  ggplot() + geom_histogram(mapping = aes(thermo_change), binwidth = .5) +
  geom_vline(xintercept = -0.57, linetype = "dashed") +
  geom_label(x=-2, y=100, label="Mean change = -0.XX?")


#Actual thermo_change by room ranked by change
irus_data %>% group_by(site_room) %>% summarise(thermo_change = mean(thermo_change)) %>%
  mutate(site_room = fct_reorder(site_room, thermo_change)) %>% 
 ggplot() + geom_col(mapping = aes(x = site_room, y = thermo_change))

#List of rooms with greater than 1 degree drop in thermostat setting
irus_data %>% group_by(site_room) %>% summarise(avg_setpoint_before = mean(
  avg_setpoint_before), avg_setpoint_after = mean(avg_setpoint_after), thermo_change = mean(
  thermo_change)) %>% filter(thermo_change < -1) %>% arrange(thermo_change) %>% print(n = 100)



#Plot of daily external temp and mean thermostat setting

irus_data %>% 
  group_by(Date) %>% filter((Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) |
            (Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01"))) %>%
  summarise(External = mean(Ext_temp_celsius), 
            `Thermostat setting exc. defaults` = mean(daily_mean_sp_excdflt, na.rm = TRUE),
            `Thermostat setting` = mean(Setpoint, na.rm = TRUE)) %>%
  ggplot() + geom_line(aes(x = Date, y = `Thermostat setting exc. defaults`, colour = "Thermostat setting exc. defaults")) + 
  geom_line(aes(x = Date, y = External, colour = "External")) + 
  labs(title = "Mean thermostat setting (inc/exc defaults) and external temperature", 
       y = "Temp (celsius)") + 
  geom_line(aes(x = Date, y = `Thermostat setting`, colour = "Thermostat setting"), linetype = "dotted") +
  scale_color_manual(values = c(
    "External" = "royalblue3", "Thermostat setting" = "black", "Thermostat setting exc. defaults" = "red")) 
  
#Plot intention versus actual behaviour
cs %>% ggplot(aes(x = `Intention`, y = thermo_change)) + geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship of thermostat change to intention", x = "Agreement with statement 'I fully intend to use my thermostat control \n to minimise energy usage in the future' (7 point scale)",
       y = "Measured change in thermostat setting (celsius)")


#Plot moving averages of non-default settings: 

irus_data %>% filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
  filter(Setpoint != 19) %>% group_by(Date) %>%
  summarise(number = sum(Setpoint)) %>%
  ggplot(aes(x = Date, y = number)) + geom_point() + theme(
    axis.text.x = element_text(angle = 90, size = 8)) + scale_x_date(breaks = "1 day") +
  geom_ma(n = 3) + labs(title = "Number of non-default settings by day & rolling 3 day average") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-01-31")))) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-02-29"))))




#TWIN PEAKS



#Average thermo setting of survey participants
irus_data %>% filter(site_room %in% c(cs$site_room)) %>%
  group_by(date_time) %>%
  summarise(mean = mean(Setpoint), ext = mean(Ext_temp_celsius)) %>%
  ggplot(aes(x = date_time, y = mean)) + geom_line() + geom_ma(n=24) + geom_point(aes(x = date_time, y = ext))

#apply to all irus_data = same artefacts - room temp v thermostat setting
irus_data %>% 
  group_by(date_time) %>%
  summarise(mean = mean(Setpoint, na.rm = TRUE), ext = mean(Ext_temp_celsius), roomtemp = mean(`Temp Air`)) %>%
  ggplot(aes(x = date_time)) + geom_line(aes(x = date_time, y = mean, colour = "Thermostat setting")) + geom_ma(aes(y = mean), n=24) +
  geom_line(aes(y = roomtemp, colour = "Room temperature")) + geom_ma(aes(y = roomtemp), n=24) + 
  xlab("Date") + ylab("Mean temperature (degrees celsius)")

irus_data %>% 
  group_by(date_time ,site_room ) %>%
  summarise(temp = Setpoint) %>%  filter(temp > 23) %>% summarise(temp = mean(temp, na.rm = TRUE)) %>%
  arrange(desc(temp)) %>% print(n=40)

#room temp does not follow high thermo settings!
irus_data %>% 
  group_by(date_time ,site_room ) %>%
  summarise(temp = Setpoint, roomtemp = `Temp Air`) %>%  filter(temp > 25)


irus_data %>% 
  group_by(date_time ,site_room ) %>%
  summarise(temp = Setpoint) %>%  summarise(temp = mean(temp, na.rm = TRUE)) %>% 
  arrange(desc(temp)) %>% print(n=40)

irus_data %>% group_by(site_room, date_time) %>%
  summarise(temp = Setpoint) %>%  
  filter(temp > 25) %>% 
  count(site_room) %>% arrange(desc(n))

irus_data %>% group_by(site_room, date_time) %>%
  filter(Setpoint > 23) %>% 
  summarise(temp = Setpoint)  %>%
  count(site_room) %>%
  arrange(desc(n))


#rank mean temp setting
irus_data %>% filter(site_room %in% c(cs$site_room)) %>% group_by(site_room, date_time) %>%
  summarise(temp = Setpoint) %>%
  mutate(site_room = fct_reorder(site_room, temp)) %>% 
 ggplot() + geom_col(mapping = aes(x = site_room, y = Setpoint))

#trying to work out what's driving the spikes..
hold <- irus_data %>% filter(site_room %in% c(cs$site_room)) %>%
  group_by(date_time, site_room) %>%
  summarise(temp = Setpoint)

hold_excdflt <- irus_data %>% filter(site_room %in% c(cs$site_room)) %>%
                         filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
                         filter(Setpoint != 19) %>%
  group_by(date_time, site_room) %>%
  summarise(temp = Setpoint)

hold
hold_excdflt
hold %>% filter(temp > 23) %>% summarise(temp = mean(temp))
hold %>% filter(temp > 25) %>% arrange(desc(date_time)) 
hold %>% arrange(desc(date_time)) 

#peaks on 7th Feb, 13th Feb (before), and double peaks 20th/21st Feb and 29th Feb/1st March (after)

hold %>% ggplot() + geom_histogram(aes(temp), binwidth = .1) + ylim(0,75000)
hold_excdflt %>% ggplot() + geom_histogram(aes(temp), binwidth = .1)
hold %>% ggplot() + geom_boxplot(aes(temp))

#Test some outcomes using A. cleaned thermostat data

#Clean thermostat data
#Define "spikes" as date_time points where mean > 20 removes 11 data points from 1,296
irus_data %>% 
  group_by(date_time ,site_room ) %>%
  summarise(temp = Setpoint) %>%  summarise(temp = mean(temp, na.rm = TRUE)) %>% 
  arrange(desc(temp)) %>% print(n=40)

#Within 2 weeks before and after, this equates to.. 6 data points
irus_data %>% filter((Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) |
(Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01"))) %>%
  group_by(date_time ,site_room ) %>%
  summarise(temp = Setpoint) %>%  summarise(temp = mean(temp, na.rm = TRUE)) %>% 
  arrange(desc(temp)) %>% filter (temp > 20)



#therefore to apply exclude (remove peaks) filter(., exclude = 1)

 #   date_time            temp
 #   <dttm>              <dbl>
 # 1 2020-02-20 18:00:00  24.5
 # 2 2020-02-29 19:00:00  22.2
 # 3 2020-02-21 00:00:00  22.2
 # 4 2020-02-07 09:00:00  22.0
 # 5 2020-02-13 00:00:00  21.9
 # 6 2020-02-07 10:00:00  20.4
 

 irus_data %>% filter((Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) |
 (Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01"))) %>%
 group_by(date_time ,site_room ) %>%  summarise(temp = mean(Setpoint))

