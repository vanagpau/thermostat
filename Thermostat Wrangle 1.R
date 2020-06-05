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

#Calculate all mean averages for EAI
means_EAI <- lapply(EAI, mean, na.rm = TRUE)

#Compute sub-scale scores for EAI
cs <- cs %>% rowwise() %>% mutate(
EAI1 = mean(c(Q6_1, Q6_2), na.rm = TRUE),
EAI2 = mean(c(Q6_3, Q6_4), na.rm = TRUE),
EAI3 = mean(c(Q6_5, Q6_6), na.rm = TRUE),
EAI4 = mean(c(Q6_7, Q6_8), na.rm = TRUE),
EAI5 = mean(c(Q6_9, Q6_10), na.rm = TRUE),
EAI6 = mean(c(Q6_11, Q6_12), na.rm = TRUE),
EAI7 = mean(c(Q6_13, Q6_14), na.rm = TRUE),
EAI8 = mean(c(Q6_15, Q6_16), na.rm = TRUE),
EAI9 = mean(c(Q6_17, Q6_18), na.rm = TRUE),
EAI10 = mean(c(Q6_19, Q6_20), na.rm = TRUE),
EAI11 = mean(c(Q6_21, Q6_22), na.rm = TRUE),
EAI12 = mean(c(Q6_23, Q6_24), na.rm = TRUE),
EAI_mean = mean(c(
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
cs <- cs %>% rename('Social norm' = Q8_3)
cs <- cs %>% rename('PBC' = Q8_5)
cs <- cs %>% rename('Awareness consequences' = Q8_1)
cs <- cs %>% rename('Intention' = Q8_6)
cs <- cs %>% rename('Ascription responsibility' = Q8_4)


#LOAD IN THE THERMOSTAT DATA

#Get a sample of the raw interaction data
# big_crescent <- vroom ("FilteredLog_CRESCENT.csv",
#                                col_select = c(1:6), col_names = c(
#                                 "Time stamp", "Node", "Unix time", "Status flag", "Set point", "Air temp"))
# big_warneford <- vroom ("FilteredLog_WARNEFORD.csv",
#                                col_select = c(1:6), col_names = c(
#                                  "Time stamp", "Node", "Unix time", "Status flag", "Set point", "Air temp"))

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
str(external_temp)

irus_data <- left_join(irus_data, external_temp, by = "Date")

#Merge the Site and Room Name fields
irus_data$site_room <- paste(irus_data$Site, irus_data$Name)
#Remove any spaces (to ensure matching)
irus_data$site_room <- gsub('\\s+', '', irus_data$site_room)

#Merge the date and time fields
irus_data$date_time <- fastPOSIXct(paste(irus_data$Date, irus_data$Time))

#Convert rooms to factors (for graphing)
irus_data$site_room <- as.factor(irus_data$site_room)

#Add daily mean setpoint and daily average temperature into dataframe
irus_data <- irus_data %>% group_by(site_room, Date) %>% 
  mutate(daily_mean_sp = mean(Setpoint, na.rm = TRUE),
         daily_airtemp = mean(`Temp Air`, na.rm = TRUE))

irus_data <- left_join(irus_data, irus_data %>% 
                         group_by(site_room, Date) %>% 
                         filter(!(Setpoint == 21 & hour(date_time) >= 7 & hour(date_time) <= 10)) %>%
                         filter(Setpoint != 19) %>% mutate(daily_mean_sp_excdflt = mean(
                           Setpoint, na.rm = TRUE)))


#Calc mean room temp before and after posters (14th Feb) for each room & append to irus_data
irus_data <- left_join (irus_data, irus_data %>% 
  filter (Date > as.Date("2020-01-30") & Date < as.Date(  "2020-02-14")) %>% 
  group_by(site_room) %>% 
  summarise(avg_setpoint_before = mean(Setpoint)), by = "site_room")

irus_data <- left_join (irus_data, irus_data %>%
 filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01")) %>%
   group_by(site_room) %>%
   summarise(avg_setpoint_after = mean(Setpoint)), by = "site_room")

irus_data <- irus_data %>% mutate (sub19_before = ifelse(Date > as.Date(
  "2020-01-30") & Date < as.Date("2020-02-14") & Setpoint <19, Setpoint, NA))

irus_data <- irus_data %>% mutate (sub19_after = ifelse(Date > as.Date(
  "2020-02-14") & Date < as.Date("2020-03-01") & Setpoint <19, Setpoint, NA))


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

irus_data <- irus_data %>% mutate (thermo_change = avg_setpoint_after - avg_setpoint_before)
irus_data <- irus_data %>% mutate (sub19_change = sub19_after - sub19_before)
irus_data <- irus_data %>% mutate (thermo_change_excdflt = avg_sp_after_excdflt - avg_sp_before_excdflt)

#Calculate number of settings below 19 degrees, before and after 14th Feb (posters date)
#Add data from irus_data tibble to data-frame 'cs' (questionnaire data)

cs <- left_join(cs, irus_data %>% filter(Date > as.Date("2020-01-30") & Date < as.Date(
  "2020-02-14")) %>% filter(Setpoint<19) %>% group_by(site_room) %>% count(
    name = "sub19_before"), by = "site_room") %>% mutate_all(~replace(., is.na(.), 0))

cs <- left_join(cs, irus_data %>% filter(Date > as.Date("2020-02-14") & Date < as.Date(
  "2020-03-01")) %>% filter(Setpoint<19) %>% group_by(site_room) %>% count(
    name = "sub19_after"), by = "site_room") %>% mutate_all(~replace(., is.na(.), 0))

cs <- left_join(cs, irus_data %>%
    filter(Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) %>% 
    group_by(site_room) %>% 
    summarise(avg_setpoint_before = mean(Setpoint)), by = "site_room")

cs <- left_join(cs, irus_data %>% 
                  filter(Date > as.Date("2020-02-14") & Date < as.Date(  "2020-03-01")) %>%
                  group_by(site_room) %>%
                  summarise(avg_setpoint_after = mean(Setpoint)), by = "site_room")

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


#Add change variable
cs <- cs %>% mutate(thermo_change = avg_setpoint_after - avg_setpoint_before)
cs <- cs %>% mutate(sub19_change = sub19_after - sub19_before)
cs <- cs %>% mutate (thermo_change_excdflt = avg_sp_after_excdflt - avg_sp_before_excdflt)

# #Add daily set point averages (inc and exc defaults) 
#and average air temp on df cs (for Adam)

# cs <-left_join(cs, irus_data %>% group_by(site_room, Date) %>%
#      summarise(daily_mean_sp = mean(Setpoint, na.rm = TRUE), daily_airtemp = mean(
#        `Temp Air`, na.rm = TRUE), daily_mean_sp_excdflt = mean(daily_mean_sp_excdflt,
#                                                                na.rm = TRUE)) %>%
#        pivot_wider(names_from = Date, values_from = 
#                      c(daily_mean_sp, daily_mean_sp_excdflt, daily_airtemp)), 
#                       by = "site_room")

#HYPOTHESIS 4 - add Before & After tags

irus_data$communications <- 0
irus_data$communications <- NA
attach(irus_data)
irus_data$communications[Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")] <-
  "Before"
irus_data$communications[Date > as.Date("2020-02-14") & Date < as.Date("2020-02-28")] <- 
  "After"
detach(irus_data)

#Remove actual (Irus) data from duplicate room L04F - replace with NAs
cs[(which(grepl("L04F", cs$Q4))),(match("sub19_before",names(cs))):(
  match("daily_airtemp_2020-03-13",names(cs)))] <- NA

#Standardise key variables
cs <- cs %>% mutate(likelyPEB_mean_sz = scale(likelyPEB_mean))
cs <- cs %>% mutate(EAI_mean_sz = scale(EAI_mean))
cs <- cs %>% mutate(PEB_activist_sz = scale(PEB_activist))
cs <- cs %>% mutate(PEB_pragmatist_sz = scale(PEB_pragmatist))
cs <- cs %>% mutate(thermo_moral_mean_sz = scale(thermo_moral_mean))
cs <- cs %>% mutate(MAC_mean_sz = scale(MAC_mean))
cs <- cs %>% mutate(MFT_mean_sz = scale(MFT_mean))
cs <- cs %>% mutate(sub19_before_sz = scale(sub19_before))
cs <- cs %>% mutate(sub19_after_sz = scale(sub19_after))
cs <- cs %>% mutate(avg_setpoint_before_sz = scale(avg_setpoint_before))
cs <- cs %>% mutate(avg_setpoint_after_sz = scale(avg_setpoint_after))
cs <- cs %>% mutate(avg_sp_before_excdflt_sz = scale(avg_sp_before_excdflt))
cs <- cs %>% mutate(avg_sp_after_excdflt_sz = scale(avg_sp_after_excdflt))
cs <- cs %>% mutate(thermo_change_sz = scale(thermo_change))
cs <- cs %>% mutate(sub19_change_sz = scale(sub19_change))
cs <- cs %>% mutate(thermo_change_excdflt_sz = scale(thermo_change_excdflt))




#END OF DATA CLEAN and SETUP








#STATISTICS

#Calculate Cronbach's Alpha for questionnaire scales

alpha(NEP)
alpha(EAI)
alpha(BSCS)
alpha(likelyPEB)
alpha(thermo_moral)
alpha(moral_ascoop)
alpha(moral_found)

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

#Factor Analysis

#FA of Attitudes scale
cs %>% select('Awareness consequences':Q8_9) %>% cor() %>% fa.parallel(
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
#Parallel analysis suggests 1 factors
cs %>% select(Q9_1:Q9_7) %>% cor(
  use = "complete.obs") %>% fa.parallel(
    n.obs=88, main = "Parallel Analysis scree plot - Morality as Co-operation")

#FA of MFT scale
#Assumption tests: KMO and Barlett's sphericity
cs %>% select(Q10_1:Q10_7) %>% cortest()
cs %>% select(Q10_1:Q10_7) %>% KMO()
#Parallel analysis suggests 1 factors
cs %>% select(Q10_1:Q10_7) %>% cor(
  use = "complete.obs") %>% fa.parallel(
    n.obs=88, main = "Parallel Analysis scree plot - Moral Foundations Theory")

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

#Correlation of political orientation w. likely_PEB & thermo_change
with(cs, cor.test(cs$'Political orientation', likelyPEB_mean))
with(cs, cor.test(cs$'Political orientation', PEB_activist))
with(cs, cor.test(cs$'Political orientation', PEB_pragmatist))
with(cs, cor.test(cs$'Political orientation', thermo_change))
with(cs, cor.test(cs$'Political orientation', sub19_change))

#Correlation Awareness of Consequences w. likely PEB
with(cs, cor.test(cs$'Awareness consequences', likelyPEB_mean))
with(cs, cor.test(cs$'Awareness consequences', PEB_activist))
with(cs, cor.test(cs$'Awareness consequences', PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(cs$'Awareness consequences', likelyPEB_mean))), n2 = 13215, r34 = .22)
r.test(n = 88, r12 = (with(cs, cor(cs$'Awareness consequences', PEB_activist))), n2 = 13215, r34 = .22)
r.test(n = 88, r12 = (with(cs, cor(cs$'Awareness consequences', PEB_pragmatist))), n2 = 13215, r34 = .22)

#Correlation Ascription of Responsibility w. likely PEB
with(cs, cor.test(cs$'Ascription responsibility', likelyPEB_mean))
with(cs, cor.test(cs$'Ascription responsibility', PEB_activist))
with(cs, cor.test(cs$'Ascription responsibility', PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(cs$'Ascription responsibility', likelyPEB_mean))), n2 = 4217, r34 = .10)
r.test(n = 88, r12 = (with(cs, cor(cs$'Ascription responsibility', PEB_activist))), n2 = 4217, r34 = .10)
r.test(n = 88, r12 = (with(cs, cor(cs$'Ascription responsibility', PEB_pragmatist))), n2 = 4217, r34 = .10)

#Correlation NEP w. likely PEB
with(cs, cor.test(NEP_mean, likelyPEB_mean))
with(cs, cor.test(NEP_mean, PEB_activist))
with(cs, cor.test(NEP_mean, PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(NEP_mean, likelyPEB_mean))), n2 = 3499, r34 = .09)
r.test(n = 88, r12 = (with(cs, cor(NEP_mean, PEB_activist))), n2 = 3499, r34 = .09)
r.test(n = 88, r12 = (with(cs, cor(NEP_mean, PEB_pragmatist))), n2 = 3499, r34 = .09)

#Correlation Social Norm w. likely PEB
with(cs, cor.test(cs$'Social norm', likelyPEB_mean))
with(cs, cor.test(cs$'Social norm', PEB_activist))
with(cs, cor.test(cs$'Social norm', PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(cs$'Social norm', likelyPEB_mean))), n2 = 14170, r34 = .24)
r.test(n = 88, r12 = (with(cs, cor(cs$'Social norm', PEB_activist))), n2 = 14170, r34 = .24)
r.test(n = 88, r12 = (with(cs, cor(cs$'Social norm', PEB_pragmatist))), n2 = 14170, r34 = .24)

#Correlation Perceived Behavioural Control w. likely PEB
with(cs, cor.test(cs$'PBC', likelyPEB_mean))
with(cs, cor.test(cs$'PBC', PEB_activist))
with(cs, cor.test(cs$'PBC', PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(cs$'PBC', likelyPEB_mean))), n2 = 15020, r34 = .40)
r.test(n = 88, r12 = (with(cs, cor(cs$'PBC', PEB_activist))), n2 = 15020, r34 = .40)
r.test(n = 88, r12 = (with(cs, cor(cs$'PBC', PEB_pragmatist))), n2 = 15020, r34 = .40)

#Correlation Habits w. likely PEB
with(cs, cor.test(cs$'Habit', likelyPEB_mean))
with(cs, cor.test(cs$'Habit', PEB_activist))
with(cs, cor.test(cs$'Habit', PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(cs$'Habit', likelyPEB_mean))), n2 = 7747, r34 = .46)
r.test(n = 88, r12 = (with(cs, cor(cs$'Habit', PEB_activist))), n2 = 7747, r34 = .46)
r.test(n = 88, r12 = (with(cs, cor(cs$'Habit', PEB_pragmatist))), n2 = 7747, r34 = .46)

#Correlation Intentions w. likely PEB
with(cs, cor.test(cs$'Intention', likelyPEB_mean))
with(cs, cor.test(cs$'Intention', PEB_activist))
with(cs, cor.test(cs$'Intention', PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(cs$'Intention', likelyPEB_mean))), n2 = 12945, r34 = .55)
r.test(n = 88, r12 = (with(cs, cor(cs$'Intention', PEB_activist))), n2 = 12945, r34 = .55)
r.test(n = 88, r12 = (with(cs, cor(cs$'Intention', PEB_pragmatist))), n2 = 12945, r34 = .55)

#Correlation Attitudes w. likely PEB
with(cs, cor.test(thermo_moral_mean, likelyPEB_mean))
with(cs, cor.test(thermo_moral_mean, PEB_activist))
with(cs, cor.test(thermo_moral_mean, PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(thermo_moral_mean, likelyPEB_mean))), n2 = 14053, r34 = .36)
r.test(n = 88, r12 = (with(cs, cor(thermo_moral_mean, PEB_activist))), n2 = 14053, r34 = .36)
r.test(n = 88, r12 = (with(cs, cor(thermo_moral_mean, PEB_pragmatist))), n2 = 14053, r34 = .36)

#Correlation BSCS with likelyPEB
with(cs, cor.test(BSCS_mean, likelyPEB_mean))
with(cs, cor.test(BSCS_mean, PEB_activist))
with(cs, cor.test(BSCS_mean, PEB_pragmatist))

#Correlation MAC w. likely PEB
with(cs, cor.test(MAC_mean, likelyPEB_mean))
with(cs, cor.test(MAC_mean, PEB_activist))
with(cs, cor.test(MAC_mean, PEB_pragmatist))
#Correlation MFT w. likely PEB
with(cs, cor.test(MFT_mean, likelyPEB_mean))
with(cs, cor.test(MFT_mean, PEB_activist))
with(cs, cor.test(MFT_mean, PEB_pragmatist))

#Do the whole thing as one big correlation matrix - ACTUAL behaviour

M <- cs %>% select('Political orientation', likelyPEB_mean, PEB_activist, PEB_pragmatist, 'Awareness consequences', 'Ascription responsibility', 
              NEP_mean, EAI_mean, 'Social norm', 'PBC', 'Habit', 'Intention', thermo_moral_mean, BSCS_mean,
              MAC_mean, MFT_mean, thermo_change_excdflt) %>% 
  cor(use = "pairwise.complete.obs")

corrplot(M, method = "number", type = "lower", order = "AOE")

res1 <- cor.mtest(M, conf.level = .95)

p_values_corr <- as.data.frame(res1$p)
low_CI_corr <- as.data.frame(res1$lowCI)
upp_CI_corr <- as.data.frame(res1$uppCI)

corrplot.mixed(M, order = "AOE",lower.col = "black", number.cex = .7, tl.pos = "lt")

write.table(res1$p, file = "p_values.txt", sep = ",", quote = FALSE, row.names = T)

#Correlations with Actual PEB

with(cs, cor.test(cs$'Political orientation', thermo_change_excdflt))
with(cs, cor.test(cs$'Awareness consequences', thermo_change_excdflt))
with(cs, cor.test(cs$'Ascription responsibility', thermo_change_excdflt))
with(cs, cor.test(NEP_mean, thermo_change_excdflt))
with(cs, cor.test(EAI_mean, thermo_change_excdflt))
with(cs, cor.test(cs$'Social norm', thermo_change_excdflt))
with(cs, cor.test(cs$'PBC', thermo_change_excdflt))
with(cs, cor.test(cs$'Habit', thermo_change_excdflt))
with(cs, cor.test(cs$'Intention', thermo_change_excdflt))
with(cs, cor.test(cs$thermo_moral_mean, thermo_change_excdflt))
with(cs, cor.test(BSCS_mean, thermo_change_excdflt))
with(cs, cor.test(MAC_mean, thermo_change_excdflt))
with(cs, cor.test(MFT_mean, thermo_change_excdflt))



#Correlation plot of morality scales with thermo attitude
cs %>% select(MAC_mean, MFT_mean, thermo_moral_mean, likelyPEB_mean) %>% cor(
  ) %>% corrplot(method = "number", type = "lower")

#Paired T-tests for before and after thermostat set temp

#averages of thermo setting before and after INC defaults
mean(irus_data$avg_setpoint_before, na.rm = TRUE)
mean(irus_data$avg_setpoint_after, na.rm = TRUE)
t_inc <- irus_data %>% group_by(site_room) %>% summarise(before = mean(avg_setpoint_before), after = mean(avg_setpoint_after)) 
t.test(t_inc$after,t_inc$before,paired=TRUE, na.rm = TRUE)

#averages of thermo setting before and after EXC defaults
mean(irus_data$avg_sp_before_excdflt, na.rm = TRUE)
mean(irus_data$avg_sp_after_excdflt, na.rm = TRUE)
t_excdflt <- irus_data %>% group_by(site_room) %>%
  summarise(before = mean(avg_sp_before_excdflt), after = mean(avg_sp_after_excdflt)) 
t.test(t_excdflt$after,t_excdflt$before, paired=TRUE)

#Mean external temp before (6.9 celsius) and after (7.0) comms
irus_data %>% ungroup() %>%
  filter(Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) %>%
  summarise(average_before = mean(Ext_temp_celsius))

irus_data %>% ungroup() %>%
  filter(Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01")) %>%
  summarise(average_before = mean(Ext_temp_celsius))


#correlation between thermostat setting and external temperature      

t_external <- irus_data %>% 
  group_by(Date) %>% 
  filter((Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) |
(Date > as.Date("2020-02-14") & Date < as.Date("2020-03-01"))) %>%
  summarise(ext = mean(Ext_temp_celsius), daily = mean(daily_mean_sp_excdflt, na.rm = TRUE))
cor.test(t_external$ext, t_external$daily)


#For each room filter to include only those which include defaults 19 and 21
#Sort by proportion of observations which are default (low propn = high interaction)

irus_data %>%   filter((Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) | (
    Date > as.Date("2020-02-14") & Date < as.Date("2020-02-28"))) %>% 
  filter(Setpoint == 19 | Setpoint == 21) %>%
  group_by(site_room) %>% 
  summarise(propn = length(Setpoint)/672*100) %>%
  arrange(., desc(propn)) %>%
  ggplot () + geom_histogram(aes(x = propn), binwidth = 5) + 
  labs(title = "Level of deference to default settings by room", x =
            "Proportion of observations at default", y = "Number of rooms")

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
    y = "Proportion of observations at default") +
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
#Out of 368k = 73% @ default setting
irus_data %>% filter((Date > as.Date("2020-01-30") & Date < as.Date("2020-02-14")) | (
+     Date > as.Date("2020-02-14") & Date < as.Date("2020-02-28")))

#Hypothesis 5: Differences between Halls

cs %>% filter(Q3 == "CRESCENT") %>%
  summarise(before = mean(avg_sp_before_excdflt), after = mean(avg_sp_after_excdflt))
cs %>% filter(Q3 == "CRESCENT") %>%
  summarise(before = mean(avg_setpoint_before), after = mean(avg_setpoint_after))
cs %>% filter(Q3 == "WARNEFORD") %>%
  summarise(before = mean(avg_sp_before_excdflt), after = mean(avg_sp_after_excdflt))
cs %>% filter(Q3 == "WARNEFORD") %>%
  summarise(before = mean(avg_setpoint_before), after = mean(avg_setpoint_after))

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




#REGRESSION MODELLING

#Model H1a (CADM validation) - PV version
form_H1a <- likelyPEB_mean ~ `Awareness consequences` + `Habit` + `Social norm` + 
  `Ascription responsibility` + `PBC` + `Intention` + NEP_mean
std_model_H1a <- standardize(form_H1a, cs)
model_H1a <- lm(std_model_H1a$formula, std_model_H1a$data)
summary(model_H1a)

#Model H1a (CADM validation) - removed formula line
std_model_H1a <- standardize(likelyPEB_mean ~ `Awareness consequences` + `Habit` + `Social norm` + 
  `Ascription responsibility` + `PBC` + `Intention` + NEP_mean, cs)
model_H1a <- lm(likelyPEB_mean ~ `Awarenessconsequences` + `Habit` + `Socialnorm` + 
  `Ascriptionresponsibility` + `PBC` + `Intention` + NEP_mean, std_model_H1a$data)
summary(model_H1a)




#Gives error
plot_model(model_H1a, type = c("pred"), terms = c(`Habit`))

#Model H1a (CADM validation) - removed formula line + manual standardisation
model_H1a <- lm(scale(cs$likelyPEB_mean) ~ `Awareness consequences` + `Habit` + `Social norm` + 
  `Ascription responsibility` + `PBC` + `Intention` + NEP_mean, cs)
summary(model_H1a)







#Model H1a_act (Activist sub-score)
form_H1a_act <- PEB_activist ~ `Awareness consequences` + `Habit` + `Social norm` + 
  `Ascription responsibility` + `PBC` + `Intention` + NEP_mean
std_model_H1a_act <- standardize(form_H1a_act, cs)
model_H1a_act <- lm(std_model_H1a_act$formula, std_model_H1a_act$data)
summary(model_H1a_act)

#Model H1a_prag (Pragmatist sub-score)
form_H1a_prag <- PEB_pragmatist ~ `Awareness consequences` + `Habit` + `Social norm` + 
  `Ascription responsibility` + `PBC` + `Intention` + NEP_mean
std_model_H1a_prag <- standardize(form_H1a_prag, cs)
model_H1a_prag <- lm(std_model_H1a_prag$formula, std_model_H1a_prag$data)
summary(model_H1a_prag)

#Model H1b
form_H1b <- thermo_change_excdflt ~ `Awareness consequences` + `Habit` + `Social norm` + 
  `Ascription responsibility` + `PBC` + `Intention` + NEP_mean
std_model_H1b <- standardize(form_H1b, cs)
model_H1b <- lm(std_model_H1b$formula, std_model_H1b$data)
summary(model_H1b)

tab_model(model_H1a, model_H1a_act, model_H1a_prag, model_H1b)

#Model H1aD (CADM validation) likely_PEB w. DEMOGRAPHICS
form_H1aD <- likelyPEB_mean ~ Q3 + Q13 + Q34 + Q15 + 
  `Political orientation` + Q17
std_model_H1aD <- standardize(form_H1aD, cs)
model_H1aD <- lm(std_model_H1aD$formula, std_model_H1aD$data)
summary(model_H1aD)

#Model H1bD (CADM validation) thermo_change_excdflt w. DEMOGRAPHICS
form_H1bD <- thermo_change_excdflt ~ Q3 + Q13 + Q34 + Q15 + 
  `Political orientation` + Q17
std_model_H1bD <- standardize(form_H1bD, cs)
model_H1bD <- lm(std_model_H1bD$formula, std_model_H1bD$data)
summary(model_H1bD)

plot_model(model_H1bD)


#HYPOTHESIS 2

#Model 2a: EAI v NEP - claimed PEB
form_H2a <- thermo_moral_mean ~ EAI_mean + NEP_mean
std_model_H2a <- standardize(form_H2a, cs)
model_H2a <- lm(std_model_H2a$formula, std_model_H2a$data)
summary(model_H2a)

plot_model(model_H2a)

#Model 2b: EAI v NEP - actual PEB
form_H2b <- thermo_change_excdflt ~ EAI_mean + NEP_mean
std_model_H2b <- standardize(form_H2b, cs)
model_H2b <- lm(std_model_H2b$formula, std_model_H2b$data)
summary(model_H2b)

tab_model(model_H2a, model_H2b)


#HYPOTHESIS 3
form_H3 <- thermo_change_excdflt ~ BSCS_mean
std_model_H3 <- standardize(form_H3, cs)
model_H3 <- lm(std_model_H3$formula, std_model_H3$data)
summary(model_H3)


#HYPOTHESIS 4

H4 <- data.frame (irus_data %>% 
  group_by(Name, Site) %>% 
  summarise(room_before = mean(avg_setpoint_before, na.rm = TRUE), 
            room_after = mean(avg_setpoint_after), external = mean(Ext_temp_celsius)))

form_H4 <- room_after ~ 1 + room_before + Site + room_before*Site + external
std_model_H4 <- standardize(form_H4, H4)
model_H4 <- lm(std_model_H4$formula, std_model_H4$data)
summary(model_H4)

plot_model(model_H4, type = c("std"))

#Match Room numbers to Time Series data
#Show any duplicate room numbers - there is one duplicate in Crescent - Room L04F
cs %>% group_by(Q4) %>% filter(n()>1) %>% select(Q4)
cs %>% group_by(Q21) %>% filter(n()>1) %>% select(Q21)








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
ggplot (cs) + geom_smooth(method = "lm", se = FALSE, mapping = aes(
  x = thermo_moral_mean, y = EAI_mean), colour = "red") + geom_point(
    mapping = aes(x = thermo_moral_mean, y = EAI_mean), colour = "red") + geom_smooth(
    method = "lm", se = FALSE, mapping = aes(x = thermo_moral_mean, y = NEP_mean), colour = "blue") + 
  geom_point(mapping = aes(x = thermo_moral_mean, y = NEP_mean), colour = "blue", shape = "triangle") +
  labs(x = "Pro-environmental Attitude to Thermostat", y = "General Environmental Attitude Scale (Mean)") +
  annotate(geom = "point", x = -1, y = 2.8, colour = "blue", shape = "triangle", size = 3) + 
  annotate(geom = "text", x = -1, y = 2.8, label = " NEP", hjust = "left", size = 6) +
  annotate(geom = "point", x = -1, y = 2.5, colour = "red", shape = "circle", size = 3) + 
  annotate(geom = "text", x = -1, y = 2.5, label = " EAI", hjust = "left", size = 6)

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
hist(cs$PEB_pragmatist, breaks = 7)


cs %>% ggplot() + geom_point(mapping = aes(
  x = site_room, y = avg_setpoint_before), colour = "blue") + geom_point(mapping = aes(
    x = site_room, y = avg_setpoint_after), colour = "red") + theme(
      axis.text.x = element_text(angle = 90, size = 8))

plotA01A <- irus_data %>% filter(Name == "A01A") %>% ggplot () + ylim(16,24) + geom_point(
    mapping = aes(x = date_time, y = Setpoint))
plotA01B <- irus_data %>% filter(Name == "A01B") %>% ggplot () + ylim(16,24) + geom_point(
    mapping = aes(x = date_time, y = Setpoint))
plotA01C <- irus_data %>% filter(Name == "A01C") %>% ggplot () + ylim(16,24) + geom_point(
    mapping = aes(x = date_time, y = Setpoint)) + ggtitle("Room A01C thermostat settings 30th Jan 2020 - 28th Feb 2020")
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
  colour=ifelse((cs$thermo_change>0), "red", "green"), size = 4) + theme(
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
            Thermostat_exc_dflt = mean(daily_mean_sp_excdflt, na.rm = TRUE),
            Thermostat = mean(Setpoint, na.rm = TRUE)) %>%
  ggplot() + geom_line(aes(x = Date, y = Thermostat_exc_dflt, colour = "Thermostat_exc_dflt")) + 
  geom_line(aes(x = Date, y = External, colour = "External")) + 
  labs(title = "Mean thermostat setting (inc/exc defaults) and external temperature", 
       y = "Temp (celsius)") + 
  geom_line(aes(x = Date, y = Thermostat, colour = "Thermostat"), linetype = "dotted") +
  scale_color_manual(values = c(
    "External" = "royalblue3", "Thermostat" = "black", "Thermostat_exc_dflt" = "red"))
  

