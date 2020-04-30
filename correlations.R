#Correlation analyses

library(tidyverse)
library(data.table)
library(psych)
library(gridExtra)
library(GPArotation)
library(corrplot)

setwd("/home/vanagpau/R/thermostat")
VEB <- fread ("VEB.csv")
as_tibble (VEB)

#Create filter for all completed and non-preview surveys: cs ('completed surveys')
cs <- filter(VEB, Status == "IP Address", Progress == "100")

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


#Coerce list back to tibble
cs <- as_tibble(cs)

#Convert Crescent & Warneford to factors
cs$Q3 <- as.factor(cs$Q3)

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

#Create EAI sub-scales data frame
EAI_subscales <- cs %>% select(EAI1:EAI12)

#Create Activist and Pragmatist variables from likely PEB scale
cs <- cs %>% rowwise() %>% mutate(
  PEB_activist = mean(c(Q7_2, Q7_3, Q7_4, Q7_5, Q7_6, Q7_8, Q7_9, Q7_10, Q7_11), na.rm = TRUE))
cs <- cs %>% rowwise() %>% mutate(
  PEB_pragmatist = mean(c(
    Q7_12, Q7_13, Q7_14, Q7_15, Q7_16, Q7_17, Q7_18, Q7_19, Q7_20, Q7_21, Q7_22), na.rm = TRUE))
cs


#Calculate overall scores for NEP, BSCS, likelyPEB, moral_ascoop, moral_found
#creates vector of means..
cs <- cbind(cs, NEP_mean = rowMeans(NEP, na.rm = TRUE))
cs <- cbind(cs, likelyPEB_mean = rowMeans(likelyPEB, na.rm = TRUE))
cs <- cbind(cs, BSCS_mean = rowMeans(BSCS, na.rm = TRUE))
cs <- cbind(cs, thermo_mean = rowMeans(thermo_attitude, na.rm = TRUE))
cs <- cbind(cs, MAC_mean = rowMeans(moral_ascoop, na.rm = TRUE))
cs <- cbind(cs, MFT_mean = rowMeans(moral_found, na.rm = TRUE))

#Correlations EAI vs NEP
with(cs, cor.test(NEP_mean, likelyPEB_mean))
with(cs, cor.test(EAI_mean, likelyPEB_mean))

#Significance test of EAI vs NEP correlations vs likelyPEB
r.test(r12=(with(cs, cor(EAI_mean, likelyPEB_mean))), n=88, r34=(
  with(cs, cor(NEP_mean, likelyPEB_mean))), n2=88)

#Correlations and significance compared to CADM

#Correlation of political orientation w. likely_PEB
with(cs, cor.test(Q35_1, likelyPEB_mean))
with(cs, cor.test(Q35_1, PEB_activist))
with(cs, cor.test(Q35_1, PEB_pragmatist))

#Correlation Awareness of Consequences w. likely PEB
with(cs, cor.test(Q8_1, likelyPEB_mean))
with(cs, cor.test(Q8_1, PEB_activist))
with(cs, cor.test(Q8_1, PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(Q8_1, likelyPEB_mean))), n2 = 13215, r34 = .22)
r.test(n = 88, r12 = (with(cs, cor(Q8_1, PEB_activist))), n2 = 13215, r34 = .22)
r.test(n = 88, r12 = (with(cs, cor(Q8_1, PEB_pragmatist))), n2 = 13215, r34 = .22)

#Correlation Ascription of Responsibility w. likely PEB
with(cs, cor.test(Q8_4, likelyPEB_mean))
with(cs, cor.test(Q8_4, PEB_activist))
with(cs, cor.test(Q8_4, PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(Q8_4, likelyPEB_mean))), n2 = 4217, r34 = .10)
r.test(n = 88, r12 = (with(cs, cor(Q8_4, PEB_activist))), n2 = 4217, r34 = .10)
r.test(n = 88, r12 = (with(cs, cor(Q8_4, PEB_pragmatist))), n2 = 4217, r34 = .10)

#Correlation NEP w. likely PEB
with(cs, cor.test(NEP_mean, likelyPEB_mean))
with(cs, cor.test(NEP_mean, PEB_activist))
with(cs, cor.test(NEP_mean, PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(NEP_mean, likelyPEB_mean))), n2 = 3499, r34 = .09)
r.test(n = 88, r12 = (with(cs, cor(NEP_mean, PEB_activist))), n2 = 3499, r34 = .09)
r.test(n = 88, r12 = (with(cs, cor(NEP_mean, PEB_pragmatist))), n2 = 3499, r34 = .09)

#Correlation Social Norm w. likely PEB
with(cs, cor.test(Q8_3, likelyPEB_mean))
with(cs, cor.test(Q8_3, PEB_activist))
with(cs, cor.test(Q8_3, PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(Q8_3, likelyPEB_mean))), n2 = 14170, r34 = .24)
r.test(n = 88, r12 = (with(cs, cor(Q8_3, PEB_activist))), n2 = 14170, r34 = .24)
r.test(n = 88, r12 = (with(cs, cor(Q8_3, PEB_pragmatist))), n2 = 14170, r34 = .24)
#Correlation Perceived Behavioural Control w. likely PEB
with(cs, cor.test(Q8_5, likelyPEB_mean))
with(cs, cor.test(Q8_5, PEB_activist))
with(cs, cor.test(Q8_5, PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(Q8_5, likelyPEB_mean))), n2 = 15020, r34 = .40)
r.test(n = 88, r12 = (with(cs, cor(Q8_5, PEB_activist))), n2 = 15020, r34 = .40)
r.test(n = 88, r12 = (with(cs, cor(Q8_5, PEB_pragmatist))), n2 = 15020, r34 = .40)
#Correlation Habits w. likely PEB
with(cs, cor.test(Q8_2, likelyPEB_mean))
with(cs, cor.test(Q8_2, PEB_activist))
with(cs, cor.test(Q8_2, PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(Q8_2, likelyPEB_mean))), n2 = 7747, r34 = .46)
r.test(n = 88, r12 = (with(cs, cor(Q8_2, PEB_activist))), n2 = 7747, r34 = .46)
r.test(n = 88, r12 = (with(cs, cor(Q8_2, PEB_pragmatist))), n2 = 7747, r34 = .46)
#Correlation Intentions w. likely PEB
with(cs, cor.test(Q8_6, likelyPEB_mean))
with(cs, cor.test(Q8_6, PEB_activist))
with(cs, cor.test(Q8_6, PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(Q8_6, likelyPEB_mean))), n2 = 12945, r34 = .55)
r.test(n = 88, r12 = (with(cs, cor(Q8_6, PEB_activist))), n2 = 12945, r34 = .55)
r.test(n = 88, r12 = (with(cs, cor(Q8_6, PEB_pragmatist))), n2 = 12945, r34 = .55)
#Correlation Attitudes w. likely PEB
with(cs, cor.test(thermo_mean, likelyPEB_mean))
with(cs, cor.test(thermo_mean, PEB_activist))
with(cs, cor.test(thermo_mean, PEB_pragmatist))
r.test(n = 88, r12 = (with(cs, cor(thermo_mean, likelyPEB_mean))), n2 = 14053, r34 = .36)
r.test(n = 88, r12 = (with(cs, cor(thermo_mean, PEB_activist))), n2 = 14053, r34 = .36)
r.test(n = 88, r12 = (with(cs, cor(thermo_mean, PEB_pragmatist))), n2 = 14053, r34 = .36)
#Correlation MAC w. likely PEB
with(cs, cor.test(MAC_mean, likelyPEB_mean))
with(cs, cor.test(MAC_mean, PEB_activist))
with(cs, cor.test(MAC_mean, PEB_pragmatist))
#Correlation MFT w. likely PEB
with(cs, cor.test(MFT_mean, likelyPEB_mean))
with(cs, cor.test(MFT_mean, PEB_activist))
with(cs, cor.test(MFT_mean, PEB_pragmatist))

